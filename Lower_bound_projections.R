# ================================================
# KPI Recalibration Projections – Global Fund
# First Version: 27 Jan, by Mikaela Smit
# ================================================

rm(list = ls())

#######################
# Central Switchboard #
#######################

## Set the user
firstrun   = 0                            # If need to install package change to 1
computer   = 1                            # 1 = Mikaela # Add additional computer if needed


# ---- Switchboard for “recent trends” ----
# Decision rule for including 2024 in slope
include_2024_if_within_log_ratio <- 0.10  # ≈ ±10% of pre-COVID trend line at 2024, set to 0 to never include, set to 1 to always include

# Guard-rail behavior:
# if trend over the window suggests increase, use linear; if decline, use exponential
guard_linear_if_increasing <- TRUE  # keep TRUE for your “lower bound” rule

covid_years      <- 2020:2023  # years treated as COVID shocks
norm_year        <- 2020       # year at which the rates will be normalized for the graphs
post_covid_year  <- 2024       # first year of "post-COVID" period
first_year_trend <- 2015       # first year of available data
end_year_trend   <- 2019       # pre_COVID end for slope
end_year_data    <- 2024       # last year with observed data
end_year         <- 2028       # projection horizon
.eps             <- 1e-9       # guard for log(0)


# Define how each disease’s rates should be expressed.
# Multipliers turn raw rated into rates “per X population”.
scaling_table <- tibble::tibble(
  disease = c("hiv", "tb", "malaria"),
  incidence_multiplier = c(1, 1, 1),    # per 1,000 or 100,000
  mortality_multiplier = c(1, 1, 1)   # per 1,000 or 100,000
)


# ========== PACKAGES ==========
if(firstrun>0) {
  install.packages(c("dplyr", "tidyverse", "tidyr", "ggplot2", "patchwork", "readr", "writexl"))
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork) # For side by side comparison
library(readr)
library(writexl)



# ========== LOAD DATA ==========
if (computer ==1){
  setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data")
  output_path = "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Report"
}

# Load the pip data 
df_hiv2     = read.csv("df_partner_hiv_8Jan.csv", stringsAsFactors = FALSE)
df_malaria2 = read.csv("df_partner_malaria_5Jan2025.csv", stringsAsFactors = FALSE)
df_tb2      =  read.csv("df_partner_tb_8Jan.csv", stringsAsFactors = FALSE)

# Load list of countries
df_iso_hiv     = read.csv("List of countries/hiv_iso.csv", stringsAsFactors = FALSE)
df_iso_tb      = read.csv("List of countries/tb_iso_v2.csv", stringsAsFactors = FALSE)
df_iso_malaria = read.csv("List of countries/malaria_iso_v2.csv", stringsAsFactors = FALSE)
df_iso_malaria_portfolio = read.csv("List of countries/malaria_iso_SSA.csv", stringsAsFactors = FALSE)

# List of indicators
list_indc_hiv_pip     = c("ISO3", "Year", "hiv_cases_n_pip", "hiv_deaths_n_pip", "HIVpos_n_pip", 'Population_n_pip', "HIVneg")
list_indc_tb_pip      = c("ISO3", "Year", "tb_cases_n_pip", "tb_deathsnohiv_n_pip", "tb_pop_n_pip")
list_indc_malaria_pip = c("ISO3", "Year", "malaria_cases_n_pip", "malaria_deaths_n_pip", "malaria_par_n_pip")  
list_ind              = c("ISO3", "Year", "incidence", "mortality")

# ========== CLEAN DATA ==========
# Filter out countries we do not need: 
df_hiv2        = filter(df_hiv2, ISO3 %in% df_iso_hiv$ISO3)             # by eligible countries
df_tb2         = filter(df_tb2, ISO3 %in% df_iso_tb$ISO3)               # by eligible countries
df_malaria2    = filter(df_malaria2, ISO3 %in% df_iso_malaria$ISO3)     # by eligible countries
df_malaria2_portfolio    = filter(df_malaria2, ISO3 %in% df_iso_malaria_portfolio$ISO3)     # by eligible countries which is SSA only

# Add HIVneg for hiv
df_hiv2 <- df_hiv2 %>%
  arrange(ISO3, Year) %>%
  group_by(ISO3) %>%
  mutate(HIVneg = Population_n_pip - lag(HIVpos_n_pip)) %>%
  ungroup()

# Filter out indicators we do not need: 
df_hiv     = subset(df_hiv2, select = names(df_hiv2) %in% list_indc_hiv_pip)
df_tb      = subset(df_tb2, select = names(df_tb2) %in% list_indc_tb_pip)
df_malaria = subset(df_malaria2, select = names(df_malaria2) %in% list_indc_malaria_pip)


# Compute incidence and mortality
df_hiv = df_hiv %>%
  mutate(
    incidence = hiv_cases_n_pip / HIVneg,
    mortality = hiv_deaths_n_pip / Population_n_pip,
  )

df_tb = df_tb %>%
  mutate(
    incidence = tb_cases_n_pip / tb_pop_n_pip,
    mortality = tb_deathsnohiv_n_pip / tb_pop_n_pip,
  )

df_malaria = df_malaria %>%
  mutate(
    incidence = malaria_cases_n_pip / malaria_par_n_pip,
    mortality = malaria_deaths_n_pip / malaria_par_n_pip,
  )


# Restrict years and drop extras variables
df_hiv     <- df_hiv     %>% filter(Year >= first_year_trend, Year <= end_year_data) %>% select(any_of(list_ind))
df_tb      <- df_tb      %>% filter(Year >= first_year_trend, Year <= end_year_data) %>% select(any_of(list_ind))
df_malaria <- df_malaria %>% filter(Year >= first_year_trend, Year <= end_year_data) %>% select(any_of(list_ind))



# ========== FUNCTIONS ==========
# Apply scaling of incidence/mortality per disease
apply_scaling <- function(df, disease_name, scaling_table) {
  scale_row <- scaling_table %>% dplyr::filter(disease == tolower(disease_name))
  if (nrow(scale_row) == 0) stop("No scaling info for disease: ", disease_name)
  
  df %>%
    dplyr::mutate(
      value_scaled = dplyr::case_when(
        metric == "incidence" ~ value * scale_row$incidence_multiplier,
        metric == "mortality" ~ value * scale_row$mortality_multiplier,
        TRUE ~ value
      ),
      scale_label = dplyr::case_when(
        metric == "incidence" ~ paste0("per ", formatC(scale_row$incidence_multiplier, format = "d", big.mark = ",")),
        metric == "mortality" ~ paste0("per ", formatC(scale_row$mortality_multiplier, format = "d", big.mark = ",")),
        TRUE ~ ""
      )
    )
}

# Long format + scaling (for lower-bound engine)
df_hiv_long <- df_hiv %>%
  tidyr::pivot_longer(c(incidence, mortality),
                      names_to = "metric", values_to = "value") %>%
  dplyr::mutate(metric = ifelse(metric == "incidence", "incidence", "mortality")) 

df_hiv_long <- apply_scaling(df_hiv_long, "hiv", scaling_table)

df_tb_long <- df_tb %>%
  tidyr::pivot_longer(c(incidence, mortality),
                      names_to = "metric", values_to = "value") %>%
  dplyr::mutate(metric = ifelse(metric == "incidence", "incidence", "mortality")) 

df_tb_long <- apply_scaling(df_tb_long, "tb", scaling_table)

df_malaria_long <- df_malaria %>%
  tidyr::pivot_longer(c(incidence, mortality),
                      names_to = "metric", values_to = "value") %>%
  dplyr::mutate(metric = ifelse(metric == "incidence", "incidence", "mortality"))

df_malaria_long <- apply_scaling(df_malaria_long, "malaria", scaling_table)


# ===================== LOWER-BOUND HELPERS (recent trends) =====================

# Decide which years to use for the slope (pre-COVID years, optionally include 2024)
choose_slope_years <- function(df, first_year_trend, end_year_trend, anchor_year,
                               threshold_log_ratio = 0.10) {
  # df must have: year, value_scaled (already scaled per disease/metric)
  trend_years <- seq(first_year_trend, end_year_trend)
  d_pre <- df %>% dplyr::filter(Year %in% trend_years, is.finite(value_scaled))
  if (nrow(d_pre) < 3) return(trend_years)
  
  fit_pre <- lm(log(value_scaled) ~ Year, data = d_pre)
  pred_anchor <- exp(predict(fit_pre, newdata = data.frame(Year = anchor_year)))
  y_anchor    <- df$value_scaled[df$Year == anchor_year][1]
  
  if (!is.finite(y_anchor) || !is.finite(pred_anchor)) return(trend_years)
  
  log_ratio <- abs(log(y_anchor / pred_anchor))
  if (!is.na(log_ratio) && log_ratio <= threshold_log_ratio) {
    sort(unique(c(trend_years, anchor_year)))  # include 2024 in slope
  } else {
    trend_years                                  # treat 2024 as level shift (anchor only)
  }
}

# Project from the anchor using exponential decline (if beta<0) or linear increase (if beta>0)
project_recent_trend <- function(df, slope_years, anchor_year, end_year,
                                 guard_linear_if_increasing = TRUE) {
  d_slope <- df %>% dplyr::filter(Year %in% slope_years, is.finite(value_scaled))
  if (nrow(d_slope) < 2) return(NULL)
  
  fit_log <- lm(log(value_scaled) ~ Year, data = d_slope)
  beta <- coef(fit_log)["Year"]; if (is.na(beta)) beta <- 0
  
  y_anchor <- df %>% dplyr::filter(Year == anchor_year) %>% dplyr::pull(value_scaled) %>% .[1]
  if (!is.finite(y_anchor)) return(NULL)
  
  use_linear <- guard_linear_if_increasing && (beta > 0)
  
  pred_fun <- if (use_linear) {
    fit_lin <- lm(value_scaled ~ Year, data = d_slope)
    slope_lin <- coef(fit_lin)["Year"]; if (is.na(slope_lin)) slope_lin <- 0
    function(yrs) y_anchor + slope_lin * (yrs - anchor_year)
  } else {
    function(yrs) y_anchor * exp(beta * (yrs - anchor_year))
  }
  
  future_years <- seq(anchor_year + 1, end_year)
  tibble::tibble(
    Year       = c(df$Year[df$Year <= anchor_year], future_years),
    observed   = c(df$value_scaled[df$Year <= anchor_year], rep(NA_real_, length(future_years))),
    projection = c(df$value_scaled[df$Year <= anchor_year], pred_fun(future_years))
  )
}

# Country-level lower bound (expects *long* data with value_scaled)
run_lower_bound_country <- function(df_long,
                                    first_year_trend, end_year_trend,
                                    anchor_year, end_year,
                                    include_2024_threshold = 0.10,
                                    guard_linear_if_increasing = TRUE) {
  
  df_long %>%
    dplyr::group_by(ISO3, metric, scale_label) %>%
    dplyr::group_modify(~{
      d <- .x %>%
        dplyr::arrange(Year) %>%
        dplyr::mutate(
          value_scaled = pmax(value_scaled, .eps),
          value_scaled = ifelse(is.finite(value_scaled), value_scaled, NA_real_)
        ) %>%
        dplyr::filter(!is.na(value_scaled))
      
      # skip if too few valid points
      if (nrow(d) < 3) {
        message("Skipping ", unique(d$ISO3), " / ", unique(d$metric),
                " (insufficient valid data)")
        return(tibble::tibble())
      }
      
      # pick trend window dynamically (based on 2024 proximity)
      slope_yrs <- choose_slope_years(
        d,
        first_year_trend,
        end_year_trend,
        anchor_year,
        threshold_log_ratio = include_2024_threshold
      )
      
      # project using exponential or linear logic
      out <- project_recent_trend(
        d,
        slope_yrs,
        anchor_year,
        end_year,
        guard_linear_if_increasing = guard_linear_if_increasing
      )
      
      if (is.null(out)) return(tibble::tibble())
      out
    }) %>%
    dplyr::ungroup()
}



# ===================== RUN LOWER-BOUND PIPELINE =====================

# Country-level projections (per country × metric)
hiv_lb_country     <- run_lower_bound_country(df_hiv_long,
                                              first_year_trend, end_year_trend,
                                              anchor_year = end_year_data, end_year = end_year,
                                              include_2024_threshold = include_2024_if_within_log_ratio,
                                              guard_linear_if_increasing = guard_linear_if_increasing) %>%
  dplyr::mutate(disease = "hiv")

tb_lb_country      <- run_lower_bound_country(df_tb_long,
                                              first_year_trend, end_year_trend,
                                              anchor_year = end_year_data, end_year = end_year,
                                              include_2024_threshold = include_2024_if_within_log_ratio,
                                              guard_linear_if_increasing = guard_linear_if_increasing) %>%
  dplyr::mutate(disease = "tb")

malaria_lb_country <- run_lower_bound_country(df_malaria_long,
                                              first_year_trend, end_year_trend,
                                              anchor_year = end_year_data, end_year = end_year,
                                              include_2024_threshold = include_2024_if_within_log_ratio,
                                              guard_linear_if_increasing = guard_linear_if_increasing) %>%
  dplyr::mutate(disease = "malaria")

# ---- Helper function to plot one country's incidence + mortality ----
plot_country <- function(country_code, data, first_year_data, end_year) {
  df_c <- data %>% dplyr::filter(ISO3 == country_code)
  
  if (nrow(df_c) == 0) {
    message("No data for ", country_code)
    return(NULL)
  }
  
  # inner helper for each metric
  make_panel <- function(d_metric, line_col, title_suffix) {
    df_panel <- df_c %>% dplyr::filter(metric == d_metric)
    if (nrow(df_panel) == 0) return(NULL)
    
    ggplot(df_panel, aes(x = Year, y = projection)) +
      geom_line(color = line_col, linewidth = 1) +
      geom_point(aes(y = observed), color = "black", size = 1.5) +
      labs(
        title = paste0(country_code, " – ", unique(df_c$disease), " ", title_suffix),
        x = "Year",
        y = unique(df_panel$scale_label)
      ) +
      scale_x_continuous(breaks = seq(first_year_trend, end_year, by = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  p_inc  <- make_panel("incidence", "blue", "Incidence")
  p_mort <- make_panel("mortality", "red",  "Mortality")
  
  # Combine the two side by side if both exist
  if (!is.null(p_inc) && !is.null(p_mort)) {
    p_inc + p_mort + patchwork::plot_layout(ncol = 2)
  } else if (!is.null(p_inc)) {
    p_inc
  } else if (!is.null(p_mort)) {
    p_mort
  } else {
    NULL
  }
}



# ---- Export PDFs (country-level) ----
pdf(file.path(output_path, "HIV_LB_projections_by_country.pdf"), width = 12, height = 6)
for (c in unique(hiv_lb_country$ISO3)) {
  print(plot_country(c, hiv_lb_country, first_year_trend, end_year))
}
dev.off()

pdf(file.path(output_path, "TB_LB_projections_by_country.pdf"), width = 12, height = 6)
for (c in unique(tb_lb_country$ISO3)) {
  print(plot_country(c, tb_lb_country, first_year_trend, end_year))
}
dev.off()

pdf(file.path(output_path, "Malaria_LB_projections_by_country.pdf"), width = 12, height = 6)
for (c in unique(malaria_lb_country$ISO3)) {
  print(plot_country(c, malaria_lb_country, first_year_trend, end_year))
}
dev.off()


# ===================== PORTFOLIO (AGGREGATED) LOWER-BOUND =====================

# 1) Build aggregated long series for a disease (sum numerators/denominators, then rate)
build_agg_long <- function(df_wide, disease_name, first_year_trend, end_year_data) {
  if (disease_name == "hiv") {
    agg <- df_wide %>%
      dplyr::filter(Year >= first_year_trend, Year <= end_year_data) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        cases  = sum(hiv_cases_n_pip, na.rm = TRUE),
        deaths = sum(hiv_deaths_n_pip, na.rm = TRUE),
        hivneg = sum(HIVneg, na.rm = TRUE),
        pop    = sum(Population_n_pip, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::transmute(
        Year,
        incidence = dplyr::if_else(hivneg > 0, cases / hivneg, NA_real_),
        mortality = dplyr::if_else(pop    > 0, deaths / pop,   NA_real_)
      )
  } else if (disease_name == "tb") {
    # Chose the TB deaths 
    agg <- df_wide %>%
      dplyr::filter(Year >= first_year_trend, Year <= end_year_data) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        cases  = sum(tb_cases_n_pip, na.rm = TRUE),
        deaths = sum(tb_deathsnohiv_n_pip, na.rm = TRUE),
        pop    = sum(tb_pop_n_pip, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::transmute(
        Year,
        incidence = dplyr::if_else(pop > 0, cases / pop, NA_real_),
        mortality = dplyr::if_else(pop > 0, deaths / pop, NA_real_)
      )
  } else if (disease_name == "malaria") {
    agg <- df_wide %>%
      dplyr::filter(Year >= first_year_trend, Year <= end_year_data) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        cases  = sum(malaria_cases_n_pip, na.rm = TRUE),
        deaths = sum(malaria_deaths_n_pip, na.rm = TRUE),
        par    = sum(malaria_par_n_pip,   na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::transmute(
        Year,
        incidence = dplyr::if_else(par > 0, cases / par,  NA_real_),
        mortality = dplyr::if_else(par > 0, deaths / par, NA_real_)
      )
  } else {
    stop("Unknown disease: ", disease_name)
  }
  
  # Long + scaling; add dummy 'country' to reuse group_modify()
  agg_long <- agg %>%
    tidyr::pivot_longer(c(incidence, mortality), names_to = "metric", values_to = "value")
  
  agg_long <- apply_scaling(agg_long, disease_name, scaling_table)
    
    agg_long %>%
    dplyr::mutate(country = disease_name)
}

# 2) Apply lower-bound logic to the aggregated long series
run_lower_bound_portfolio <- function(df_wide, disease_name,
                                      first_year_trend, end_year_trend,
                                      anchor_year, end_year,
                                      include_2024_threshold = 0.10,
                                      guard_linear_if_increasing = TRUE) {
  
  agg_long <- build_agg_long(df_wide, disease_name, first_year_trend, end_year_data)
  
  agg_long %>%
    dplyr::group_by(country, metric, scale_label) %>%
    dplyr::group_modify(~{
      d <- .x %>%
        dplyr::arrange(Year) %>%
        dplyr::mutate(
          value_scaled = pmax(value_scaled, .eps),
          value_scaled = dplyr::if_else(is.finite(value_scaled), value_scaled, NA_real_)
        ) %>%
        dplyr::filter(!is.na(value_scaled))
      
      if (nrow(d) < 3) {
        message("Portfolio: insufficient valid data for ", disease_name, " / ", unique(.x$metric))
        return(tibble::tibble())
      }
      
      slope_yrs <- choose_slope_years(
        d,
        first_year_trend,
        end_year_trend,
        anchor_year,
        threshold_log_ratio = include_2024_threshold
      )
      
      out <- project_recent_trend(
        d,
        slope_yrs,
        anchor_year,
        end_year,
        guard_linear_if_increasing = guard_linear_if_increasing
      )
      
      if (is.null(out)) return(tibble::tibble())
      out
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(disease = disease_name)
}



# ---- Portfolio-level (aggregated) ----
hiv_lb_portfolio     <- run_lower_bound_portfolio(df_hiv2, "hiv",
                                                  first_year_trend, end_year_trend,
                                                  anchor_year = end_year_data, end_year = end_year,
                                                  include_2024_threshold = include_2024_if_within_log_ratio,
                                                  guard_linear_if_increasing = guard_linear_if_increasing)
tb_lb_portfolio      <- run_lower_bound_portfolio(df_tb2, "tb",
                                                  first_year_trend, end_year_trend,
                                                  anchor_year = end_year_data, end_year = end_year,
                                                  include_2024_threshold = include_2024_if_within_log_ratio,
                                                  guard_linear_if_increasing = guard_linear_if_increasing)
malaria_lb_portfolio <- run_lower_bound_portfolio(df_malaria2_portfolio, "malaria",
                                                  first_year_trend, end_year_trend,
                                                  anchor_year = end_year_data, end_year = end_year,
                                                  include_2024_threshold = include_2024_if_within_log_ratio,
                                                  guard_linear_if_increasing = guard_linear_if_increasing)

# Export aggregated series for dashboards
readr::write_csv(hiv_lb_portfolio,     file.path(output_path, "HIV_LB_portfolio_projection.csv"))
readr::write_csv(tb_lb_portfolio,      file.path(output_path, "TB_LB_portfolio_projection.csv"))
readr::write_csv(malaria_lb_portfolio, file.path(output_path, "Malaria_LB_portfolio_projection.csv"))

# ==== PORTFOLIO-LEVEL PLOTS ====
plot_portfolio <- function(df, disease_name, start_year, end_year) {
  ggplot(df, aes(x = Year, y = projection)) +
    geom_line(color = "blue", linewidth = 1.2) +
    geom_point(aes(y = observed), color = "black", size = 1.5) +
    facet_wrap(~ metric, scales = "free_y") +
    labs(
      title = paste0(toupper(disease_name), " – Portfolio projection to ", end_year),
      subtitle = "Observed (black) vs. projected (blue)",
      x = "Year",
      y = unique(df$scale_label)
    ) +
    scale_x_continuous(breaks = seq(start_year, end_year, by = 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create and export the portfolio plots as PDF
pdf(file.path(output_path, "Portfolio_LB_Projections.pdf"), width = 10, height = 6)
print(plot_portfolio(hiv_lb_portfolio,     "HIV",      first_year_trend, end_year))
print(plot_portfolio(tb_lb_portfolio,      "TB",       first_year_trend, end_year))
print(plot_portfolio(malaria_lb_portfolio, "Malaria",  first_year_trend, end_year))
dev.off()


# ==== EXPORT COUNTRY-LEVEL LOWER BOUND TO EXCEL ====
write_xlsx(list(
  HIV      = hiv_lb_country,
  TB       = tb_lb_country,
  Malaria  = malaria_lb_country
), path = file.path(output_path, "Country_LB_Projections.xlsx"))


# ===================== COMBINED (ACROSS-DISEASE) PORTFOLIO INDEX =====================

# 1) Bind portfolio series together
portfolio_all <- dplyr::bind_rows(
  dplyr::mutate(hiv_lb_portfolio,     disease = "HIV"),
  dplyr::mutate(tb_lb_portfolio,      disease = "TB"),
  dplyr::mutate(malaria_lb_portfolio, disease = "Malaria")
)

# 2) Helper to build a base-2020=100 index and average across diseases
make_portfolio_index <- function(df, metric_name, base_year = norm_year) {
  df %>%
    dplyr::filter(metric == metric_name) %>%
    # use the full "projection" series (observed through anchor, projected after)
    dplyr::group_by(disease) %>%
    dplyr::mutate(base_val = projection[Year == base_year][1]) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(base_val), base_val > 0) %>%         # drop if no valid base
    dplyr::mutate(index = 100 * projection / base_val) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
      index_mean   = mean(index, na.rm = TRUE),   # simple average across diseases
      n_diseases   = sum(is.finite(index)),       # how many diseases contributed
      .groups = "drop"
    )
}

incidence_index  <- make_portfolio_index(portfolio_all, "incidence", base_year = norm_year)
mortality_index  <- make_portfolio_index(portfolio_all, "mortality", base_year = norm_year)


# 3) Add to your Portfolio PDF (recreate with the combined pages appended)
pdf(file.path(output_path, "Portfolio_LB_Projections.pdf"), width = 10, height = 6)
# Existing disease pages (reprint to include everything in one file)
print(plot_portfolio(hiv_lb_portfolio,     "HIV",     first_year_trend, end_year))
print(plot_portfolio(tb_lb_portfolio,      "TB",      first_year_trend, end_year))
print(plot_portfolio(malaria_lb_portfolio, "Malaria", first_year_trend, end_year))
# New combined index pages
#print(plot_portfolio_index(incidence_index,
#                           "Combined Portfolio Incidence Index (base 2020 = 100)",
#                           first_year_trend, end_year))
#print(plot_portfolio_index(mortality_index,
#                           "Combined Portfolio Mortality Index (base 2020 = 100)",
#                           first_year_trend, end_year))
dev.off()

# 4) Export a portfolio Excel with all sheets (diseases + combined indices)
write_xlsx(
  list(
    HIV_Portfolio      = hiv_lb_portfolio,
    TB_Portfolio       = tb_lb_portfolio,
    Malaria_Portfolio  = malaria_lb_portfolio,
    Combined_Incidence_Index = incidence_index,
    Combined_Mortality_Index = mortality_index),
  path = file.path(output_path, "Portfolio_LB_Projections.xlsx")
)





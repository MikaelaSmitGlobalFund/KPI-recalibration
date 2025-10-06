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

# Parameters
covid_years     <- 2020:2023  # years treated as COVID shocks
post_covid_year <- 2024       # first year of "post-COVID" period
start_year      <- 2015       # first year of available data
end_year_data   <- 2024       # last year with observed data
end_year        <- 2028       # projection horizon
.eps            <- 1e-9       # guard for log(0)


# Define how each disease’s rates should be expressed.
# Multipliers turn raw rated into rates “per X population”.
scaling_table <- tibble::tibble(
  disease = c("hiv", "tb", "malaria"),
  incidence_multiplier = c(1000, 100000, 1000),    # per 1,000 or 100,000
  mortality_multiplier = c(1000, 100000, 100000)   # per 1,000 or 100,000
)

# ========== PACKAGES ==========
if(firstrun>0) {
  install.packages(c("dplyr", "tidyverse", "tidyr", "ggplot2", "patchwork"))
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork) # For side by side comparison


# ========== LOAD DATA ==========
if (computer ==1){
  setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data")
  output_path = "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Report"
}

# Load the pip data 
df_hiv2     = read.csv("df_data_hiv.csv", stringsAsFactors = FALSE)
df_malaria2 = read.csv("df_data_malaria.csv", stringsAsFactors = FALSE)
df_tb2      =  read.csv("df_data_tb.csv", stringsAsFactors = FALSE)

# Load list of countries
df_iso_hiv     = read.csv("List of countries/hiv_iso.csv", stringsAsFactors = FALSE)
df_iso_tb      = read.csv("List of countries/tb_iso_v2.csv", stringsAsFactors = FALSE)
df_iso_malaria = read.csv("List of countries/malaria_iso_v2.csv", stringsAsFactors = FALSE)

# List of indicators
list_indc_hiv_pip     = c("country", "year", "cases_central", "deaths_central", "incidence_central", "mortality_central", "hivneg_central", 'plhiv_central', "population_central")
list_indc_tb_pip      = c("country", "year", "cases_central", "deaths_central", "incidence_central", "mortality_central", "population_central")
list_indc_malaria_pip = c("country", "year", "cases_central", "deaths_central", "incidence_central", "mortality_central", "par_central")  
list_ind              = c("country", "year", "incidence_central", "mortality_central")

# ========== CLEAN DATA ==========
# Filter out countries we do not need: 
df_hiv2        = filter(df_hiv2, country %in% df_iso_hiv$ISO3)             # by eligible countries
df_tb2         = filter(df_tb2, country %in% df_iso_tb$ISO3)               # by eligible countries
df_malaria2    = filter(df_malaria2, country %in% df_iso_malaria$ISO3)     # by eligible countries

# Filter out indicators we do not need: 
df_hiv     = subset(df_hiv2, select = names(df_hiv2) %in% list_indc_hiv_pip)
df_tb      = subset(df_tb2, select = names(df_tb2) %in% list_indc_tb_pip)
df_malaria = subset(df_malaria2, select = names(df_malaria2) %in% list_indc_malaria_pip)

# Make incidence and mortality


# Restrict years and drop extras variables
df_hiv     <- df_hiv     %>% filter(year >= start_year, year <= end_year_data) %>% select(any_of(list_ind))
df_tb      <- df_tb      %>% filter(year >= start_year, year <= end_year_data) %>% select(any_of(list_ind))
df_malaria <- df_malaria %>% filter(year >= start_year, year <= end_year_data) %>% select(any_of(list_ind))


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


# ========== FIT MODEL ==========
# Fit and forecast for one country × metric
fit_projection <- function(df, covid_years, post_covid_year, end_year) {
  df <- df %>%
    mutate(
      value  = pmax(value_scaled, .eps),
      year_c = year - mean(year, na.rm = TRUE),
      post   = as.integer(year >= post_covid_year)
    )
  
  # Add COVID dummies
  for (y in covid_years) {
    df[[paste0("covid_", y)]] <- as.integer(df$year == y)
  }
  
  # Build formula
  covid_terms <- paste0("covid_", covid_years)
  form <- as.formula(
    paste("log(value) ~ year_c + post +", paste(covid_terms, collapse = " + "))
  )
  fit <- lm(form, data = df)
  
  # Future years
  new_years <- (max(df$year, na.rm = TRUE) + 1):end_year
  if (length(new_years) == 0) {
    return(df %>% mutate(projection = value, observed = value))
  }
  
  newd <- tibble(
    year   = new_years,
    year_c = new_years - mean(df$year, na.rm = TRUE),
    post   = as.integer(new_years >= post_covid_year)
  )
  for (y in covid_years) {
    newd[[paste0("covid_", y)]] <- 0L
  }
  
  preds <- predict(fit, newdata = newd, se.fit = TRUE)
  
  bind_rows(
    df %>% transmute(year, projection = value, observed = value),
    tibble(year = newd$year,
           projection = exp(preds$fit),
           observed = NA_real_)
  )
}

# Unified processing pipeline for any disease
process_disease <- function(df, disease_name,
                            covid_years,
                            post_covid_year,
                            end_year,
                            scaling_table) {
  message("Processing ", toupper(disease_name))
  # Long format + scaling
  df_long <- df %>%
    pivot_longer(cols = c(incidence_central, mortality_central),
                 names_to = "metric", values_to = "value") %>%
    mutate(metric = ifelse(metric == "incidence_central", "incidence", "mortality")) %>%
    apply_scaling(disease_name, scaling_table)
  
  # Fit + forecast per country × metric
  df_proj <- df_long %>%
    group_by(country, metric, scale_label) %>%
    group_modify(~ fit_projection(.x,
                                  covid_years = covid_years,
                                  post_covid_year = post_covid_year,
                                  end_year = end_year)) %>%
    ungroup() %>%
    mutate(disease = disease_name)
  
  df_proj
}

# Generic country plot: incidence (blue) + mortality (red)
plot_country <- function(country_code, data, start_year, end_year) {
  df_c <- data %>% filter(country == country_code)
  
  make_panel <- function(d_metric, line_col, title_suffix) {
    df_panel <- df_c %>% filter(metric == d_metric)
    ggplot(df_panel, aes(x = year, y = projection)) +
      geom_line(color = line_col) +
      geom_point(aes(y = observed), color = "black") +
      labs(
        title = paste0(country_code, " – ", unique(df_c$disease), " ", title_suffix),
        x = "Year", y = unique(df_panel$scale_label)
      ) +
      scale_x_continuous(breaks = seq(start_year, end_year, by = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  p_inc  <- make_panel("incidence", "blue", "Incidence")
  p_mort <- make_panel("mortality", "red",  "Mortality")
  p_inc + p_mort + patchwork::plot_layout(ncol = 2)
}


# ========== RUN PIPELINE ==========
# Build projections
hiv_proj <- process_disease(df_hiv, "hiv",
                            covid_years = covid_years,
                            post_covid_year = post_covid_year,
                            end_year = end_year,
                            scaling_table = scaling_table)
tb_proj <- process_disease(df_tb, "tb",
                            covid_years = covid_years,
                            post_covid_year = post_covid_year,
                            end_year = end_year,
                            scaling_table = scaling_table)
malaria_proj <- process_disease(df_malaria, "malaria",
                            covid_years = covid_years,
                            post_covid_year = post_covid_year,
                            end_year = end_year,
                            scaling_table = scaling_table)

# (Optional) combine for cross-disease views
all_proj <- bind_rows(hiv_proj, tb_proj, malaria_proj)

# ========== EXPORT PDFs ==========
# HIV
pdf(file.path(output_path, "HIV_projections_by_country.pdf"), width = 12, height = 6)
for (c in unique(hiv_proj$country)) {
  print(plot_country(c, hiv_proj, start_year, end_year))
}
dev.off()

# TB
pdf(file.path(output_path, "TB_projections_by_country.pdf"), width = 12, height = 6)
for (c in unique(tb_proj$country)) {
  print(plot_country(c, tb_proj, start_year, end_year))
}
dev.off()

# Malaria
pdf(file.path(output_path, "Malaria_projections_by_country.pdf"), width = 12, height = 6)
for (c in unique(malaria_proj$country)) {
  print(plot_country(c, malaria_proj, start_year, end_year))
}
dev.off()
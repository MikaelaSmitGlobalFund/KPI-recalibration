# First Version: 26th January 2026 by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code generates all data for KPI reporting (country and portfolio level)
# It also generated the "distance to gap" for the Allocation Team
# Thr Central switchboard provides all options to define parameters for reporting
# Specifically, the output includes at country-level 1. baseline data on cases/deaths and incidence/mortality; 
# 2. share to gap to SDG which is the gap between SDG projections in 2028 (and for Allocation Team 2030) and 
# the projections of cases/deaths based on continuation of recent trends and 
# 3. variation in rate between 2021 baseline and latest partner year we are reporting on
# At portfolio level it contain: 1. Disease-specific and combined projected rates in 2028

rm(list = ls())

#######################
# Central Switchboard #
#######################

## Set the user
firstrun   = 0                            # If need to install package change to 1
computer   = 1                            # 1 = Mikaela # Add additional computer if needed

# ========== SET YEAR PREFERENCES ==========
start_year_min       = 2010                 # This cuts off the first year and removes years were HIVneg with the lag are wrong
end_year_data        = 2024                 # This is the year of latest partner data
end_year_sdg         = 2030                 # This is the final year of prediction
start_year_reporting = 2021                 # This year is the base year of reporting, in this case 2021
reporting_year       = 2026                 # This will appear in the final csv and should be the years we are reporitng in
end_year_reporting   = 2028

start_year_gp_hiv    = 2020
start_year_gp_tm     = 2015

norm_year            = 2020       # year at which the rates will be normalized for the graphs
.eps                 = 1e-9       # guard for log(0)


# ========== SET PARAMETERS FOR PROJECTIONS ==========
# 1. Decision rule for including 2024 in slope
include_2024_if_within_log_ratio <- 0.1  # ≈ ±10% of pre-COVID trend line at 2024, set to 0 to never include, set to 1 to always include

# 2. Guard-rail behavior:
# if trend over the window suggests increase, use linear; if decline, use exponential
guard_linear_if_increasing <- TRUE  # keep TRUE for your “lower bound” rule

# 3. Decide which years to use to establish slope by disease
trend_windows <- list(
  hiv     = list(censor = FALSE,  start = 2014, end = 2019, recent_start = 2015, recent_end = 2024),
  tb      = list(censor = TRUE,  start = 2010, end = 2019, recent_start = 2018, recent_end = 2023),
  malaria = list(censor = TRUE,  start = 2010, end = 2019, recent_start = 2018, recent_end = 2023)
)

covid_years     = 2020:2023  # years treated as COVID shocks
post_covid_year = 2024       # first year of "post-COVID" period

# ========== SET TB DEATH HIV STATUS ==========
# TB country reporting choice
tb_death_country    = "deaths"        # options: "deaths" or "deathshivneg"
tb_mort_country     = "mortality"     # options: "mortality" or "mortality_hivneg"
tb_deaths_portfolio = "deathshivneg"  # or "deaths"
tb_mort_portfolio   = "mortality_hivneg"  # or "mortality"



# ========== SET LIBRARIES ==========
if(firstrun>0) {
  install.packages("dplyr")
  install.packages("data.table")
  installed.packages("tidyverse")
  install.packages(c("tidyr", "ggplot2", "patchwork", "readr", "writexl"))
}

library(dplyr) # require instead
library(data.table)   # For like function (%like%)
library(tidyr)
library(ggplot2)
library(patchwork) # For side by side comparison
library(readr)
library(writexl)


# ========== LOAD DATA AND DEFINE INDICATORS AND EXCEPTIONS ==========
if (computer ==1){
  setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data")
  output_path = "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/ROutput"
}
  
# Load the pip data 
df_hiv2      = read.csv("df_partner_hiv_8Jan.csv", stringsAsFactors = FALSE)
df_malaria2  = read.csv("df_partner_malaria_5Jan2025.csv", stringsAsFactors = FALSE)
df_tb2       =  read.csv("df_partner_tb_8Jan.csv", stringsAsFactors = FALSE)

# Load GP data
df_hiv_gp2     = read.csv("hiv_gp_for_kpi.csv", stringsAsFactors = FALSE)
df_tb_gp2      = read.csv("tb_gp_for_kpi_v2.csv", stringsAsFactors = FALSE)
df_malaria_gp2 = read.csv("malaria_gp_for_kpi.csv", stringsAsFactors = FALSE)

# Load list of countries
df_iso_hiv     = read.csv("List of countries/hiv_iso.csv", stringsAsFactors = FALSE)
df_iso_tb      = read.csv("List of countries/tb_iso_v2.csv", stringsAsFactors = FALSE)
df_iso_malaria = read.csv("List of countries/malaria_iso_v2.csv", stringsAsFactors = FALSE)
df_iso_hiv_portfolio    = read.csv("List of countries/hiv_iso_portfolio.csv", stringsAsFactors = FALSE)
df_iso_malaria_portfolio = read.csv("List of countries/malaria_iso_SSA.csv", stringsAsFactors = FALSE)

# List of indicators
list_indc_hiv_pip     = c("ISO3", "Year", "cases", "deaths", "HIVpos_n_pip", "Population_n_pip", "HIVneg", "incidence", "mortality")
list_indc_tb_pip      = c("ISO3", "Year", "cases", "deaths", "deathshivneg", "tb_pop_n_pip","incidence", "mortality", "mortality_hivneg")
list_indc_malaria_pip = c("ISO3", "Year", "cases", "deaths", "malaria_par_n_pip","incidence", "mortality")  

# data that needs to be removed because nto public for hiv - map countries and indicator
countries_to_na_aids_deaths <- c("CAF", "CMR", "GNQ", "KAZ", "MDV", "PRK", "STP", "UKR", "VEN")
countries_to_na_hiv_inc <- c("CAF", "CMR", "GNQ", "MDG", "MDV", "PAK", "PRK", "SOM", "STP", "UKR")


# ========== CLEAN PARTNER DATA ==========
# Order Partner data by country alphabetically
df_hiv2     = df_hiv2[order(df_hiv2$ISO3),]
df_tb2      = df_tb2[order(df_tb2$ISO3),]
df_malaria2 = df_malaria2[order(df_malaria2$ISO3),]


# Add HIVneg for hiv
df_hiv2 <- df_hiv2 %>%
  arrange(ISO3, Year) %>%
  group_by(ISO3) %>%
  mutate(HIVneg = lag(Population_n_pip) - lag(HIVpos_n_pip)) %>%
  ungroup()

# Compute incidence and mortality
df_hiv2 = df_hiv2 %>%
  mutate(
    cases  = hiv_cases_n_pip,
    deaths = hiv_deaths_n_pip,
    incidence = hiv_cases_n_pip / HIVneg,
    mortality = hiv_deaths_n_pip / Population_n_pip,
  )

df_tb2 = df_tb2 %>%
  mutate(
    cases        = tb_cases_n_pip,
    deaths       = tb_deaths_n_pip,
    deathshivneg = tb_deathsnohiv_n_pip, 
    incidence = tb_cases_n_pip / tb_pop_n_pip,
    mortality = tb_deaths_n_pip / tb_pop_n_pip,
    mortality_hivneg = tb_deathsnohiv_n_pip / tb_pop_n_pip,
  )

df_malaria2 = df_malaria2 %>%
  mutate(
    cases  = malaria_cases_n_pip,
    deaths = malaria_deaths_n_pip, 
    incidence = malaria_cases_n_pip / malaria_par_n_pip,
    mortality = malaria_deaths_n_pip / malaria_par_n_pip,
  )


# Filter out indicators we do not need: 
df_hiv     = subset(df_hiv2, select = names(df_hiv2) %in% list_indc_hiv_pip)
df_tb      = subset(df_tb2, select = names(df_tb2) %in% list_indc_tb_pip)
df_malaria = subset(df_malaria2, select = names(df_malaria2) %in% list_indc_malaria_pip)

# Filter out years we do not need (which will also remove hivneg that are wrong in first year per country)
df_hiv      = df_hiv %>% filter(Year>=start_year_min)
df_tb       = df_tb %>% filter(Year>=start_year_min)
df_malaria  = df_malaria %>% filter(Year>=start_year_min)

# Filter out countries we do not need: 
df_hiv       = filter(df_hiv, ISO3 %in% df_iso_hiv$ISO3)                    # by eligible countries
df_tb        = filter(df_tb, ISO3 %in% df_iso_tb$ISO3)                      # by eligible countries
df_malaria   = filter(df_malaria, ISO3 %in% df_iso_malaria$ISO3)            # by eligible countries



# ========== CLEAN GP DATA ==========
# Order GP data by country alphabetically
df_hiv_gp2     = df_hiv_gp2[order(df_hiv_gp2$country),]
df_tb_gp2      = df_tb_gp2[order(df_tb_gp2$ISO3),]
df_malaria_gp2 = df_malaria_gp2[order(df_malaria_gp2$ISO3),]

# Filter GP df to key indicators
#df_tb_gp      = subset(df_tb_gp, select = c(ISO3, Year, Cases, Deaths, Population))
df_malaria_gp2 = subset(df_malaria_gp2, select = -c(country))

# Filter out countries we do not need: 
df_hiv_gp2       = filter(df_hiv_gp2, country %in% df_iso_hiv_portfolio$ISO3)                    # by eligible countries
df_tb_gp2        = filter(df_tb_gp2, ISO3 %in% df_iso_tb$ISO3)                      # by eligible countries
df_malaria_gp2   = filter(df_malaria_gp2, ISO3 %in% df_iso_malaria$ISO3)            # by eligible countries

# Rename variables
df_hiv_gp2 <- df_hiv_gp2 %>%
  mutate(
    HIVneg_sdg = cases / incidence,
    Population_sdg = deaths / mortality
  )%>%
  rename(
    ISO3          = country,
    Year          = year,
    cases_sdg     = cases,
    deaths_sdg    = deaths,
  )%>%
  select(ISO3, Year, cases_sdg, deaths_sdg, HIVneg_sdg, Population_sdg)

df_tb_gp2 <- df_tb_gp2 %>% 
  rename(
    cases_sdg  = Cases,
    deaths_ori_sdg = Deaths
  ) %>%
  select(ISO3, Year, cases_sdg, deaths_ori_sdg, Population)

df_malaria_gp2 <- df_malaria_gp2 %>%
  mutate(
    par_sdg = ((cases / incidence) + (deaths / mortality))/2
  )%>%
  rename(
    Year          = year,
    cases_sdg     = cases,
    deaths_sdg    = deaths,
  )%>%
  select(ISO3, Year, cases_sdg, deaths_sdg, par_sdg)


# Make clean subset of pip data
df_hiv_baseline <- df_hiv %>%
  filter(Year == start_year_gp_hiv) %>%
  select(ISO3, cases, deaths, HIVneg, Population_n_pip)

df_tb_baseline <- df_tb %>%
  filter(Year == start_year_gp_tm) %>%
  select(ISO3, cases, deaths, deathshivneg, tb_pop_n_pip)

df_malaria_baseline <- df_malaria %>%
  filter(Year == start_year_gp_tm) %>%
  select(ISO3, cases, deaths, malaria_par_n_pip)


# Compute ratio at baseline
df_hiv_gp = df_hiv_gp2
df_tb_gp = df_tb_gp2
df_malaria_gp = df_malaria_gp2

df_hiv_gp_baseline = df_hiv_gp %>% filter(Year==start_year_gp_hiv)
df_hiv_gp_baseline = df_hiv_gp_baseline %>% left_join(df_hiv_baseline, by='ISO3')
df_hiv_gp_baseline = df_hiv_gp_baseline %>%
  mutate(
    ratio_cases      = (cases/cases_sdg),
    ratio_deaths     = (deaths/deaths_sdg),
    ratio_hivneg     = (HIVneg/HIVneg_sdg),
    ratio_population = (Population_n_pip/Population_sdg)
  )
df_hiv_gp_baseline = subset(df_hiv_gp_baseline, select = c(ISO3, ratio_cases, ratio_deaths, ratio_hivneg, ratio_population))

df_tb_gp_baseline = df_tb_gp %>% filter(Year==start_year_gp_tm)
df_tb_gp_baseline = df_tb_gp_baseline %>% left_join(df_tb_baseline, by='ISO3')
df_tb_gp_baseline = df_tb_gp_baseline %>%
  mutate(
    ratio_cases      = (cases/cases_sdg),
    ratio_deaths     = (deaths/deaths_ori_sdg),
    ratio_deathshivneg = (deathshivneg/deaths_ori_sdg), # We do not have hivneg deaths, we use scaling to get it
    ratio_population = (tb_pop_n_pip/Population)
  )
df_tb_gp_baseline = subset(df_tb_gp_baseline, select = c(ISO3, ratio_cases, ratio_deaths, ratio_deathshivneg, ratio_population))

df_malaria_gp_baseline = df_malaria_gp %>% filter(Year==start_year_gp_tm)
df_malaria_gp_baseline = df_malaria_gp_baseline %>% left_join(df_malaria_baseline, by='ISO3')
df_malaria_gp_baseline = df_malaria_gp_baseline %>%
  mutate(
    ratio_cases     = (cases/cases_sdg),
    ratio_deaths    = (deaths/deaths_sdg),
    ratio_par       = (malaria_par_n_pip/par_sdg)
  )
df_malaria_gp_baseline = subset(df_malaria_gp_baseline, select = c(ISO3, ratio_cases, ratio_deaths, ratio_par))


# Merge pip and gp data and make baseline adjustment
df_hiv_gp = df_hiv_gp %>% left_join(df_hiv_gp_baseline, by='ISO3')
df_hiv_gp = df_hiv_gp %>%
  mutate(
    cases_sdg      = cases_sdg *ratio_cases,
    deaths_sdg     = deaths_sdg*ratio_deaths,
    HIVneg_sdg     = HIVneg_sdg*ratio_hivneg,
    Population_sdg = Population_sdg*ratio_population
  )

df_tb_gp = df_tb_gp %>% left_join(df_tb_gp_baseline, by='ISO3')
df_tb_gp = df_tb_gp %>%
  mutate(
    cases_sdg     = (cases_sdg *ratio_cases),
    deaths_sdg    = (deaths_ori_sdg*ratio_deaths),
    deathshivneg_sdg = (deaths_ori_sdg*ratio_deathshivneg),
    Population_sdg= (Population*ratio_population)
  )

df_malaria_gp = df_malaria_gp %>% left_join(df_malaria_gp_baseline, by='ISO3')
df_malaria_gp = df_malaria_gp %>%
  mutate(
    cases_sdg  = (cases_sdg *ratio_cases),
    deaths_sdg = (deaths_sdg*ratio_deaths),
    par_sdg    = (par_sdg*ratio_par)
  )


# Filter for 2030 in case needed
df_hiv_gp_2030     = df_hiv_gp %>% filter(Year==end_year_sdg)
df_tb_gp_2030      = df_tb_gp %>% filter(Year==end_year_sdg)
df_malaria_gp_2030 = df_malaria_gp %>% filter(Year==end_year_sdg)

#Keep only data we need
df_hiv_gp_2030     = subset(df_hiv_gp_2030, select = -c(Year, ratio_cases, ratio_deaths, ratio_hivneg, ratio_population))
df_tb_gp_2030      = subset(df_tb_gp_2030, select = -c(Year, deaths_ori_sdg, Population, ratio_cases, ratio_deaths, ratio_deathshivneg, ratio_population))
df_malaria_gp_2030 = subset(df_malaria_gp_2030, select = -c(Year, ratio_cases, ratio_deaths, ratio_par))


# Filter for the reporting year
df_hiv_gp_rep     = df_hiv_gp %>% filter(Year==end_year_reporting)
df_tb_gp_rep      = df_tb_gp %>% filter(Year==end_year_reporting)
df_malaria_gp_rep = df_malaria_gp %>% filter(Year==end_year_reporting)

#Keep only data we need
df_hiv_gp_rep     = subset(df_hiv_gp_rep, select = -c(Year, ratio_cases, ratio_deaths, ratio_hivneg, ratio_population))
df_tb_gp_rep      = subset(df_tb_gp_rep, select = -c(Year, deaths_ori_sdg, Population, ratio_cases, ratio_deaths, ratio_deathshivneg, ratio_population))
df_malaria_gp_rep = subset(df_malaria_gp_rep, select = -c(Year, ratio_cases, ratio_deaths, ratio_par))


#Keep only data we need for full time series
df_hiv_gp     = subset(df_hiv_gp, select = -c(ratio_cases, ratio_deaths, ratio_hivneg, ratio_population))
df_tb_gp      = subset(df_tb_gp, select = -c(deaths_ori_sdg, Population, ratio_cases, ratio_deaths, ratio_deathshivneg, ratio_population))
df_malaria_gp = subset(df_malaria_gp, select = -c(ratio_cases, ratio_deaths, ratio_par))


# =========================================================
# FUNCTIONS: lower-bound projection engine + plotting
# =========================================================

# Decide which years to use for the slope (pre-COVID years, optionally include 2024)
choose_slope_years <- function(df,
                               disease_name,
                               anchor_year,
                               threshold_log_ratio = include_2024_if_within_log_ratio,
                               covid_years = covid_years) {
  
  cfg <- trend_windows[[tolower(disease_name)]]
  if (is.null(cfg)) stop("No trend window config for disease: ", disease_name)
  
  # Pick the slope years depending on censor switch
  if (isTRUE(cfg$censor)) {
    trend_years <- seq(cfg$start, cfg$end)                # e.g. 2014–2019
    trend_years <- setdiff(trend_years, covid_years)      # safety (no-op here)
  } else {
    trend_years <- seq(cfg$recent_start, cfg$recent_end)  # e.g. 2018–2023
  }
  
  d_pre <- df %>%
    dplyr::filter(Year %in% trend_years, is.finite(value_scaled)) %>%
    dplyr::mutate(value_scaled = pmax(value_scaled, .eps))
  if (nrow(d_pre) < 3) return(trend_years)
  
  fit_pre <- lm(log(value_scaled) ~ Year, data = d_pre)
  pred_anchor <- exp(predict(fit_pre, newdata = data.frame(Year = anchor_year)))
  y_anchor <- df$value_scaled[df$Year == anchor_year][1]
  
  if (!is.finite(y_anchor) || !is.finite(pred_anchor)) return(trend_years)
  
  log_ratio <- abs(log(y_anchor / pred_anchor))
  if (!is.na(log_ratio) && log_ratio <= threshold_log_ratio) {
    sort(unique(c(trend_years, anchor_year)))  # optionally include anchor in slope
  } else {
    trend_years
  }
}

# Project from the anchor using exponential decline (if beta<0) or linear increase (if beta>0)
project_recent_trend <- function(df, slope_years, anchor_year, end_year,
                                 guard_linear_if_increasing = TRUE) {
  d_slope <- df %>%
    dplyr::filter(Year %in% slope_years, is.finite(value_scaled)) %>%
    dplyr::mutate(value_scaled = pmax(value_scaled, .eps))
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

# Long format (for lower-bound engine)
make_long_rates <- function(df) {
  df %>%
    tidyr::pivot_longer(c(incidence, mortality), names_to = "metric", values_to = "value") %>%
    dplyr::mutate(
      value_scaled = value,
      scale_label = metric
    )
}

# Country-level lower bound (expects *long* data with value_scaled)
run_lower_bound_country <- function(df_long,
                                    disease_name,
                                    anchor_year, end_year,
                                    include_2024_threshold = include_2024_if_within_log_ratio,
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
      
      if (nrow(d) < 3) return(tibble::tibble())
      
      slope_yrs <- choose_slope_years(
        d,
        disease_name = disease_name,
        anchor_year = anchor_year,
        threshold_log_ratio = include_2024_threshold,
        covid_years = covid_years
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
    dplyr::ungroup()
}

# ---- Helper function to plot one country's incidence + mortality ----
# ---- Helper function to plot one country's metrics (cases/deaths/deathshivneg/etc.) ----
plot_country <- function(country_code, data, first_year_data, end_year) {
  df_c <- data %>% dplyr::filter(ISO3 == country_code)
  
  if (nrow(df_c) == 0) {
    message("No data for ", country_code)
    return(NULL)
  }
  
  metrics <- unique(df_c$metric)
  
  make_panel <- function(m) {
    df_panel <- df_c %>% dplyr::filter(metric == m) %>% dplyr::arrange(Year)
    if (nrow(df_panel) == 0) return(NULL)
    
    # safer y label
    y_lab <- df_panel$scale_label[which(!is.na(df_panel$scale_label))[1]]
    if (is.na(y_lab) || length(y_lab) == 0) y_lab <- m
    
    ggplot2::ggplot(df_panel, ggplot2::aes(x = Year, y = projection)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(y = observed), size = 1.5) +
      ggplot2::labs(
        title = paste0(country_code, " – ", unique(df_c$disease), " – ", m),
        x = "Year",
        y = y_lab
      ) +
      ggplot2::scale_x_continuous(breaks = seq(first_year_data, end_year, by = 1)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }
  
  plots <- lapply(metrics, make_panel)
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)
  
  patchwork::wrap_plots(plots, ncol = min(2, length(plots)))
}


# ===================== PORTFOLIO (AGGREGATED) LOWER-BOUND =====================

# 1) Build aggregated long series for a disease (sum numerators/denominators, then rate)
build_agg_long <- function(df_wide, disease_name, end_year_data) {
  if (disease_name == "hiv") {
    agg <- df_wide %>%
      dplyr::filter(Year <= end_year_data) %>%
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
      dplyr::filter(Year <= end_year_data) %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(
        cases  = sum(tb_cases_n_pip, na.rm = TRUE),
        deaths = sum(.data[[tb_deaths_portfolio]], na.rm = TRUE),
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
      dplyr::filter(Year <= end_year_data) %>%
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
  
  # Long; add dummy 'country' to reuse group_modify()
  agg_long <- agg %>%
    tidyr::pivot_longer(c(incidence, mortality), names_to = "metric", values_to = "value") %>%
    dplyr::mutate(
      value_scaled = value,
      scale_label  = metric,
      country = disease_name
    )
  
  return(agg_long)
}

# 2) Apply lower-bound logic to the aggregated long series
run_lower_bound_portfolio <- function(df_wide, disease_name,
                                      anchor_year, end_year,
                                      include_2024_threshold = include_2024_if_within_log_ratio,
                                      guard_linear_if_increasing = TRUE) {
  
  agg_long <- build_agg_long(df_wide, disease_name, end_year_data)
  
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
        disease_name = disease_name,
        anchor_year = anchor_year,
        threshold_log_ratio = include_2024_threshold,
        covid_years = covid_years
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


# =========================================================
# COUNTRY-LEVEL DATA: create country-level csv and plots
# =========================================================

# ===================== MAKE PROJECTIONS ==================

# Make df long for use in helper functions 
df_hiv_long <- df_hiv %>%
  pivot_longer(c(cases, deaths), names_to = "metric", values_to = "value") %>%
  mutate(value_scaled = value, scale_label = metric)


df_tb_long <- df_tb %>%
  pivot_longer(c(cases, deaths, deathshivneg), names_to = "metric", values_to = "value") %>%
  mutate(value_scaled = value, scale_label = metric)


df_malaria_long <- df_malaria %>%
  pivot_longer(c(cases, deaths), names_to = "metric", values_to = "value") %>%
  mutate(value_scaled = value, scale_label = metric)



# Make projections
df_hiv_lb <- run_lower_bound_country(df_hiv_long, "hiv",
                                     anchor_year = end_year_data,
                                     end_year    = end_year_reporting,
                                     include_2024_threshold     = include_2024_if_within_log_ratio,
                                     guard_linear_if_increasing = guard_linear_if_increasing) %>%
  dplyr::mutate(disease = "hiv")

df_tb_lb  <- run_lower_bound_country(df_tb_long, "tb",
                                     anchor_year = end_year_data,
                                     end_year    = end_year_reporting,
                                     include_2024_threshold     = include_2024_if_within_log_ratio,
                                     guard_linear_if_increasing = guard_linear_if_increasing) %>%
  dplyr::mutate(disease = "tb")

df_mal_lb <- run_lower_bound_country(df_malaria_long, "malaria",
                                     anchor_year = end_year_data,
                                     end_year    = end_year_reporting,
                                     include_2024_threshold     = include_2024_if_within_log_ratio,
                                     guard_linear_if_increasing = guard_linear_if_increasing) %>%
  dplyr::mutate(disease = "malaria")


# Plot the country level projections for sanity 
pdf(file.path(output_path, "HIV_LB_projections_by_country.pdf"), width = 12, height = 6)
for (c in unique(df_hiv_lb$ISO3)) {
  print(plot_country(c, df_hiv_lb, start_year_min, end_year_reporting))
}
dev.off()

pdf(file.path(output_path, "TB_LB_projections_by_country.pdf"), width = 12, height = 6)
for (c in unique(df_tb_lb$ISO3)) {
  print(plot_country(c, df_tb_lb, start_year_min, end_year_reporting))
}
dev.off()

pdf(file.path(output_path, "Malaria_LB_projections_by_country.pdf"), width = 12, height = 6)
for (c in unique(df_mal_lb$ISO3)) {
  print(plot_country(c, df_mal_lb, start_year_min, end_year_reporting))
}
dev.off()


# Return to wide for below code 
df_hiv_inc_final <- df_hiv_lb %>%
  filter(metric == "cases", Year %in% c(end_year_data, end_year_reporting)) %>%
  select(ISO3, Year, projection) %>%
  pivot_wider(names_from = Year, values_from = projection)

colnames(df_hiv_inc_final) <- c("ISO3",
                              "Number_of_cases_new_infections_latest",
                              "Number_of_cases_new_infections_gap_year")

df_hiv_mort_final <- df_hiv_lb %>%
  filter(metric == "deaths", Year %in% c(end_year_data, end_year_reporting)) %>%
  select(ISO3, Year, projection) %>%
  pivot_wider(names_from = Year, values_from = projection)

colnames(df_hiv_mort_final) <- c("ISO3",
                               "Number_of_deaths_latest",
                               "Number_of_deaths_gap_year")


# ---- TB ----
df_tb_inc_final <- df_tb_lb %>%
  filter(metric == "cases", Year %in% c(end_year_data, end_year_reporting)) %>%
  select(ISO3, Year, projection) %>%
  pivot_wider(names_from = Year, values_from = projection)

colnames(df_tb_inc_final) <- c("ISO3",
                             "Number_of_cases_new_infections_latest",
                             "Number_of_cases_new_infections_gap_year")

# TB deaths: choose which series is used in country outputs via switch
tb_death_metric <- tb_death_country  # "deaths" or "deathshivneg"

df_tb_mort_final <- df_tb_lb %>%
  filter(metric == tb_death_metric, Year %in% c(end_year_data, end_year_reporting)) %>%
  select(ISO3, Year, projection) %>%
  pivot_wider(names_from = Year, values_from = projection)

colnames(df_tb_mort_final) <- c("ISO3",
                              "Number_of_deaths_latest",
                              "Number_of_deaths_gap_year")


# ---- Malaria ----
df_malaria_inc_final <- df_mal_lb %>%
  filter(metric == "cases", Year %in% c(end_year_data, end_year_reporting)) %>%
  select(ISO3, Year, projection) %>%
  pivot_wider(names_from = Year, values_from = projection)

colnames(df_malaria_inc_final) <- c("ISO3",
                              "Number_of_cases_new_infections_latest",
                              "Number_of_cases_new_infections_gap_year")

df_malaria_mort_final <- df_mal_lb %>%
  filter(metric == "deaths", Year %in% c(end_year_data, end_year_reporting)) %>%
  select(ISO3, Year, projection) %>%
  pivot_wider(names_from = Year, values_from = projection)

colnames(df_malaria_mort_final) <- c("ISO3",
                               "Number_of_deaths_latest",
                               "Number_of_deaths_gap_year")



# ===================== RATE DIFFERENCE 2021 TO REPORTING YEAR ==================
# Rate difference between 2021 and year of reporting
df_hiv_rate_red      = subset(df_hiv, select = names(df_hiv) %in% c("ISO3", "Year", "incidence", "mortality"))
df_tb_rate_red       = subset(df_tb, select = names(df_tb) %in% c("ISO3", "Year", "incidence", "mortality"))
df_malaria_rate_red  = subset(df_malaria, select = names(df_malaria) %in% c("ISO3", "Year", "incidence", "mortality"))

# Replace tb mortality if needed
df_tb_rate_red$mortality <- df_tb_rate_red[[tb_mort_country]]

# Filter for the years to be compared
df_hiv_rate_red     = df_hiv_rate_red %>% filter(Year==end_year_data | Year==start_year_reporting)
df_tb_rate_red      = df_tb_rate_red %>% filter(Year==end_year_data | Year==start_year_reporting)
df_malaria_rate_red = df_malaria_rate_red %>% filter(Year==end_year_data | Year==start_year_reporting)

# Compute reduction, first pivot
df_hiv_rate_red = df_hiv_rate_red %>%
  pivot_wider(names_from = Year, values_from = c(incidence, mortality))

names(df_hiv_rate_red) <- gsub(paste0("_", end_year_data, "$"), "_latest", names(df_hiv_rate_red))

df_hiv_rate_red = df_hiv_rate_red %>%
  mutate(
    Change_in_incidence_rate = (incidence_latest - incidence_2021) / incidence_2021,
    Change_in_mortality_rate = (mortality_latest - mortality_2021) / mortality_2021,
  )
df_hiv_rate_red = subset(df_hiv_rate_red, select = -c(incidence_2021, mortality_2021))

df_tb_rate_red = df_tb_rate_red %>%
  pivot_wider(names_from = Year, values_from = c(incidence, mortality))

names(df_tb_rate_red) <- gsub(paste0("_", end_year_data, "$"), "_latest", names(df_tb_rate_red))

df_tb_rate_red = df_tb_rate_red %>%
  mutate(
    Change_in_incidence_rate = (incidence_latest - incidence_2021) / incidence_2021,
    Change_in_mortality_rate = (mortality_latest - mortality_2021) / mortality_2021,
  )
df_tb_rate_red = subset(df_tb_rate_red, select = -c(incidence_2021, mortality_2021))

df_tb_rate_red[df_tb_rate_red == Inf | df_tb_rate_red == -Inf] <- 0

df_malaria_rate_red = df_malaria_rate_red %>%
  pivot_wider(names_from = Year, values_from = c(incidence, mortality))

names(df_malaria_rate_red) <- gsub(paste0("_", end_year_data, "$"), "_latest", names(df_malaria_rate_red))

df_malaria_rate_red = df_malaria_rate_red %>%
  mutate(
    Change_in_incidence_rate = (incidence_latest - incidence_2021) / incidence_2021,
    Change_in_mortality_rate = (mortality_latest - mortality_2021) / mortality_2021,
  )
df_malaria_rate_red = subset(df_malaria_rate_red, select = -c(incidence_2021, mortality_2021))

df_malaria_rate_red[df_malaria_rate_red == Inf | df_malaria_rate_red == -Inf] <- 0



# ===================== COMBINE AND SAVE FINAL DATA ==================
df_hiv_final = df_hiv_inc_final %>% left_join(df_hiv_mort_final, by='ISO3') %>% 
  left_join(df_hiv_gp_rep, by='ISO3') %>% 
  left_join(df_hiv_rate_red, by='ISO3')

df_tb_final = df_tb_inc_final %>% left_join(df_tb_mort_final, by='ISO3') %>% 
  left_join(df_tb_gp_rep, by='ISO3') %>% 
  left_join(df_tb_rate_red, by='ISO3')

df_malaria_final = df_malaria_inc_final %>% left_join(df_malaria_mort_final, by='ISO3') %>% 
  left_join(df_malaria_gp_rep, by='ISO3') %>% 
  left_join(df_malaria_rate_red, by='ISO3')

# Make the gap
df_hiv_final = df_hiv_final %>%
  mutate(
    Gap_in_nr_cases = (Number_of_cases_new_infections_gap_year - cases_sdg),
    Gap_in_nr_deaths = (Number_of_deaths_gap_year - deaths_sdg),
  )

tb_gp_deaths_col <- ifelse(tb_death_country == "deathshivneg",
                           "deathshivneg_sdg",
                           "deaths_sdg")

df_tb_final = df_tb_final %>%
  mutate(
    Gap_in_nr_cases  = Number_of_cases_new_infections_gap_year - cases_sdg,
    Gap_in_nr_deaths = Number_of_deaths_gap_year - .data[[tb_gp_deaths_col]]
  )


df_malaria_final = df_malaria_final %>%
  mutate(
    Gap_in_nr_cases = (Number_of_cases_new_infections_gap_year - cases_sdg),
    Gap_in_nr_deaths = (Number_of_deaths_gap_year - deaths_sdg),
  )

# Compute % share of the gap
df_hiv_final = df_hiv_final %>%
  mutate(
    cases_share = ifelse(Gap_in_nr_cases > 0, Gap_in_nr_cases / sum(Gap_in_nr_cases[Gap_in_nr_cases > 0], na.rm = TRUE)*100, 0),
    deaths_share = ifelse(Gap_in_nr_deaths > 0, Gap_in_nr_deaths / sum(Gap_in_nr_deaths[Gap_in_nr_deaths > 0], na.rm = TRUE)*100, 0)
  )

df_tb_final = df_tb_final %>%
  mutate(
    cases_share = ifelse(Gap_in_nr_cases > 0, Gap_in_nr_cases / sum(Gap_in_nr_cases[Gap_in_nr_cases > 0], na.rm = TRUE)*100, 0),
    deaths_share = ifelse(Gap_in_nr_deaths > 0, Gap_in_nr_deaths / sum(Gap_in_nr_deaths[Gap_in_nr_deaths > 0], na.rm = TRUE)*100, 0)
  )

df_malaria_final = df_malaria_final %>%
  mutate(
    cases_share = ifelse(Gap_in_nr_cases > 0, Gap_in_nr_cases / sum(Gap_in_nr_cases[Gap_in_nr_cases > 0], na.rm = TRUE)*100, 0),
    deaths_share = ifelse(Gap_in_nr_deaths > 0, Gap_in_nr_deaths / sum(Gap_in_nr_deaths[Gap_in_nr_deaths > 0], na.rm = TRUE)*100, 0)
  )

# Make a clean df for each indicator
df_kpi_I2_hiv     = subset(df_hiv_final, select = names(df_hiv_final) %in% c("ISO3", "Number_of_cases_new_infections_latest", "incidence_latest", "cases_share", "Change_in_incidence_rate"))
df_kpi_I2_hiv <- df_kpi_I2_hiv %>%
  mutate(
    'Component'      = "HIV/AIDS",
  )

# remove some data that is not public
df_kpi_I2_hiv$incidence_latest[df_kpi_I2_hiv$ISO3 %in% countries_to_na_hiv_inc] <- NA

df_kpi_I2_tb      = subset(df_tb_final, select = names(df_tb_final) %in% c("ISO3", "Number_of_cases_new_infections_latest", "incidence_latest", "cases_share", "Change_in_incidence_rate"))
df_kpi_I2_tb <- df_kpi_I2_tb %>%
  mutate(
    'Component'      = "TB",
  )

df_kpi_I2_malaria = subset(df_malaria_final, select = names(df_malaria_final) %in% c("ISO3", "Number_of_cases_new_infections_latest", "incidence_latest", "cases_share", "Change_in_incidence_rate"))
df_kpi_I2_malaria <- df_kpi_I2_malaria %>%
  mutate(
    'Component'      = "Malaria",
  )

df_kpi_I2 = rbind(df_kpi_I2_hiv, df_kpi_I2_tb, df_kpi_I2_malaria)


df_kpi_I2 <- df_kpi_I2 %>%
  mutate(
    'KPI code'       = "KPI I2",
    'Reporting year' = reporting_year,
    "Reporting half" = "H1",
    "Data period"    = end_year_data,
  ) %>%
  select("KPI code", "Reporting year", "Reporting half", "Data period", 
         "ISO3", "Component", "Number_of_cases_new_infections_latest", "incidence_latest", "cases_share", "Change_in_incidence_rate")

df_kpi_I2 <- df_kpi_I2 %>%
  rename(
    "ISO" = "ISO3",
    "Number of cases/infections, latest" = "Number_of_cases_new_infections_latest",
    "Incidence rate" = 'incidence_latest',
    "share of gap to SDG" = "cases_share",
    "variation incidence rate" = "Change_in_incidence_rate"
  )

df_kpi_I1_hiv     = subset(df_hiv_final, select = names(df_hiv_final) %in% c("ISO3", "Number_of_deaths_latest", "mortality_latest", "deaths_share", "Change_in_mortality_rate"))
df_kpi_I1_hiv <- df_kpi_I1_hiv %>%
  mutate(
    'Component'      = "HIV/AIDS",
  )
# remove some data that is not public
df_kpi_I1_hiv$Number_of_deaths_latest[df_kpi_I1_hiv$ISO3 %in% countries_to_na_aids_deaths] <- NA

df_kpi_I1_tb      = subset(df_tb_final, select = names(df_tb_final) %in% c("ISO3", "Number_of_deaths_latest", "mortality_latest", "deaths_share", "Change_in_mortality_rate"))
df_kpi_I1_tb <- df_kpi_I1_tb %>%
  mutate(
    'Component'      = "TB",
  )

df_kpi_I1_malaria = subset(df_malaria_final, select = names(df_malaria_final) %in% c("ISO3", "Number_of_deaths_latest", "mortality_latest", "deaths_share", "Change_in_mortality_rate"))
df_kpi_I1_malaria <- df_kpi_I1_malaria %>%
  mutate(
    'Component'      = "Malaria",
  )

df_kpi_I1 = rbind(df_kpi_I1_hiv, df_kpi_I1_tb, df_kpi_I1_malaria)

df_kpi_I1 <- df_kpi_I1 %>%
  mutate(
    'KPI code'       = "KPI I1",
    'Reporting year' = reporting_year,
    "Reporting half" = "H1",
    "Data period"    = end_year_data,
  ) %>%
  select("KPI code", "Reporting year", "Reporting half", "Data period", 
         "ISO3", "Component", "Number_of_deaths_latest", "mortality_latest", "deaths_share", "Change_in_mortality_rate")

df_kpi_I1 <- df_kpi_I1 %>%
  rename(
    "ISO" = "ISO3",
    "Number of deaths, latest" = "Number_of_deaths_latest",
    "Mortality rate" = 'mortality_latest',
    "share of gap to SDG" = "deaths_share",
    "variation mortality rate" = "Change_in_mortality_rate"
  )

# Save the 2030 targets (adjusted for baseline GP data)
write.csv(df_hiv_gp_2030, file = file.path(output_path, "HIV_GP_2030_target_data.csv"), row.names=FALSE)
write.csv(df_tb_gp_2030, file=file.path(output_path,"TB_GP_2030_target.csv"), row.names=FALSE)
write.csv(df_malaria_gp_2030, file=file.path(output_path,"Malaria_GP_2030_target.csv"), row.names=FALSE)


# Save the final output
write.csv(df_kpi_I1, file=file.path(output_path,"df_kpi_I1_Jan2026.csv"), na = "", row.names=FALSE)
write.csv(df_kpi_I2, file=file.path(output_path,"df_kpi_I2_Jan2026.csv"), na = "", row.names=FALSE)



# =========================================================
# PORTFOLIO-LEVEL DATA
# Observed (partner) 2014–end_year_data
# GP (baseline-adjusted) 2015/2020–end_year_sdg
# + combined indices (base = norm_year)
# =========================================================

# ---------- 0) Prepare malaria SSA-only partner + GP ----------
df_malaria_portfolio <- df_malaria2 %>% filter(ISO3 %in% df_iso_malaria_portfolio$ISO3)
df_malaria_gp_portfolio <- df_malaria_gp2 %>% filter(ISO3 %in% df_iso_malaria_portfolio$ISO3)

df_hiv2       = filter(df_hiv2, ISO3 %in% df_iso_hiv_portfolio$ISO3)                    # by eligible countries
df_tb2        = filter(df_tb2, ISO3 %in% df_iso_tb$ISO3)                      # by eligible countries


# ---------- 2) Observed portfolio rates from PARTNER data ----------
# HIV: incidence uses HIVneg; mortality uses Population
df_hiv_actual <- df_hiv2 %>%
  filter(Year >= start_year_min, Year <= end_year_data) %>%
  group_by(Year) %>%
  summarise(
    cases  = sum(hiv_cases_n_pip, na.rm = TRUE),
    deaths = sum(hiv_deaths_n_pip, na.rm = TRUE),
    HIVneg  = sum(HIVneg, na.rm = TRUE),
    Population = sum(Population_n_pip, na.rm = TRUE),
    .groups = "drop"
  ) 

# TB: incidence uses tb_pop; mortality uses HIV-neg deaths / tb_pop
df_tb_actual <- df_tb2 %>%
  filter(Year >= start_year_min, Year <= end_year_data) %>%
  group_by(Year) %>%
  summarise(
    cases     = sum(cases, na.rm = TRUE),
    deaths = sum(.data[[tb_deaths_portfolio]], na.rm = TRUE),
    Population = sum(tb_pop_n_pip, na.rm = TRUE),
    .groups = "drop"
  )

# Malaria (SSA): incidence uses par; mortality uses deaths/par
df_malaria_actual <- df_malaria_portfolio %>%
  filter(Year >= start_year_min, Year <= end_year_data) %>%
  group_by(Year) %>%
  summarise(
    cases  = sum(malaria_cases_n_pip, na.rm = TRUE),
    deaths = sum(malaria_deaths_n_pip, na.rm = TRUE),
    par  = sum(malaria_par_n_pip, na.rm = TRUE),
    .groups = "drop"
  )



# ---------- 2) GP portfolio rates from ADJUSTED GP time series ----------
# HIV GP: 2020–2030
df_hiv_gp_port <- df_hiv_gp2 %>%
  filter(Year >= start_year_gp_hiv, Year <= end_year_sdg) %>%
  group_by(Year) %>%
  summarise(
    cases_sdg      = sum(cases_sdg, na.rm = TRUE),
    deaths_sdg     = sum(deaths_sdg, na.rm = TRUE),
    HIVneg_sdg     = sum(HIVneg_sdg, na.rm = TRUE),
    Population_sdg = sum(Population_sdg, na.rm = TRUE),
    .groups = "drop"
  ) 

# TB GP: 2015–2030 (we do not have hivneg deaths, we use scaling to get it)
df_tb_gp_port <- df_tb_gp2 %>%
  filter(Year >= start_year_gp_tm, Year <= end_year_sdg) %>%
  group_by(Year) %>%
  summarise(
    cases_sdg      = sum(cases_sdg, na.rm = TRUE),
    deaths_sdg     = sum(deaths_ori_sdg, na.rm = TRUE),
    Population_sdg = sum(Population, na.rm = TRUE),
    .groups = "drop"
  ) 

# Malaria GP (SSA): 2015–2030
df_malaria_gp_port <- df_malaria_gp_portfolio %>%
  filter(Year >= start_year_gp_tm, Year <= end_year_sdg) %>%
  group_by(Year) %>%
  summarise(
    cases_sdg  = sum(cases_sdg, na.rm = TRUE),
    deaths_sdg = sum(deaths_sdg, na.rm = TRUE),
    par_sdg    = sum(par_sdg, na.rm = TRUE),
    .groups = "drop"
  ) 

# ------------ READJUST AT PORTFOLIO LEVEL FOR MISSING COUTNRIES --------

hiv_partner_base <- df_hiv_actual %>% filter(Year == start_year_gp_hiv)
hiv_gp_base      <- df_hiv_gp_port %>% filter(Year == start_year_gp_hiv)

# Compute portfolio-level ratios (scalars)
ratio_cases      <- hiv_partner_base$cases      / hiv_gp_base$cases_sdg
ratio_deaths     <- hiv_partner_base$deaths     / hiv_gp_base$deaths_sdg
ratio_hivneg     <- hiv_partner_base$HIVneg     / hiv_gp_base$HIVneg_sdg
ratio_population <- hiv_partner_base$Population / hiv_gp_base$Population_sdg

df_hiv_gp_port <- df_hiv_gp_port %>%
  mutate(
    cases_sdg      = cases_sdg      * ratio_cases,
    deaths_sdg     = deaths_sdg     * ratio_deaths,
    HIVneg_sdg     = HIVneg_sdg     * ratio_hivneg,
    Population_sdg = Population_sdg * ratio_population,
    incidence_gp   = ifelse(HIVneg_sdg > 0, cases_sdg / HIVneg_sdg, NA_real_),
    mortality_gp   = ifelse(Population_sdg > 0, deaths_sdg / Population_sdg, NA_real_)
  )

tb_partner_base <- df_tb_actual %>% filter(Year == start_year_gp_tm)
tb_gp_base      <- df_tb_gp_port %>% filter(Year == start_year_gp_tm)


# Scalars (ratios)
tb_ratio_cases      <- tb_partner_base$cases      / tb_gp_base$cases_sdg
tb_ratio_deaths     <- tb_partner_base$deaths     / tb_gp_base$deaths_sdg
tb_ratio_population <- tb_partner_base$Population / tb_gp_base$Population_sdg


# ---- Apply ratios to ALL TB GP years + recompute rates ----
df_tb_gp_port <- df_tb_gp_port %>%
  mutate(
    cases_sdg         = cases_sdg * tb_ratio_cases,
    deaths_sdg        = deaths_sdg * tb_ratio_deaths,
    Population_sdg    = Population_sdg * tb_ratio_population,
    incidence_gp      = ifelse(Population_sdg > 0, cases_sdg / Population_sdg, NA_real_),
    mortality_gp      = ifelse(Population_sdg > 0, deaths_sdg / Population_sdg, NA_real_)
  )

mal_partner_base <- df_malaria_actual %>% filter(Year == start_year_gp_tm)
mal_gp_base      <- df_malaria_gp_port %>% filter(Year == start_year_gp_tm)

# ---- Malaria ratios (scalars) ----
mal_ratio_cases  <- mal_partner_base$cases  / mal_gp_base$cases_sdg
mal_ratio_deaths <- mal_partner_base$deaths / mal_gp_base$deaths_sdg
mal_ratio_par    <- mal_partner_base$par    / mal_gp_base$par_sdg

# ---- Apply ratios to ALL Malaria GP years + recompute rates ----
df_malaria_gp_port <- df_malaria_gp_port %>%
  mutate(
    cases_sdg     = cases_sdg * mal_ratio_cases,
    deaths_sdg    = deaths_sdg * mal_ratio_deaths,
    par_sdg       = par_sdg * mal_ratio_par,
    incidence_gp  = ifelse(par_sdg > 0, cases_sdg / par_sdg, NA_real_),
    mortality_gp  = ifelse(par_sdg > 0, deaths_sdg / par_sdg, NA_real_)
  )




# ---------- 3) Portfolio projections (continuation of recent trends) ----------
# These come from your existing lower-bound portfolio engine
df_hiv_lb_portfolio <- run_lower_bound_portfolio(
  df_hiv2, "hiv",
  anchor_year = end_year_data, end_year = end_year_reporting,
  include_2024_threshold     = include_2024_if_within_log_ratio,
  guard_linear_if_increasing = guard_linear_if_increasing
)

df_tb_lb_portfolio <- run_lower_bound_portfolio(
  df_tb2, "tb",
  anchor_year = end_year_data, end_year = end_year_reporting,
  include_2024_threshold     = include_2024_if_within_log_ratio,
  guard_linear_if_increasing = guard_linear_if_increasing
)

df_malaria_lb_portfolio <- run_lower_bound_portfolio(
  df_malaria_portfolio, "malaria",
  anchor_year = end_year_data, end_year = end_year_reporting,
  include_2024_threshold     = include_2024_if_within_log_ratio,
  guard_linear_if_increasing = guard_linear_if_increasing
)

# Make these dfs wide again
# HIV portfolio: observed + projections from lower-bound output
df_hiv_lb_portfolio <- df_hiv_lb_portfolio %>%
  select(Year, metric, observed, projection) %>%
  pivot_wider(
    names_from = metric,
    values_from = c(observed, projection)
  ) %>%
  rename(
    incidence_actual  = observed_incidence,
    mortality_actual  = observed_mortality,
    incidence_proj    = projection_incidence,
    mortality_proj    = projection_mortality
  )

df_tb_lb_portfolio <- df_tb_lb_portfolio %>%
  select(Year, metric, observed, projection) %>%
  pivot_wider(names_from = metric, values_from = c(observed, projection)) %>%
  rename(
    incidence_actual  = observed_incidence,
    mortality_actual  = observed_mortality,
    incidence_proj    = projection_incidence,
    mortality_proj    = projection_mortality
  )

df_malaria_lb_portfolio <- df_malaria_lb_portfolio %>%
  select(Year, metric, observed, projection) %>%
  pivot_wider(names_from = metric, values_from = c(observed, projection)) %>%
  rename(
    incidence_actual  = observed_incidence,
    mortality_actual  = observed_mortality,
    incidence_proj    = projection_incidence,
    mortality_proj    = projection_mortality
  )




# ---------- 4) Stitch observed + GP into ONE series per disease ----------
hiv_portfolio <- full_join(df_hiv_lb_portfolio, df_hiv_gp_port, by = "Year") %>%
  filter(Year >= 2010) %>%
  arrange(Year)

tb_portfolio <- full_join(df_tb_lb_portfolio, df_tb_gp_port, by = "Year") %>%
  filter(Year >= 2010) %>%
  arrange(Year) 

malaria_portfolio <- full_join(df_malaria_lb_portfolio, df_malaria_gp_port, by = "Year") %>%
  filter(Year >= 2010) %>%
  arrange(Year) 



# ---------- 5) Combined indices (base = norm_year = 2020) ----------
# 1) Bind portfolio series together
portfolio_all <- dplyr::bind_rows(
  dplyr::mutate(hiv_portfolio,     disease = "HIV"),
  dplyr::mutate(tb_portfolio,      disease = "TB"),
  dplyr::mutate(malaria_portfolio, disease = "Malaria")
)

# 2) NORAMLISE: Helper to build a base-2020=100 index and average across diseases
make_portfolio_index <- function(df,
                                 actual_col,
                                 proj_col,
                                 base_year = norm_year) {
  
  df %>%
    dplyr::group_by(disease) %>%
    dplyr::mutate(
      base_actual = .data[[actual_col]][Year == base_year][1],
      base_proj   = .data[[proj_col]][Year == base_year][1],
      
      index_actual = 100 * .data[[actual_col]] / base_actual,
      index_proj   = 100 * .data[[proj_col]]   / base_proj
    ) %>%
    dplyr::ungroup() %>%
    # keep only rows where at least one index is valid
    dplyr::filter(
      (is.finite(index_actual) & base_actual > 0) |
        (is.finite(index_proj)   & base_proj   > 0)
    ) %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(
      index_actual_mean = mean(index_actual, na.rm = TRUE),
      index_proj_mean   = mean(index_proj,   na.rm = TRUE),
      
      n_diseases_actual = sum(is.finite(index_actual)),
      n_diseases_proj   = sum(is.finite(index_proj)),
      .groups = "drop"
    )
}


# Normalize
incidence_index <- make_portfolio_index(
  portfolio_all,
  actual_col = "incidence_actual",
  proj_col   = "incidence_proj",
  base_year  = norm_year
)

mortality_index <- make_portfolio_index(
  portfolio_all,
  actual_col = "mortality_actual",
  proj_col   = "mortality_proj",
  base_year  = norm_year
)


# 6) Export a portfolio Excel with all sheets (diseases + combined indices)
write_xlsx(
  list(
    HIV_Portfolio      = hiv_portfolio,
    TB_Portfolio       = tb_portfolio,
    Malaria_Portfolio  = malaria_portfolio,
    Combined_Incidence_Index = incidence_index,
    Combined_Mortality_Index = mortality_index),
  path = file.path(output_path, "Portfolio_LB_Projections.xlsx")
)








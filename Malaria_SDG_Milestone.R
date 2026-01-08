
# First Version: 5th January 2026 by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will generate the data milestone based SDG at country-level
# Central switchboard gives you option of choosing which diseases to run
# UNWPP downloaded 5th Jan 2025
# This code is for malaria

# If you want to clean the workspace
rm(list = ls())

#######################
# Central Switchboard #
#######################

# Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(readxl)

# Parameter
sdg_base_year = 2015
last_year = 2024
final_year = 2030

# SDG malaria milestone reductions (relative to 2015 baseline)
milestones <- tibble::tribble(
  ~year, ~reduction,
  2015, 0.0000,
  2016, 0.0226,
  2017, 0.1163,
  2018, 0.2140,
  2019, 0.3094,
  2020, 0.4000,
  2021, 0.4845,
  2022, 0.5622,
  2023, 0.6325,
  2024, 0.6952,
  2025, 0.7500,
  2026, 0.7967,
  2027, 0.8351,
  2028, 0.8652,
  2029, 0.8869,
  2030, 0.9000
)


# Set wd
setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/")

# Load data
malaria_raw <- read.csv(
  "df_partner_malaria_5Jan2025.csv",
  stringsAsFactors = FALSE
)

unwpp <- read_excel(
  "Raw Data/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
  sheet = "Medium variant",
  skip = 16,
  col_types = "text"
)


######################
# Clean partner data #
######################

malaria_actual <- malaria_raw %>%
  transmute(
    iso3   = toupper(trimws(ISO3)),
    year   = as.integer(Year),
    cases  = malaria_cases_n_pip,
    deaths = malaria_deaths_n_pip,
    par    = malaria_par_n_pip
  ) %>%
  filter(
    !is.na(iso3), iso3 != "",
    !is.na(year),
    year >= sdg_base_year, year <= last_year
  )

# Retain final year
par_latest <- malaria_actual %>%
  filter(year == last_year) %>%
  select(iso3, par_latest = par)


### Clean UNWPP data###
un_growth <- unwpp %>%
  transmute(
    iso3 = toupper(trimws(`ISO3 Alpha-code`)),
    year = as.integer(Year),
    pop_growth = parse_number(`Population Growth Rate (percentage)`,
                              na = c("", "NA", "...", "…")) / 100
  ) %>%
  filter(!is.na(iso3), iso3 != "", !is.na(year)) %>%
  filter(year >= last_year, year <= final_year) %>%
  arrange(iso3, year)


### Project par ###

# Includes baseline year unchanged (2024 factor = 1)
par_proj <- un_growth %>%
  left_join(par_latest, by = "iso3") %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(
    growth_factor = if_else(year == last_year, 1, 1 + pop_growth),
    par = par_latest * cumprod(growth_factor)
  ) %>%
  ungroup() %>%
  select(iso3, year, pop_growth, par_latest, par)

# Keep only future projected years (2025–2030)
par_future <- par_proj %>%
  filter(year > last_year) %>%
  select(iso3, year, par)


### Combine actual and projected par ###
par_all <- malaria_actual %>%
  select(iso3, year, par) %>%
  bind_rows(par_future) %>%
  arrange(iso3, year)


### Combine all par with estimates of cases/deaths
malaria_full <- malaria_actual %>%
  select(iso3, year, cases, deaths) %>%
  full_join(par_all, by = c("iso3", "year")) %>%
  arrange(iso3, year)


# -----------------------------
# 1) Baseline rates in 2015
# -----------------------------
malaria_base <- malaria_full %>%
  filter(year == sdg_base_year) %>%
  transmute(
    iso3,
    incidence_2015  = cases  / par,  # incidence per person
    mortality_2015 = deaths / par   # mortality per person
  )

# -----------------------------
# 2) Join milestones + compute SDG target cases/deaths (2015–2030)
# -----------------------------
malaria_targets <- malaria_full %>%
  # keep the SDG window only
  filter(year >= sdg_base_year, year <= final_year) %>%
  # bring in country baseline rates
  left_join(malaria_base, by = "iso3") %>%
  # bring in milestone reduction by year
  left_join(milestones, by = "year") %>%
  mutate(
    # apply reductions to baseline rates
    sdg_incidence_rate  = incidence_2015  * (1 - reduction),
    sdg_mortality_rate = mortality_2015 * (1 - reduction),
    
    # convert rates back to counts using PAR
    sdg_cases  = sdg_incidence_rate  * par,
    sdg_deaths = sdg_mortality_rate * par
  ) %>%
  select(
    iso3, year,
    sdg_incidence_rate, sdg_mortality_rate,
    sdg_cases, sdg_deaths
  ) %>%
  filter(
    !is.na(sdg_incidence_rate),
    !is.na(sdg_mortality_rate),
    !is.na(sdg_cases),
    !is.na(sdg_deaths)
  ) %>%
  arrange(iso3, year)

# Save 
write.csv(malaria_targets,file="/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/malaria_sdg_milestones.csv", row.names=FALSE)


# First Version: 7th January 2026 by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will generate the data milestone based SDG at country-level
# Central switchboard gives you option of choosing which diseases to run
# UNWPP downloaded 5th Jan 2025
# This code is for malaria

# First Version: 5th January 2026 by Mikaela Smit
rm(list = ls())

#######################
# Central Switchboard #
#######################

library(dplyr)
library(tidyr)
library(readr)
library(readxl)

# Parameters
sdg_base_year = 2015
last_year = 2024
final_year = 2030

# SDG TB milestone reductions (relative to 2015 baseline)
# incidence reduction applies to incidence rate
# death reduction applies to mortality rate
tb_milestones <- tibble::tribble(
  ~year, ~inc_reduction, ~death_reduction,
  2015, 0.00, 0.00,
  2016, 0.04, 0.08,
  2017, 0.08, 0.15,
  2018, 0.12, 0.22,
  2019, 0.16, 0.29,
  2020, 0.20, 0.35,
  2021, 0.27, 0.46,
  2022, 0.33, 0.55,
  2023, 0.39, 0.63,
  2024, 0.45, 0.69,
  2025, 0.50, 0.75,
  2026, 0.58, 0.79,
  2027, 0.65, 0.82,
  2028, 0.71, 0.85,
  2029, 0.75, 0.87,
  2030, 0.80, 0.90
)

# Set wd
setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/")

# Load data
tb_raw <- read.csv("Raw Data/pip_extract_7Jan2025_tb_hiv.csv", stringsAsFactors = FALSE)

unwpp <- read_excel(
  "Raw Data/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx",
  sheet = "Medium variant",
  skip = 16,
  col_types = "text"
)

######################
# Clean partner data #
######################

# Expecting columns like: iso3, year, pip_code, value
tb_long <- tb_raw %>%
  transmute(
    iso3     = toupper(trimws(iso3)),
    year     = as.integer(year),
    pip_code = trimws(pip_code),
    value    = suppressWarnings(as.numeric(value))
  ) %>%
  filter(
    !is.na(iso3), iso3 != "",
    !is.na(year),
    year >= sdg_base_year, year <= last_year
  )

# Keep only what we need: TB11 cases, TB50 deaths, TB67 population
tb_actual <- tb_long %>%
  tidyr::pivot_wider(names_from = pip_code, values_from = value) %>%
  transmute(
    iso3,
    year,
    cases          = TB11,
    pop            = TB67,
    deaths_all     = TB50,
    deaths_hivpos  = TB64,
    deaths_hivneg  = TB50 - TB64
  ) %>%
  # keep rows where we can compute rates + do population projection baseline
  filter(!is.na(pop))

# Retain final year population for projection baseline
pop_latest <- tb_actual %>%
  filter(year == last_year) %>%
  select(iso3, pop_latest = pop)

######################
# Clean UNWPP growth #
######################

un_growth <- unwpp %>%
  transmute(
    iso3 = toupper(trimws(`ISO3 Alpha-code`)),
    year = as.integer(Year),
    pop_growth = parse_number(
      `Population Growth Rate (percentage)`,
      na = c("", "NA", "...", "…")
    ) / 100
  ) %>%
  filter(!is.na(iso3), iso3 != "", !is.na(year)) %>%
  filter(year >= last_year, year <= final_year) %>%
  arrange(iso3, year)

#################
# Project POP   #
#################

# Includes baseline year unchanged (2024 factor = 1)
pop_proj <- un_growth %>%
  left_join(pop_latest, by = "iso3") %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(
    growth_factor = if_else(year == last_year, 1, 1 + pop_growth),
    pop = pop_latest * cumprod(growth_factor)
  ) %>%
  ungroup() %>%
  select(iso3, year, pop_growth, pop_latest, pop)

# Keep only future projected years (2025–2030)
pop_future <- pop_proj %>%
  filter(year > last_year) %>%
  select(iso3, year, pop)


###############################
# Combine actual + projected  #
###############################

pop_all <- tb_actual %>%
  select(iso3, year, pop) %>%
  bind_rows(pop_future) %>%
  arrange(iso3, year)

# Combine POP with actual cases/deaths (cases/deaths only exist through 2024)
tb_full <- tb_actual %>%
  select(iso3, year, cases, deaths_all, deaths_hivneg) %>%
  full_join(pop_all, by = c("iso3", "year")) %>%
  arrange(iso3, year)

# -----------------------------
# 1) Baseline rates in 2015
# -----------------------------
tb_base <- tb_full %>%
  filter(year == sdg_base_year) %>%
  transmute(
    iso3,
    incidence_2015           = cases / pop,
    mortality_all_2015       = deaths_all / pop,
    mortality_hivneg_2015    = deaths_hivneg / pop
  )

# -----------------------------
# 2) Join milestones + compute SDG target cases/deaths (2015–2030)
# -----------------------------
tb_targets <- tb_full %>%
  filter(year >= sdg_base_year, year <= final_year) %>%
  left_join(tb_base, by = "iso3") %>%
  left_join(tb_milestones, by = "year") %>%
  mutate(
    # apply reductions to baseline rates
    sdg_incidence_rate         = incidence_2015        * (1 - inc_reduction),
    sdg_mortality_all_rate     = mortality_all_2015    * (1 - death_reduction),
    sdg_mortality_hivneg_rate  = mortality_hivneg_2015 * (1 - death_reduction),
    
    # convert rates back to counts using POP
    sdg_cases           = sdg_incidence_rate        * pop,
    sdg_deaths_all      = sdg_mortality_all_rate    * pop,
    sdg_deaths_hivneg   = sdg_mortality_hivneg_rate * pop
  ) %>%
  select(
    iso3, year,
    sdg_incidence_rate,
    sdg_mortality_all_rate,
    sdg_mortality_hivneg_rate,
    sdg_cases,
    sdg_deaths_all,
    sdg_deaths_hivneg
  ) %>%
  filter(
    !is.na(sdg_incidence_rate),
    !is.na(sdg_mortality_all_rate),
    !is.na(sdg_mortality_hivneg_rate),
    !is.na(sdg_cases),
    !is.na(sdg_deaths_all),
    !is.na(sdg_deaths_hivneg)
  ) %>%
  arrange(iso3, year)

# Save
write.csv(
  tb_targets,
  file = "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/tb_sdg_milestones.csv",
  row.names = FALSE
)

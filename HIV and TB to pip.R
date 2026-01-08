# First Version: 8th January 2026 by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will generate pip format partner data for HIV and TB
# Central switchboard gives you option of choosing which diseases to run
# Data source: Erica Kufa 7 Jan 2026


# First Version: 5th January 2026 by Mikaela Smit
rm(list = ls())

#######################
# Central Switchboard #
#######################

library(dplyr)
library(tidyr)
library(readr)
library(readxl)


# Set wd
setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/")

# Load data
tb_hiv_raw <- read.csv("Raw Data/pip_extract_7Jan2025_tb_hiv.csv", stringsAsFactors = FALSE)


######################
# Clean partner data #
######################


# Standardise names + types (assumes columns: iso3, year, pip_code, indicator, value)
partner_long <- tb_hiv_raw %>%
  transmute(
    iso3      = toupper(trimws(iso3)),
    year      = as.integer(year),
    pip_code  = toupper(trimws(pip_code)),
    indicator = trimws(indicator),
    value     = suppressWarnings(as.numeric(value))
  ) %>%
  filter(
    !is.na(iso3), iso3 != "",
    !is.na(year),
    !is.na(pip_code), pip_code != ""
  )

#############################
# Clean HIV and TB data dfs #
#############################

df_hiv2 <- partner_long %>%
  filter(
    startsWith(pip_code, "HIV") |
      startsWith(pip_code, "POPULATION")
  )
df_tb2  <- partner_long %>% filter(substr(pip_code, 1, 2) == "TB")


# Map variable names for HIV
df_hiv2 <- df_hiv2 %>%
  mutate(
    indicator_std = case_when(
      pip_code == "HIV1"        ~ "hiv_cases_n_pip",
      pip_code == "HIV121"      ~ "hiv_deaths_n_pip",
      pip_code == "Population2" ~ "population_n_pip",
      pip_code == "HIV46"       ~ "hivpos_n_pip",
      
      pip_code == "HIV163"      ~ "art_n_pip",
      pip_code == "HIV168"      ~ "art_p_pip",
      pip_code == "HIV186"      ~ "pmtct_n_pip",
      pip_code == "HIV187"      ~ "pmtct_p_pip",
      pip_code == "HIV211"      ~ "vmmc_n_pip",
      
      pip_code == "HIV469"      ~ "sw_reached_p_pip",
      pip_code == "HIV472"      ~ "msm_reached_p_pip",
      pip_code == "HIV470"      ~ "pwid_reached_p_pip",
      pip_code == "HIV471"      ~ "tg_reached_p_pip",
      
      pip_code == "HIV420"      ~ "vls_p_pip",
      pip_code == "HIV417"      ~ "status_p_pip",
      
      # HIV893 is present but not in old mapping — keep explicit
      pip_code == "HIV893"      ~ "hiv_opiod_p_pip",
      
      TRUE ~ NA_character_
    )
  )


# Pivot to wide
df_hiv <- df_hiv2 %>%
  filter(!is.na(indicator_std)) %>%              # keep only mapped codes
  select(ISO3 = iso3, Year = year, indicator_std, value) %>%  # match column names
  pivot_wider(
    id_cols = c(ISO3, Year),
    names_from = indicator_std,
    values_from = value
  ) %>%
  arrange(ISO3, Year)


# Map variables for TB
df_tb2 <- df_tb2 %>%
  mutate(
    indicator_std = case_when(
      pip_code == "TB11"  ~ "tb_cases_n_pip",
      pip_code == "TB50"  ~ "tb_deaths_n_pip",
      pip_code == "TB58"  ~ "tb_deathsnohiv_n_pip",
      pip_code == "TB67"  ~ "tb_pop_n_pip",
      pip_code == "TB81"  ~ "tb_notified_n_pip",
      pip_code == "TB84"  ~ "mdr_notified_n_pip",
      pip_code == "TB190" ~ "tbhiv_art_n_pip",
      pip_code == "TB85"  ~ "mdr_tx_n_pip",
      pip_code == "TB249" ~ "tb_success_p_pip",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(indicator_std))

# Pivot TB to wide (match 2023 format)
df_tb <- df_tb2 %>%
  select(ISO3 = iso3, Year = year, indicator_std, value) %>%
  pivot_wider(
    id_cols = c(ISO3, Year),
    names_from = indicator_std,
    values_from = value
  ) %>%
  arrange(ISO3, Year)


# Save 
write.csv(
  df_hiv,
  file = "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/df_partner_hiv_8Jan.csv",
  row.names = FALSE
)

# Save TB partner data
write.csv(
  df_tb,
  file = "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/df_partner_tb_8Jan.csv",
  row.names = FALSE
)





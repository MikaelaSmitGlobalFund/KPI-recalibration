# First Version: 5th Jan 2026 by Mikaela Smit

# SCRIPT DESCRIPTION: 
# This code will extract the malaria partner data from wmr report
# The list of indicators and what they refer to can be found in the ReadMe file

# If you want to clean the workspace
#rm(list = ls())

#### ---- CENTRAL SWITCHBOARD ---- ####

# Set computer
computer = 1        # 1 = Mikaela 

# Load library
library(dplyr)
library(tidyr)
library(data.table)
library("readxl")

# Set computer and wd
if (computer ==1){
  setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/")
}

# Define indicators for PIP data
list_indc_malaria_pip = c("Estimated malaria cases (Point estimate)", "Estimated malaria deaths (Point estimate)", "Population at risk")  # treatment variables not included as do not align: "Malaria50", "Malaria52", "Malaria54",


# Load data
library(readxl)

df_malaria2 <- read_excel(
  "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/Raw Data/wmr2025_gf_indicators20251209.xlsx",
  sheet = 2
)


# Harmonize variable names and filter df
df_malaria <- df_malaria2 %>%
  filter(indicator %in% list_indc_malaria_pip) %>%
  select(code, year, indicator, value) %>%
  rename(
    ISO3 = code,
    Year = year
  )


# Rename variable names
df_malaria <- df_malaria %>%
  mutate(
    indicator = recode(indicator,
                       "Estimated malaria cases (Point estimate)"  = "malaria_cases_n_pip",
                       "Estimated malaria deaths (Point estimate)" = "malaria_deaths_n_pip",
                       "Population at risk"                        = "malaria_par_n_pip"
    )
  )

# Remove regional data
df_malaria <- df_malaria %>%
  filter(nchar(ISO3) == 3)

# Pivot
df_malaria <- df_malaria %>%
  pivot_wider(
    id_cols = c(ISO3, Year),
    names_from = indicator,
    values_from = value
  )


# Save
out_path <- file.path(getwd(), "df_partner_malaria_5Jan2025.csv")
write.csv(df_malaria, out_path, row.names = FALSE)






# First Version: 27th January by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will generate the data for KPI reporting at country-level
# Central switchboard gives you option of choosing which diseases to run

rm(list = ls())

#######################
# Central Switchboard #
#######################

## Set the user
firstrun   = 0                            # If need to install package change to 1
computer   = 1                            # 1 = Mikaela # Add additional computer if needed

# Set central parameters
covid_years = 2020:2022
post_covid_year = 2023
start_year = 2015
end_year_data = 2024
end_year = 2028

# Install packages if neccesary
if(firstrun>0) {
  install.packages("dplyr")
  installed.packages("tidyverse")
}

library(dplyr)
library(broom)
library(tidyr)

# Set computer, wd and load data
if (computer ==1){
  setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data")
  output_path = "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/RCode"
  
  # Load the pip data 
  df_hiv2 = read.csv("df_data_hiv.csv", stringsAsFactors = FALSE)
  df_malaria2  = read.csv("df_data_malaria.csv", stringsAsFactors = FALSE)
  df_tb2      =  read.csv("df_data_tb.csv", stringsAsFactors = FALSE)
  
  # Load list of countries
  df_iso_hiv     = read.csv("List of countries/hiv_iso.csv", stringsAsFactors = FALSE)
  df_iso_tb      = read.csv("List of countries/tb_iso_v2.csv", stringsAsFactors = FALSE)
  df_iso_malaria = read.csv("List of countries/malaria_iso_v2.csv", stringsAsFactors = FALSE)
}

# List of indicators
list_indc_hiv_pip     = c("country", "year", "cases_central", "deaths_central", "incidence_central", "mortality_central", "hivneg_central", 'plhiv_central', "population_central")
list_indc_tb_pip      = c("country", "year", "cases_central", "deaths_central", "incidence_central", "mortality_central", "population_central")
list_indc_malaria_pip = c("country", "year", "cases_central", "deaths_central", "incidence_central", "mortality_central", "par_central")  
list_ind              = c("country", "year", "incidence_central", "mortality_central")


#--- Clean files ---

# Filter out countries we do not need: 
df_hiv2        = filter(df_hiv2, country %in% df_iso_hiv$ISO3)             # by eligible countries
df_tb2         = filter(df_tb2, country %in% df_iso_tb$ISO3)               # by eligible countries
df_malaria2    = filter(df_malaria2, country %in% df_iso_malaria$ISO3)     # by eligible countries

# Filter out indicators we do not need: 
df_hiv     = subset(df_hiv2, select = names(df_hiv2) %in% list_indc_hiv_pip)
df_tb      = subset(df_tb2, select = names(df_tb2) %in% list_indc_tb_pip)
df_malaria = subset(df_malaria2, select = names(df_malaria2) %in% list_indc_malaria_pip)

# Make incidence and mortality


# Filter out years we do not need (which will also remove hivneg that are wrong in first year per country)
df_hiv      = df_hiv %>% filter(year>=start_year & year<=end_year_data)
df_tb       = df_tb %>% filter(year>=start_year & year<=end_year_data)
df_malaria  = df_malaria %>% filter(year>=start_year & year<=end_year_data)

# Remove all variables we do not need
df_hiv     = subset(df_hiv, select = names(df_hiv) %in% list_ind)
df_tb      = subset(df_tb, select = names(df_tb) %in% list_ind)
df_malaria = subset(df_malaria, select = names(df_malaria) %in% list_ind)

# Pivot
df_hiv_long <- df_hiv %>%
  pivot_longer(
    cols = c(incidence_central, mortality_central),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = ifelse(metric == "incidence_central", "incidence", "mortality")
  )


#--- Function to fit projection per disease x metric ---
fit_projection <- function(df, covid_years, post_covid_year, end_year) {
  # Center year for stability
  df <- df %>%
    mutate(
      year_c = year - mean(year),
      post = as.integer(year >= post_covid_year)
    )
  
  # Add covid dummies
  for (y in covid_years) {
    df[[paste0("covid_", y)]] <- as.integer(df$year == y)
  }
  
  # Formula: log(value) ~ trend + post + covid dummies
  covid_terms <- paste0("covid_", covid_years)
  form <- as.formula(
    paste("log(value) ~ year_c + post +", paste(covid_terms, collapse = " + "))
  )
  
  fit <- lm(form, data = df)
  
  # Future years
  future <- tibble(year = (max(df$year)+1):end_year)
  future <- future %>%
    mutate(
      year_c = year - mean(df$year),
      post = as.integer(year >= post_covid_year)
    )
  for (y in covid_years) {
    future[[paste0("covid_", y)]] <- 0  # shocks assumed over
  }
  
  # Predict on original scale
  preds <- predict(fit, newdata = future, se.fit = TRUE)
  tibble(
    year = future$year,
    projection = exp(preds$fit)
  )
}

#--- Example usage ---
# Suppose your data looks like:
# df <- tibble(
#   year = 2000:2024,
#   disease = "HIV",
#   metric = "incidence",
#   value = runif(25, 10, 100)  # placeholder
# )

# Apply per disease x metric
# projections <- df %>%
#   group_by(disease, metric) %>%
#   group_modify(~ fit_projection(.x, end_year = 2028)) %>%
#   ungroup()

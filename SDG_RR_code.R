# First Version: 3rd March 2026 by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code generates data for RR vizualization at country-level
# Regional will be added later 
# The Central switchboard provides all options to define parameters for reporting
# It uses milestone based SDG for TB and malaria and the same projection functionality
# as applied to the KPI reporting

rm(list = ls())

#######################
# Central Switchboard #
#######################

## Set the user
firstrun   = 0                            # If need to install package change to 1
computer   = 1                            # 1 = Mikaela # Add additional computer if needed

# ========== SET YEAR PREFERENCES ==========
start_year_min       = 2010                 # This cuts off the first year and removes years were HIVneg with the lag are wrong
end_year_data        = 2024                 # This is the year of latest partner data, used as anchor year for projections
end_year_sdg         = 2030                 # This is the final year of prediction

start_year_gp_hiv    = 2020
start_year_gp_tm     = 2015

norm_year            = 2020       # year at which the rates will be normalized for the graphs
.eps                 = 1e-9       # guard for log(0)

# ==== DEFINE PROJECT ROOT (EDIT ONLY IF MOVED) ====
project_root <- "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration"

#data_path    <- file.path(project_root, "Data")
#output_path  <- file.path(project_root, "ROutput")
rcode_path   <- file.path(project_root, "RCode")

# ---- Source lower-bound engine ----
engine_file <- file.path(rcode_path, "lb_engine.R")
source(engine_file)

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
df_tb_gp2      = read.csv("tb_sdg_milestones.csv", stringsAsFactors = FALSE)
df_malaria_gp2 = read.csv("malaria_sdg_milestones.csv", stringsAsFactors = FALSE)

# Load list of countries
df_iso_hiv     = read.csv("List of countries/hiv_iso.csv", stringsAsFactors = FALSE)
df_iso_tb      = read.csv("List of countries/tb_iso_v2.csv", stringsAsFactors = FALSE)
df_iso_malaria = read.csv("List of countries/malaria_iso_v2.csv", stringsAsFactors = FALSE)

# List of indicators
list_indc_hiv_pip     = c("ISO3", "Year", "cases", "deaths", "HIVpos_n_pip", "Population_n_pip", "HIVneg", "incidence", "mortality")
list_indc_tb_pip      = c("ISO3", "Year", "cases", "deaths", "deathshivneg", "tb_pop_n_pip","incidence", "mortality", "mortality_hivneg")
list_indc_malaria_pip = c("ISO3", "Year", "cases", "deaths", "malaria_par_n_pip","incidence", "mortality")  

# data that needs to be removed because nto public for hiv - map countries and indicator
#countries_to_na_aids_deaths <- c("CAF", "CMR", "GNQ", "KAZ", "MDV", "PRK", "STP", "UKR", "VEN")
#countries_to_na_hiv_inc <- c("CAF", "CMR", "GNQ", "MDG", "MDV", "PAK", "PRK", "SOM", "STP", "UKR")


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
df_tb_gp2      = df_tb_gp2[order(df_tb_gp2$iso3),]
df_malaria_gp2 = df_malaria_gp2[order(df_malaria_gp2$iso3),]

# Filter GP df to key indicators
#df_tb_gp      = subset(df_tb_gp, select = c(ISO3, Year, Cases, Deaths, Population))
#df_malaria_gp2 = subset(df_malaria_gp2, select = -c(country))


# Filter out countries we do not need: 
df_hiv_gp2       = filter(df_hiv_gp2, country %in% df_iso_hiv$ISO3)                 # by eligible countries
df_tb_gp2        = filter(df_tb_gp2, iso3 %in% df_iso_tb$ISO3)                      # by eligible countries
df_malaria_gp2   = filter(df_malaria_gp2, iso3 %in% df_iso_malaria$ISO3)            # by eligible countries

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
    ISO3             = iso3,
    Year             = year,
    cases_sdg        = sdg_cases,
    deaths_sdg       = sdg_deaths_all,
    deathshivneg_sdg = sdg_deaths_hivneg, 
    Population       = pop,
  ) %>%
  select(ISO3, Year, cases_sdg, deaths_sdg, deathshivneg_sdg, Population)

df_malaria_gp2 <- df_malaria_gp2 %>%
  rename(
    ISO3          = iso3,
    Year          = year,
    cases_sdg     = sdg_cases,
    deaths_sdg    = sdg_deaths,
    par_sdg       = par,
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
    ratio_cases        = (cases/cases_sdg),
    ratio_deaths       = (deaths/deaths_sdg),
    ratio_deathshivneg = (deathshivneg/deathshivneg_sdg), 
    ratio_population   = (tb_pop_n_pip/Population)
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
    deaths_sdg    = (deaths_sdg*ratio_deaths),
    deathshivneg_sdg = (deathshivneg_sdg*ratio_deathshivneg),
    Population_sdg= (Population*ratio_population)
  )

df_malaria_gp = df_malaria_gp %>% left_join(df_malaria_gp_baseline, by='ISO3')
df_malaria_gp = df_malaria_gp %>%
  mutate(
    cases_sdg  = (cases_sdg *ratio_cases),
    deaths_sdg = (deaths_sdg*ratio_deaths),
    par_sdg    = (par_sdg*ratio_par)
  )

#Keep only data we need for full time series
df_hiv_gp     = subset(df_hiv_gp, select = -c(ratio_cases, ratio_deaths, ratio_hivneg, ratio_population))
df_tb_gp      = subset(df_tb_gp, select = -c(Population, ratio_cases, ratio_deaths, ratio_deathshivneg, ratio_population))
df_malaria_gp = subset(df_malaria_gp, select = -c(ratio_cases, ratio_deaths, ratio_par))




# =========================================================
# COUNTRY-LEVEL DATA: create country-level csv and plots
# =========================================================

# ===================== MAKE PROJECTIONS ==================
df_hiv_long <- df_hiv %>%
  pivot_longer(c(cases, deaths), names_to = "metric", values_to = "value") %>%
  mutate(value_scaled = value, scale_label = metric)


df_tb_long <- df_tb %>%
  pivot_longer(c(incidence, deaths), names_to = "metric", values_to = "value") %>%
  mutate(value_scaled = value, scale_label = metric)


df_malaria_long <- df_malaria %>%
  pivot_longer(c(incidence, mortality), names_to = "metric", values_to = "value") %>%
  mutate(value_scaled = value, scale_label = metric)



# Make projections
df_hiv_lb <- run_lower_bound_country(df_hiv_long, "hiv",
                                     anchor_year = end_year_data,
                                     end_year    = end_year_sdg,
                                     include_2024_threshold     = include_2024_if_within_log_ratio,
                                     guard_linear_if_increasing = guard_linear_if_increasing) %>%
  dplyr::mutate(disease = "hiv")

df_tb_lb  <- run_lower_bound_country(df_tb_long, "tb",
                                     anchor_year = end_year_data,
                                     end_year    = end_year_sdg,
                                     include_2024_threshold     = include_2024_if_within_log_ratio,
                                     guard_linear_if_increasing = guard_linear_if_increasing) %>%
  dplyr::mutate(disease = "tb")

df_mal_lb <- run_lower_bound_country(df_malaria_long, "malaria",
                                     anchor_year = end_year_data,
                                     end_year    = end_year_sdg,
                                     include_2024_threshold     = include_2024_if_within_log_ratio,
                                     guard_linear_if_increasing = guard_linear_if_increasing) %>%
  dplyr::mutate(disease = "malaria")


# Plot the country level projections for sanity 
pdf(file.path(output_path, "HIV_LB_projections_by_country_SDG.pdf"), width = 12, height = 6)
for (c in unique(df_hiv_lb$ISO3)) {
  print(plot_country(c, df_hiv_lb, start_year_min, end_year_sdg))
}
dev.off()

pdf(file.path(output_path, "TB_LB_projections_by_country_SDG.pdf"), width = 12, height = 6)
for (c in unique(df_tb_lb$ISO3)) {
  print(plot_country(c, df_tb_lb, start_year_min, end_year_sdg))
}
dev.off()

pdf(file.path(output_path, "Malaria_LB_projections_by_country_SDG.pdf"), width = 12, height = 6)
for (c in unique(df_mal_lb$ISO3)) {
  print(plot_country(c, df_mal_lb, start_year_min, end_year_sdg))
}
dev.off()


# Reformat
df_hiv_proj <- df_hiv_lb %>%
  dplyr::select(ISO3, Year, metric, projection) %>%
  tidyr::pivot_wider(
    names_from  = metric,
    values_from = projection
  )

df_tb_proj <- df_tb_lb %>%
  dplyr::select(ISO3, Year, metric, projection) %>%
  tidyr::pivot_wider(
    names_from  = metric,
    values_from = projection
  )

df_mal_proj <- df_mal_lb %>%
  dplyr::select(ISO3, Year, metric, projection) %>%
  tidyr::pivot_wider(
    names_from  = metric,
    values_from = projection
  )



# ===================== COMBINE PROJECTIONS AND SDG AND SAVE FINAL DATA ==================
df_hiv_final = df_hiv_proj %>% left_join(df_hiv_gp, by=c('ISO3', 'Year')) 
df_tb_final  = df_tb_proj %>% left_join(df_tb_gp, by=c('ISO3', 'Year')) 
df_mal_final = df_mal_proj %>% left_join(df_malaria_gp, by=c('ISO3', 'Year')) 


# Remove variables not needed
df_hiv_final <- df_hiv_final %>%
  select(-c(HIVneg_sdg, -Population_sdg))

df_tb_final <- df_tb_final %>%
  mutate(
    incidence_sdg = cases_sdg / Population_sdg,
  ) %>%
  select(-cases_sdg, -deathshivneg_sdg, -Population_sdg)

df_mal_final <- df_mal_final %>%
  mutate(
    incidence_sdg = cases_sdg / par_sdg,
    mortality_sdg = deaths_sdg / par_sdg
  ) %>%
  select(-cases_sdg, -deaths_sdg, -par_sdg)


# Save for QA
write.csv(df_hiv_final, file=file.path(output_path,"df_hiv_forSDG.csv"), na = "", row.names=FALSE)
write.csv(df_tb_final, file=file.path(output_path,"df_tb_SDG.csv"), na = "", row.names=FALSE)
write.csv(df_mal_final, file=file.path(output_path,"df_mal_forSDG.csv"), na = "", row.names=FALSE)



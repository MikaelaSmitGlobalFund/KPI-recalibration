
# First Version: 10th March by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will generate the country groups (which end year)
# Central switchboard gives you option of choosing which diseases to run
# It will also map the country name to ISO names 


rm(list = ls())

# Libraries
library(dplyr)

# Set wd
setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/Raw data")

hiv_groups = read.csv("hiv_group.csv", stringsAsFactors = FALSE)
tb_groups = read.csv("tb_group.csv", stringsAsFactors = FALSE)
mal_groups = read.csv("mal_group.csv", stringsAsFactors = FALSE)

elig_iso = read_excel(
  "core_historicaleligibility_database_en.xlsx",
  , sheet = "Eligibility Lists"

)

# Change Country to ISO3 code

# Keep only unique Country-ISO3 pairs
elig_iso_small <- elig_iso %>% distinct(Country, ISO3, .keep_all = FALSE)

hiv_groups <- left_join(hiv_groups, elig_iso_small, by = "Country")
hiv_groups <- hiv_groups %>% select(-Country)

tb_groups <- left_join(tb_groups, elig_iso_small, by = "Country")
tb_groups <- tb_groups %>% select(-Country)

mal_groups <- left_join(mal_groups, elig_iso_small, by = "Country")
mal_groups <- mal_groups %>% select(-Country)


# Regarrange
hiv_groups <- hiv_groups %>%
  select(ISO3, EndYear, everything(), -MaxYear)

tb_groups <- tb_groups %>%
  select(ISO3, EndYear, everything(), -MaxYear)

mal_groups <- mal_groups %>%
  select(ISO3, EndYear, everything(), -MaxYear)


# Make groups
hiv_groups <- hiv_groups %>%
  mutate(Group = case_when(
    EndYear == 2026 ~ "A",
    EndYear > 2026  ~ "B",
  ))

tb_groups <- tb_groups %>%
  mutate(Group = case_when(
    EndYear == 2026 ~ "A",
    EndYear > 2026  ~ "B",
  ))

mal_groups <- mal_groups %>%
  mutate(Group = case_when(
    EndYear == 2026 ~ "A",
    EndYear > 2026  ~ "B",
  ))


# Save files
write_csv(
  hiv_groups,
  "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/hiv_groups.csv"
)

write_csv(
  tb_groups,
  "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/tb_groups.csv"
)

write_csv(
  mal_groups,
  "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/mal_groups.csv"
)

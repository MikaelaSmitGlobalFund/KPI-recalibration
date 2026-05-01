library(dplyr)
library(tidyr)
# Install the readxl package if you haven't already
install.packages("readxl")

# Load the readxl package
library(readxl)

# Read the Excel file into a data frame
file_path <- "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/Raw Data/hiv_input_28March2024.xlsx"
df <- read_excel(file_path)

# Step 1: Extract the rows where data_type is "targets"
targets_df <- df %>%
  filter(data_type == "targets")

# Step 2 & 3: Sum columns ending in "_n" (except art_n) and take the last value for everything else
processed_targets_df <- targets_df %>%
  group_by(iso3) %>%
  summarize(
    # Sum columns ending in "_n" except art_n
    across(ends_with("_n") & !all_of(c("art_n")), ~ if (all(is.na(.x))) NA else sum(.x, na.rm = TRUE)),
    # Take the last value for art_n
    art_n = last(art_n[!is.na(art_n)]),
    # Take the last value for everything else
    across(!ends_with("_n") & !all_of(c("art_n")), ~ last(.x[!is.na(.x)])),
    .groups = "drop"
  )

# Step 3. remove variables we do not need and fromat for merge
# remove columns we do not need
processed_targets_df <- processed_targets_df %>%
  select(-c(ost_n, prep_n, pwid_prep_n, tg_prep_n, agyw_prep_n,ipt_p, pwid_reached_p, tg_reached_p, vls_p, status_p))

# Identify numeric columns
numeric_cols <- processed_targets_df %>%
  select(where(is.numeric)) %>%
  names()

# Pivot processed_targets_df to long format
long_df <- processed_targets_df %>%
  pivot_longer(
    cols = all_of(numeric_cols),
    names_to = "Indicator",
    values_to = "Value"
  )

# Rename to map to service df
long_df <- long_df %>%
  mutate(Indicator = recode(Indicator,
                       pmtct_n = "PMTCT_num",
                       pmtct_p = "PMTCT_cov",
                       msm_prep_n = "MSM_PrEP",
                       msm_reached_p = "MSM_cov",
                       sw_prep_n = "FSW_PrEP",
                       sw_reached_p = "FSW_cov",
                       pwid_prep_n = "PWID_PrEP",
                       vmmc_n = "VMMC_n",
                       art_n = "ART_total", 
                       art_p = "ART_cov")) %>%
  rename(ISO3 = iso3, Value2 = Value, Name = Indicator)


# 4. Merge
df2 = merge (long_df, df_processed, by = c("ISO3", "Name"))

df2 <- df2 %>%
  select(-c(year, data_type, Year, Component, Indicator))

df2 = df2 %>%
  mutate(Diff = Value/Value2)


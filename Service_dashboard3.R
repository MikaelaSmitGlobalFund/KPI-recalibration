# First Version: 25th October by Mikaela Smit


# SCRIPT DISCRIPTION: 
# This code will generate the key impact measures
# Central switchboard gives you option of choosing which diseases to run
# Note that the benchmarking code does not make rows for non-modelled countries (e.g. TB for SSD)
# This would need to be added to the code (use eligeible iso by disease)


rm(list = ls())

#######################
# Central Switchboard #
#######################

# User
firstrun   = 0                            # If need to install package change to 1
computer   = 1                            # 1 = Mikaela # Add additional computer if needed

# Set Year reference
start_year = 2027 # Note because the model assumes constant coverage in the IC we aggregate all numbers for 2027-2029 rather than groupA for 2027:2029 and groupB 2028:2030 as this may reduce numbers
end_year   = 2029 


# Which diseases do we run here
HIV        = 1                            # 1=yes; 0=no
Malaria    = 1
TB         = 1

# Which scenarios to run
scenario_output = c("IC")     # Choose either: "ConsFund" OR "ConsOptFund" OR "AdjPF_ConsFund" OR "AdPF_ConsOptFund"

# Set output file
date = Sys.Date()                        # Set data


# Install packages if neccesary
if(firstrun>0) {
  install.packages("dplyr")
}

# Load Libraries
library(dplyr)
library(data.table)   # For like function (%like%)
#library(zoo)
library(tidyr)


# Set computer and wd
#Load the output files and set output path
if (computer ==1){
  setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data/MCP data")
  output_path = "/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/ROutput"
  
  if(HIV==1){
    df_hiv2 = read.csv("dump_ic_hiv.csv", stringsAsFactors = FALSE)
  }
  
  if(Malaria==1){
    df_malaria2  = read.csv("dump_ic_malaria.csv", stringsAsFactors = FALSE)
  }
  
  if(TB==1){
    df_tb2      =  read.csv("dump_ic_tb.csv", stringsAsFactors = FALSE)
  }
  
  setwd("/Users/mc1405/Dropbox/The Global Fund/KPI re-calibration/Data")
  hiv_groups = read.csv("hiv_groups.csv", stringsAsFactors = FALSE)
  hiv_groups = hiv_groups[!duplicated(hiv_groups), ]
  hiv_groups = hiv_groups %>% select(-EndYear)
  
  malaria_groups = read.csv("mal_groups.csv", stringsAsFactors = FALSE)
  malaria_groups <- malaria_groups[!duplicated(malaria_groups), ]
  malaria_groups = malaria_groups %>% select(-EndYear)
  
  tb_groups = read.csv("tb_groups.csv", stringsAsFactors = FALSE)
  tb_groups <- tb_groups[!duplicated(tb_groups), ]
  tb_groups = tb_groups %>% select(-EndYear)
  
  #region = read.csv("region.csv", stringsAsFactors = FALSE)
  #df_hiv_iso     = read.csv("List of countries/hiv_iso.csv", stringsAsFactors = FALSE)
  #df_tb_iso      = read.csv("List of countries/tb_iso.csv", stringsAsFactors = FALSE)
  #df_malaria_iso = read.csv("List of countries/malaria_iso.csv", stringsAsFactors = FALSE)
} 



###################################
## Consolidate naming convention ##
###################################

df_hiv2_wide <- df_hiv2 %>%
  rename(ISO3 = country, Year = year, Scenario = scenario_descriptor) %>%
  pivot_wider(
    id_cols = c(ISO3, Year, Scenario),
    names_from = indicator,
    values_from = c(model_central, model_low, model_high),
    names_glue = "{indicator}_{.value}"
  ) %>%
  rename_with(
    ~ gsub("model_low", "LB", .) %>%
      gsub("model_high", "UB", .) %>%
      gsub("_model_central", "", .),
    cols = -c(ISO3, Year, Scenario)
  )


df_hiv <- df_hiv2_wide %>%
  rename(
    # ART
    ART_total = art,
    ART_total_LB = art_LB,
    ART_total_UB = art_UB,
    # PMTCT
    PMTCT_num = pmtct,
    PMTCT_num_LB = pmtct_LB,
    PMTCT_num_UB = pmtct_UB,
    # Reached
    FSW_reached = fswreached,
    FSW_reached_LB = fswreached_LB,
    FSW_reached_UB = fswreached_UB,
    MSM_reached = msmreached,
    MSM_reached_LB = msmreached_LB,
    MSM_reached_UB = msmreached_UB,
    PWID_reached = pwidreached,
    PWID_reached_LB = pwidreached_LB,
    PWID_reached_UB = pwidreached_UB,
    # Coverage
    FSW_cov = fswcoverage,
    MSM_cov = msmcoverage,
    PWID_cov = pwidcoverage,
    # PrEP
    MSM_PrEP = msmprep,
    MSM_PrEP_LB = msmprep_LB,
    MSM_PrEP_UB = msmprep_UB,
    FSW_PrEP = fswprep,
    FSW_PrEP_LB = fswprep_LB,
    FSW_PrEP_UB = fswprep_UB,
    # OST
    OST_n = ost,
    OST_n_LB = ost_LB,
    OST_n_UB = ost_UB,
    # VMMC
    VMMC_n = vmmc,
    VMMC_n_LB = vmmc_LB,
    VMMC_n_UB = vmmc_UB,
    # Population denominators
    PLHIV = plhiv,
    PMTCT_need = pmtctneed
  ) %>%
  select(
    ISO3, Year, Scenario,
    ART_total, ART_total_LB, ART_total_UB,
    PMTCT_num, PMTCT_num_LB, PMTCT_num_UB,
    FSW_reached, FSW_reached_LB, FSW_reached_UB,
    MSM_reached, MSM_reached_LB, MSM_reached_UB,
    PWID_reached, PWID_reached_LB, PWID_reached_UB,
    FSW_cov, 
    MSM_cov, 
    PWID_cov, 
    MSM_PrEP, MSM_PrEP_LB, MSM_PrEP_UB,
    FSW_PrEP, FSW_PrEP_LB, FSW_PrEP_UB,
    OST_n, OST_n_LB, OST_n_UB,
    VMMC_n, VMMC_n_LB, VMMC_n_UB,
    PLHIV, PMTCT_need
  )


df_hiv <- df_hiv %>%
  mutate(
    # Calculate denominators from existing coverage
    ART_cov    = ifelse(PLHIV != 0, ART_total / PLHIV, 0),
    ART_cov_LB = ifelse(PLHIV != 0, ART_total_LB / PLHIV, 0),
    ART_cov_UB = ifelse(PLHIV != 0, ART_total_UB / PLHIV, 0),
    
    # PMTCT
    PMTCT_cov    = ifelse(PMTCT_need != 0, PMTCT_num / PMTCT_need, 0),
    PMTCT_cov_LB = ifelse(PMTCT_need != 0, PMTCT_num_LB / PMTCT_need, 0),
    PMTCT_cov_UB = ifelse(PMTCT_need != 0, PMTCT_num_UB / PMTCT_need, 0),
    
    # KP pop
    MSM_pop = ifelse(MSM_cov != 0, MSM_reached / MSM_cov, 0),
    FSW_pop = ifelse(FSW_cov != 0, FSW_reached / FSW_cov, 0),
    PWID_pop = ifelse(PWID_cov != 0, PWID_reached / PWID_cov, 0),
    
    # KP coverage
    FSW_cov_LB = ifelse(FSW_pop != 0, FSW_reached_LB / FSW_pop, 0),
    FSW_cov_UB = ifelse(FSW_pop != 0, FSW_reached_UB / FSW_pop, 0),
    MSM_cov_LB = ifelse(MSM_pop != 0, MSM_reached_LB / MSM_pop, 0),
    MSM_cov_UB = ifelse(MSM_pop != 0, MSM_reached_UB / MSM_pop, 0),
    PWID_cov_LB = ifelse(PWID_pop != 0, PWID_reached_LB / PWID_pop, 0),
    PWID_cov_UB = ifelse(PWID_pop != 0, PWID_reached_UB / PWID_pop, 0),
    
    # Calculate OST coverage from PWID_pop
    OST_cov = ifelse(PWID_pop != 0, OST_n / PWID_pop, 0),
    OST_cov_LB = ifelse(PWID_pop != 0, OST_n_LB / PWID_pop, 0),
    OST_cov_UB = ifelse(PWID_pop != 0, OST_n_UB / PWID_pop, 0)
  )


df_hiv <- df_hiv %>%
  select(
    ISO3, Year, Scenario,
    # Get all other columns, split by prefix, and sort alphabetically
    sort(names(.)[!names(.) %in% c("ISO3", "Year", "Scenario")])
  )


# MALARIA
df_mal2_wide <- df_malaria2 %>%
  rename(ISO3 = country, Year = year, Scenario = scenario_descriptor) %>%
  pivot_wider(
    id_cols = c(ISO3, Year, Scenario),
    names_from = indicator,
    values_from = c(model_central, model_low, model_high),
    names_glue = "{indicator}_{.value}"
  ) %>%
  rename_with(
    ~ gsub("model_low", "LB", .) %>%
      gsub("model_high", "UB", .) %>%
      gsub("_model_central", "", .),
    cols = -c(ISO3, Year, Scenario)
  )


df_malaria <- df_mal2_wide %>%
  rename(
    # Nets
    nets_distributed = llins,
    nets_distributed_LB = llins_LB,
    nets_distributed_UB = llins_UB,
    # SMC
    smc_targeted_cov = smccoverage,
    smc_targeted_cov_LB = smccoverage_LB,
    smc_targeted_cov_UB = smccoverage_UB,
    smc_par = partargetedsmc,  # Assuming 'smc' is the new name for 'smc_par'

    # ITN use
    itn_use_p = llinsuse,
    itn_use_p_LB = llinsuse_LB,
    itn_use_p_UB = llinsuse_UB,
    # Population
    par = par
  ) %>%
  select(
    ISO3, Year, Scenario,
    nets_distributed, nets_distributed_LB, nets_distributed_UB,
    smc_targeted_cov, smc_targeted_cov_LB, smc_targeted_cov_UB,
    itn_use_p, itn_use_p_LB, itn_use_p_UB,
    par, smc_par,
  )


df_malaria <- df_malaria %>%
  mutate(
    smc_n = smc_targeted_cov * smc_par,
    smc_n_LB = smc_targeted_cov_LB * smc_par,
    smc_n_UB = smc_targeted_cov_UB * smc_par
  )

df_malaria <- df_malaria %>%
  select(
    ISO3, Year, Scenario,
    # Get all other columns, split by prefix, and sort alphabetically
    sort(names(.)[!names(.) %in% c("ISO3", "Year", "Scenario")])
  )


# TB
df_tb2_wide <- df_tb2 %>%
  rename(ISO3 = country, Year = year, Scenario = scenario_descriptor) %>%
  pivot_wider(
    id_cols = c(ISO3, Year, Scenario),
    names_from = indicator,
    values_from = c(model_central, model_low, model_high),
    names_glue = "{indicator}_{.value}"
  ) %>%
  rename_with(
    ~ gsub("model_low", "LB", .) %>%
      gsub("model_high", "UB", .) %>%
      gsub("_model_central", "", .),
    cols = -c(ISO3, Year, Scenario)
  )


# Subset and rename the data
df_tb <- df_tb2_wide %>%
  rename(
    # Notified
    Notified_n = notified,
    Notified_n_LB = notified_LB,
    Notified_n_UB = notified_UB,
    
    # MDR Tx
    mdr_Tx = mdrTx,
    mdr_Tx_LB = mdrTx_LB,
    mdr_Tx_UB = mdrTx_UB,

    # TB ART
    tb_art_n = tbart,
    tb_art_n_LB = tbart_LB,
    tb_art_n_UB = tbart_UB,
    
    # MDR Notified
    mdr_notified_n = mdrnotified,
    mdr_notified_n_LB = mdrnotified_LB,
    mdr_notified_n_UB = mdrnotified_UB,
    
    # TB ART Numbers
    tb_art_n = tbart,
    tb_art_n_LB = tbart_LB,
    tb_art_n_UB = tbart_UB,
    
    # TB ART Coverage
    tb_art_p = tbartcoverage,
    
    # New Cases
    NewCases = cases
  ) %>%
  select(
    ISO3, Year, Scenario,
    Notified_n, Notified_n_LB, Notified_n_UB,
    mdr_Tx, mdr_Tx_LB, mdr_Tx_UB,
    tb_art_n, tb_art_n_LB, tb_art_n_UB,
    mdr_notified_n, mdr_notified_n_LB, mdr_notified_n_UB,
    tb_art_n, tb_art_n_LB, tb_art_n_UB,
    tb_art_p,
    NewCases
  )

# Calculate HIVpop
df_tb = df_tb %>%
  mutate(
    HIVpop = tb_art_n / tb_art_p,
    tb_art_p_LB = tb_art_n_LB / HIVpop,
    tb_art_p_UB = tb_art_n_UB / HIVpop,
    
    # Calculate mdr_Tx_p and its bounds
    mdr_Tx_p = mdr_Tx / mdr_notified_n,
    mdr_Tx_p_LB = mdr_Tx_LB / mdr_notified_n,
    mdr_Tx_p_UB = mdr_Tx_UB / mdr_notified_n,
    
    # Calculate Notified_p and its bounds
    Notified_p = Notified_n / NewCases,
    Notified_p_LB = Notified_n_LB / NewCases,
    Notified_p_UB = Notified_n_UB / NewCases,
  )


df_tb <- df_tb %>%
  select(
    ISO3, Year, Scenario,
    # Get all other columns, split by prefix, and sort alphabetically
    sort(names(.)[!names(.) %in% c("ISO3", "Year", "Scenario")])
  )



####################
## Prepare output ##
####################

# Subset the data for TA for that year 
df_hiv     = df_hiv %>% dplyr::filter(Scenario %in% scenario_output)
df_hiv     = subset(df_hiv, select = -c(Scenario))
df_hiv     = subset(df_hiv, select = -c(PLHIV, PMTCT_need, FSW_pop, MSM_pop, PWID_pop))
hiv_variables    = names(df_hiv)
hiv_variables    = hiv_variables[-c(1,2)]
df_hiv           = df_hiv %>% pivot_longer(cols = hiv_variables,names_to = "Name", values_to = "Value")
df_hiv = merge (df_hiv, hiv_groups, by = c("ISO3"), all=T)
df_hiv$Component = "HIV"

df_malaria     = df_malaria %>% dplyr::filter(Scenario %in% scenario_output)
df_malaria     = subset(df_malaria, select = -c(Scenario))
df_malaria     = subset(df_malaria, select = -c(par, smc_par))
malaria_variables    = names(df_malaria)
malaria_variables    = malaria_variables[-c(1,2)]
df_malaria           = df_malaria %>% pivot_longer(cols = malaria_variables, names_to = "Name", values_to = "Value")
df_malaria = merge (df_malaria, malaria_groups, by = c("ISO3"), all=T)
df_malaria$Component = "Malaria"

df_tb     = df_tb %>% dplyr::filter(Scenario %in% scenario_output)
df_tb     = subset(df_tb, select = -c(Scenario))
df_tb     = subset(df_tb, select = -c(HIVpop, NewCases))
tb_variables    = names(df_tb)
tb_variables    = tb_variables[-c(1,2)]
df_tb           = df_tb %>% pivot_longer(cols = tb_variables, names_to = "Name", values_to = "Value")
df_tb = merge (df_tb, tb_groups, by = c("ISO3"), all=T)
df_tb$Component = "TB"


##### AT THE END OF MODEL ####
df_ct_projections = rbind(df_hiv, df_malaria)
df_ct_projections = rbind(df_ct_projections, df_tb)

# Filter for correct years
df_ct_projections = df_ct_projections %>% filter(Year>start_year-1 & Year <end_year+1)


##### AT THE VERY END  #####
# Data Type
df_ct_projections = df_ct_projections %>%
  mutate(DataType = case_when(grepl("_LB", Name) ~ "Lower",
                              grepl("_UB", Name ) ~ "Upper",
                              TRUE ~ "Point"))

df_ct_projections$Category = "Projections"


# Indicator
df_ct_projections = df_ct_projections %>%
  dplyr::mutate(Indicator = case_when(
    grepl("ART_total", Name)              ~ "# of people on ART [TCS-1.1, TCS-1b, TCS-1c ]", 
    grepl("ART_cov", Name)                ~ "% of people on ART among all people  living with HIV [TCS-1.1, TCS-1b, TCS-1c]",
    grepl("PMTCT_n", Name)                ~ "# of pregnant women who received ART [TCS-10 ]",
    grepl("PMTCT_cov", Name)              ~ "% of pregnant women who received ART to reduce the risk of vertical transmission of HIV [TCS-10]",
    grepl("FSW_cov", Name)                ~ "% of female sex workers reached with HIV prevention programs [KP-1c]",
    grepl("MSM_cov", Name)                ~ "% of men who have sex with men reached with HIV prevention programs [KP-1a]",
    grepl("PWID_cov", Name)               ~ "% of people who inject drugs reached with HIV prevention programs [KP-1d]",
    grepl("VMMC", Name)                   ~ "# of medical male circumcisions [YP-6 ]",
    grepl("OST_cov", Name)                ~ "% of people receiving Opioid Substitution Therapy [KP-8 ]",
    grepl("MSM_Pr", Name)                 ~ "# of men who have sex with men using pre-explosure prophylaxis [KP-6a ]",
    grepl("FSW_Pr", Name)                 ~ "# of female sex workers using pre-exposure prophylaxis [KP-6c ]",
    # grepl("AGYW_Pr", Name)                 ~ "# of adolescent girls or young women using pre-exposure prophylaxis [YP-4 ]",
    
    grepl("Notified_n", Name)             ~ "# of patients with all forms of TB notified (only new and relapse) [TBDT-1]",
    grepl("Notified_p", Name)             ~ "% of estimated new TB cases notified - TB treatment coverage [TB-O5]",
    grepl("mdr_Tx_p", Name)               ~ "% of people with confirmed RR-TB and/or MDR-TB that began second-line treatment [DRTB-3]",
    grepl("mdr_Tx_n", Name)               ~ "# of people with confirmed RR-TB and/or MDR-TB that began second-line treatment [DRTB-3]",
    grepl("tb_art_p", Name)               ~ "% of HIV-positive new and relapse TB patients on ART during TB treatment [TB/HIV-6]",
    grepl("tb_art_n", Name)               ~ "# of HIV-positive new and relapse TB patients on ART during TB treatment [TB/HIV-6]",
    
    
    grepl("nets_distributed", Name)       ~ "# of LLINs distributed through mass campaign and continuous distribution [VC-1 + VC-3]", 
    grepl("smc_targeted_cov", Name)       ~ "% of children who received the full number of courses of SMC per transmission season [SPI-2.1]", 
    # grepl("irs", Name)                    ~ "# households sprayed with Indoor residual spraying"
    grepl("itn_access_p", Name)           ~ "% of population with access to an ITN [Malaria O-2]",
    # grepl("itn_use_p", Name)                ~ "% of population that slept under an insecticide-treated net [Malaria O-1a]",
    # grepl("vector", Name)               ~ "% of population with access to vector control coverage"
  ))

# # Update order
df_ct_projections = df_ct_projections %>% relocate(Category, .before = Name)



# Take sum of three years or last year
df_processed <- df_ct_projections %>%
  # Group by Indicator and other relevant columns (except Year and Value)
  group_by(ISO3, Name, Group, Component, DataType, Indicator) %>%
  # Sum values for indicators starting with #
  mutate(
    Value = if_else(grepl("^#", Indicator), sum(Value, na.rm = TRUE), Value)
  ) %>%
  # For indicators starting with %, take the value from the last year (end_year)
  group_by(ISO3, Name, Group, Component, DataType, Indicator) %>%
  slice(if (any(grepl("^%", Indicator))) {
    # For % indicators, keep only the row with Year == end_year
    which(Year == end_year)
  } else {
    1:n()
  }) %>%
  # Set Year to "GC8" for indicators starting with # or %
  mutate(Year = if_else(grepl("^#", Indicator) | grepl("^%", Indicator), "GC8", as.character(Year))) %>%
  ungroup()



# Cap at 100%
df_processed = df_processed %>%
  mutate(Value =  ifelse(Value>100 & grepl("%",df_processed$Indicator ) , 100, Value)) # Add year


# Save output array
if (computer==1) {
  write.csv(df_processed, paste0(output_path, "/", "dashboard3_capped_", date, "fv.csv"), row.names = FALSE)
}

# Extract names and variable
short_names = df_processed[!duplicated(df_processed[ , c("Name", "Indicator")]), ]
short_names = subset(short_names, select = c(Name, Indicator))

write.csv(short_names, paste0(output_path, "/", "variable_mapping", date, ".csv"), row.names = FALSE)




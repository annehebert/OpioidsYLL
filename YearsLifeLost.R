## code written and developed by Anne H. Hébert and Alison L. Hill

## this code estimates the years of life lost (YLL) and the Loss in Life Expectancy 
## to opioid overdose deaths in the United States, for the specified year and 
## demographic group.
## DATA USED:
## 1. NVSS lifetables, projecting US population mortality experience
## 2. All-cause and opioid-related death counts from CDC WONDER

setwd("~/Opioid_project/opioid_burden")
source("functions.R")
library(tidyverse)


#### Select settings ----
## select the following:
# - demographic variable to stratify by (race/ethn group and/or gender)
# - age grouping (5 year age groups reduce data suppression of opioid death counts)
# - year
# - whether to filter for polysubstance co-use

selectStratVars <- 'race and gender' #'race and gender' or 'none'
selectAgeGrouping <- 'fiveyear' # 'singleyear' or 'fiveyear'
selectYear <- '2021' # 2019, 2020, 2021 or 2022
selectCoOccurrence = 'both' #'meth' or 'cocaine' or 'none' or 'both'

# whether opioid deaths should be resampled with a poisson distribution or not
# for generating CIs
selectResampling <- FALSE 

# --------------------
#### IMPORT -----

## define which year's NVSS lifetables we use.
## lifetable data is often delayed relative to the death count data, so may need 
# to use earlier year. 
if (selectYear %in% c('2019', '2020', '2021')){
  selectLifetableYear <- selectYear} else if (selectYear == '2022'){
    selectLifetableYear <- '2021'
  }

# run data import function 
deathdata_list <- import.deaths.data(selectAgeGrouping, 
                                     selectStratVars, 
                                     selectCoOccurrence)
df_all_deaths <- deathdata_list[[1]]
df_opioids_deaths <- deathdata_list[[2]]
df_opioid_death_dist <- deathdata_list[[3]]
age_data = load("data/Ages.Rda")

#### --- Select race and gender -----
## select the gender and race/ethnicity group for which to estimate the YLL
## Note that these will simply be ignored if race or gender is not selected for 
## above via `selectStratVars`.
selectGender = "F" # options are ("F" or "M") 
selectRaceEthn = "AmericanIndian_NonHisp" # options are: (Hisp, AmericanIndian_NonHisp, 
# Asian_NonHisp, Black_NonHisp, White_NonHisp)

# define corresponding NVSS lifetable (this needs to be re-run when changing 
# 'selectGender' or 'selectRaceEthn')
df_lifetable_nvss <- import.nvss.lifetable(selectStratVars, 
                                           selectLifetableYear, 
                                           selectRaceEthn)
df_deaths_data_all <- format.deaths.data(df_data = df_all_deaths) %>% 
  rename(Deaths.all = Deaths)
df_deaths_data_od <- format.deaths.data(df_data = df_opioids_deaths) %>% 
  rename(Deaths.cause = Deaths)

calculate.yll(opioid_data = df_deaths_data_od, all_data = df_deaths_data_all, 
              resampling = selectResampling, stratVars = selectStratVars, 
              coOccurrence = selectCoOccurrence, 
              target_distribution = df_opioid_death_dist)




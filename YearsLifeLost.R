## code written and developed by Anne H. Hébert and Alison L. Hill

## this code estimates the years of life lost (YLL) to opioid overdose deaths in 
## the United States, for the specified year and demographic group.

setwd("~/Opioid_project/opioid_burden")
source("github_repo/functions.R")
library(tidyverse)

#--------------------------------------------------
##### 1. --- Specify stratification variables -----
#--------------------------------------------------


#### --- Specify desired year and strata -----
## `selectYear` specifies the year for which the YLL are estimated (ie which 
## year the death data is from)

## `selectLifetableYear` specifies which year's NVSS lifetables to use. The
## NVSS data seems to have more of a lag, so may need to use older tables.

## `selectStratVars` and `selectAgeGrouping` allows you to select by which 
## variables you want the YLL to be stratified. 

selectYear = '2021' #selects which year's death data is used
selectLifetableYear = '2020' #lifetable data delayed, so may need to use earlier year
selectStratVars = 'race and gender' #'race', 'gender' or 'race and gender' or 'none'
selectAgeGrouping = 'fiveyear' # 'singleyear' or 'fiveyear'
if (selectStratVars == 'race' | selectStratVars == 'gender'){
  selectAgeGrouping = 'singleyear' # 'singleyear' or 'fiveyear'
}

#### --- Select race and gender -----
## select the gender and race/ethnicity group for which to estimate the YLL
## Note that these will simply be ignored if race or gender is not selected for 
## above via `selectStratVars`.
selectGender = "M" # options are ("F" or "M") 
selectRaceEthn = "Black_NonHisp" # options are (Hisp, AmericanIndian_NonHisp, 
                                              # Asian_NonHisp, Black_NonHisp,
                                              # White_NonHisp, Multiple_NonHisp) 


#--------------------------------------------------
##### 2. --- Import data for YLL ----
#--------------------------------------------------

## Import and clean/format death data and lifetables. 
## Note, no need to re-run when changing `selectGender` or `selectRaceEthn`

load("data/Ages.Rda")
if (selectStratVars == 'race and gender'){
  ## All deaths by Age, Gender and Race
  df_all_deaths_age_gender_race <-
    read.delim(
      file = paste('data/All_deaths_',
                   selectYear, '_age_gender_race.txt',
                   sep = ""),
      header = TRUE,
      sep = "\t"
    )
  df_all_deaths_age_gender_race <-
    clean.mortality.data(df_all_deaths_age_gender_race)
  if (selectAgeGrouping == 'singleyear') {
    ## Overdose deaths by Age, Gender, Race
    df_opioid_deaths_age_gender_race <-
      read.delim(
        file = paste(
          'data/Opioid_deaths_',
          selectYear,
          '_age_gender_race.txt',
          sep = ""
        ),
        header = TRUE,
        sep = "\t"
      )
    df_opioid_deaths_age_gender_race <-
      clean.mortality.data(df_opioid_deaths_age_gender_race)
    
} else if (selectAgeGrouping == 'fiveyear'){
    ## Overdose deaths by Age, Gender, Race
    ## Data on race and Hispanic origin is downloaded separately to minimize 
    ## loss of data to censoring of counts <10.
    ## `df_opioid_deaths_age_gender_race` contains death counts by race for
    ## non-Hispanic people, and `df_opioid_deaths_age_gender_hisp_origin` has
    ## death counts for Hispanic people of all races.
  df_opioid_deaths_age_gender_race <- read.delim(
    file = paste(
      'data/Opioid_deaths_',
      selectYear,
      '_5yage_gender_race.txt',
      sep = ""
    ),
    header = TRUE,
    sep = "\t"
  )
  df_opioid_deaths_age_gender_hisp_origin <-
    read.delim(
      file = paste(
        'data/Opioid_deaths_',
        selectYear,
        '_5yage_gender_hisp_origin.txt',
        sep = ""
      ),
      header = TRUE,
      sep = "\t"
    )
  df_opioid_deaths_age_gender_race <-
    clean.mortality.data(df_opioid_deaths_age_gender_race)
  ## combine death counts of non-Hispanic and Hispanic people
  df_opioid_deaths_age_gender_race <-
    combine.hisp.data(racedata = df_opioid_deaths_age_gender_race,
                      hispdata = df_opioid_deaths_age_gender_hisp_origin)
  }
} else if (selectStratVars == 'race'){
  ## Overdose deaths by Age, Gender, Race
  df_opioid_deaths_age_race <-
    read.delim(
      file = paste('data/Opioid_deaths_',
                   selectYear, '_age_race.txt',
                   sep = ""),
      header = TRUE,
      sep = "\t"
    )
  df_opioid_deaths_age_race <-
    clean.mortality.data(df_opioid_deaths_age_race)
  
  ## All deaths by Age, Gender and Race
  df_all_deaths_age_race <-
    read.delim(
      file = paste('data/All_deaths_',
                   selectYear, '_age_race.txt',
                   sep = ""),
      header = TRUE,
      sep = "\t"
    )
  df_all_deaths_age_race <-
    clean.mortality.data(df_all_deaths_age_race)
  
} else if (selectStratVars == 'gender'){
  ## Overdose deaths by Age, Gender, Race
  df_opioid_deaths_age_gender <-
    read.delim(
      file = paste('data/Opioid_deaths_',
                   selectYear, '_age_gender.txt',
                   sep = ""),
      header = TRUE,
      sep = "\t"
    )
  df_opioid_deaths_age_gender <-
    clean.mortality.data(df_opioid_deaths_age_gender)
  
  ## All deaths by Age, Gender and Race
  df_all_deaths_age_gender <-
    read.delim(
      file = paste('data/All_deaths_',
                   selectYear, '_age_gender.txt',
                   sep = ""),
      header = TRUE,
      sep = "\t"
    )
  df_all_deaths_age_gender <-
    clean.mortality.data(df_all_deaths_age_gender)
  
} else if (selectStratVars == 'none'){
  ## All deaths by Age, Gender and Race
  df_all_deaths_age <- read.delim(
    file = paste('data/All_deaths_',
                 selectYear, '_age.txt',
                 sep = ""),
    header = TRUE,
    sep = "\t"
  )
  df_all_deaths_age <- clean.mortality.data(df_all_deaths_age)
  if (selectAgeGrouping == 'singleyear') {
    ## Overdose deaths by Age, Gender, Race
    df_opioid_deaths_age <-
      read.delim(
        file = paste('data/Opioid_deaths_',
                     selectYear, '_age.txt',
                     sep = ""),
        header = TRUE,
        sep = "\t"
      )
    df_opioid_deaths_age <-
      clean.mortality.data(df_opioid_deaths_age)
  } else   if (selectAgeGrouping == 'fiveyear') {
    ## Overdose deaths by Age, Gender, Race
    df_opioid_deaths_age <-
      read.delim(
        file = paste('data/Opioid_deaths_',
                     selectYear, '_5yage.txt',
                     sep = ""),
        header = TRUE,
        sep = "\t"
      )
    df_opioid_deaths_age <-
      clean.mortality.data(df_opioid_deaths_age)
  }
} 




#--------------------------------------------------
#### 3. --- Create dataframes w counts of all deaths and opioid deaths ----
#--------------------------------------------------

## !! DOES need to be re-run what changing any of the group/strata choices
## this loop defines `df_all_deaths` and `df_opioid_deaths` for the selected demographic
## group and year

## `df_all_deaths` contains total US mortality rates, calculated from all deaths in 
  ## selected year
## `df_opioid_deaths` contains opioid mortality rates, calculated from opioid-related
  ## deaths in selected year


if (selectStratVars == "gender") {
  ## race/ethnicity = all, separates only by gender
  
  ## filter columns in all deaths data and opioid-related deaths data to select 
  ## age, all deaths, crude rate, and population 
  df_all_deaths <- df_all_deaths_age_gender %>%
    filter(Gender == paste(selectGender)) %>%
    select(Ages.Text, Ages, Deaths, Crude.Rate, Population)
  df_opioid_deaths <- df_opioid_deaths_age_gender %>%
    filter(Gender == paste(selectGender)) %>%
    select(Ages.Text, Ages, Deaths, Crude.Rate, Population)
  
  ## import relevant NVSS lifetable, given race/eth/gender selected
  df_lifetable_nvss <- read.csv(paste("data/lifetables/",
                                      selectLifetableYear,"_Lifetable_",
                                      selectGender, ".csv",
                                      sep = ""), 
                                header = TRUE) 
  ## clean NVSS lifetable
  df_lifetable_nvss <- clean.lifetable(df_lifetable_nvss)
  
  
} else if(selectStratVars == 'race and gender'){
  ## for separating race/ethnicity and gender
  
  ## filter columns in all deaths data to select 
  ## age, all deaths, and population 
  df_all_deaths <- df_all_deaths_age_gender_race %>%
    filter(Gender == paste(selectGender) & Race.Eth == paste(selectRaceEthn)) %>%
    select(c("Ages.Text", "Ages", "Deaths", "Population"))
  df_all_deaths <- aggregate(cbind(Deaths, Population) ~ Ages + Ages.Text, 
                         df_all_deaths, 
                         sum)
  ## calculate crude death rate
  df_all_deaths <- df_all_deaths %>%
    mutate(Crude.Rate = Deaths/Population*1e5) %>%
    arrange(Ages)
  
  ## filter columns in opioid-related deaths data to select 
  ## age, all deaths, and population 
  df_opioid_deaths <- df_opioid_deaths_age_gender_race %>%
    filter(Gender == paste(selectGender) & 
             Race.Eth == paste(selectRaceEthn)) %>%
    select(c("Ages.Text", "Ages", "Deaths", "Population"))%>%
    mutate(Population = as.integer(Population),
           Ages = as.integer(Ages))
  df_opioid_deaths <- aggregate(cbind(Deaths, Population) ~ Ages + 
                              Ages.Text, df_opioid_deaths, sum)
  ## calculate crude death rate
  df_opioid_deaths <- df_opioid_deaths %>%
    mutate(Crude.Rate = Deaths/Population*1e5) %>%
    arrange(Ages)
  
  ## import relevant NVSS lifetable, given race/eth/gender selected
  if (selectRaceEthn != "Multiple_NonHisp"){
    df_lifetable_nvss <- read.csv(paste("data/lifetables/",
                                        selectLifetableYear,"_Lifetable_",
                                        selectGender,"_", selectRaceEthn, ".csv",
                                        sep = ""), 
                                  header = TRUE)
    df_lifetable_nvss <- clean.lifetable(df_lifetable_nvss)
  } else{ df_lifetable_nvss <- read.csv(paste("data/lifetables/",
                                              selectLifetableYear,"_Lifetable_",
                                              selectGender, ".csv",
                                              sep = ""), 
                                        header = TRUE)
  # clean nvss lifetable
  df_lifetable_nvss <- clean.lifetable(df_lifetable_nvss)
  
  }
} else if (selectStratVars == 'race') {
  # for separating race/ethnicity
  df_all_deaths<- subset(df_all_deaths_age_race, 
                      Race.Eth == paste(selectRaceEthn), 
                      select = c("Ages.Text", "Ages", "Deaths", "Population"))
  df_all_deaths<- aggregate(cbind(Deaths, Population) ~ Ages + 
                           Ages.Text, df_all_deaths, sum)
  df_all_deaths<- df_all_deaths%>%
    mutate(Crude.Rate = Deaths/Population*1e5) %>%
    arrange(Ages)
  
  df_opioid_deaths <- subset(df_opioid_deaths_age_race, 
                         Race.Eth == paste(selectRaceEthn), 
                         select = c("Ages.Text", "Ages", "Deaths", "Population"))
  df_opioid_deaths <- aggregate(cbind(Deaths, Population) ~ Ages + 
                              Ages.Text, df_opioid_deaths, sum)
  df_opioid_deaths <- df_opioid_deaths %>%
    mutate(Crude.Rate = Deaths/Population*1e5) %>%
    arrange(Ages)
  
  ## import relevant NVSS lifetable, given race/eth/gender selected
  df_lifetable_nvss <- read.csv(paste("data/lifetables/",
                                      selectLifetableYear,"_Lifetable_",
                                      selectRaceEthn, ".csv",
                                      sep = ""), 
                                header = TRUE)
  # clean nvss lifetable
  df_lifetable_nvss <- clean.lifetable(df_lifetable_nvss)
  
} else if (selectStratVars == 'none') {
  # for separating race/ethnicity
  df_all_deaths<- subset(df_all_deaths_age, 
                      select = c("Ages.Text", "Ages", "Deaths", "Population"))
  # df_all_deaths<- aggregate(cbind(Deaths, Population) ~ Ages + 
  #                          Ages.Text, df_all_deaths, sum)
  df_all_deaths<- df_all_deaths%>%
    mutate(Crude.Rate = Deaths/Population*1e5) %>%
    arrange(Ages)
  
  df_opioid_deaths <- subset(df_opioid_deaths_age,
                         select = c("Ages.Text", "Ages", "Deaths", "Population"))
  # df_opioid_deaths <- aggregate(cbind(Deaths, Population) ~ Ages + 
  #                             Ages.Text, df_opioid_deaths, sum)
  df_opioid_deaths <- df_opioid_deaths %>%
    mutate(Crude.Rate = Deaths/Population*1e5) %>%
    arrange(Ages)
  
  df_lifetable_nvss <- read.csv(paste("data/lifetables/",
                                      selectLifetableYear,"_Lifetable.csv",
                                      sep = ""), 
                                header = TRUE)
  # clean nvss lifetable
  df_lifetable_nvss <- clean.lifetable(df_lifetable_nvss)
  
}

#### --- Combine all deaths and OD deaths (`df_all_deaths` and `df_opioid_deaths`) into
    ## one dataframe `df_all_and_opioid_deaths`

if (selectAgeGrouping == 'fiveyear'){
  ## for 5 year age groups
  ## combines `df_all_deaths` with Age.data and replace NAs with 0s
  df_all_deaths<- df_all_deaths%>%
    right_join(Age.data, by = c("Ages")) %>%
    replace_na(list(Crude.Rate = 0, Deaths = 0)) %>%
    arrange(Ages)%>%
    select(-c("Ages.Text.y")) %>%
    rename("Ages.Text" = "Ages.Text.x")
  ## combines `df_opioid_deaths` with Age.data and replace NAs with 0s
  df_opioid_deaths <- df_opioid_deaths %>%
    right_join(Age.data, by = c("Ages")) %>%
    replace_na(list(Crude.Rate = 0, Deaths = 0)) %>%
    arrange(Ages) %>%
    select(-c("Ages.Text.y")) %>%
    rename("Ages.Text" = "Ages.Text.x")
  
  ## vector with lower bound ages of the 5y age groups used
  vec_5y_ages <- c(0, 1, seq(5,100,5)) 
  
  for (i in 1:length(df_opioid_deaths$Ages)){
    if(df_opioid_deaths$Crude.Rate[i]==0 & !((i-1) %in% vec_5y_ages)){
      df_opioid_deaths$Crude.Rate[i] <- df_opioid_deaths$Crude.Rate[i-1]
    }
  }
  
  df_all_and_opioid_deaths <- df_all_deaths%>%
    left_join(df_opioid_deaths, by = c("Ages"), suffix = c(".all", ".od")) %>%
    mutate(Od.Perc = Deaths.od/Deaths.all*100)
  df_all_and_opioid_deaths <- df_all_and_opioid_deaths %>%
    mutate(Crude.Rate.no.cause = Crude.Rate.all - Crude.Rate.od)
} else{
  ## for single year age groups
  
  ## combines `df_all_deaths` with Age.data and replace NAs with 0s
  df_all_deaths<- df_all_deaths%>%
    right_join(Age.data, by = c("Ages.Text","Ages")) %>%
    replace_na(list(Crude.Rate = 0, Deaths = 0)) %>%
    arrange(Ages)
  ## combines `df_opioid_deaths` with Age.data and replace NAs with 0s
  df_opioid_deaths <- df_opioid_deaths %>%
    right_join(Age.data, by = c("Ages.Text","Ages")) %>%
    replace_na(list(Crude.Rate = 0, Deaths = 0)) %>%
    arrange(Ages)

  df_all_and_opioid_deaths <- df_all_deaths%>%
    inner_join(df_opioid_deaths, by = c("Ages", "Ages.Text"), suffix = c(".all", ".od")) %>%
    filter(Ages.Text != "Not Stated") %>%
    mutate(Od.Perc = Deaths.od/Deaths.all*100)

  df_all_and_opioid_deaths <- df_all_and_opioid_deaths %>%
    mutate(Crude.Rate.no.cause = Crude.Rate.all - Crude.Rate.od)
  
}
 

#---------------------------------------------------------------------
#### --- Calculate Years of Life Lost due to opioid overdose deaths ----
#--------------------------------------------------------------------

## create one lifetable for ALL deaths `lifetable_final_OD`, and one lifetable 
## for ALL minus OD deaths `lifetable_final_no_OD`
lifetable_final_OD <- make.lifetable(cause = TRUE, 
                                     df_data = df_all_and_opioid_deaths, 
                                     lifetable = df_lifetable_nvss)
lifetable_final_no_OD <- make.lifetable(cause = FALSE, 
                                        df_data = df_all_and_opioid_deaths, 
                                        lifetable = df_lifetable_nvss)

#### ---- calculate YLL
YLL_table <- df_all_and_opioid_deaths %>%filter(Ages != 101)
YLL_table$LE.OD = lifetable_final_OD$e
YLL_table$LE.no.OD = lifetable_final_no_OD$e
YLL_table <- YLL_table %>%
  mutate(LE.diff = LE.no.OD - LE.OD)

if (selectAgeGrouping == 'fiveyear'){
  vec_od_deaths <- YLL_table$Deaths.od
  vec_od_deaths_shifted <- c(0,0,vec_od_deaths[1:99])
  vec_od_deaths_shifted[1] <- vec_od_deaths[1]
  vec_od_deaths_shifted[3] <- 0
  
  YLL_table$Deaths.od.shifted <- vec_od_deaths_shifted
  YLL_table <- YLL_table %>%
    mutate(YLL = Deaths.od.shifted*(LE.no.OD-0.5))
  
} else{
  YLL_table <- YLL_table %>%
    mutate(YLL = Deaths.od*(LE.no.OD-0.5))
}

## reminder/sanity check of which grouping and demographic group is selected
print(paste(selectAgeGrouping, ' age groups, stratified by ', 
            selectStratVars, sep=""))
print(paste('Selected demographic group:', selectRaceEthn, selectGender, 
            sep=" "))

sum(YLL_table$YLL)
YLL_table$LE.diff[1]

sum(YLL_table$Deaths.od)




#---------------------------------------------------------------------
# 4. Calculate Years of Life Lost due to OD deaths by STATE
#--------------------------------------------------------------------


test <- aggregate(Deaths ~ State.Text, df.od.deaths.age.gender.race.state, sum)


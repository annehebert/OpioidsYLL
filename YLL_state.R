## code written and developed by Anne H. Hébert and Alison L. Hill

## this code estimates the years of life lost (YLL) and the Loss in Life Expectancy 
## to opioid overdose deaths by state in the United States, for the specified year
## DATA USED:
## 1. NVSS state lifetables, projecting US population mortality experience by state
## 2. All-cause and opioid-related death counts from CDC WONDER by state

setwd("~/Opioid_project/opioid_burden")
source("functions.R")
library(tidyverse)

#### Select settings ----
## select the following:
# - year
# - whether to include all drugs or only opioid 
# - whether to resample for generating confidence intervals
selectYear = '2019'
selectDrug = 'opioids' # 'opioids' or 'all drugs'
selectResampling <- TRUE # TRUE or FALSE

#### Import data ----
age.data = load("data/Ages.Rda")
# function imports death counts data
state_data_list <- import.state.deaths.data(year = selectYear, drug = selectDrug)
# assigns output to named dataframes 
df_all_deaths_5yage_state <- state_data_list[[1]]
df_all_deaths_state <- state_data_list[[2]]
df_all_death_dist <- state_data_list[[3]]
df_opioid_deaths_5yage_state <- state_data_list[[4]]
df_opioid_deaths_state <- state_data_list[[5]]
df_opioid_death_dist <- state_data_list[[6]]

#### loop through states, estimating YLL and LLE ----
# vector with all state names
statesVec = unique(df_all_deaths_5yage_state$State.Text)
# set up empty dataframe and vectors
df_YLL_State <- df_all_deaths_state %>%
  filter(!is.na(Deaths)) %>%
  select(State.Text, State.Num)
state_vec <- c()
yll_mean <- c()
yll_sd <- c()
od_deaths_mean<-c()
od_deaths_sd<-c()
le_mean <- c()
le_sd <- c()

#statesVec <- c('Massachusetts') #c('North Dakota') #for debugging

for(state in sort(statesVec)){
  # state = 'Massachusetts' # for debugging
  yll_vec <- c()
  le_lost_vec <- c()
  od_deaths_vec<-c()
  
  if(selectYear != '2021'){
    # import NVSS life table for corresponding year and state
    state_lifetable <- read.csv(file = paste('data/lifetables/state/',
                                             state,selectYear, '.csv', sep = ""),
                                header = TRUE)
    state_lifetable <- clean.lifetable(state_lifetable)
  } else if(selectYear == '2021'){
    # import 2020 NVSS life table for corresponding state
    # at each age apply shift in probability of death that was observed at
    # national level, thus approximating the 2021 state life table
    state_lifetable <- read.csv(file = paste('data/lifetables/state/',
                                             state, '2020.csv', sep = ""),
                                header = TRUE)
    state_lifetable <- clean.lifetable(state_lifetable)
    state_lifetable <- state_lifetable %>%
      left_join(df_q_diff[,c('Ages', 'q.diff')], by = 'Ages') %>%
      rename(q.old = q) %>%
      mutate(q = as.numeric(q.old) - q.diff)
    state_lifetable <- rebuild.lifetable.from.q(state_lifetable)
  }
  # filter opioid and all cause deaths by age from corresponding state and select necessary columns
  df_deaths_data_od <- df_opioid_deaths_5yage_state %>%
    filter(State.Text == state) %>%
    mutate(Population = as.integer(Population),
           Ages = as.integer(Ages)) %>%
    select(c("Ages", "Deaths", "Population"))
  df_deaths_data_all <- df_all_deaths_5yage_state %>%
    filter(State.Text == state) %>%
    filter(!is.na(Ages)) %>%
    select(c("Ages", "Deaths", "Population"))%>%
    mutate(Population = as.integer(Population),
           Ages = as.integer(Ages))
  # total number of opioid and all cause deaths for corresponding state
  total_od_deaths <- df_opioid_deaths_state %>%
    filter(State.Text==state)
  total_all_deaths <- df_all_deaths_state %>%
    filter(State.Text==state)
  
  for (i in 1:100){
    # redistribute missing opioid deaths
    df_od_data_redist <- redistribute.suppressed.deaths.state(df_deaths_data_od,
                                                              df_opioid_death_dist,
                                                              total_deaths = total_od_deaths
    )
    df_od_data_redist <- df_od_data_redist %>%
      rename(Deaths.cause = Deaths)
    # redistribute missing all cause deaths
    df_all_data_redist <- redistribute.suppressed.deaths.state(df_deaths_data_all,
                                                               df_all_death_dist,
                                                               total_all_deaths
    )
    df_all_data_redist <- df_all_data_redist %>%
      rename(Deaths.all = Deaths)
    # merge the two redistributed dataframes
    df_deaths_data <- df_all_data_redist %>%
      full_join(df_od_data_redist, by = c('Ages', 'Population', 'Ind'))
    
    # for generating CIs
    if (selectResampling){
      #resamples opioid deaths at each age
      sample.deaths <- c()
      for (j in 1:length(df_deaths_data$Ages)){
        if (df_deaths_data$Deaths.all[j] !=0){
          sample.deaths[j] <- rpois(1, df_deaths_data$Deaths.cause[j])
        } else if (df_deaths_data$Deaths.all[j] == 0 ){
          sample.deaths[j] <-0
        }
      }
      df_deaths_data$Deaths.cause <- sample.deaths
    }
    od_deaths_vec <- append(od_deaths_vec, sum(df_deaths_data$Deaths.cause))
    df_deaths_data <- df_deaths_data %>%
      mutate(R = (as.numeric(Deaths.all)-as.numeric(Deaths.cause))/as.numeric(Deaths.all))  %>%
      mutate(R := ifelse(is.nan(R), 1, R))
    
    #make cause-eliminated life table from redistributed (and resampled) deaths data
    lifetable_cd <- make.cause.eliminated.lifetable(df_data = df_deaths_data,
                                                    lifetable = state_lifetable)
    yll <- lifetable_cd[,c('LE_diff', 'Deaths.cause','Ages', 'e_cd', 'e')] 
    e_cd_shifted <- append(yll$e_cd[2:22], yll$e_cd[22])
    yll$e_cd_shifted <- e_cd_shifted
    yll <- yll %>%
      mutate(e_cd_avg = (e_cd_shifted + e_cd)/2)%>%
      mutate(YLL = e_cd_avg*Deaths.cause)
    # make vector with total YLL at each iteration
    yll_vec <- append(yll_vec, sum(yll$YLL))
    # make vector with life expectancy lost at birth at each iteration
    le_lost_vec <- append(le_lost_vec, yll$LE_diff[1])
  }
  
  # save mean and standard deviation of YLL, LE and number of opioid deaths
  state_vec <- append(state_vec, state)
  yll_mean <- append(yll_mean, mean(yll_vec))
  yll_sd <- append(yll_sd, sd(yll_vec))
  le_mean <- append(le_mean,mean(le_lost_vec))
  le_sd <-append(le_sd,sd(le_lost_vec))
  od_deaths_mean <- append(od_deaths_mean, mean(od_deaths_vec))
  od_deaths_sd <- append(od_deaths_sd, sd(od_deaths_vec))
  
}

df_YLL_State$state <- state_vec
df_YLL_State$YLL_sd <- yll_sd
df_YLL_State$YLL_mean <- yll_mean
df_YLL_State$LE_lost_sd<-le_sd
df_YLL_State$LE_lost_mean <- le_mean
df_YLL_State$OD_deaths_sd<-od_deaths_sd
df_YLL_State$OD_deaths_mean <- od_deaths_mean

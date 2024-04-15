# setup
setwd("~/Opioid_project/opioid_burden")
source("functions.R")
library(tidyverse)

# using this R script to create approximations of 2021 life tables by state

# starting by finding the probability of death at each age in 2020 and 2021
# at the national level, from the NVSS life tables. 
# this allows us to find the change in probability of death at each age between 
# those two years.

# then from there we can apply this change to the existing state-specific 
# life tables from NVSS.

# start by making the general US population life tables:
selectStratVars = 'none'

# import corresponding NVSS life table
df_lifetable_nvss_2020 <- import.nvss.lifetable(selectStratVars = selectStratVars,
                                                selectLifetableYear = '2020')
df_lifetable_nvss_2021 <- import.nvss.lifetable(selectStratVars = selectStratVars,
                                                selectLifetableYear = '2021')

df_q_diff <- df_lifetable_nvss_2020[,c('Ages', 'q')] %>%
  left_join(df_lifetable_nvss_2021[,c('Ages', 'q')], by = 'Ages', 
            suffix = c('.2020', '.2021'))
df_q_diff <-df_q_diff %>%
  mutate(q.diff = as.numeric(q.2020) - as.numeric(q.2021)) %>%
  mutate(perc = q.diff/as.numeric(q.2021)*100)






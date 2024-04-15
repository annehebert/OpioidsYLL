
## functions used in `YearsLifeLost.R`


clean.mortality.data <- function(data){
  ## input `data` is a raw death counts file downloaded from CDC WONDER
  ## data should have a deaths column, and some subset of Gender, Race,
  ## Hispanic origin, state, single or 5 year age group
  
  ## this function cleans file, renames some columns, and merges race/ethnic 
  ## origin columns into a new race.eth column
  ## returns dataframe data, but cleaned up and with new columns
  
  ## filters for which columns are present, so should be flexible enough to use
  ## on death counts that are stratified by a variety of variables.
  
  ## remove notes which contain download info (this way you can keep them in
  ## original file which is ++)
  data$Notes=NULL
  #data <- df_all_deaths_state
  if ("Gender" %in% colnames(data)) {
    data$Gender=NULL
    colnames(data)[colnames(data)=="Gender.Code"] <- "Gender"
  }
  
  if ("Race" %in% colnames(data)){
    data$Race.Code=NULL
  }
  if ("Single.Race.6" %in% colnames(data)){
    data$Single.Race.6.Code=NULL
    data$Race <- data$Single.Race.6
    data$Single.Race.6=NULL
    
  }
  if ("Hispanic.Origin.Code" %in% colnames(data)){
    data$Hispanic.Origin.Code=NULL
    colnames(data)[colnames(data)=="Hispanic.Origin"] <- "Ethnicity"
  } 
  
  if ("State.Code" %in% colnames(data)){
    colnames(data)[colnames(data)=="State.Code"] <- "State.Num"
    colnames(data)[colnames(data)=="State"] <- "State.Text"
  }
  
  if ("Single.Year.Ages" %in% colnames(data)){
    colnames(data)[colnames(data)=="Single.Year.Ages"] <- "Ages.Text"
    colnames(data)[colnames(data)=="Single.Year.Ages.Code"] <- "Ages"
  }
  
  if ("Five.Year.Age.Groups" %in% colnames(data)){
    colnames(data)[colnames(data)=="Five.Year.Age.Groups"] <- "Ages.Text"
    colnames(data)[colnames(data)=="Five.Year.Age.Groups.Code"] <- "Ages"
    
    for (i in 1:length(data$Ages)){
      if (data$Ages[i]=="1"){
        data$Ages[i] = 0
      } else if (data$Ages[i]=="4-Jan" | data$Ages[i]== "1-4"){
        data$Ages[i] = 1
      } else if (data$Ages[i]=="9-May" | data$Ages[i]== "5-9"){
        data$Ages[i] = 5
      } else if (data$Ages[i]=="14-Oct" | data$Ages[i]== "10-14"){
        data$Ages[i] = 10
      } else if (data$Ages[i]=="15-19"){
        data$Ages[i] = 15                  
      } else if (data$Ages[i]=="20-24"){
        data$Ages[i] = 20
      } else if (data$Ages[i]=="25-29"){
        data$Ages[i] = 25
      } else if (data$Ages[i]=="30-34"){
        data$Ages[i] = 30
      }  else if (data$Ages[i]=="35-39"){
        data$Ages[i] = 35
      } else if (data$Ages[i]=="40-44"){
        data$Ages[i] = 40              
      } else if (data$Ages[i]=="45-49"){
        data$Ages[i] = 45
      } else if (data$Ages[i]=="50-54"){
        data$Ages[i] = 50
      } else if (data$Ages[i]=="55-59"){
        data$Ages[i] = 55
      }  else if (data$Ages[i]=="60-64"){
        data$Ages[i] = 60
      }  else if (data$Ages[i]=="65-69"){
        data$Ages[i] = 65
      } else if (data$Ages[i]=="70-74"){
        data$Ages[i] = 70              
      } else if (data$Ages[i]=="75-79"){
        data$Ages[i] = 75
      } else if (data$Ages[i]=="80-84"){
        data$Ages[i] = 80
      } else if (data$Ages[i]=="85-89"){
        data$Ages[i] = 85
      }  else if (data$Ages[i]=="90-94"){
        data$Ages[i] = 90
      } else if (data$Ages[i]=="95-99"){
        data$Ages[i] = 95
      } else if (data$Ages[i]=="100+"){
        data$Ages[i] = 100
      } 
    }
    
    
  }
  
  if (("Race" %in% colnames(data)) & ("Ethnicity" %in% colnames(data))){
    #re-group the race and ethnicity categories into 5 combined categories
    data$Race.Eth = data$Race
    data$Race.Eth = "Not Stated" #if either category missing
    data$Race.Eth[data$Race == "White" &
                    data$Ethnicity == "Not Hispanic or Latino"] = "White_NonHisp"
    data$Race.Eth[data$Race == "Black or African American" &
                    data$Ethnicity == "Not Hispanic or Latino"] = "Black_NonHisp"
    data$Race.Eth[data$Race == "Native Hawaiian or Other Pacific Islander" &
                    data$Ethnicity == "Not Hispanic or Latino"] = "NativePI_NonHisp"
    data$Race.Eth[data$Race == "Asian or Pacific Islander" &
                    data$Ethnicity == "Not Hispanic or Latino"] = "Asian_NonHisp"
    data$Race.Eth[data$Race == "Asian" &
                    data$Ethnicity == "Not Hispanic or Latino"] = "Asian_NonHisp"
    data$Race.Eth[data$Race == "American Indian or Alaska Native" &
                    data$Ethnicity == "Not Hispanic or Latino"] = "AmericanIndian_NonHisp"
    data$Race.Eth[data$Race == "More than one race" &
                    data$Ethnicity == "Not Hispanic or Latino"] = "Multiple_NonHisp"
    data$Race.Eth[data$Ethnicity == "Hispanic or Latino"] = "Hisp"
    
    ## from here till end of if statement, just filtering on existing colnames
    ## to determine which columns should be included in output dataframe
    if (("State.Num" %in% colnames(data)) &
        ("Ages.Text" %in% colnames(data)) &
        ("Ages" %in% colnames(data))) {
      data = data[c(
        "Ages.Text", "Ages","Gender", "Ethnicity", "Race", "Race.Eth",
        "State.Text", "State.Num", "Deaths", "Population", "Crude.Rate"
      )]
    } else if (("State.Num" %in% colnames(data)) &
               !("Ages.Text" %in% colnames(data)) &
               !("Ages" %in% colnames(data))) {
      data = data[c(
        "Gender", "Ethnicity", "Race","Race.Eth","State.Text", "State.Num",
        "Deaths","Population",  "Crude.Rate"
      )]
    } else if ("Gender" %in% colnames(data) &
               !("State.Num" %in% colnames(data)) &
               ("Ages.Text" %in% colnames(data))
               & ("Ages" %in% colnames(data))) {
      data = data[c(
        "Ages.Text","Ages","Gender","Ethnicity","Race", "Race.Eth", "Deaths",
        "Population", "Crude.Rate"
      )]
    }else if (!("State.Num" %in% colnames(data)) & 
              ("Ages.Text" %in% colnames(data)) 
              & ("Ages" %in% colnames(data))){
      data = data[c(
        "Ages.Text", "Ages","Ethnicity","Race", "Race.Eth","Deaths",
        "Population","Crude.Rate"
      )]
    } else if (('Age.Adjusted.Rate' %in% colnames(data)) & 
               !("State.Num" %in% colnames(data)) & 
               !("Ages.Text" %in% colnames(data)) 
               & !("Ages" %in% colnames(data))){
      data = data[c( 
        "Ethnicity","Race", "Gender", "Year", "Age.Adjusted.Rate",
        "Race.Eth","Deaths","Population", "Crude.Rate"
      )]
    } else if (!("State.Num" %in% colnames(data)) &
               !("Ages.Text" %in% colnames(data)) &
               !("Ages" %in% colnames(data)) & 
               !('Age.Adjusted.Rate' %in% colnames(data))){
      data = data[c( 
        "Ethnicity","Race", "Year","Race.Eth","Deaths","Population",
        "Crude.Rate"
      )]
    }
  }
  
  
  if (("Race" %in% colnames(data)) & (!"Ethnicity" %in% colnames(data))){
    #print(aggregate(Deaths ~ Race, data, sum))
    data$Race.Eth=data$Race
    data$Race.Eth="Not Stated" #if either category missing
    data$Race.Eth[data$Race=="White"]="White_NonHisp"
    data$Race.Eth[data$Race=="Black or African American"]="Black_NonHisp"
    data$Race.Eth[data$Race=="Hispanic or Latino"]="Hisp"
    data$Race.Eth[data$Race=="American Indian or Alaska Native"]="AmericanIndian_NonHisp"
    data$Race.Eth[data$Race=="Asian"]="Asian_NonHisp"
    
    if ("Ages.Text" %in% colnames(data) & "Ages" %in% colnames(data)){
      if ("State.Num" %in% colnames(data)){
        data=data[c(
          "Ages.Text", "Ages", "Gender","Race","Race.Eth","State.Text",
          "State.Num","Deaths","Population","Crude.Rate"
        )]   
      }else{
        data=data[c(
          "Ages.Text", "Ages", "Gender","Race","Race.Eth","Deaths",
          "Population","Crude.Rate"
        )]   
      }
    } else{
      if ("State.Num" %in% colnames(data)){
        data=data[c(
          "Gender","Race","Race.Eth","State.Text","State.Num",
          "Deaths","Population","Crude.Rate"
        )]   
      } else{
        data=data[c(
          "Gender","Race","Race.Eth","Deaths","Population","Crude.Rate"
        )]   
      }  
    }
    
  }
  
  if ((!"Race" %in% colnames(data)) & ("Ethnicity" %in% colnames(data))){
    #print(aggregate(Deaths ~ Ethnicity, data, sum))
    data$Race.Eth=data$Ethnicity
    data$Race.Eth="Not Stated" #if either category missing
    data$Race.Eth[data$Ethnicity=="Hispanic or Latino"]="Hisp"
    data$Race.Eth[data$Ethnicity=="Not Hispanic or Latino"]="NonHisp"
    
    if ("State.Num" %in% colnames(data)){
      data=data[c(
        "Ages.Text", "Ages", "Gender","Ethnicity","Race.Eth","State.Text",
        "State.Num","Deaths","Population","Crude.Rate"
      )]
    } else{
      data=data[c(
        "Ages.Text", "Ages", "Gender","Ethnicity","Race.Eth","Deaths",
        "Population","Crude.Rate"
      )]
    }
  }
  
  if ((!"Race" %in% colnames(data)) & (!"Ethnicity" %in% colnames(data))){
    #data$Race.Eth=Ages.Text
    #data$Race.Eth="Not Stated" #if either category missing
    
    if (("State.Num" %in% colnames(data)) & ("Gender" %in% colnames(data))){
      data=data[c(
        "Ages.Text", "Ages", "Gender","State.Text","State.Num","Deaths",
        "Population","Crude.Rate"
      )]
    }else if (("State.Num" %in% colnames(data)) & !("Gender" %in% colnames(data)) &
              ("Ages" %in% colnames(data))){
      data=data[c(
        "Ages.Text", "Ages","State.Text","State.Num","Deaths",
        "Population","Crude.Rate"
      )]
    } else if (("State.Num" %in% colnames(data)) & !("Gender" %in% colnames(data)) &
               !("Ages" %in% colnames(data))){
      data=data[c(
        "State.Text","State.Num","Deaths",
        "Population","Crude.Rate"
      )]
    }else if ("Gender" %in% colnames(data)){
      data=data[c(
        "Ages.Text", "Ages", "Gender","Deaths","Population","Crude.Rate"
      )]
    }else{
      data=data[c('Ages.Text',"Ages","Deaths","Population","Crude.Rate"
      )]
    }
  }
  
  #print(sapply(data, class)) # check data types
  
  if ("Ages" %in% colnames(data)) {
    #remove extra rows that have empty data (can be identified by empty values in any column)
    data=data[data$Ages!="",]
    data=data[is.na(data$Ages)==FALSE,]
    #fix data types of variables that got imported incorrectly as factors
    
    #convert ages into numbers
    levels(data$Ages)[levels(data$Ages)=="NS"] = "101"
    data$Ages=as.numeric(as.character(data$Ages))
  }
  
  #convert population into numbers
  #NOTE: many population values say "not applicable", for all ages >85
  levels(data$Population)[levels(data$Population)=="Not Applicable"] = "0"
  data$Population=as.numeric(as.character(data$Population))
  
  #print(sapply(data, class)) # check data types
  
  #re-calculate crude death rate
  if (is.numeric(data$Deaths)) {
    data$Crude.Rate=100000*data$Deaths/data$Population
  }
  return(data)
}

clean.lifetable <- function(lifetable){
  ## input `lifetable`, a lifetable downloaded from NVSS
  ## this function renames columns and removes extra rows/columns
  ## returns cleaned up lifetable
  
  for (i in 3:ncol(lifetable)){
    lifetable[,i] = as.numeric(as.character(gsub(",","", lifetable[,i])))
  }
  if (colnames(lifetable[2]) == "X"){
    lifetable <- lifetable %>%
      filter(!is.na(X.1)) %>%
      select(c(X, X.1, X.2, X.3, X.4, X.5))
    lifetable$Ages <- Age.data$Ages[1:101]
    lifetable$Ages.Text <- Age.data$Ages.Text[1:101]
    vec_colnames <- c("q", "l", "d", "L", "T", "e", "Ages", "Ages.Text")
    colnames(lifetable) <- vec_colnames
  }
  return(lifetable)
}

combine.hisp.data <- function(racedata, hispdata){
  ## this function combines two dataframes containing death counts
  ## `racedata` contains death counts of non-Hispanic people, by race
  ## `hispdata` contains death counts of Hispanic people of all races
  ## both dataframes should have deaths stratified by five year age group in
  ## column Five.Year.Age.Groups
  
  hispdata$Race.Eth <- rep('Hisp', length(hispdata$Deaths))
  hispdata$Gender <- NULL
  
  if ("Five.Year.Age.Groups" %in% colnames(hispdata)){
    colnames(hispdata)[colnames(hispdata)=="Five.Year.Age.Groups"] <- "Ages.Text"
    colnames(hispdata)[colnames(hispdata)=="Five.Year.Age.Groups.Code"] <- "Ages"
    
    for (i in 1:length(hispdata$Ages)){
      if (hispdata$Ages[i]=="1"){
        hispdata$Ages[i] = 0
      } else if (hispdata$Ages[i]=="4-Jan" | hispdata$Ages[i]== "1-4"){
        hispdata$Ages[i] = 1
      } else if (hispdata$Ages[i]=="9-May" | hispdata$Ages[i]== "5-9"){
        hispdata$Ages[i] = 5
      } else if (hispdata$Ages[i]=="14-Oct" | hispdata$Ages[i]== "10-14"){
        hispdata$Ages[i] = 10
      } else if (hispdata$Ages[i]=="15-19"){
        hispdata$Ages[i] = 15                  
      } else if (hispdata$Ages[i]=="20-24"){
        hispdata$Ages[i] = 20
      } else if (hispdata$Ages[i]=="25-29"){
        hispdata$Ages[i] = 25
      } else if (hispdata$Ages[i]=="30-34"){
        hispdata$Ages[i] = 30
      }  else if (hispdata$Ages[i]=="35-39"){
        hispdata$Ages[i] = 35
      } else if (hispdata$Ages[i]=="40-44"){
        hispdata$Ages[i] = 40              
      } else if (hispdata$Ages[i]=="45-49"){
        hispdata$Ages[i] = 45
      } else if (hispdata$Ages[i]=="50-54"){
        hispdata$Ages[i] = 50
      } else if (hispdata$Ages[i]=="55-59"){
        hispdata$Ages[i] = 55
      }  else if (hispdata$Ages[i]=="60-64"){
        hispdata$Ages[i] = 60
      }  else if (hispdata$Ages[i]=="65-69"){
        hispdata$Ages[i] = 65
      } else if (hispdata$Ages[i]=="70-74"){
        hispdata$Ages[i] = 70              
      } else if (hispdata$Ages[i]=="75-79"){
        hispdata$Ages[i] = 75
      } else if (hispdata$Ages[i]=="80-84"){
        hispdata$Ages[i] = 80
      } else if (hispdata$Ages[i]=="85-89"){
        hispdata$Ages[i] = 85
      }  else if (hispdata$Ages[i]=="90-94"){
        hispdata$Ages[i] = 90
      } else if (hispdata$Ages[i]=="95-99"){
        hispdata$Ages[i] = 95
      } else if (hispdata$Ages[i]=="100+"){
        hispdata$Ages[i] = 100
      } 
    }
    hispdata <- hispdata %>%
      rename(Ethnicity = Hispanic.Origin) %>%
      rename(Gender = Gender.Code) %>%
      select(-c('Notes', 'Hispanic.Origin.Code')) %>%
      filter(Ethnicity == 'Hispanic or Latino')
    
    racedata <- racedata %>%
      select(-c('Race')) %>%
      filter(Race.Eth != 'Hisp')
    
    combined <- rbind(racedata, hispdata)
    return(combined)
    
  } else{print('Error: could not find 5-year age group column')}
  
}


import.nvss.lifetable <- function(selectStratVars, selectLifetableYear, selectRaceEthn){
  ## import relevant NVSS lifetable, given race/eth/gender selected
  
  if (selectStratVars == "gender"){
    df_lifetable_nvss <- read.csv(
      paste("data/lifetables/", selectLifetableYear,"_Lifetable_", 
            selectGender, ".csv", sep = ""), 
      header = TRUE) 
  }  else if(selectStratVars == 'race and gender'){
    ## import relevant NVSS lifetable, given race/eth/gender selected
    if (selectRaceEthn != "Multiple_NonHisp"){
      df_lifetable_nvss <- read.csv(
        paste("data/lifetables/", selectLifetableYear,"_Lifetable_",
              selectGender,"_", selectRaceEthn, ".csv", sep = ""), 
        header = TRUE)
    } else{ 
      df_lifetable_nvss <- read.csv(
        paste("data/lifetables/", selectLifetableYear,"_Lifetable_",
              selectGender, ".csv", sep = ""), 
        header = TRUE)
    }
  } else if (selectStratVars == 'race') {
    ## import relevant NVSS lifetable, given race/eth/gender selected
    df_lifetable_nvss <- read.csv(
      paste("data/lifetables/", selectLifetableYear,"_Lifetable_",
            selectRaceEthn, ".csv", sep = ""), 
      header = TRUE)
  } else if (selectStratVars == 'none'){
    df_lifetable_nvss <- read.csv(
      paste("data/lifetables/", selectLifetableYear,"_Lifetable.csv",
            sep = ""), 
      header = TRUE)
  }
  
  ## clean NVSS lifetable
  df_lifetable_nvss <- clean.lifetable(df_lifetable_nvss)
  return(df_lifetable_nvss)
}


import.deaths.data <- function(selectAgeGrouping, selectStratVars, selectCoOccurrence){
  if (selectAgeGrouping=='fiveyear'){
    df_opioid_death_dist <- read.delim(
      file = paste('data/Opioid_deaths_',
                   selectYear, '_age_distribution.txt',
                   sep = ""),
      header = TRUE, sep = "\t")
  } else if (selectAgeGrouping=='singleyear'){
    df_opioid_death_dist <- read.delim(
      file = paste('data/2019_2021_singleyear_age_dist.txt',
                   sep = ""),
      header = TRUE, sep = "\t")
  }
  
  if(selectAgeGrouping=='fiveyear'){
    if (selectStratVars == 'race and gender'){
      df_all_deaths_age_gender_race <- read.delim(
        file = paste('data/All_deaths_', selectYear, '_5yage_gender_race.txt',
                     sep = ""),
        header = TRUE, sep = "\t")      
      df_all_deaths <-clean.mortality.data(df_all_deaths_age_gender_race)
      if(selectCoOccurrence =='none'){
        df_opioid_deaths_age_gender_race <- read.delim(
          file = paste( 'data/Opioid_deaths_',
                        selectYear,'_5yage_gender_race_eth_suppressed.txt', sep = ""),
          header = TRUE, sep = "\t")
        df_opioid_deaths <- clean.mortality.data(df_opioid_deaths_age_gender_race)
      } else if (selectCoOccurrence == 'meth'){
        df_opioid_deaths_age_gender_race <- read.delim(
          file = paste('data_co_occurrence/2019_2021_5yage_race_gender_opioid_psychostim_suppressed.txt', 
                       sep = ""),
          header = TRUE, sep = "\t")
        df_opioid_deaths_age_gender_race <- df_opioid_deaths_age_gender_race %>%
          filter(Year == selectYear)
        df_opioid_deaths <- clean.mortality.data(df_opioid_deaths_age_gender_race)
      } else if (selectCoOccurrence == 'cocaine'){
        df_opioid_deaths_age_gender_race <- read.delim(
          file = paste('data_co_occurrence/2019_2021_5yage_race_gender_opioid_cocaine_suppressed.txt', 
                       sep = ""),
          header = TRUE, sep = "\t")
        df_opioid_deaths_age_gender_race <- df_opioid_deaths_age_gender_race %>%
          filter(Year == selectYear)
        df_opioid_deaths <- clean.mortality.data(df_opioid_deaths_age_gender_race)
      }else if (selectCoOccurrence == 'both'){
        df_opioid_deaths_age_gender_race <- read.delim(
          file = paste('data_co_occurrence/2019_2021_5yage_gender_race_opioid_cocaine_psychostim.txt', 
                       sep = ""),
          header = TRUE, sep = "\t")
        df_opioid_deaths_age_gender_race <- df_opioid_deaths_age_gender_race %>%
          filter(Year == selectYear)
        df_opioid_deaths <- clean.mortality.data(df_opioid_deaths_age_gender_race)
      }
    } else if (selectStratVars == 'none'){
      df_all_deaths_age <- read.delim(file = paste('data/All_deaths_',
                                                   selectYear, '_5yage.txt',
                                                   sep = ""),
                                      header = TRUE, sep = "\t")      
      df_all_deaths <-clean.mortality.data(df_all_deaths_age)
      
      if(selectCoOccurrence == 'none') {
        df_opioid_deaths_age <- read.delim(
          file = paste( 'data/Opioid_deaths_',
                        selectYear,'_5yage.txt', sep = ""),
          header = TRUE, sep = "\t")
        df_opioid_deaths <- clean.mortality.data(df_opioid_deaths_age)
      } else if (selectCoOccurrence == 'both'){
        df_opioid_deaths_age <- read.delim(
          file = paste('data_co_occurrence/2019_2021_5yage_opioid_cocaine_psychostim.txt', 
                       sep = ""),
          header = TRUE, sep = "\t")
        df_opioid_deaths_age <- df_opioid_deaths_age %>%
          filter(Year == selectYear)
        df_opioid_deaths <- clean.mortality.data(df_opioid_deaths_age)
      }else if (selectCoOccurrence == 'cocaine'){
        df_opioid_deaths_age <- read.delim(
          file = paste('data_co_occurrence/2019_2021_5yage_opioid_cocaine_suppressed.txt', 
                       sep = ""),
          header = TRUE, sep = "\t")
        df_opioid_deaths_age <- df_opioid_deaths_age %>%
          filter(Year == selectYear)
        df_opioid_deaths <- clean.mortality.data(df_opioid_deaths_age)
      } else if (selectCoOccurrence == 'meth'){
        df_opioid_deaths_age <- read.delim(
          file = paste('data_co_occurrence/2019_2021_5yage_opioid_psychostim_suppressed.txt', 
                       sep = ""),
          header = TRUE, sep = "\t")
        df_opioid_deaths_age <- df_opioid_deaths_age %>%
          filter(Year == selectYear)
        df_opioid_deaths <- clean.mortality.data(df_opioid_deaths_age)
      }
    } else if (selectStratVars == 'gender'){
      df_all_deaths_age_gender <-
        read.delim(
          file = paste('data/All_deaths_',
                       selectYear, '_5yage_gender.txt',
                       sep = ""),
          header = TRUE, sep = "\t")      
      df_all_deaths <-clean.mortality.data(df_all_deaths_age_gender)
      
      df_opioid_deaths_age_gender <- read.delim(
        file = paste( 'data/Opioid_deaths_',
                      selectYear,'_5yage_gender.txt', sep = ""),
        header = TRUE, sep = "\t")
      df_opioid_deaths <- clean.mortality.data(df_opioid_deaths_age_gender)
    }}
  
  
  if(selectAgeGrouping=='singleyear'){
    if(selectStratVars=='gender'){
      df_all_deaths_age_gender <-
        read.delim(
          file = paste('data/All_deaths_',
                       selectYear, '_age_gender.txt',
                       sep = ""),
          header = TRUE, sep = "\t")      
      df_all_deaths <-clean.mortality.data(df_all_deaths_age_gender)
      
      df_opioid_deaths_age_gender <- read.delim(
        file = paste( 'data/Opioid_deaths_',
                      selectYear,'_age_gender.txt', sep = ""),
        header = TRUE, sep = "\t")
      df_opioid_deaths <- clean.mortality.data(df_opioid_deaths_age_gender)
    }
  }
  return(list(df_all_deaths, df_opioid_deaths, df_opioid_death_dist))
  
}


format.deaths.data <- function(stratVars = selectStratVars, 
                               gender = selectGender, 
                               raceEthn = selectRaceEthn, 
                               df_data){
  ## filters deaths data according to selected gender and race/ethn
  
  if(stratVars == 'race and gender'){
    ## for separating race/ethnicity and gender
    ## filter columns in deaths data to select 
    ## age, all deaths, and population 
    df_data_formatted <- df_data %>%
      filter(Gender == paste(gender) & Race.Eth == paste(raceEthn) & 
               Ages.Text != 'Not Stated') %>%
      mutate(Ages := ifelse(Ages.Text=='Total', 999, Ages)) %>%
      mutate(Population = as.integer(Population),
             Ages = as.integer(Ages)) %>%
      select(c("Ages", "Deaths", "Population"))
  } else if (stratVars == 'none') {
    ## filter columns in deaths data to select 
    ## age, all deaths, and population 
    df_data_formatted <- df_data %>%
      filter(Ages.Text != 'Not Stated') %>%
      mutate(Ages := ifelse(Ages.Text=='Total', 999, Ages)) %>%
      mutate(Population = as.integer(Population),
             Ages = as.integer(Ages)) %>%
      select(c("Ages", "Deaths", "Population"))
  } else if (stratVars == 'gender') {
    ## filter columns in deaths data to select 
    ## age, all deaths, and population 
    df_data_formatted <- df_data %>%
      filter(Ages.Text != 'Not Stated') %>%
      filter(Gender == gender) %>%
      mutate(Ages := ifelse(Ages.Text=='Total', 999, Ages)) %>%
      mutate(Population = as.integer(Population),
             Ages = as.integer(Ages)) %>%
      select(c("Ages", "Deaths", "Population"))
  }
  return(df_data_formatted)
}


confint <- function(sample_vec){
  # returns 95% CI of the mean for a sample of 100 measurements 
  if (length(sample_vec)==100){
    error <- qnorm(0.975)*sd(sample_vec)/sqrt(100)
    confint_vec <- c(mean(sample_vec)-error, mean(sample_vec)+error)
  }
  return(confint_vec)
} # end function



redistribute.suppressed.deaths.state <- function(df_data, 
                                                 target_distribution,
                                                 total_deaths = df_opioid_deaths_state){
  # redistributes the total number of suppressed deaths across the ages for
  # which the death counts were suppressed, according to a multinomial 
  # distribution. (this data suppression occurs for all categories with counts 
  # <10, for confidentiality reasons) 
  
  #### INPUTS
  # df_data : dataframe with death counts data by age, with some counts suppressed
  # 'Deaths' column with suppressed counts labeled 'Suppressed'
  # 'Ages' column in 5y age groups
  # target_distribution : dataframe with age distribution to seed multinomial probabilities
  # 'Deaths' column with no suppression 
  # 'Ages' column in 5y age groups
  # total_deaths : dataframe with number of total deaths
  # 'Deaths' column has total number of deaths 
  
  #### FUNCTION
  # extract target age distribution as a vector
  target_distribution <- target_distribution %>% filter(!is.na(Deaths))
  target_distribution <- as.vector(target_distribution[,4])
  
  df_data_redist <- df_data[0,]
  # make temp copy of df_data and extract location of suppressed counts
  df_temp <- df_data %>% arrange(Ages)
  df_temp$Ind <- c(1:22)
  inds <- df_temp$Ind[df_temp$Deaths == 'Suppressed']
  # first check if there is any suppression, and if so, redistribute
  if (length(inds)==0){inds <- NULL}
  if(is.null(inds)){ } else {
    #total number of deaths
    num_total <- as.numeric(total_deaths$Deaths)
    #find number of unsuppressed and suppressed deaths
    df_temp_unsup <- df_temp %>% 
      filter(Deaths != 'Suppressed') %>%
      mutate(Deaths = as.numeric(Deaths))
    num <- sum(as.numeric(df_temp_unsup$Deaths))
    num_suppressed <- num_total - num
    if(num_suppressed!=0){
      df_temp[inds,]$Deaths <- as.numeric(rmultinom(1, num_suppressed,
                                                    target_distribution[inds]))
    }
  }
  
  df_temp <- df_temp %>%
    mutate(Deaths = as.numeric(Deaths))
}


redistribute.suppressed.deaths <- function(df_data, 
                                           target_distribution, 
                                           agegroup='fiveyear'){
  # redistributes the total number of suppressed deaths across the ages for
  # which the death counts were suppressed, according to a multinomial 
  # distribution. (this data suppression occurs for all categories with counts 
  # <10, for confidentiality reasons) 
  
  #### INPUTS
  # df_data : dataframe with death counts data by age, with some counts suppressed
  # 'Deaths' column with death counts for each age group 
  # suppressed counts labeled 'Suppressed'
  # total death count labeled by the age '999'
  # 'Ages' column in 1 or 5y age groups
  # target_distribution : dataframe with age distribution to seed multinomial probabilities
  # 'Deaths' column with no suppression 
  # 'Ages' column in 5y age groups
  # agegroup='fiveyear'
  
  # df_data <- df_deaths_data_od
  # target_distribution <- df_opioid_death_dist
  # agegroup <- 'fiveyear'
  df_data <- df_data %>%
    arrange(Ages)
  if(agegroup=='fiveyear'){
    df_data$Ind <- c(1:23)
  } else if (agegroup== 'singleyear'){
    df_data$Ind <- c(1:102)
  }
  
  target_distribution <- target_distribution %>% filter(!is.na(Deaths))
  target_distribution <- as.vector(target_distribution[,4])
  
  inds <- df_data$Ind[df_data$Deaths.cause == 'Suppressed']
  
  # extract number of total deaths
  num_total <- df_data %>% 
    filter(Ages == 999)
  num_total <- as.numeric(num_total$Deaths.cause[1])
  
  df_data_temp <- df_data %>%
    filter(Ages != 999) %>%
    filter(Deaths.cause != 'Suppressed')
  num <- sum(as.numeric(df_data_temp$Deaths.cause))
  num_suppressed <- num_total - num
  
  df_data[inds,]$Deaths.cause <- rmultinom(1, num_suppressed, target_distribution[inds])
  df_data <- df_data %>%
    mutate(Deaths.cause = as.numeric(Deaths.cause))
  return(df_data)
} # end function


make.cause.eliminated.lifetable.single.year <- function(df_data, lifetable, R = df_data$R){
  #### INPUTS
  # df.data : data frame containing a column 
  # "Deaths.all" column with all-cause deaths by single year age
  # "Deaths.cause" column, with number of deaths due to the cause we're eliminating by single year age
  # 'R' column, which is the risk ratio by single year age
  # lifetable : data frame with NVSS life table
  
  
  #### FUNCTION
  ages <- df_data$Ages
  endIndex <- length(df_data$Ages)
  # make vectors from the columns of the NVSS lifetable dataframe
  # for 1 year intervals, a = 0.5 deaths on average occur midway through the interval
  # this is the same assumption made by NVSS. 
  a <- rep(0.5, endIndex) 
  a[1] <- 0.25 # for ages <1, deaths occur on average 1/4 of the way through the interval
  # save risk factor as vector
  R <- df_data$R
  
  # make cause-deleted life table
  lifetable_cd <- lifetable %>%
    left_join(df_data[c('Ages', 'R')], by='Ages') %>%
    mutate(p=1-as.numeric(q)) %>%
    #mutate(R=1)%>% for debugging, check if R=1 (if there were no OD deaths)
    mutate(p_cd = p^R)
  
  l_cd <- c()
  d_cd <- c()
  q_cd <- c()
  p_cd <- lifetable_cd$p_cd
  e <- lifetable_cd$e
  L_cd <- c()
  n <- c(rep(1, endIndex))
  l_cd[1] <- 1e5
  
  for (i in 1:(endIndex-1)){
    l_cd[i+1] <- l_cd[i]*p_cd[i] 
    q_cd[i] <- 1-p_cd[i]
    d_cd[i] <- l_cd[i] * q_cd[i]
  }
  q_cd[endIndex] <- 1-p_cd[endIndex]
  d_cd[endIndex] <- l_cd[endIndex] * q_cd[endIndex]
  
  a_cd <- a
  
  for (i in 1:(endIndex-1)){
    L_cd[i] <- n[i]*l_cd[i+1] + a_cd[i]*d_cd[i]
  }
  L_cd[endIndex] <- e[endIndex]*l_cd[endIndex]/R[endIndex]
  
  Tot_cd <- c()
  e_cd <- c()
  # Calculate Tot and e
  for (i in 1:endIndex) {
    Tot_cd[i]=sum(L_cd[i:endIndex])
    e_cd[i]=Tot_cd[i]/l_cd[i]
    
  }
  
  lifetable_cd$a_cd <- a_cd
  lifetable_cd$q_cd <- q_cd
  lifetable_cd$l_cd <- l_cd
  lifetable_cd$d_cd <- d_cd
  lifetable_cd$L_cd <- L_cd
  lifetable_cd$Tot_cd <- Tot_cd
  lifetable_cd$e_cd <- e_cd
  
  lifetable_cd$LE_diff <- lifetable_cd$e_cd - lifetable_cd$e
  lifetable_cd$Deaths.cause <- df_data$Deaths.cause
  return(lifetable_cd)
  
} # end function


make.cause.eliminated.lifetable <- function(df_data, lifetable, R = df_data$R){
  #### INPUTS
  # df.data : deaths data frame 
  # "Deaths.all" column 
  # "Deaths.cause" column (deaths due to the cause we're eliminating)
  # 'R' column, which is the risk ratio
  # lifetable : corresponding NVSS lifetable
  
  # first we abridge the lifetable to 5y age groups to match the opioid death data
  # l (number of people surviving to age x), Tot (person-years lived above age x)
  # and e (remaining life expectancy at age x) are exact values for that age and
  # do not need to be modified, can simply be extracted from original life table
  
  # q (the prob of dying in the interval), d (number of people dying in the interval)
  # and L (total number of person-years lived in the interval) describe the interval,
  # so need to be recalculated, accounting for the length of the interval
  ages <- df_data$Ages
  
  lifetable_abr <- lifetable %>%
    filter(Ages %in% ages) %>%
    select(c('Ages', 'l',  'T', 'e'))
  endIndex = length(lifetable_abr$Ages)
  
  # make vectors from the columns of the NVSS lifetable dataframe
  # for 5 year intervals, a = 2.5 deaths on average occur midway through the interval
  # this is the same assumption made by NVSS. 
  a <- rep(2.5, endIndex) 
  a[1] <- 0.25 # for ages <1, deaths occur on average 1/4 of the way through the interval
  a[2] <- 2 # save risk factor as vector
  R <- df_data$R
  
  l <- lifetable_abr$l
  Tot <- lifetable_abr$T
  e <- lifetable_abr$e
  d <- c()
  q <- c()
  L <- c()
  p <- c()
  
  for (i in 1:endIndex){
    d[i] <- l[i]-l[i+1]
    q[i] <- d[i]/l[i]
    L[i] <- Tot[i]-Tot[i+1]
    p[i] <- l[i+1]/l[i]
  }
  q[endIndex] <- 1
  d[endIndex] <- l[endIndex]
  L[endIndex] <- Tot[endIndex]
  p[endIndex] <- 0
  
  lifetable_abr$d <- d
  lifetable_abr$q <- q
  lifetable_abr$L <- L
  lifetable_abr$a <- a
  lifetable_abr$p <- p
  lifetable_abr$R <- R
  
  # make cause-deleted life table
  lifetable_cd <- lifetable_abr %>%
    #mutate(R=1)%>% for debugging, check if R=1 (if there were no OD deaths)
    mutate(p_cd = p^R)
  
  l_cd <- c()
  d_cd <- c()
  q_cd <- c()
  p_cd <- lifetable_cd$p_cd
  e <- lifetable_cd$e
  L_cd <- c()
  n <- c(1,4,rep(5, endIndex-2))
  l_cd[1] <- 1e5
  
  for (i in 1:(endIndex-1)){
    l_cd[i+1] <- l_cd[i]*p_cd[i] 
    q_cd[i] <- 1-p_cd[i]
    d_cd[i] <- l_cd[i] * q_cd[i]
  }
  q_cd[endIndex] <- 1-p_cd[endIndex]
  d_cd[endIndex] <- l_cd[endIndex] * q_cd[endIndex]
  
  a_cd <- rep(0, endIndex)
  for (i in c(1,2,3,21)){
    a_cd[i] = n[i] + R[i]*q[i]/q_cd[i]*(a[i]-n[i])
  }
  a_cd[endIndex] <- e[endIndex]/R[endIndex]
  
  for (i in 4:(endIndex-2)){
    a_cd[i] <- ((-5/24)*d_cd[i-1] + 2.5*d_cd[i] + (5/24)*d_cd[i+1])/d_cd[i]
  }
  
  for (i in 1:(endIndex-1)){
    L_cd[i] <- n[i]*l_cd[i+1] + a_cd[i]*d_cd[i]
  }
  L_cd[endIndex] <- e[endIndex]*l_cd[endIndex]/R[endIndex]
  
  Tot_cd <- c()
  e_cd <- c()
  # Calculate Tot and e
  for (i in 1:endIndex) {
    Tot_cd[i]=sum(L_cd[i:endIndex])
    e_cd[i]=Tot_cd[i]/l_cd[i]
    
  }
  
  lifetable_cd$a_cd <- a_cd
  lifetable_cd$q_cd <- q_cd
  lifetable_cd$l_cd <- l_cd
  lifetable_cd$d_cd <- d_cd
  lifetable_cd$L_cd <- L_cd
  lifetable_cd$Tot_cd <- Tot_cd
  lifetable_cd$e_cd <- e_cd
  
  lifetable_cd$LE_diff <- lifetable_cd$e_cd - lifetable_cd$e
  lifetable_cd$Deaths.cause <- df_data$Deaths.cause
  return(lifetable_cd)
  
} # end function

calculate.yll <- function(opioid_data, all_data, resampling,
                          stratVars, coOccurrence, target_distribution){
  # redistributes missing deaths
  # option to additionally redistribute all deaths according to Poisson distribution
  # calculates YLL and loss in LE at each iteration
  # returns mean and 95% CI
  
  yll_sample <- c()
  le_lost_sample <- c()
  deaths_sample <- c()
  
  for (i in 1:100){
    # redistribute missing death counts
    if (stratVars != 'none' | coOccurrence != 'none'){
      df_deaths_data_od_redist_sample <- redistribute.suppressed.deaths(df_data = opioid_data,
                                                                        target_distribution = target_distribution)
      
      df_deaths_data <- df_deaths_data_od_redist_sample %>%
        full_join(all_data, by = c('Ages', 'Population')) %>%
        filter(Ages != 999) %>%
        mutate(Deaths.all = as.numeric(Deaths.all),
               Deaths.cause = as.numeric(Deaths.cause))
    } else if (stratVars == 'none' &  coOccurrence == 'none'){
      df_deaths_data <- df_deaths_data_od %>%
        full_join(all_data, by = c('Ages', 'Population')) %>%
        filter(Ages != 999) %>%
        mutate(Deaths.all = as.numeric(Deaths.all),
               Deaths.cause = as.numeric(Deaths.cause))
    }
    if (resampling){
      sample.deaths <- c()
      for (j in 1:length(df_deaths_data$Ages)){
        if (df_deaths_data$Deaths.all[j] !=0){
          sample.deaths[j] <- rpois(1, df_deaths_data$Deaths.cause[j])
          # print(sample.deaths[j])
          # print(df_deaths_data$Ages[j])
        }else if (df_deaths_data$Deaths.all[j] == 0 ){
          sample.deaths[j] <-0
        }}
      df_deaths_data$Deaths.cause <- sample.deaths
    }
    df_deaths_data <- df_deaths_data %>%
      mutate(R = (as.numeric(Deaths.all)-as.numeric(Deaths.cause))/as.numeric(Deaths.all))
    
    # make cause eliminated life table
    lifetable_cd <- make.cause.eliminated.lifetable(df_data = df_deaths_data,
                                                    lifetable = df_lifetable_nvss)
    yll <- lifetable_cd[,c('LE_diff', 'Deaths.cause', 'Ages', 'e_cd', 'e')]
    e_cd_shifted <- append(yll$e_cd[2:22], yll$e_cd[22])
    yll$e_cd_shifted <- e_cd_shifted
    yll <- yll %>%
      mutate(e_cd_avg = (e_cd_shifted + e_cd)/2)%>%
      mutate(YLL = e_cd_avg*Deaths.cause)
    
    yll_sample <- append(yll_sample, sum(yll$YLL))
    le_lost_sample <- append(le_lost_sample, yll$LE_diff[1])
    deaths_sample <- append(deaths_sample, sum(yll$Deaths.cause))
  }
  results <- c(selectRaceEthn, selectGender, selectYear, 'yll', 
               mean(yll_sample), confint(yll_sample), 
               'le lost', mean(le_lost_sample), confint(le_lost_sample))
  return(results)
}




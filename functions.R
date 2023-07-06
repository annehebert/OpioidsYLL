
## functions used in `YearsLifeLost.R`

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
    data$Race.Eth[data$Race=="White"]="White"
    data$Race.Eth[data$Race=="Black or African American"]="Black"
    data$Race.Eth[data$Race=="Asian or Pacific Islander"]="Asian"
    data$Race.Eth[data$Race=="American Indian or Alaska Native"]="Native"
    
    if ("Ages.Text" %in% colnames(data) & 
        "Ages" %in% colnames(data)){
      if ("State.Num" %in% colnames(data)){
        data=data[c(
          "Ages.Text", "Ages", "Gender","Race","Race.Eth","State.Text",
          "State.Num","Deaths","Population","Crude.Rate"
          )]   
      }else{
        data=data[c(
          "Ages.Text", "Ages", "Gender","Race","Race.Eth","Deaths","
          Population","Crude.Rate"
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
  
  if ((!"Race" %in% colnames(data)) & ("!Ethnicity" %in% colnames(data))){
    data$Race.Eth=Ages.Text
    data$Race.Eth="Not Stated" #if either category missing
    
    if ("State.Num" %in% colnames(data)){
      data=data[c(
        "Ages.Text", "Ages", "Gender","State.Text","State.Num","Deaths",
        "Population","Crude.Rate"
        )]
    }else{
      data=data[c(
        "Ages.Text", "Ages", "Gender","Deaths","Population","Crude.Rate"
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
  data$Crude.Rate=100000*data$Deaths/data$Population
  
  return(data)
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


make.lifetable <- function(cause = TRUE, df_data, lifetable){
  ## input df_data, a dataframe containing death rates by age. (
  ## When used with YearsLifeLost.R, df_data = `df_all_and_opioid_deaths`
  
  ## if cause = TRUE, will use death rates from a column named 'Crude.Rate.all'
  ## if cause = FALSE, will use death rates from column 'Crude.Rate.no.cause'

  
  ## also input lifetable, a dataframe pre-filled with lifetable data.
  ## When used with YearsLifeLost.R, lifetable = `df_lifetable_nvss`
  
  ## returns a completed lifetable calculated from the death rates in df_data
  ## output lifetable has a column for each standard lifetable variable, including
  ## e, the remaining life expectancy for each age. If cause=TRUE, estimates e 
  ## for the population given the measured deaths. If cause=FALSE, estimates 
  ## counterfactual e, for current population assuming absence of this  cause of
  ## death.
  
  
  #### Find number of rows in order to know how times to loop through dataframe
  endIndex = length(lifetable$Ages)
  ## find row at which there are no more (reported/uncensored) deaths
  midIndex = length(df_data$Crude.Rate.all[df_data$Crude.Rate.all > 0]) - 1 

  #### Make empty vectors which will be the columns of the lifetable matrix
  a = rep(0.5, endIndex) # for one year intervals, a = 1/2. deaths on average 
                         # occur midway through the interval
  q = rep(0, endIndex) # q is the prob of dying
  l = rep(0, endIndex) # l is the number of people surviving
  d = rep(0, endIndex) # d is the number of people dying
  L = rep(0, endIndex) # L is the total number of person-years
  m = rep(0, endIndex) # vector of mortality rates
  Tot = rep(0, endIndex) # T is Total number of person–years lived above age x
  e = rep(0, endIndex) # e is Expectation of life at age x
  
  
  #### Set initial values for first row
  l0 = 1e5  
  a[1] = 0.25 #for infants
  q[1] = m[1] / (1+(1-a[1])*m[1])
  l[1] = l0
  d[1] = q[1] * l[1]
  
  ## initial mortality rate is taken from df_data
  if (cause){
    m[1] = df_data$Crude.Rate.all[1]/l0 #crude death rate per 100,000
  } else{
    m[1] = df_data$Crude.Rate.no.cause[1]/l0 #crude death rate per 100,000
  }
  
  #### Fill in all rows of lifetable
  for (i in 2:midIndex){
    ## mortality is taken from df_data
    if (cause){
      m[i] = df_data$Crude.Rate.all[i]/l0 #crude death rate per 100,000
    } else{
      m[i] = df_data$Crude.Rate.no.cause[i]/l0 #crude death rate per 100,000
    }
    ## other values are calculated from m
    q[i] = m[i] / (1+(1-a[i])*m[i]) # probability of surviving to age x
    l[i] = l[i-1] * (1-q[i-1]) # number of people surviving to age x
    d[i] = q[i] * l[i] # number of people dying between age x and x+1
    L[i-1] = l[i] + d[i-1] * a[i-1] #total person-years above x
  }
  #print(midIndex)
  #print(endIndex)
  
  ## midIndex is point after which measured mortality rate is unavailable. We
  ## fill in q (probability of death) from NVSS table, scaling the NVSS values 
  ## at row midIndex to match our calculated values. Other lifetable variables
  ## are then estimated from q.
  
  for (i in (midIndex+1):(endIndex-1)){
    # crude death rate m is unavailable past this point so just set to 0
    m[i] = 0 
    # q[x] is probability of dying between age x and x+1. Use NVSS values for q
    # for remaining rows of lifetable, but scaled by q[midIndex]
    q[i] = as.numeric(lifetable$q[i]) * (as.numeric(q[midIndex])
                                         /as.numeric(lifetable$q[midIndex])) 
    # l[x] is number of people surviving to age x
    l[i] = l[i-1] * (1-q[i-1]) 
    # d[x] is number of people dying between age x and x+1
    d[i] = q[i] * l[i] 
    # L[x] is total person-years above x
    L[i-1] = l[i] + d[i-1] * a[i-1] 
  }
  # print((as.numeric(q[midIndex])
  #        /as.numeric(lifetable$q[midIndex])))
  
  ## final row
  i=endIndex
  q[i]=1
  l[i]=l[i-1]*(1-q[i-1])
  d[i]=q[i]*l[i]
  L[i-1]=l[i]+d[i-1]*a[i-1]
  a[i]=lifetable$L[i]/lifetable$d[i] #back out what NVSS a value must have been
  L[i]=a[i]*d[i]
  
  ## Calculate Tot and e
  for (i in 1:endIndex) {
    
    Tot[i]=sum(L[i:endIndex])
    e[i]=Tot[i]/l[i]
    
  }
  
  ## Create output dataframe from these calculated vectors.
  final_lifetable <- lifetable
  final_lifetable$a = a
  final_lifetable$q = q
  final_lifetable$l = l
  final_lifetable$d = d
  final_lifetable$L = L
  final_lifetable$Tot = Tot
  final_lifetable$e = e
  final_lifetable$m = m
  
  return(final_lifetable)
  
} # end function




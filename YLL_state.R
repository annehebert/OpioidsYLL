setwd("~/Opioid_project/opioid_burden")
source("functions.R")
library(tidyverse)

### Generate lifetables for specified demographic groups -----------------------
### below for OD deaths CDC data 

selectYear = '2021'
isFiveYearAgeGrouping = TRUE
onlyOpioids <- TRUE

### Import data
age.data = load("data/Ages.Rda")

df_all_deaths_age_state <- read.delim(file = paste('data/state_map_data/All_deaths_',
                                                   selectYear,
                                                   '_age_state.txt', sep = ""),
                                      header = TRUE, sep = "\t")
df_all_deaths_age_state <- clean.mortality.data(df_all_deaths_age_state) 

df_opioid_deaths_5yage_state <- read.delim(file = paste('data/state_map_data/Opioid_deaths_',
                                                        selectYear,
                                                   '_5yage_state.txt', sep = ""),
                                      header = TRUE, sep = "\t")
df_opioid_deaths_5yage_state <- clean.mortality.data(df_opioid_deaths_5yage_state) 

df_all_deaths_state <- read.delim(file = paste('data/state_map_data/All_deaths_',
                                               selectYear, '_state.txt', sep = ""),
                                      header = TRUE, sep = "\t")
df_all_deaths_state <- clean.mortality.data(df_all_deaths_state) 


# df_opioid_deaths_state <- read.delim(file = paste('data/state_map_data/Opioid_deaths_',
#                                                   selectYear,
#                                                '_state.txt', sep = ""),
#                                   header = TRUE, sep = "\t")
# df_opioid_deaths_state <- clean.mortality.data(df_opioid_deaths_state) 

statesVec = unique(df_all_deaths_age_state$State.Text)
df_YLL_State <- aggregate(Deaths ~ State.Text, df_all_deaths_age_state, sum)


### combining genders 
j = 0

for(state in sort(statesVec)){
  j = j + 1
  #state <- statesVec[1]
  #print(j)
  print(state)
  lifetable <- subset(df_all_deaths_age_state, State.Text == state, 
                      select = c("Ages.Text", "Ages", "Deaths", "Population",
                                 "State.Text"))
  
  lifetable <- lifetable %>%
    mutate(Crude.Rate = Deaths/Population*1e5) %>%
    arrange(Ages)
  
  if(onlyOpioids){
    if(isFiveYearAgeGrouping){
      lifetable_od_prelim <- subset(df_opioid_deaths_5yage_state, State.Text == state, 
                             select = c("Ages.Text", "Ages", "Deaths",
                                        "Population", "State.Text"))
    } else {lifetable_od_prelim <- subset(df.opioid.od.deaths.age.state, State.Text == state, 
                           select = c("Ages.Text", "Ages", "Deaths",
                                      "Population", "State.Text"))
  } 
  #   else{lifetable_od <- subset(df.all.od.deaths.age.state, State.Text == state, 
  #                               select = c("Ages.Text", "Ages", "Deaths",
  #                                          "Population", "State.Text"))
  # }
  }
  #lifetable_od <- aggregate(cbind(Deaths, Population) ~ Ages + Ages.Text, lifetable_od, sum)
  lifetable_od_prelim <- lifetable_od_prelim %>%
    mutate(Crude.Rate = Deaths/Population*1e5) %>%
    arrange(Ages) %>%
    rename(Age.groups = Ages.Text)
  
  lifetable_nvss <- read.csv(paste("data/lifetables/2020_Lifetable.csv",
                                   sep = ""), header = TRUE)
  lifetable_nvss <- clean.lifetable(lifetable_nvss)

  
  lifetable <- lifetable %>%
    right_join(Age.data, by = c("Ages.Text","Ages")) %>%
    replace_na(list(Crude.Rate = 0, Deaths = 0)) %>%
    arrange(Ages)
  lifetable_od <- lifetable_od_prelim %>%
    right_join(Age.data, by = c("Ages")) %>%
    replace_na(list(Crude.Rate = 0, Deaths = 0)) %>%
    arrange(Ages)

  if (isFiveYearAgeGrouping){
    lifetable_all <- lifetable %>%
      inner_join(lifetable_od, by = c("Ages", "Ages.Text"), suffix = c(".all", ".od")) %>%
      filter(Ages.Text != "Not Stated")
  } else{
    lifetable_all <- lifetable %>%
      inner_join(lifetable_od, by = c("Ages", "Ages.Text"), suffix = c(".all", ".od")) %>%
      filter(Ages.Text != "Not Stated") %>%
      mutate(Od.Perc = Deaths.od/Deaths.all*100)}
  
  if (isFiveYearAgeGrouping){
    for (i in c(1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)){
     # print(i+1)
     # print(lifetable_all$Crude.Rate.od[i+1])
      if (lifetable_all$Crude.Rate.od[i+1] != 0){
        #print(lifetable_all$Crude.Rate.od[i+1])
        lifetable_all$Crude.Rate.od[i+2] <- lifetable_all$Crude.Rate.od[i+1]
        lifetable_all$Crude.Rate.od[i+3] <- lifetable_all$Crude.Rate.od[i+1]
        lifetable_all$Crude.Rate.od[i+4] <- lifetable_all$Crude.Rate.od[i+1]
        lifetable_all$Crude.Rate.od[i+5] <- lifetable_all$Crude.Rate.od[i+1]
      }
    }
  }
  
  lifetable_all <- lifetable_all %>%
    mutate(Crude.Rate.no.cause = Crude.Rate.all - Crude.Rate.od)
  
  
  #create lifetables for ALL deaths and ALL deaths minus OD deaths
  lifetable.final.OD <- make.lifetable(cause = TRUE, lifetable_all, lifetable_nvss)
  lifetable.final.no.OD <- make.lifetable(cause = FALSE, lifetable_all, lifetable_nvss)
  


  #calculate YLL
  YLL_table <- lifetable_all
  YLL_table$LE.OD = lifetable.final.OD$e
  YLL_table$LE.no.OD = lifetable.final.no.OD$e
  YLL_table <- YLL_table %>%
    mutate(LE.diff = LE.no.OD - LE.OD)
  YLL_table <- YLL_table %>%
    mutate(YLL = Deaths.od*(LE.no.OD-0.5))
  
  #sum(YLL_table$YLL)
  
  #for (i in 1:nrow(lifetable)) {
  #  YLL_table$YLL[i] = lifetable_od$Deaths[i]*(lifetable.no.OD$e[i]-0.5)
  #print(paste("Age=",lifetable.no.OD$Ages[i],", LE=",lifetable.no.OD$e[i],", 
  #YLL=",lifetable_od$Deaths[i]*(lifetable.no.OD$e[i]-0.5)))
  #}
  
  print(sum(YLL_table$YLL))
  
  df_YLL_State$Deaths.OD[j] <- sum(YLL_table$Deaths.od)
  df_YLL_State$YLL[j] <- sum(YLL_table$YLL)
  df_YLL_State$YLLpp[j] <- sum(YLL_table$YLL)/sum(YLL_table$Deaths.od)
}

#commented section to normalize the YLL per capita
df_YLL_State$Population <- aggregate(Population ~ State.Text, 
                                     df_all_deaths_state, sum)$Population

df_YLL_State <- df_YLL_State %>%
  mutate(YLL.per.pop = YLL/Population)

df_YLL_map_state <- df_YLL_State %>%
  select(c('State.Text', 'YLL', 'YLL.per.pop'))

#write.csv(df_YLL_map_state, "2021_YLL_state_map.csv")


# create map dataframe with YLL data
df_YLL_map_all_ages <- merge(df_YLL_State,urbnmapr::states,
                             by.x="State.Text",by.y="state_name",all=TRUE)
df.YLL.map.20.50 <- merge(df_YLL_State,urbnmapr::states,
                          by.x="State",by.y="state_name",all=TRUE)



ggplot(data = df_YLL_map_all_ages, 
       mapping = aes(x = long, y = lat, group = group, fill = YLL.per.pop*100000)) +
  geom_polygon(color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_distiller(aesthetics = "fill", palette = "Blues", direction = 1,
                       limits = c(0,
                                  max(df.YLL.map.all.ages$YLL.per.pop)*100000)) +
  labs(fill = "YLL",
       title = paste("YLL/1000 in", selectYear, "using age_gender_state data")) +
  map_theme

ggplot(data = df.YLL.map.all.ages, 
       mapping = aes(x = long, y = lat, group = group, fill = YLL.per.pop)) +
  geom_polygon(color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_distiller(aesthetics = "fill", palette = "Blues", direction = 1,
                       limits = c(0,
                                  max(df.YLL.map.all.ages$YLL.per.pop)),
                       breaks = c(0,max(df.YLL.map.all.ages$YLL.per.pop)/2,
                                  max(df.YLL.map.all.ages$YLL.per.pop)),
                       labels = c(0, 130/2, 130)) +
  labs(fill = "YLL per 10,000 people",
       title = paste("YLL per capita in", year, "using age_gender_state data")) +
  map_theme


sum(df_YLL_State$YLL.both)


ggplot(data = df.YLL.map.20.50, 
       mapping = aes(x = long, y = lat, group = group, fill = YLL.both/1000)) +
  geom_polygon(color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_distiller(aesthetics = "fill", palette = "Blues", direction = 1,
                       limits = c(0,
                                  150)) +
  labs(fill = "YLL",
       title = paste("20-50 yo YLL/1000 in", year)) +
  map_theme




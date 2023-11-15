library(lookup)
library(tidyverse)
library(rvest)
library(lubridate)
library(readxl)
library(dplyr)
library(readxl)
library(aod)
library(ggplot2)
library(tidyr)
library(stringr)
library(lookup)
library(reshape)
library(coda)

setwd('/Users/matty/Documents/BYU/undergrad_research/filter')

playoff_games <- read.csv('/Users/matty/Documents/BYU/undergrad_research/old_data/playoff_games.csv')
regular_season <- read.csv('/Users/matty/Documents/BYU/undergrad_research/old_data/regular_season.csv')

names <- c('Visitor/Neutral','Visitor Points','Home/Neutral','Home Points','id','year','win','team','season_id','margin','playoff')

names(regular_season) <- names
names(playoff_games) <- names




###################
### My Function ###
###################

filter_teams1 <- function(first,last,part,team1,team2,team3){
  temp <- part$year >= first & part$year < last
  test <- part
  test$correct <- temp
  temp2 <- part$`Visitor/Neutral` == team1 | part$`Home/Neutral` == team1 | part$`Visitor/Neutral` == team2 | part$`Home/Neutral` == team2 | part$`Visitor/Neutral` == team3 | part$`Home/Neutral` == team3 
  test$cteams <- temp2
  df <- filter(test,correct == T & cteams == T)
  return(df)
}

filter_teams2 <- function(first,last,part,team1,team2,team3,team4){
  temp <- part$year >= first & part$year < last
  test <- part
  test$correct <- temp
  temp2 <- part$`Visitor/Neutral` == team1 | part$`Home/Neutral` == team1 | part$`Visitor/Neutral` == team2 | part$`Home/Neutral` == team2 | part$`Visitor/Neutral` == team3 | part$`Home/Neutral` == team3 | part$`Visitor/Neutral` == team4 | part$`Home/Neutral` == team4
  test$cteams <- temp2
  df <- filter(test,correct == T & cteams == T)
  return(df)
}

#80s
#Celtics
#Lakers
#Pistons

reg80 <- filter_teams1(1980,1990,regular_season,'BOS','DET','LAL')
post80 <- filter_teams1(1980,1990,playoff_games,'BOS','DET','LAL')

#90s
#Rockets
#Bulls
#Jazz

reg90 <- filter_teams1(1990,2000,regular_season,'HOU','CHI','UTA')
post90 <- filter_teams1(1990,2000,playoff_games,'HOU','CHI','UTA')

#00s
#Lakers
#Celtics
#Pistons
#Spurs

reg00 <- filter_teams2(2000,2010,regular_season,'LAL','BOS','DET','SAS')
post00 <- filter_teams2(2000,2010,playoff_games,'LAL','BOS','DET','SAS')

#10s
#Heat
#Cavs
#Warriors
#Spurs

reg10 <- filter_teams2(2010,2020,regular_season,'MIA','CLE','GSW','SAS')
post10 <- filter_teams2(2010,2020,playoff_games,'MIA','CLE','GWS','SAS')



write_csv(reg80,'reg80.csv')
write_csv(post80,'post80.csv')
write_csv(reg90,'reg90.csv')
write_csv(post90,'post90.csv')
write_csv(reg00,'reg00.csv')
write_csv(post00,'post00.csv')
write_csv(reg10,'reg10.csv')
write_csv(post10,'post10.csv')




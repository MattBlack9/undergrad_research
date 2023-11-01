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





playoff_games <- read.csv('playoff_games.csv')
regular_season <- read.csv('regular_season.csv')

names <- c('Visitor/Neutral','Visitor Points','Home/Neutral','Home Points','id','year','win','team','season_id','margin','playoff')

names(regular_season) <- names
names(playoff_games) <- names

nrow(playoff_games) / 2
playoff_games1 <- playoff_games[1:2953,]
playoff_games2 <- playoff_games[2954:5906,]

nrow(regular_season) / 7
regular_season1 <- regular_season[1:12362,]
regular_season2 <- regular_season[12362:24724,]
regular_season3 <- regular_season[24725:49448,]
regular_season4 <- regular_season[49449:61810,]
regular_season5 <- regular_season[61811:74172,]
regular_season6 <- regular_season[74173:86534,]

write_csv(playoff_games1,'playoff_games1.csv')
write_csv(playoff_games2,'playoff_games2.csv')

write_csv(regular_season1,'regular_season1.csv')
write_csv(regular_season2,'regular_season2.csv')
write_csv(regular_season3,'regular_season3.csv')
write_csv(regular_season4,'regular_season4.csv')
write_csv(regular_season5,'regular_season5.csv')
write_csv(regular_season6,'regular_season6.csv')




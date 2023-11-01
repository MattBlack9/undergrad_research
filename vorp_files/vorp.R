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

#####################################################
### Scraping Player Data and Calculating Avg VORP ###
#####################################################

### VORP of the Regular Season ###

regularstats <- data.frame(NULL)
for(g in 1:nrow(regular_season)){
  id <- regular_season$id[g]
  team <- regular_season$team[g]
  node <- paste0("table#box-", team, "-game-advanced > tbody > tr > td")
  node2 <- paste0("table#box-", team, "-game-advanced > tbody > tr > th")
  url <- paste0("https://www.basketball-reference.com/boxscores/", id, ".html")
  webpage <- read_html(url)
  vorp <- webpage %>% 
    html_nodes(node) %>% 
    html_attr("data-tip") %>%
    data.frame()
  vorp <- vorp[-which(is.na(vorp[,1])),]
  vorp <- data.frame(vorp)
  colnames(vorp) <- "VORP"
  for(i in 1:nrow(vorp)){
    vorp$VORP[i] <- as.numeric(substr(vorp$VORP[i],
                                      unlist(gregexpr("VORP", vorp$VORP[i]))[1] + 6,
                                      unlist(gregexpr("VORP", vorp$VORP[i]))[2] - 17))
  }
  
  min <- webpage %>% 
    html_nodes(node) %>% 
    html_text() %>%
    data.frame()
  colnames(min) <- "min"
  elim <- which(min$min == "Did Not Play")
  if(length(elim) > 0){  
    min <- min[-elim,]
    min <- data.frame(min)
    colnames(min) <- "min"
  }
  elim <- which(min$min == "Did Not Dress")
  if(length(elim) > 0){  
    min <- min[-elim,]
    min <- data.frame(min)
    colnames(min) <- "min"
  }
  elim <- which(min$min == "Inactive")
  if(length(elim) > 0){  
    min <- min[-elim,]
    min <- data.frame(min)
    colnames(min) <- "min"
  }
  elim <- which(min$min == "Not With Team")
  if(length(elim) > 0){  
    min <- min[-elim,]
    min <- data.frame(min)
    colnames(min) <- "min"
  }
  elim <- which(min$min == "Player Suspended")
  if(length(elim) > 0){  
    min <- min[-elim,]
    min <- data.frame(min)
    colnames(min) <- "min"
  }
  min <- min[which(unlist(gregexpr(":", min$min)) > 0),]
  min <- data.frame(min)
  for(i in 1:nrow(min)){
    min$sec[i] <- as.numeric(substr(min$min[i],
                                    nchar(min$min[i]) - 1,
                                    nchar(min$min[i])))
    min$min[i] <- as.numeric(substr(min$min[i],
                                    1 ,
                                    nchar(min$min[i]) - 3))
    if(is.na(min$sec[i])){
      min$sec[i] <- 0
    }
  }
  min$min <- as.numeric(min$min)
  min$min <- min$min + (min$sec / 60)
  min <- min %>%
    select(1)
  
  player <- webpage %>% 
    html_nodes(node2) %>% 
    html_attr("data-append-csv") %>%
    matrix(ncol = 1, byrow = TRUE) %>%
    data.frame()
  player <- player[-which(is.na(player[,1])),]
  player <- player[c(1:nrow(min))]
  
  
  adder <- cbind(player, min, vorp)
  adder <- adder[order(min, decreasing = TRUE),]
  adder$id <- paste0(id, "-", team)
  if(team == regular_season$`Home/Neutral`[g]){
    adder$opp_id <- paste0(id, "-", regular_season$`Visitor/Neutral`[g])
  } else {
    adder$opp_id <- paste0(id, "-", regular_season$`Home/Neutral`[g])
  }
  adder$season_id <- regular_season$season_id[g]
  adder$win <- regular_season$win[g]
  adder$year <- regular_season$year[g]
  adder$adj_VORP <- as.numeric(adder$VORP) * as.numeric(adder$min)
  adder$avg_VORP <- mean(adder$adj_VORP)
  
  regularstats <- rbind(regularstats, adder)
  
  print(paste0(round((g/nrow(regular_season)) * 100, 3),"%"))
  print(regularstats$player[g])
  Sys.sleep(5)
}
regularstats$link <- paste0(regularstats$player,"-",regularstats$year)

write_csv(regularstats,'regularstats.csv')

### VORP of the Playoffs ###

playoffstats <- data.frame(NULL)
for(g in 1:nrow(playoff_games)){
  id <- playoff_games$id[g]
  team <- playoff_games$team[g]
  node <- paste0("table#box-", team, "-game-advanced > tbody > tr > td")
  node2 <- paste0("table#box-", team, "-game-advanced > tbody > tr > th")
  url <- paste0("https://www.basketball-reference.com/boxscores/", id, ".html")
  webpage <- read_html(url)
  vorp <- webpage %>% 
    html_nodes(node) %>% 
    html_attr("data-tip") %>%
    data.frame()
  vorp <- vorp[-which(is.na(vorp[,1])),]
  vorp <- data.frame(vorp)
  colnames(vorp) <- "VORP"
  for(i in 1:nrow(vorp)){
    vorp$VORP[i] <- as.numeric(substr(vorp$VORP[i],
                                      unlist(gregexpr("VORP", vorp$VORP[i]))[1] + 6,
                                      unlist(gregexpr("VORP", vorp$VORP[i]))[2] - 17))
  }
  
  min <- webpage %>% 
    html_nodes(node) %>% 
    html_text() %>%
    data.frame()
  colnames(min) <- "min"
  elim <- which(min$min == "Did Not Play")
  if(length(elim) > 0){  
    min <- min[-elim,]
    min <- data.frame(min)
    colnames(min) <- "min"
  }
  elim <- which(min$min == "Did Not Dress")
  if(length(elim) > 0){  
    min <- min[-elim,]
    min <- data.frame(min)
    colnames(min) <- "min"
  }
  elim <- which(min$min == "Inactive")
  if(length(elim) > 0){  
    min <- min[-elim,]
    min <- data.frame(min)
    colnames(min) <- "min"
  }
  elim <- which(min$min == "Not With Team")
  if(length(elim) > 0){  
    min <- min[-elim,]
    min <- data.frame(min)
    colnames(min) <- "min"
  }
  elim <- which(min$min == "Player Suspended")
  if(length(elim) > 0){  
    min <- min[-elim,]
    min <- data.frame(min)
    colnames(min) <- "min"
  }
  min <- min[which(unlist(gregexpr(":", min$min)) > 0),]
  min <- data.frame(min)
  for(i in 1:nrow(min)){
    min$sec[i] <- as.numeric(substr(min$min[i],
                                    nchar(min$min[i]) - 1,
                                    nchar(min$min[i])))
    min$min[i] <- as.numeric(substr(min$min[i],
                                    1 ,
                                    nchar(min$min[i]) - 3))
    if(is.na(min$sec[i])){
      min$sec[i] <- 0
    }
  }
  min$min <- as.numeric(min$min)
  min$min <- min$min + (min$sec / 60)
  min <- min %>%
    select(1)
  
  player <- webpage %>% 
    html_nodes(node2) %>% 
    html_attr("data-append-csv") %>%
    matrix(ncol = 1, byrow = TRUE) %>%
    data.frame()
  player <- player[-which(is.na(player[,1])),]
  player <- player[c(1:nrow(min))]
  
  
  adder <- cbind(player, min, vorp)
  adder <- adder[order(min, decreasing = TRUE),]
  adder$id <- paste0(id, "-", team)
  if(team == playoff_games$`Home/Neutral`[g]){
    adder$opp_id <- paste0(id, "-", playoff_games$`Visitor/Neutral`[g])
  } else {
    adder$opp_id <- paste0(id, "-", playoff_games$`Home/Neutral`[g])
  }
  adder$season_id <- playoff_games$season_id[g]
  adder$win <- playoff_games$win[g]
  adder$year <- playoff_games$year[g]
  adder$adj_VORP <- as.numeric(adder$VORP) * as.numeric(adder$min)
  adder$avg_VORP <- mean(adder$adj_VORP)
  
  playoffstats <- rbind(playoffstats, adder)
  
  print(paste0(round((g/nrow(playoff_games)) * 100, 3),"%"))
  print(playoffstats$player[g])
  Sys.sleep(5)
}
playoffstats$link <- paste0(playoffstats$player,"-",playoffstats$year)

write_csv(playoffstats, 'playoffstats.csv')





# Getting stuck at 15% 

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


#####################################
### Setting the right directories ###
#####################################

setwd('/home/mtb77/linear')

playoff_games <- read.csv('playoff_games.csv')
regular_season <- read.csv('regular_season.csv')

names <- c('Visitor/Neutral','Visitor Points','Home/Neutral','Home Points','id','year','win','team','season_id','margin','playoff')

names(regular_season) <- names
names(playoff_games) <- names




####################
### My Functions ###
####################

filter_teams1 <- function(first,last,part,team1){
  temp <- part$year >= first & part$year < last
  test <- part
  test$correct <- temp
  temp2 <- part$`Visitor/Neutral` == team1 | part$`Home/Neutral` == team1 
  test$cteams <- temp2
  df <- filter(test,correct == T & cteams == T)
  return(df)
}

cleaning <- function(decade,team,list){
  temp <- decade
  temp$teamer <- temp$`Visitor/Neutral` == team | temp$`Home/Neutral` == team
  temp <- filter(temp,teamer == T)
  holder <- 1
  for(i in unique(temp$year)){
    curr <- temp[temp$year == i, ]
    curr <- curr[,1:10]
    list[[holder]] <- curr
    holder <- holder + 1
  }
  return(list)
}


# This will be getting all the years of a specific player
# The player will be John Stockton

my_list <- list()

regjohn <- filter_teams1(1985,2004,regular_season,'UTA')
postjohn <- filter_teams1(1985,2004,playoff_games,'UTA')

reg.stock <- cleaning(regjohn,'UTA',my_list)
post.stock <- cleaning(postjohn,'UTA',my_list)





#################################
### Calculating VORP Function ###
#################################

vorp <- function(df){
  stats <- data.frame(NULL)
  for(g in 1:nrow(df)){
    id <- df$id[g]
    team <- df$team[g]
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
    if(team == df$`Home/Neutral`[g]){
      adder$opp_id <- paste0(id, "-", df$`Visitor/Neutral`[g])
    } else {
      adder$opp_id <- paste0(id, "-", df$`Home/Neutral`[g])
    }
    adder$season_id <- df$season_id[g]
    adder$win <- df$win[g]
    adder$year <- df$year[g]
    adder$adj_VORP <- as.numeric(adder$VORP) * as.numeric(adder$min)
    adder$avg_VORP <- mean(adder$adj_VORP)
    
    stats <- rbind(stats, adder)
    
    print(paste0(round((g/nrow(df)) * 100, 3),"%"))
    Sys.sleep(5)
  }
  stats$link <- paste0(stats$player,"-",stats$year)
  
  return(stats)
}




##############################################################
### Separating the decades into years and calculating VORP ###
##############################################################

### Regular Season ###

john.reg.years <- c('john.reg85.csv','john.reg86.csv','john.reg87.csv','john.reg88.csv','john.reg89.csv',
                     'john.reg90.csv','john.reg91.csv','john.reg92.csv','john.reg93.csv','john.reg94.csv',
                     'john.reg95.csv','john.reg96.csv','john.reg97.csv','john.reg98.csv','john.reg99.csv',
                     'john.reg00.csv','john.reg01.csv','john.reg02.csv','john.reg03.csv')




### Post Season ###

john.post.years <- c('john.post85.csv','john.post86.csv','john.post87.csv','john.post88.csv','john.post89.csv',
                      'john.post90.csv','john.post91.csv','john.post92.csv','john.post93.csv','john.post94.csv',
                      'john.post95.csv','john.post96.csv','john.post97.csv','john.post98.csv','john.post99.csv',
                      'john.post00.csv','john.post01.csv','john.post02.csv','john.post03.csv')



### Function to Separate ###

ind.years <- function(list,years){
  holder <- 1
  for(i in list){
    temp <- vorp(i)
    write_csv(temp,years[holder])
    holder <- holder + 1
  }
}



### Separating the decades into individual csv's ###

ind.years(reg.stock,john.reg.years)
Sys.sleep(300)

ind.years(post.stock,john.post.years)



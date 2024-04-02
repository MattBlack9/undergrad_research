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


# This will be getting all the players from a specific season

# Regular Season
my_list <- list()

reg_alt <- filter_teams1(2011,2011,regular_season,'ATL')
reg_bos <- filter_teams1(2011,2011,regular_season,'BOS')
reg_bkn <- filter_teams1(2011,2011,regular_season,'BKN')
reg_cha <- filter_teams1(2011,2011,regular_season,'CHA')
reg_chi <- filter_teams1(2011,2011,regular_season,'CHI')
reg_cle <- filter_teams1(2011,2011,regular_season,'CLE')
reg_dal <- filter_teams1(2011,2011,regular_season,'DAL')
reg_den <- filter_teams1(2011,2011,regular_season,'DEN')
reg_det <- filter_teams1(2011,2011,regular_season,'DET')
reg_gsw <- filter_teams1(2011,2011,regular_season,'GSW')
reg_hou <- filter_teams1(2011,2011,regular_season,'HOU')
reg_ind <- filter_teams1(2011,2011,regular_season,'IND')
reg_lac <- filter_teams1(2011,2011,regular_season,'LAC')
reg_lal <- filter_teams1(2011,2011,regular_season,'LAL')
reg_mem <- filter_teams1(2011,2011,regular_season,'MEM')
reg_mia <- filter_teams1(2011,2011,regular_season,'MIA')
reg_mil <- filter_teams1(2011,2011,regular_season,'MIL')
reg_min <- filter_teams1(2011,2011,regular_season,'MIN')
reg_nop <- filter_teams1(2011,2011,regular_season,'NOP')
reg_nyk <- filter_teams1(2011,2011,regular_season,'NYK')
reg_okc <- filter_teams1(2011,2011,regular_season,'OKC')
reg_orl <- filter_teams1(2011,2011,regular_season,'ORL')
reg_phi <- filter_teams1(2011,2011,regular_season,'PHI')
reg_phx <- filter_teams1(2011,2011,regular_season,'PHX')
reg_por <- filter_teams1(2011,2011,regular_season,'POR')
reg_sac <- filter_teams1(2011,2011,regular_season,'SAC')
reg_sas <- filter_teams1(2011,2011,regular_season,'SAS')
reg_tor <- filter_teams1(2011,2011,regular_season,'TOR')
reg_uta <- filter_teams1(2011,2011,regular_season,'UTA')
reg_was <- filter_teams1(2011,2011,regular_season,'WAS')



# Post Season
post_alt <- filter_teams1(2011,2011,regular_season,'ATL')
post_bos <- filter_teams1(2011,2011,regular_season,'BOS')
post_bkn <- filter_teams1(2011,2011,regular_season,'BKN')
post_cha <- filter_teams1(2011,2011,regular_season,'CHA')
post_chi <- filter_teams1(2011,2011,regular_season,'CHI')
post_cle <- filter_teams1(2011,2011,regular_season,'CLE')
post_dal <- filter_teams1(2011,2011,regular_season,'DAL')
post_den <- filter_teams1(2011,2011,regular_season,'DEN')
post_det <- filter_teams1(2011,2011,regular_season,'DET')
post_gsw <- filter_teams1(2011,2011,regular_season,'GSW')
post_hou <- filter_teams1(2011,2011,regular_season,'HOU')
post_ind <- filter_teams1(2011,2011,regular_season,'IND')
post_lac <- filter_teams1(2011,2011,regular_season,'LAC')
post_lal <- filter_teams1(2011,2011,regular_season,'LAL')
post_mem <- filter_teams1(2011,2011,regular_season,'MEM')
post_mia <- filter_teams1(2011,2011,regular_season,'MIA')
post_mil <- filter_teams1(2011,2011,regular_season,'MIL')
post_min <- filter_teams1(2011,2011,regular_season,'MIN')
post_nop <- filter_teams1(2011,2011,regular_season,'NOP')
post_nyk <- filter_teams1(2011,2011,regular_season,'NYK')
post_okc <- filter_teams1(2011,2011,regular_season,'OKC')
post_orl <- filter_teams1(2011,2011,regular_season,'ORL')
post_phi <- filter_teams1(2011,2011,regular_season,'PHI')
post_phx <- filter_teams1(2011,2011,regular_season,'PHX')
post_por <- filter_teams1(2011,2011,regular_season,'POR')
post_sac <- filter_teams1(2011,2011,regular_season,'SAC')
post_sas <- filter_teams1(2011,2011,regular_season,'SAS')
post_tor <- filter_teams1(2011,2011,regular_season,'TOR')
post_uta <- filter_teams1(2011,2011,regular_season,'UTA')
post_was <- filter_teams1(2011,2011,regular_season,'WAS')


# Saving them
reg.alt <- cleaning(reg_atl,'ATL',my_list) 
reg.bos <- cleaning(reg_bos,'BOS',my_list)
reg.bkn <- cleaning(reg_bkn,'BKN',my_list)
reg.cha <- cleaning(reg_cha,'CHA',my_list)
reg.chi <- cleaning(reg_chi,'CHI',my_list)
reg.cle <- cleaning(reg_cle,'CLE',my_list)
reg.dal <- cleaning(reg_dal,'DAL',my_list)
reg.den <- cleaning(reg_den,'DEN',my_list)
reg.det <- cleaning(reg_det,'DET',my_list)
reg.gsw <- cleaning(reg_gsw,'GSW',my_list)
reg.hou <- cleaning(reg_hou,'HOU',my_list)
reg.ind <- cleaning(reg_ind,'IND',my_list)
reg.lac <- cleaning(reg_lac,'LAC',my_list)
reg.lal <- cleaning(reg_lal,'LAL',my_list)
reg.mem <- cleaning(reg_mem,'MEM',my_list)
reg.mia <- cleaning(reg_mia,'MIA',my_list)
reg.mil <- cleaning(reg_mil,'MIL',my_list)
reg.min <- cleaning(reg_min,'MIN',my_list)
reg.nop <- cleaning(reg_nop,'NOP',my_list)
reg.nyk <- cleaning(reg_nyk,'NYK',my_list)
reg.okc <- cleaning(reg_okc,'OKC',my_list)
reg.orl <- cleaning(reg_orl,'ORL',my_list)
reg.phi <- cleaning(reg_phi,'PHI',my_list)
reg.phx <- cleaning(reg_phx,'PHX',my_list)
reg.por <- cleaning(reg_por,'POR',my_list)
reg.sac <- cleaning(reg_sac,'SAC',my_list)
reg.sas <- cleaning(reg_sas,'SAS',my_list)
reg.tor <- cleaning(reg_tor,'TOR',my_list)
reg.uta <- cleaning(reg_uta,'UTA',my_list)
reg.was <- cleaning(reg_was,'WAS',my_list)

# Post Season
post.alt <- cleaning(post_atl,'ATL',my_list) 
post.bos <- cleaning(post_bos,'BOS',my_list)
post.bkn <- cleaning(post_bkn,'BKN',my_list)
post.cha <- cleaning(post_cha,'CHA',my_list)
post.chi <- cleaning(post_chi,'CHI',my_list)
post.cle <- cleaning(post_cle,'CLE',my_list)
post.dal <- cleaning(post_dal,'DAL',my_list)
post.den <- cleaning(post_den,'DEN',my_list)
post.det <- cleaning(post_det,'DET',my_list)
post.gsw <- cleaning(post_gsw,'GSW',my_list)
post.hou <- cleaning(post_hou,'HOU',my_list)
post.ind <- cleaning(post_ind,'IND',my_list)
post.lac <- cleaning(post_lac,'LAC',my_list)
post.lal <- cleaning(post_lal,'LAL',my_list)
post.mem <- cleaning(post_mem,'MEM',my_list)
post.mia <- cleaning(post_mia,'MIA',my_list)
post.mil <- cleaning(post_mil,'MIL',my_list)
post.min <- cleaning(post_min,'MIN',my_list)
post.nop <- cleaning(post_nop,'NOP',my_list)
post.nyk <- cleaning(post_nyk,'NYK',my_list)
post.okc <- cleaning(post_okc,'OKC',my_list)
post.orl <- cleaning(post_orl,'ORL',my_list)
post.phi <- cleaning(post_phi,'PHI',my_list)
post.phx <- cleaning(post_phx,'PHX',my_list)
post.por <- cleaning(post_por,'POR',my_list)
post.sac <- cleaning(post_sac,'SAC',my_list)
post.sas <- cleaning(post_sas,'SAS',my_list)
post.tor <- cleaning(post_tor,'TOR',my_list)
post.uta <- cleaning(post_uta,'UTA',my_list)
post.was <- cleaning(post_was,'WAS',my_list)




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
ind.years(reg.alt,'atl.reg.csv') 
Sys.sleep(300)
ind.years(reg.bos,'bos.reg.csv')
Sys.sleep(300)
ind.years(reg.bkn,'bkn.reg.csv') 
Sys.sleep(300)
ind.years(reg.cha,'cha.reg.csv')
Sys.sleep(300)
ind.years(reg.chi,'chi.reg.csv')
Sys.sleep(300)
ind.years(reg.cle,'cle.reg.csv')
Sys.sleep(300)
ind.years(reg.dal,'dal.reg.csv')
Sys.sleep(300)
ind.years(reg.den,'den.reg.csv')
Sys.sleep(300)
ind.years(reg.det,'det.reg.csv')
Sys.sleep(300)
ind.years(reg.gsw,'gsw.reg.csv')
Sys.sleep(300)
ind.years(reg.hou,'hou.reg.csv')
Sys.sleep(300)
ind.years(reg.ind,'ind.reg.csv')
Sys.sleep(300)
ind.years(reg.lac,'lac.reg.csv')
Sys.sleep(300)
ind.years(reg.lal,'lal.reg.csv')
Sys.sleep(300)
ind.years(reg.mem,'mem.reg.csv')
Sys.sleep(300)
ind.years(reg.mia,'mia.reg.csv')
Sys.sleep(300)
ind.years(reg.mil,'mil.reg.csv')
Sys.sleep(300)
ind.years(reg.min,'min.reg.csv')
Sys.sleep(300)
ind.years(reg.nop,'nop.reg.csv') 
Sys.sleep(300)
ind.years(reg.nyk,'nyk.reg.csv')
Sys.sleep(300)
ind.years(reg.okc,'okc.reg.csv')
Sys.sleep(300)
ind.years(reg.orl,'orl.reg.csv')
Sys.sleep(300)
ind.years(reg.phi,'phi.reg.csv')
Sys.sleep(300)
ind.years(reg.phx,'phx.reg.csv')
Sys.sleep(300)
ind.years(reg.por,'por.reg.csv')
Sys.sleep(300)
ind.years(reg.sac,'sac.reg.csv')
Sys.sleep(300)
ind.years(reg.sas,'sas.reg.csv')
Sys.sleep(300)
ind.years(reg.tor,'tor.reg.csv')
Sys.sleep(300)
ind.years(reg.uta,'uta.reg.csv')
Sys.sleep(300)
ind.years(reg.was,'was.reg.csv')
Sys.sleep(300)


ind.years(post.alt,'atl.post.csv') 
Sys.sleep(300)
ind.years(post.bos,'bos.post.csv')
Sys.sleep(300)
ind.years(post.bkn,'bkn.post.csv') 
Sys.sleep(300)
ind.years(post.cha,'cha.post.csv')
Sys.sleep(300)
ind.years(post.chi,'chi.post.csv')
Sys.sleep(300)
ind.years(post.cle,'cle.post.csv')
Sys.sleep(300)
ind.years(post.dal,'dal.post.csv')
Sys.sleep(300)
ind.years(post.den,'den.post.csv')
Sys.sleep(300)
ind.years(post.det,'det.post.csv')
Sys.sleep(300)
ind.years(post.gsw,'gsw.post.csv')
Sys.sleep(300)
ind.years(post.hou,'hou.post.csv')
Sys.sleep(300)
ind.years(post.ind,'ind.post.csv')
Sys.sleep(300)
ind.years(post.lac,'lac.post.csv')
Sys.sleep(300)
ind.years(post.lal,'lal.post.csv')
Sys.sleep(300)
ind.years(post.mem,'mem.post.csv')
Sys.sleep(300)
ind.years(post.mia,'mia.post.csv')
Sys.sleep(300)
ind.years(post.mil,'mil.post.csv')
Sys.sleep(300)
ind.years(post.min,'min.post.csv')
Sys.sleep(300)
ind.years(post.nop,'nop.post.csv') 
Sys.sleep(300)
ind.years(post.nyk,'nyk.post.csv')
Sys.sleep(300)
ind.years(post.okc,'okc.post.csv')
Sys.sleep(300)
ind.years(post.orl,'orl.post.csv')
Sys.sleep(300)
ind.years(post.phi,'phi.post.csv')
Sys.sleep(300)
ind.years(post.phx,'phx.post.csv')
Sys.sleep(300)
ind.years(post.por,'por.post.csv')
Sys.sleep(300)
ind.years(post.sac,'sac.post.csv')
Sys.sleep(300)
ind.years(post.sas,'sas.post.csv')
Sys.sleep(300)
ind.years(post.tor,'tor.post.csv')
Sys.sleep(300)
ind.years(post.uta,'uta.post.csv')
Sys.sleep(300)
ind.years(post.was,'was.post.csv')








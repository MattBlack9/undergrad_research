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


# 1.
# a)

###############################
### Team Name and ID Scrape ###
###############################

## Creating Data Frame for Lookup for Teams and IDs ##

url <- paste0("https://www.basketball-reference.com/leagues/NBA_1985.html")
webpage <- read_html(url)
comp <- webpage %>% 
  html_nodes("table#per_game-team > tbody > tr > td") %>% 
  html_text() %>%
  matrix(ncol = 24, byrow = TRUE) %>%
  data.frame() %>%
  select(1)
ids <- c("DEN", "LAL", "DET", "POR", "BOS", "SAS",
         "KCK", "PHI", "HOU", "DAL", "MIL", "GSW",
         "NJN", "UTA", "CHI", "CLE", "IND", "PHO",
         "LAC", "ATL", "WSB", "NYK", "SEA")
comp$ids <- ids
colnames(comp) <- c("teams", "ids")
for(j in 1:nrow(comp)){
  if(substring(comp$teams[j], nchar(comp$teams[j]), nchar(comp$teams[j])) == "*") {
    comp$teams[j] <- substring(comp$teams[j], 1, nchar(comp$teams[j]) - 1)
  } else {
    comp$teams[j] <- comp$teams[j]
  }
}
add <- data.frame(teams = c("Brooklyn Nets", "Charlotte Bobcats",
                            "Charlotte Bobcats", "Memphis Grizzlies",
                            "Miami Heat", "Minnesota Timberwolves",
                            "New Orleans Hornets", "New Orleans Pelicans",
                            "New Orleans/Oklahoma City Hornets", "Oklahoma City Thunder",
                            "Orlando Magic", "Sacramento Kings",
                            "Toronto Raptors", "Vancouver Grizzlies",
                            "Washington Wizards", "Charlotte Hornets"),
                  ids = c("BRK", "CHA",
                          "CHO", "MEM",
                          "MIA", "MIN",
                          "NOH", "NOP",
                          "NOK", "OKC",
                          "ORL", "SAC",
                          "TOR", "VAN",
                          "WAS", "CHH"))
comp <- rbind(comp, add)

# b)

######################################
### Scraping all games all seasons ###
######################################

### Before Starting Times & October - June ###

gamesdf <- data.frame(NULL)
for(y in c(1985, 1998)){
  for(m in c("october", "november", "december", "january", "february",
             "march", "april", "may", "june")){
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",y,"_games-",m,".html")
    webpage <- read_html(url)
    
    mon <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = 9, byrow = TRUE) %>%
      data.frame() %>%
      select(c(1:4))
    hed <- webpage %>%
      html_nodes("table#schedule > thead > tr > th") %>%
      html_attr("aria-label")
    hed <- hed[c(2:5)]
    colnames(mon) <- hed
    mon$`Visitor/Neutral_id` <- lookup(mon$`Visitor/Neutral`, comp$teams, comp$ids)
    mon$`Home/Neutral_id` <- lookup(mon$`Home/Neutral`, comp$teams, comp$ids)
    mon[,1] <- mon[,5]
    mon[,3] <- mon[,6]
    mon <- mon[,c(1:4)]
    dat <- webpage %>%
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk") %>%
      matrix(ncol = 1, byrow = TRUE) %>%
      data.frame()
    elim <- which(is.na(dat[,1]))
    if(length(elim) > 0){
      dat <- dat[-elim,]
    }
    
    mon <- cbind(mon, dat)
    hed <- append(hed, "id")
    colnames(mon) <- hed
    mon$`year` <- y
    
    mon$win <- 0
    mon$team <- NULL
    for(i in 1:nrow(mon)){
      if(substring(mon[i,5], nchar(mon[i,5]) - 2, nchar(mon[i,5])) == mon[i,3]){
        if(as.numeric(mon[i,4]) > as.numeric(mon[i,2])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,3]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,3]
        }
      } else {
        if(as.numeric(mon[i,2]) > as.numeric(mon[i,4])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,1]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,1]
        }
      }
    }
    gamesdf <- rbind(gamesdf, mon)
    Sys.sleep(5)
  }
}

### Before Starting Times & November - June ###

for(y in c(1988:1997, 2000)){
  for(m in c("november", "december", "january", "february",
             "march", "april", "may", "june")){
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",y,"_games-",m,".html")
    webpage <- read_html(url)
    
    mon <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = 9, byrow = TRUE) %>%
      data.frame() %>%
      select(c(1:4))
    hed <- webpage %>%
      html_nodes("table#schedule > thead > tr > th") %>%
      html_attr("aria-label")
    hed <- hed[c(2:5)]
    colnames(mon) <- hed
    mon$`Visitor/Neutral_id` <- lookup(mon$`Visitor/Neutral`, comp$teams, comp$ids)
    mon$`Home/Neutral_id` <- lookup(mon$`Home/Neutral`, comp$teams, comp$ids)
    mon[,1] <- mon[,5]
    mon[,3] <- mon[,6]
    mon <- mon[,c(1:4)]
    dat <- webpage %>%
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk") %>%
      matrix(ncol = 1, byrow = TRUE) %>%
      data.frame()
    elim <- which(is.na(dat[,1]))
    if(length(elim) > 0){
      dat <- dat[-elim,]
    }
    
    mon <- cbind(mon, dat)
    hed <- append(hed, "id")
    colnames(mon) <- hed
    mon$`year` <- y
    
    mon$win <- 0
    mon$team <- NULL
    for(i in 1:nrow(mon)){
      if(substring(mon[i,5], nchar(mon[i,5]) - 2, nchar(mon[i,5])) == mon[i,3]){
        if(as.numeric(mon[i,4]) > as.numeric(mon[i,2])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,3]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,3]
        }
      } else {
        if(as.numeric(mon[i,2]) > as.numeric(mon[i,4])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,1]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,1]
        }
      }
    }
    gamesdf <- rbind(gamesdf, mon)
    Sys.sleep(5)
  }
}

### Before Starting Times Other ###

for(y in c(1999)){
  for(m in c("february", "march", "april", "may", "june")){
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",y,"_games-",m,".html")
    webpage <- read_html(url)
    
    mon <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = 9, byrow = TRUE) %>%
      data.frame() %>%
      select(c(1:4))
    hed <- webpage %>%
      html_nodes("table#schedule > thead > tr > th") %>%
      html_attr("aria-label")
    hed <- hed[c(2:5)]
    colnames(mon) <- hed
    mon$`Visitor/Neutral_id` <- lookup(mon$`Visitor/Neutral`, comp$teams, comp$ids)
    mon$`Home/Neutral_id` <- lookup(mon$`Home/Neutral`, comp$teams, comp$ids)
    mon[,1] <- mon[,5]
    mon[,3] <- mon[,6]
    mon <- mon[,c(1:4)]
    dat <- webpage %>%
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk") %>%
      matrix(ncol = 1, byrow = TRUE) %>%
      data.frame()
    elim <- which(is.na(dat[,1]))
    if(length(elim) > 0){
      dat <- dat[-elim,]
    }
    
    mon <- cbind(mon, dat)
    hed <- append(hed, "id")
    colnames(mon) <- hed
    mon$`year` <- y
    
    mon$win <- 0
    mon$team <- NULL
    for(i in 1:nrow(mon)){
      if(substring(mon[i,5], nchar(mon[i,5]) - 2, nchar(mon[i,5])) == mon[i,3]){
        if(as.numeric(mon[i,4]) > as.numeric(mon[i,2])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,3]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,3]
        }
      } else {
        if(as.numeric(mon[i,2]) > as.numeric(mon[i,4])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,1]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,1]
        }
      }
    }
    gamesdf <- rbind(gamesdf, mon)
    Sys.sleep(5)
  }
}

### After Starting Times & October - June  ###

for(y in c(1986:1987, 2001:2004, 2007:2011, 2013:2019, 2022)){
  for(m in c("october", "november", "december", "january", "february", "march", "april", "may", "june")){
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",y,"_games-",m,".html")
    webpage <- read_html(url)
    
    mon <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = 10, byrow = TRUE) %>%
      data.frame() %>%
      select(c(2:5))
    hed <- webpage %>%
      html_nodes("table#schedule > thead > tr > th") %>%
      html_attr("aria-label")
    hed <- hed[c(3:6)]
    colnames(mon) <- hed
    mon$`Visitor/Neutral_id` <- lookup(mon$`Visitor/Neutral`, comp$teams, comp$ids)
    mon$`Home/Neutral_id` <- lookup(mon$`Home/Neutral`, comp$teams, comp$ids)
    mon[,1] <- mon[,5]
    mon[,3] <- mon[,6]
    mon <- mon[,c(1:4)]
    dat <- webpage %>%
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk") %>%
      matrix(ncol = 1, byrow = TRUE) %>%
      data.frame()
    elim <- which(is.na(dat[,1]))
    if(length(elim) > 0){
      dat <- dat[-elim,]
    }
    
    mon <- cbind(mon, dat)
    hed <- append(hed, "id")
    colnames(mon) <- hed
    mon$`year` <- y
    
    mon$win <- 0
    mon$team <- NULL
    for(i in 1:nrow(mon)){
      if(substring(mon[i,5], nchar(mon[i,5]) - 2, nchar(mon[i,5])) == mon[i,3]){
        if(as.numeric(mon[i,4]) > as.numeric(mon[i,2])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,3]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,3]
        }
      } else {
        if(as.numeric(mon[i,2]) > as.numeric(mon[i,4])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,1]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,1]
        }
      }
    }
    gamesdf <- rbind(gamesdf, mon)
    Sys.sleep(5)
  }
}

### After Starting Times & November - June  ###

for(y in c(2005:2006)){
  for(m in c("november", "december", "january", "february", "march", "april", "may", "june")){
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",y,"_games-",m,".html")
    webpage <- read_html(url)
    
    mon <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = 10, byrow = TRUE) %>%
      data.frame() %>%
      select(c(2:5))
    hed <- webpage %>%
      html_nodes("table#schedule > thead > tr > th") %>%
      html_attr("aria-label")
    hed <- hed[c(3:6)]
    colnames(mon) <- hed
    mon$`Visitor/Neutral_id` <- lookup(mon$`Visitor/Neutral`, comp$teams, comp$ids)
    mon$`Home/Neutral_id` <- lookup(mon$`Home/Neutral`, comp$teams, comp$ids)
    mon[,1] <- mon[,5]
    mon[,3] <- mon[,6]
    mon <- mon[,c(1:4)]
    dat <- webpage %>%
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk") %>%
      matrix(ncol = 1, byrow = TRUE) %>%
      data.frame()
    elim <- which(is.na(dat[,1]))
    if(length(elim) > 0){
      dat <- dat[-elim,]
    }
    
    mon <- cbind(mon, dat)
    hed <- append(hed, "id")
    colnames(mon) <- hed
    mon$`year` <- y
    
    mon$win <- 0
    mon$team <- NULL
    for(i in 1:nrow(mon)){
      if(substring(mon[i,5], nchar(mon[i,5]) - 2, nchar(mon[i,5])) == mon[i,3]){
        if(as.numeric(mon[i,4]) > as.numeric(mon[i,2])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,3]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,3]
        }
      } else {
        if(as.numeric(mon[i,2]) > as.numeric(mon[i,4])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,1]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,1]
        }
      }
    }
    gamesdf <- rbind(gamesdf, mon)
    Sys.sleep(5)
  }
}

### After Starting Times & December - June  ###

for(y in c(2012, 2021)){
  for(m in c("december", "january", "february", "march", "april", "may", "june")){
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",y,"_games-",m,".html")
    webpage <- read_html(url)
    
    mon <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = 10, byrow = TRUE) %>%
      data.frame() %>%
      select(c(2:5))
    hed <- webpage %>%
      html_nodes("table#schedule > thead > tr > th") %>%
      html_attr("aria-label")
    hed <- hed[c(3:6)]
    colnames(mon) <- hed
    mon$`Visitor/Neutral_id` <- lookup(mon$`Visitor/Neutral`, comp$teams, comp$ids)
    mon$`Home/Neutral_id` <- lookup(mon$`Home/Neutral`, comp$teams, comp$ids)
    mon[,1] <- mon[,5]
    mon[,3] <- mon[,6]
    mon <- mon[,c(1:4)]
    dat <- webpage %>%
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk") %>%
      matrix(ncol = 1, byrow = TRUE) %>%
      data.frame()
    elim <- which(is.na(dat[,1]))
    if(length(elim) > 0){
      dat <- dat[-elim,]
    }
    
    mon <- cbind(mon, dat)
    hed <- append(hed, "id")
    colnames(mon) <- hed
    mon$`year` <- y
    
    mon$win <- 0
    mon$team <- NULL
    for(i in 1:nrow(mon)){
      if(substring(mon[i,5], nchar(mon[i,5]) - 2, nchar(mon[i,5])) == mon[i,3]){
        if(as.numeric(mon[i,4]) > as.numeric(mon[i,2])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,3]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,3]
        }
      } else {
        if(as.numeric(mon[i,2]) > as.numeric(mon[i,4])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,1]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,1]
        }
      }
    }
    gamesdf <- rbind(gamesdf, mon)
    Sys.sleep(5)
  }
}

### After Starting Times & December - June  ###

for(y in c(2020)){
  for(m in c("october-2019", "november", "december", "january",
             "february", "march", "july", "august", "september", "october-2020")){
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_",y,"_games-",m,".html")
    webpage <- read_html(url)
    
    mon <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = 10, byrow = TRUE) %>%
      data.frame() %>%
      select(c(2:5))
    hed <- webpage %>%
      html_nodes("table#schedule > thead > tr > th") %>%
      html_attr("aria-label")
    hed <- hed[c(3:6)]
    colnames(mon) <- hed
    mon$`Visitor/Neutral_id` <- lookup(mon$`Visitor/Neutral`, comp$teams, comp$ids)
    mon$`Home/Neutral_id` <- lookup(mon$`Home/Neutral`, comp$teams, comp$ids)
    mon[,1] <- mon[,5]
    mon[,3] <- mon[,6]
    mon <- mon[,c(1:4)]
    dat <- webpage %>%
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk") %>%
      matrix(ncol = 1, byrow = TRUE) %>%
      data.frame()
    elim <- which(is.na(dat[,1]))
    if(length(elim) > 0){
      dat <- dat[-elim,]
    }
    
    mon <- cbind(mon, dat)
    hed <- append(hed, "id")
    colnames(mon) <- hed
    mon$`year` <- y
    
    mon$win <- 0
    mon$team <- NULL
    for(i in 1:nrow(mon)){
      if(substring(mon[i,5], nchar(mon[i,5]) - 2, nchar(mon[i,5])) == mon[i,3]){
        if(as.numeric(mon[i,4]) > as.numeric(mon[i,2])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,3]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,3]
        }
      } else {
        if(as.numeric(mon[i,2]) > as.numeric(mon[i,4])){
          mon$win[i] <- 1
          mon$team[i] <- mon[i,1]
        } else {
          mon$win[i] <- 0
          mon$team[i] <- mon[i,1]
        }
      }
    }
    gamesdf <- rbind(gamesdf, mon)
    Sys.sleep(5)
  }
}

############################################
### Cleaning and Manipulating Data Frame ###
############################################

gamesdf$`season_id` <- paste0(gamesdf$team, "-", gamesdf$year)

gamesdf_2 <- gamesdf
for(i in 1:nrow(gamesdf_2)){
  if(gamesdf_2$team[i] == gamesdf_2$`Home/Neutral`[i]){
    gamesdf_2$team[i] <- gamesdf_2$`Visitor/Neutral`[i]
  } else {
    gamesdf_2$team[i] <- gamesdf_2$`Home/Neutral`[i]
  }
}
for(i in 1:nrow(gamesdf_2)){
  if(gamesdf_2$win[i] == 0){
    gamesdf_2$win[i] <- 1
  } else {
    gamesdf_2$win[i] <- 0
  }
}
gamesdf_2$season_id <- paste0(gamesdf_2$team, "-", gamesdf_2$year)

gamesdf <- rbind(gamesdf, gamesdf_2)  


for(i in 1:nrow(gamesdf)){
  if(gamesdf$year[i] > 2014 & gamesdf$team[i] == "CHH"){
    gamesdf$team[i] <- "CHO"
  }
  if(gamesdf$year[i] > 2014 & gamesdf$`Visitor/Neutral`[i] == "CHH"){
    gamesdf$`Visitor/Neutral`[i] <- "CHO"
  }
  if(gamesdf$year[i] > 2014 & gamesdf$`Home/Neutral`[i] == "CHH"){
    gamesdf$`Home/Neutral`[i] <- "CHO"
  }
  print(paste0(round((i/nrow(gamesdf)) * 100, 3),"%"))
}

for(i in 1:nrow(gamesdf)){
  if(gamesdf$team[i] == gamesdf$`Home/Neutral`[i]){
    gamesdf$margin[i] <- as.numeric(gamesdf$`Home Points`[i]) - as.numeric(gamesdf$`Visitor Points`[i])
  } else {
    gamesdf$margin[i] <- as.numeric(gamesdf$`Visitor Points`[i]) - as.numeric(gamesdf$`Home Points`[i])
  }
  print(paste0(round((i/nrow(gamesdf)) * 100, 3),"%"))
}

#################################################
### Separating Regular Season and Post Season ###
#################################################

playoff <- read_csv('playoff.csv')

gamesdf$playoff 

for(i in 1:nrow(gamesdf)){
  if(gamesdf$id[i] %in% playoff$Games){
    gamesdf$playoff[i] <- T
  }
  else{
    gamesdf$playoff[i] <- F
  }
}

playoff_games <- data.frame(NULL)
regular_season <- data.frame(NULL)

playoff_games <- filter(gamesdf, playoff == T)
regular_season <- filter(gamesdf, playoff == F)

write_csv(regular_season, 'regular_season.csv')
write_csv(playoff_games, 'playoff_games.csv')

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
  Sys.sleep(5)
}
playoffstats$link <- paste0(playoffstats$player,"-",playoffstats$year)

write_csv(playoffstats, 'playoffstats.csv')

library(tidyverse)
library(ggplot2)

###########################
### Loading in the data ###
###########################

### Setting the directory of the data ####

setwd('/Users/matty/Documents/BYU/undergrad_research/merging')

### Loading in the data ###

bulls <- read.csv('bulls.csv')
celtics <- read.csv('celtics.csv')
jazz <- read.csv('jazz.csv')
rockets <- read.csv('rockets.csv')
warriors <- read.csv('warriors.csv')
caveliers <- read.csv('caveliers.csv')
heat <- read.csv('heat.csv')
lakers <- read.csv('lakers.csv')
pistons <- read.csv('pistons.csv')
spurs <- read.csv('spurs.csv')

### Setting directory of the file ###

setwd('/Users/matty/Documents/BYU/undergrad_research/stats')


#############################
### Creating My Functions ###
#############################

### This function plots a scatter-plot and line ###

scatt <- function(data,years=NULL){
  if(is.null(years) == TRUE){
    temp1 <- filter(data, time=='regular')
    temp2 <- filter(data,time=='post')
    plot(temp1$TVORP,temp2$TVORP)
    abline(0,1)
  }
  else{
    temp1 <- filter(data, time=='regular', year == years)
    temp2 <- filter(data,time=='post', year == years)
    plot(temp1$TVORP,temp2$TVORP)
    abline(0,1)
  }
}

### This function will plot the player over years ###

player <- function(play, team){
  temp <- filter(team, player == play)
  ggplot(temp, aes(x = year, y = TVORP, color = time)) +
    geom_point() +
    geom_line(aes(group = time)) +
    labs(title = "Player performance in the regular season vs post season")
}



############################
### Creating My Analyses ###
############################

player('stockjo01',jazz)
player('malonka01',jazz)

player('curryst01',warriors)
player('thompkl01',warriors)

player('jordami01',bulls)
player('pippesc01',bulls)

player('jamesle01',caveliers)
player('irvinky01',caveliers)

player('abdulka01',lakers)
player('johnsma02',lakers)

player('bryanko01',lakers)
player('onealsh01',lakers)












summary(lm(data$post~data$reg))

summary(lm(data$reg~data$post))


mean(data$reg<data$post,na.rm = T)







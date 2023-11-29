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

scatt <- function(data,years=NULL,show=NULL){
  if(is.null(years) == TRUE){
    ggplot(data)+
      geom_point(aes(x=RVORP,y=PVORP), color = '#e8e301') +
      geom_abline(intercept = 0,slope=1, color='red')
  }
  else{
    temp <- filter(data, year == years)
    ggplot(temp)+
      geom_point(aes(x=RVORP,y=PVORP), color = '#e8e301') +
      geom_abline(intercept = 0,slope=1, color='red')
  }
}


### This function will plot the player over years ###

player <- function(play, team){
  temp <- filter(team, player == play)
  ggplot(temp) +
    geom_point(aes(x = year, y= RVORP),color = '#3d34e1') +
    geom_point(aes(x = year, y= PVORP), color = '#e13434') +
    geom_line(aes(x = year,y=RVORP),color='#3d34e1') +
    geom_line(aes(x = year,y=PVORP),color='#e13434')
}




# Yellow #e1dc34
# I need to set the axis for the player graph, set the points to yellow if they
# won the championship that year, and show the players on the scatter plot
# if wanted.
# Additionally, I need to make sure that the axis, title, and legends all make sense.
# Furthermore, I need to create a new function that takes in numerous players
# and will graph the difference of their playoff vorp - regular season vorp.
# If I have time, I will also make sense of all of the weird strings used for players.
# Lastly, I will create plots of all of the important players from every team in
# each decade.
# Max vorp will be 15, min -15

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

player('jamesle01',heat)
player('wadedw01',heat)

player('birdla01',celtics)









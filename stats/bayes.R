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
playoffers <- read.csv('playoffers.csv')
regulars <- read.csv('regulars.csv')

### Setting directory of the file ###

setwd('/Users/matty/Documents/BYU/undergrad_research/stats')


############################
### Creating my analysis ###
############################


mean(filter(bulls,year==1998)$PVORP)
mean(filter(jazz,year==1998)$PVORP)

sum(filter(bulls,year==1998)$PVORP)
sum(filter(jazz,year==1998)$PVORP)

mean(filter(spurs,year==2014)$PVORP)
mean(filter(heat,year==2014)$PVORP)

sum(filter(spurs,year==2009)$PVORP)
sum(filter(lakers,year==2009)$PVORP)

mean(filter(spurs,year==2009)$PVORP)
mean(filter(lakers,year==2009)$PVORP)

hist(filter(lakers,year==2009)$PVORP)
hist(filter(spurs,year==2009)$PVORP)


plot(density(bulls$PVORP), xlab=expression(PVORP), ylab="density", main= 'testing')
lines(plot(density(jazz$PVORP)), add = T)

plot(density(bulls$PVORP), xlab=expression(PVORP), ylab="density", main="Multiple Density Plots", ylim = c(0,0.35))
lines(density(jazz$PVORP), col = "purple")
lines(density(celtics$PVORP), col = "green")
lines(density(lakers$PVORP), col = "orange")
lines(density(warriors$PVORP), col = 'blue')
lines(density(pistons$PVORP), col = 'red')









#> Run a linear regression for every player and create a histogram of the coefficients
















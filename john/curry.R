library(tidyverse)
library(ggplot2)

###########################
### Loading in the data ###
###########################

### Setting the directory of the data ####

setwd('/Users/matty/Documents/BYU/undergrad_research/john')

### Loading in the data ###

michael <- read.csv('curry_test.csv')


#########################################
### Cleaning the data and graphing it ###
#########################################

### Cleaning the data ###

temp <- michael$player == 'curryst01'
michael$correct <- temp
pippin <- filter(michael, correct == T)
others <- filter(michael, correct == F)

reg90 <- filter(pippin, year == 2013, time == 'reg')$VORP - mean(filter(others, year == 2013, time == 'reg')$VORP)
post90 <- filter(pippin, year == 2013, time == 'post')$VORP - mean(filter(others, year == 2013, time == 'post')$VORP)

reg91 <- filter(pippin, year == 2015, time == 'reg')$VORP - mean(filter(others, year == 2015, time == 'reg')$VORP)
post91 <- filter(pippin, year == 2015, time == 'post')$VORP - mean(filter(others, year == 2015, time == 'post')$VORP)

reg92 <- filter(pippin, year == 2016, time == 'reg')$VORP - mean(filter(others, year == 2016, time == 'reg')$VORP)
post92 <- filter(pippin, year == 2016, time == 'post')$VORP - mean(filter(others, year == 2016, time == 'post')$VORP)

reg93 <- filter(pippin, year == 2017, time == 'reg')$VORP - mean(filter(others, year == 2017, time == 'reg')$VORP)
post93 <- filter(pippin, year == 2017, time == 'post')$VORP - mean(filter(others, year == 2017, time == 'post')$VORP)

reg94 <- filter(pippin, year == 2018, time == 'reg')$VORP - mean(filter(others, year == 2018, time == 'reg')$VORP)
post94 <- filter(pippin, year == 2018, time == 'post')$VORP - mean(filter(others, year == 2018, time == 'post')$VORP)


reg <- rbind(reg90,reg91,reg92,reg93,reg94)
post <- rbind(post90,post91,post92,post93,post94)

reg <- as.numeric(reg)
post <- as.numeric(post)

write.csv(reg,'curry_reg.csv')
write.csv(post,'curry_post.csv')

### Graphing the data ###

dens_reg <- density(reg)
plot(dens_reg, xlab = expression('Game VORP - Team Mean Regular Season Adjusted VORP'), ylab = 'Density', main = '', ylim = c(0,0.07))

dens_post <- density(post)
lines(dens_post, col = 'blue')
legend('topright', legend = c('reg', 'post'), col = c('black', 'blue'), lty = 1)
title(main = 'Stephen Curry')




test1 <- c(post[1,], post[2,], post[3,], post[4,], post[5,])

test2 <- c(reg[1,], reg[2,], reg[3,], reg[4,], reg[5,])

t1 <- data.frame(VORP = test1, time = 'post')
t2 <- data.frame(VORP = test2, time = 'reg')

test <- data.frame()

test <- rbind(test,t1,t2)

write.csv(test,'curry_t.csv')



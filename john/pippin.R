library(tidyverse)
library(ggplot2)

###########################
### Loading in the data ###
###########################

### Setting the directory of the data ####

setwd('/Users/matty/Documents/BYU/undergrad_research/john')

### Loading in the data ###

michael <- read.csv('jordan_test.csv')


#########################################
### Cleaning the data and graphing it ###
#########################################

### Cleaning the data ###

temp <- michael$player == 'pippesc01'
michael$correct <- temp
pippin <- filter(michael, correct == T)
others <- filter(michael, correct == F)

reg90 <- filter(pippin, year == 1990, time == 'reg')$VORP - mean(filter(others, year == 1990, time == 'reg')$VORP)
post90 <- filter(pippin, year == 1990, time == 'post')$VORP - mean(filter(others, year == 1990, time == 'post')$VORP)

reg91 <- filter(pippin, year == 1991, time == 'reg')$VORP - mean(filter(others, year == 1991, time == 'reg')$VORP)
post91 <- filter(pippin, year == 1991, time == 'post')$VORP - mean(filter(others, year == 1991, time == 'post')$VORP)

reg92 <- filter(pippin, year == 1992, time == 'reg')$VORP - mean(filter(others, year == 1992, time == 'reg')$VORP)
post92 <- filter(pippin, year == 1992, time == 'post')$VORP - mean(filter(others, year == 1992, time == 'post')$VORP)

reg93 <- filter(pippin, year == 1993, time == 'reg')$VORP - mean(filter(others, year == 1993, time == 'reg')$VORP)
post93 <- filter(pippin, year == 1993, time == 'post')$VORP - mean(filter(others, year == 1993, time == 'post')$VORP)

reg94 <- filter(pippin, year == 1994, time == 'reg')$VORP - mean(filter(others, year == 1994, time == 'reg')$VORP)
post94 <- filter(pippin, year == 1994, time == 'post')$VORP - mean(filter(others, year == 1994, time == 'post')$VORP)

reg95 <- filter(pippin, year == 1995, time == 'reg')$VORP - mean(filter(others, year == 1995, time == 'reg')$VORP)
post95 <- filter(pippin, year == 1995, time == 'post')$VORP - mean(filter(others, year == 1995, time == 'post')$VORP)

reg96 <- filter(pippin, year == 1996, time == 'reg')$VORP - mean(filter(others, year == 1996, time == 'reg')$VORP)
post96 <- filter(pippin, year == 1996, time == 'post')$VORP - mean(filter(others, year == 1996, time == 'post')$VORP)

reg97 <- filter(pippin, year == 1997, time == 'reg')$VORP - mean(filter(others, year == 1997, time == 'reg')$VORP)
post97 <- filter(pippin, year == 1997, time == 'post')$VORP - mean(filter(others, year == 1997, time == 'post')$VORP)

reg98 <- filter(pippin, year == 1998, time == 'reg')$VORP - mean(filter(others, year == 1998, time == 'reg')$VORP)
post98 <- filter(pippin, year == 1998, time == 'post')$VORP - mean(filter(others, year == 1998, time == 'post')$VORP)

reg <- rbind(reg90,reg91,reg92,reg93,reg94,reg95,reg96,reg97,reg98)
post <- rbind(post90,post91,post92,post93,post94,post95,post96,post97,post98)

reg <- as.numeric(reg)
post <- as.numeric(post)

write.csv(reg,'pippin_reg.csv')
write.csv(post,'pippin_post.csv')

### Graphing the data ###

dens_reg <- density(reg, adjust = 2)
plot(dens_reg, xlab = expression('Game VORP - Team Mean Regular Season Adjusted VORP'), ylab = 'Density', main = '', ylim = c(0,0.07))

dens_post <- density(post, adjust = 2)
lines(dens_post, col = 'red')
legend('topright', legend = c('reg', 'post'), col = c('black', 'red'), lty = 1)
title(main = 'Scottie Pippen')





test1 <- c(post[1,], post[2,], post[3,], post[4,], post[5,], post[6,], post[7,],post[8,],post[9,])

test2 <- c(reg[1,], reg[2,], reg[3,], reg[4,], reg[5,], reg[6,], reg[7,],reg[8,],reg[9,])

t1 <- data.frame(VORP = test1, time = 'post')
t2 <- data.frame(VORP = test2, time = 'reg')

test <- data.frame()

test <- rbind(test,t1,t2)

write.csv(test,'pippin_t.csv')






library(tidyverse)
library(ggplot2)

###########################
### Loading in the data ###
###########################

### Setting the directory of the data ####

setwd('/Users/matty/Documents/BYU/undergrad_research/john')

### Loading in the data ###

michael <- read.csv('kobe_test.csv')


#########################################
### Cleaning the data and graphing it ###
#########################################

### Cleaning the data ###

temp <- michael$player == 'bryanko01'
michael$correct <- temp
pippin <- filter(michael, correct == T)
others <- filter(michael, correct == F)

reg90 <- filter(pippin, year == 2000, time == 'reg')$VORP - mean(filter(others, year == 2000, time == 'reg')$VORP)
post90 <- filter(pippin, year == 2000, time == 'post')$VORP - mean(filter(others, year == 2000, time == 'post')$VORP)

reg91 <- filter(pippin, year == 2001, time == 'reg')$VORP - mean(filter(others, year == 2001, time == 'reg')$VORP)
post91 <- filter(pippin, year == 2001, time == 'post')$VORP - mean(filter(others, year == 2001, time == 'post')$VORP)

reg92 <- filter(pippin, year == 2002, time == 'reg')$VORP - mean(filter(others, year == 2002, time == 'reg')$VORP)
post92 <- filter(pippin, year == 2002, time == 'post')$VORP - mean(filter(others, year == 2002, time == 'post')$VORP)

reg93 <- filter(pippin, year == 2003, time == 'reg')$VORP - mean(filter(others, year == 2003, time == 'reg')$VORP)
post93 <- filter(pippin, year == 2003, time == 'post')$VORP - mean(filter(others, year == 2003, time == 'post')$VORP)

reg94 <- filter(pippin, year == 2004, time == 'reg')$VORP - mean(filter(others, year == 2004, time == 'reg')$VORP)
post94 <- filter(pippin, year == 2004, time == 'post')$VORP - mean(filter(others, year == 2004, time == 'post')$VORP)

reg95 <- filter(pippin, year == 2006, time == 'reg')$VORP - mean(filter(others, year == 2006, time == 'reg')$VORP)
post95 <- filter(pippin, year == 2006, time == 'post')$VORP - mean(filter(others, year == 2006, time == 'post')$VORP)

reg96 <- filter(pippin, year == 2007, time == 'reg')$VORP - mean(filter(others, year == 2007, time == 'reg')$VORP)
post96 <- filter(pippin, year == 2007, time == 'post')$VORP - mean(filter(others, year == 2007, time == 'post')$VORP)

reg97 <- filter(pippin, year == 2008, time == 'reg')$VORP - mean(filter(others, year == 2008, time == 'reg')$VORP)
post97 <- filter(pippin, year == 2008, time == 'post')$VORP - mean(filter(others, year == 2008, time == 'post')$VORP)

reg98 <- filter(pippin, year == 2009, time == 'reg')$VORP - mean(filter(others, year == 2009, time == 'reg')$VORP)
post98 <- filter(pippin, year == 2009, time == 'post')$VORP - mean(filter(others, year == 2009, time == 'post')$VORP)

reg <- rbind(reg90,reg91,reg92,reg93,reg94,reg95,reg96,reg97,reg98)
post <- rbind(post90,post91,post92,post93,post94,post95,post96,post97,post98)

reg <- as.numeric(reg)
post <- as.numeric(post)

write.csv(reg,'kobe_reg.csv')
write.csv(post,'kobe_post.csv')

### Graphing the data ###

dens_reg <- density(reg)
plot(dens_reg, xlab = expression('Game VORP - Team Mean Regular Season Adjusted VORP'), ylab = 'Density', main = '', ylim = c(0,0.07))

dens_post <- density(post)
lines(dens_post, col = '#EEBC1D')
legend('topright', legend = c('reg', 'post'), col = c('black', '#EEBC1D'), lty = 1)
title(main = 'Kobe Bryant')



test1 <- c(post[1,], post[2,], post[3,], post[4,], post[5,], post[6,], post[7,],post[8,],post[9,])

test2 <- c(reg[1,], reg[2,], reg[3,], reg[4,], reg[5,], reg[6,], reg[7,],reg[8,],reg[9,])

t1 <- data.frame(VORP = test1, time = 'post')
t2 <- data.frame(VORP = test2, time = 'reg')

test <- data.frame()

test <- rbind(test,t1,t2)

write.csv(test,'kobe_t.csv')







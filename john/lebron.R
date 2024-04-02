library(tidyverse)
library(ggplot2)

###########################
### Loading in the data ###
###########################

### Setting the directory of the data ####

setwd('/Users/matty/Documents/BYU/undergrad_research/john')

### Loading in the data ###

michael <- read.csv('lebron_cav_test.csv')
jordan <- read.csv('lebron_heat_test.csv')

#########################################
### Cleaning the data and graphing it ###
#########################################

### Cleaning the data ###

temp <- michael$player == 'jamesle01'
michael$correct <- temp
pippin <- filter(michael, correct == T)
others <- filter(michael, correct == F)

temp2 <- jordan$player == 'jamesle01'
jordan$correct <- temp2
pippin2 <- filter(jordan, correct == T)
others2 <- filter(jordan, correct == F)

reg90 <- filter(pippin, year == 2010, time == 'reg')$VORP - mean(filter(others, year == 2010, time == 'reg')$VORP)
post90 <- filter(pippin, year == 2010, time == 'post')$VORP - mean(filter(others, year == 2010, time == 'post')$VORP)

reg91 <- filter(pippin2, year == 2011, time == 'reg')$VORP - mean(filter(others2, year == 2011, time == 'reg')$VORP)
post91 <- filter(pippin2, year == 2011, time == 'post')$VORP - mean(filter(others2, year == 2011, time == 'post')$VORP)

reg92 <- filter(pippin2, year == 2012, time == 'reg')$VORP - mean(filter(others2, year == 2012, time == 'reg')$VORP)
post92 <- filter(pippin2, year == 2012, time == 'post')$VORP - mean(filter(others2, year == 2012, time == 'post')$VORP)

reg93 <- filter(pippin2, year == 2013, time == 'reg')$VORP - mean(filter(others2, year == 2013, time == 'reg')$VORP)
post93 <- filter(pippin2, year == 2013, time == 'post')$VORP - mean(filter(others2, year == 2013, time == 'post')$VORP)

reg94 <- filter(pippin2, year == 2014, time == 'reg')$VORP - mean(filter(others2, year == 2014, time == 'reg')$VORP)
post94 <- filter(pippin2, year == 2014, time == 'post')$VORP - mean(filter(others2, year == 2014, time == 'post')$VORP)

reg95 <- filter(pippin, year == 2015, time == 'reg')$VORP - mean(filter(others, year == 2015, time == 'reg')$VORP)
post95 <- filter(pippin, year == 2015, time == 'post')$VORP - mean(filter(others, year == 2015, time == 'post')$VORP)

reg96 <- filter(pippin, year == 2016, time == 'reg')$VORP - mean(filter(others, year == 2016, time == 'reg')$VORP)
post96 <- filter(pippin, year == 2016, time == 'post')$VORP - mean(filter(others, year == 2016, time == 'post')$VORP)

reg97 <- filter(pippin, year == 2017, time == 'reg')$VORP - mean(filter(others, year == 2017, time == 'reg')$VORP)
post97 <- filter(pippin, year == 2017, time == 'post')$VORP - mean(filter(others, year == 2017, time == 'post')$VORP)

reg98 <- filter(pippin, year == 2018, time == 'reg')$VORP - mean(filter(others, year == 2018, time == 'reg')$VORP)
post98 <- filter(pippin, year == 2018, time == 'post')$VORP - mean(filter(others, year == 2018, time == 'post')$VORP)

reg <- rbind(reg90,reg91,reg92,reg93,reg94,reg95,reg96,reg97,reg98)
post <- rbind(post90,post91,post92,post93,post94,post95,post96,post97,post98)

reg <- as.numeric(reg)
post <- as.numeric(post)

write.csv(reg,'lebron_reg.csv')
write.csv(post,'lebron_post.csv')

### Graphing the data ###

dens_reg <- density(reg, adjust = 2)
plot(dens_reg, xlab = expression('Game VORP - Team Mean Regular Season Adjusted VORP'), ylab = 'Density', main = '', ylim = c(0,0.07))

dens_post <- density(post, adjust = 2)
lines(dens_post, col = 'maroon')
legend('topright', legend = c('reg', 'post'), col = c('black', 'maroon'), lty = 1)
title(main = 'LeBron James')




test1 <- c(post[1,], post[2,], post[3,], post[4,], post[5,], post[6,], post[7,],post[8,], post[9,])

test2 <- c(reg[1,], reg[2,], reg[3,], reg[4,], reg[5,], reg[6,], reg[7,],reg[8,],post[9,])

t1 <- data.frame(VORP = test1, time = 'post')
t2 <- data.frame(VORP = test2, time = 'reg')

test <- data.frame()

test <- rbind(test,t1,t2)

write.csv(test,'lebron_t.csv')


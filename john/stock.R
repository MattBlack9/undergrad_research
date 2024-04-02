library(tidyverse)
library(ggplot2)

###########################
### Loading in the data ###
###########################

### Setting the directory of the data ####

setwd('/Users/matty/Documents/BYU/undergrad_research/john')

### Loading in the data ###

johnnyboy <- read.csv('stock_test.csv')

#########################################
### Cleaning the data and graphing it ###
#########################################

### Cleaning the data ###

temp <- johnnyboy$player == 'stockjo01'
johnnyboy$correct <- temp
stock <- filter(johnnyboy, correct == T)
others <- filter(johnnyboy, correct == F)

reg85 <- filter(stock, year == 1985, time == 'reg')$VORP - mean(filter(others, year == 1985, time == 'reg')$VORP)
post85 <- filter(stock, year == 1985, time == 'post')$VORP - mean(filter(others, year == 1985, time == 'post')$VORP)

reg86 <- filter(stock, year == 1986, time == 'reg')$VORP - mean(filter(others, year == 1986, time == 'reg')$VORP)
post86 <- filter(stock, year == 1986, time == 'post')$VORP - mean(filter(others, year == 1986, time == 'post')$VORP)

reg87 <- filter(stock, year == 1987, time == 'reg')$VORP - mean(filter(others, year == 1987, time == 'reg')$VORP)
post87 <- filter(stock, year == 1987, time == 'post')$VORP - mean(filter(others, year == 1987, time == 'post')$VORP)

reg88 <- filter(stock, year == 1988, time == 'reg')$VORP - mean(filter(others, year == 1988, time == 'reg')$VORP)
post88 <- filter(stock, year == 1988, time == 'post')$VORP - mean(filter(others, year == 1988, time == 'post')$VORP)

reg89 <- filter(stock, year == 1989, time == 'reg')$VORP - mean(filter(others, year == 1989, time == 'reg')$VORP)
post89 <- filter(stock, year == 1989, time == 'post')$VORP - mean(filter(others, year == 1989, time == 'post')$VORP)

reg90 <- filter(stock, year == 1990, time == 'reg')$VORP - mean(filter(others, year == 1990, time == 'reg')$VORP)
post90 <- filter(stock, year == 1990, time == 'post')$VORP - mean(filter(others, year == 1990, time == 'post')$VORP)

reg91 <- filter(stock, year == 1991, time == 'reg')$VORP - mean(filter(others, year == 1991, time == 'reg')$VORP)
post91 <- filter(stock, year == 1991, time == 'post')$VORP - mean(filter(others, year == 1991, time == 'post')$VORP)

reg92 <- filter(stock, year == 1992, time == 'reg')$VORP - mean(filter(others, year == 1992, time == 'reg')$VORP)
post92 <- filter(stock, year == 1992, time == 'post')$VORP - mean(filter(others, year == 1992, time == 'post')$VORP)

reg93 <- filter(stock, year == 1993, time == 'reg')$VORP - mean(filter(others, year == 1993, time == 'reg')$VORP)
post93 <- filter(stock, year == 1993, time == 'post')$VORP - mean(filter(others, year == 1993, time == 'post')$VORP)

reg94 <- filter(stock, year == 1994, time == 'reg')$VORP - mean(filter(others, year == 1994, time == 'reg')$VORP)
post94 <- filter(stock, year == 1994, time == 'post')$VORP - mean(filter(others, year == 1994, time == 'post')$VORP)

reg95 <- filter(stock, year == 1995, time == 'reg')$VORP - mean(filter(others, year == 1995, time == 'reg')$VORP)
post95 <- filter(stock, year == 1995, time == 'post')$VORP - mean(filter(others, year == 1995, time == 'post')$VORP)

reg96 <- filter(stock, year == 1996, time == 'reg')$VORP - mean(filter(others, year == 1996, time == 'reg')$VORP)
post96 <- filter(stock, year == 1996, time == 'post')$VORP - mean(filter(others, year == 1996, time == 'post')$VORP)

reg97 <- filter(stock, year == 1997, time == 'reg')$VORP - mean(filter(others, year == 1997, time == 'reg')$VORP)
post97 <- filter(stock, year == 1997, time == 'post')$VORP - mean(filter(others, year == 1997, time == 'post')$VORP)

reg98 <- filter(stock, year == 1998, time == 'reg')$VORP - mean(filter(others, year == 1998, time == 'reg')$VORP)
post98 <- filter(stock, year == 1998, time == 'post')$VORP - mean(filter(others, year == 1998, time == 'post')$VORP)

reg99 <- filter(stock, year == 1999, time == 'reg')$VORP - mean(filter(others, year == 1999, time == 'reg')$VORP)
post99 <- filter(stock, year == 1999, time == 'post')$VORP - mean(filter(others, year == 1999, time == 'post')$VORP)

reg00 <- filter(stock, year == 2000, time == 'reg')$VORP - mean(filter(others, year == 2000, time == 'reg')$VORP)
post00 <- filter(stock, year == 2000, time == 'post')$VORP - mean(filter(others, year == 2000, time == 'post')$VORP)

reg01 <- filter(stock, year == 2001, time == 'reg')$VORP - mean(filter(others, year == 2001, time == 'reg')$VORP)
post01 <- filter(stock, year == 2001, time == 'post')$VORP - mean(filter(others, year == 2001, time == 'post')$VORP)

reg02 <- filter(stock, year == 2002, time == 'reg')$VORP - mean(filter(others, year == 2002, time == 'reg')$VORP)
post02 <- filter(stock, year == 2002, time == 'post')$VORP - mean(filter(others, year == 2002, time == 'post')$VORP)

reg03 <- filter(stock, year == 2003, time == 'reg')$VORP - mean(filter(others, year == 2003, time == 'reg')$VORP)
post03 <- filter(stock, year == 2003, time == 'post')$VORP - mean(filter(others, year == 2003, time == 'post')$VORP)



reg <- rbind(reg85,reg86, reg87, reg88, reg89,reg90,reg91,reg92,reg93,reg94,reg95,reg96,reg97,reg98,reg99,reg00,reg01,reg02,reg03)
post <- rbind(post85,post86,post87,post88,post89,post90,post91,post92,post93,post94,post95,post96,post97,post98,post99,post00,post01,post02,post03)

reg <- as.numeric(reg)
post <- as.numeric(post)

write.csv(reg,'stock_reg.csv')
write.csv(post,'stock_post.csv')

### Graphing the data ###

dens_reg <- density(reg, adjust = 2)
plot(dens_reg, xlab = expression('Game VORP - Team Mean Regular Season Adjusted VORP'), ylab = 'Density', main = '', ylim = c(0,0.07))

dens_post <- density(post, adjust = 2)
lines(dens_post, col = 'purple')
legend('topright', legend = c('reg', 'post'), col = c('black', 'purple'), lty = 1)
title(main = 'John Stockton')




test1 <- c(post[1,], post[2,], post[3,], post[4,], post[5,], post[6,], post[7,],post[8,],post[9,],post[10,],post[11,],post[12,],post[13,],post[14,],post[15,],post[16,],post[17,],post[18,])

test2 <- c(reg[1,], reg[2,], reg[3,], reg[4,], reg[5,], reg[6,], reg[7,],reg[8,],reg[9,],reg[10,],reg[11,],reg[12,],reg[13,],reg[14,],reg[15,],reg[16,],reg[17,],reg[18,])

t1 <- data.frame(VORP = test1, time = 'post')
t2 <- data.frame(VORP = test2, time = 'reg')

test <- data.frame()

test <- rbind(test,t1,t2)

write.csv(test,'stock_t.csv')
















# Do the same thing for the players, but use the whole league's mean for the VORP























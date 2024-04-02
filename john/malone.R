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

temp <- johnnyboy$player == 'malonka01'
johnnyboy$correct <- temp
malone <- filter(johnnyboy, correct == T)
others <- filter(johnnyboy, correct == F)

reg86 <- filter(malone, year == 1986, time == 'reg')$VORP - mean(filter(others, year == 1986, time == 'reg')$VORP)
post86 <- filter(malone, year == 1986, time == 'post')$VORP - mean(filter(others, year == 1986, time == 'post')$VORP)

reg87 <- filter(malone, year == 1987, time == 'reg')$VORP - mean(filter(others, year == 1987, time == 'reg')$VORP)
post87 <- filter(malone, year == 1987, time == 'post')$VORP - mean(filter(others, year == 1987, time == 'post')$VORP)

reg88 <- filter(malone, year == 1988, time == 'reg')$VORP - mean(filter(others, year == 1988, time == 'reg')$VORP)
post88 <- filter(malone, year == 1988, time == 'post')$VORP - mean(filter(others, year == 1988, time == 'post')$VORP)

reg89 <- filter(malone, year == 1989, time == 'reg')$VORP - mean(filter(others, year == 1989, time == 'reg')$VORP)
post89 <- filter(malone, year == 1989, time == 'post')$VORP - mean(filter(others, year == 1989, time == 'post')$VORP)

reg90 <- filter(malone, year == 1990, time == 'reg')$VORP - mean(filter(others, year == 1990, time == 'reg')$VORP)
post90 <- filter(malone, year == 1990, time == 'post')$VORP - mean(filter(others, year == 1990, time == 'post')$VORP)

reg91 <- filter(malone, year == 1991, time == 'reg')$VORP - mean(filter(others, year == 1991, time == 'reg')$VORP)
post91 <- filter(malone, year == 1991, time == 'post')$VORP - mean(filter(others, year == 1991, time == 'post')$VORP)

reg92 <- filter(malone, year == 1992, time == 'reg')$VORP - mean(filter(others, year == 1992, time == 'reg')$VORP)
post92 <- filter(malone, year == 1992, time == 'post')$VORP - mean(filter(others, year == 1992, time == 'post')$VORP)

reg93 <- filter(malone, year == 1993, time == 'reg')$VORP - mean(filter(others, year == 1993, time == 'reg')$VORP)
post93 <- filter(malone, year == 1993, time == 'post')$VORP - mean(filter(others, year == 1993, time == 'post')$VORP)

reg94 <- filter(malone, year == 1994, time == 'reg')$VORP - mean(filter(others, year == 1994, time == 'reg')$VORP)
post94 <- filter(malone, year == 1994, time == 'post')$VORP - mean(filter(others, year == 1994, time == 'post')$VORP)

reg95 <- filter(malone, year == 1995, time == 'reg')$VORP - mean(filter(others, year == 1995, time == 'reg')$VORP)
post95 <- filter(malone, year == 1995, time == 'post')$VORP - mean(filter(others, year == 1995, time == 'post')$VORP)

reg96 <- filter(malone, year == 1996, time == 'reg')$VORP - mean(filter(others, year == 1996, time == 'reg')$VORP)
post96 <- filter(malone, year == 1996, time == 'post')$VORP - mean(filter(others, year == 1996, time == 'post')$VORP)

reg97 <- filter(malone, year == 1997, time == 'reg')$VORP - mean(filter(others, year == 1997, time == 'reg')$VORP)
post97 <- filter(malone, year == 1997, time == 'post')$VORP - mean(filter(others, year == 1997, time == 'post')$VORP)

reg98 <- filter(malone, year == 1998, time == 'reg')$VORP - mean(filter(others, year == 1998, time == 'reg')$VORP)
post98 <- filter(malone, year == 1998, time == 'post')$VORP - mean(filter(others, year == 1998, time == 'post')$VORP)

reg99 <- filter(malone, year == 1999, time == 'reg')$VORP - mean(filter(others, year == 1999, time == 'reg')$VORP)
post99 <- filter(malone, year == 1999, time == 'post')$VORP - mean(filter(others, year == 1999, time == 'post')$VORP)

reg00 <- filter(malone, year == 2000, time == 'reg')$VORP - mean(filter(others, year == 2000, time == 'reg')$VORP)
post00 <- filter(malone, year == 2000, time == 'post')$VORP - mean(filter(others, year == 2000, time == 'post')$VORP)

reg01 <- filter(malone, year == 2001, time == 'reg')$VORP - mean(filter(others, year == 2001, time == 'reg')$VORP)
post01 <- filter(malone, year == 2001, time == 'post')$VORP - mean(filter(others, year == 2001, time == 'post')$VORP)

reg02 <- filter(malone, year == 2002, time == 'reg')$VORP - mean(filter(others, year == 2002, time == 'reg')$VORP)
post02 <- filter(malone, year == 2002, time == 'post')$VORP - mean(filter(others, year == 2002, time == 'post')$VORP)

reg03 <- filter(malone, year == 2003, time == 'reg')$VORP - mean(filter(others, year == 2003, time == 'reg')$VORP)
post03 <- filter(malone, year == 2003, time == 'post')$VORP - mean(filter(others, year == 2003, time == 'post')$VORP)



reg <- rbind(reg86, reg87, reg88, reg89,reg90,reg91,reg92,reg93,reg94,reg95,reg96,reg97,reg98,reg99,reg00,reg01,reg02,reg03)
post <- rbind(post86,post87,post88,post89,post90,post91,post92,post93,post94,post95,post96,post97,post98,post99,post00,post01,post02,post03)

reg <- as.numeric(reg)
post <- as.numeric(post)

write.csv(reg,'malone_reg.csv')
write.csv(post,'malone_post.csv')

### Graphing the data ###

dens_reg <- density(reg, adjust = 2)
plot(dens_reg, xlab = expression('Game VORP - Team Mean Regular Season Adjusted VORP'), ylab = 'Density', main = '', ylim = c(0,0.07))

dens_post <- density(post, adjust = 2)
lines(dens_post, col = 'purple')
legend('topright', legend = c('reg', 'post'), col = c('black', 'purple'), lty = 1)
title(main = "Karl Malone")




test1 <- c(post[1,], post[2,], post[3,], post[4,], post[5,], post[6,], post[7,],post[8,],post[9,],post[10,],post[11,],post[12,],post[13,],post[14,],post[15,],post[16,],post[17,])

test2 <- c(reg[1,], reg[2,], reg[3,], reg[4,], reg[5,], reg[6,], reg[7,],reg[8,],reg[9,],reg[10,],reg[11,],reg[12,],reg[13,],reg[14,],reg[15,],reg[16,],reg[17,])

t1 <- data.frame(VORP = test1, time = 'post')
t2 <- data.frame(VORP = test2, time = 'reg')

test <- data.frame()

test <- rbind(test,t1,t2)

write.csv(test,'malone_t.csv')


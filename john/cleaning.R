library(tidyverse)


###################################
### Setting the right directory ###
###################################

setwd('/Users/matty/Documents/BYU/undergrad_research/john/')

###########################
### Loading in the Data ###
###########################

# Regular Season

john.reg85 <- read.csv('john.reg85.csv')
john.reg86 <- read.csv('john.reg86.csv')
john.reg87 <- read.csv('john.reg87.csv')
john.reg88 <- read.csv('john.reg88.csv')
john.reg89 <- read.csv('john.reg89.csv')
john.reg90 <- read.csv('john.reg90.csv')
john.reg91 <- read.csv('john.reg91.csv')
john.reg92 <- read.csv('john.reg92.csv')
john.reg93 <- read.csv('john.reg93.csv')
john.reg94 <- read.csv('john.reg94.csv')
john.reg95 <- read.csv('john.reg95.csv')
john.reg96 <- read.csv('john.reg96.csv')
john.reg97 <- read.csv('john.reg97.csv')
john.reg98 <- read.csv('john.reg98.csv')
john.reg99 <- read.csv('john.reg99.csv')
john.reg00 <- read.csv('john.reg00.csv')
john.reg01 <- read.csv('john.reg01.csv')
john.reg02 <- read.csv('john.reg02.csv')
john.reg03 <- read.csv('john.reg03.csv')

# Post Season

john.post85 <- read.csv('john.post85.csv')
john.post86 <- read.csv('john.post86.csv')
john.post87 <- read.csv('john.post87.csv')
john.post88 <- read.csv('john.post88.csv')
john.post89 <- read.csv('john.post89.csv')
john.post90 <- read.csv('john.post90.csv')
john.post91 <- read.csv('john.post91.csv')
john.post92 <- read.csv('john.post92.csv')
john.post93 <- read.csv('john.post93.csv')
john.post94 <- read.csv('john.post94.csv')
john.post95 <- read.csv('john.post95.csv')
john.post96 <- read.csv('john.post96.csv')
john.post97 <- read.csv('john.post97.csv')
john.post98 <- read.csv('john.post98.csv')
john.post99 <- read.csv('john.post99.csv')
john.post00 <- read.csv('john.post00.csv')
john.post01 <- read.csv('john.post01.csv')
john.post02 <- read.csv('john.post02.csv')
john.post03 <- read.csv('john.post03.csv')


##############################
### Creating my own data frame ###
##############################

johnnyboy <- data.frame()

johnnyboy.regular <- rbind(johnnyboy,john.reg85,john.reg86,john.reg87,
                           john.reg88,john.reg89,john.reg90,
                           john.reg91,john.reg92,john.reg93,
                           john.reg94,john.reg95,john.reg96,
                           john.reg97,john.reg98,john.reg99,
                           john.reg00,john.reg02,john.reg03)

johnnyboy.post <- rbind(johnnyboy,john.post85,john.post86,john.post87,
                        john.post88,john.post89,john.post90,
                        john.post91,john.post92,john.post93,
                        john.post94,john.post95,john.post96,
                        john.post97,john.post98,john.post99,
                        john.post00,john.post02,john.post03)

johnnyboy.regular[,12] <- 'reg'

johnnyboy.post[,12] <- 'post'

johnnyboy <- rbind(johnnyboy,johnnyboy.regular,johnnyboy.post)

johnnyboy$team <- str_detect(johnnyboy$season_id, "^UTA")
johnnyboy <- filter(johnnyboy, team == T)

temp1 <- johnnyboy$player
temp2 <- johnnyboy$VORP
temp3 <- johnnyboy$V12
temp4 <- johnnyboy$year

career <- data.frame(player = temp1, VORP = temp2, time = temp3, year = temp4)



###############################
### Saving my data as a csv ###
###############################

write.csv(career,'stock_test.csv')







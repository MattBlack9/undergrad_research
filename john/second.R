library(tidyverse)


###################################
### Setting the right directory ###
###################################

setwd('/Users/matty/Documents/BYU/undergrad_research/server/')

###########################
### Loading in the Data ###
###########################

# Regular Season

bulls.post90 <- read.csv('bulls.post90.csv')       
bulls.post91 <- read.csv('bulls.post91.csv')       
bulls.post92 <- read.csv('bulls.post92.csv')      
bulls.post93 <- read.csv('bulls.post93.csv')       
bulls.post94 <- read.csv('bulls.post94.csv')     
bulls.post95 <- read.csv('bulls.post95.csv')      
bulls.post96 <- read.csv('bulls.post96.csv')       
bulls.post97 <- read.csv('bulls.post97.csv')        
bulls.post98 <- read.csv('bulls.post98.csv')

bulls.reg90 <- read.csv('bulls.reg90.csv')        
bulls.reg91 <- read.csv('bulls.reg91.csv')        
bulls.reg92 <- read.csv('bulls.reg92.csv')        
bulls.reg93 <- read.csv('bulls.reg93.csv')         
bulls.reg94 <- read.csv('bulls.reg94.csv')    
bulls.reg95 <- read.csv('bulls.reg95.csv')        
bulls.reg96 <- read.csv('bulls.reg96.csv')         
bulls.reg97 <- read.csv('bulls.reg97.csv')         
bulls.reg98 <- read.csv('bulls.reg98.csv')


##############################
### Creating my own data frame ###
##############################

michael <- data.frame()

michael.regular <- rbind(michael,bulls.reg90,
                          bulls.reg91,bulls.reg92,bulls.reg93,
                          bulls.reg94,bulls.reg95,bulls.reg96,
                          bulls.reg97,bulls.reg98)

michael.post <- rbind(michael,bulls.post90,
                      bulls.post91,bulls.post92,bulls.post93,
                      bulls.post94,bulls.post95,bulls.post96,
                      bulls.post97,bulls.post98)

michael.regular[,12] <- 'reg'

michael.post[,12] <- 'post'

michael <- rbind(michael,michael.regular,michael.post)

michael$team <- str_detect(michael$season_id, "^CHI")
michael <- filter(michael, team == T)

temp1 <- michael$player
temp2 <- michael$VORP
temp3 <- michael$V12
temp4 <- michael$year

career <- data.frame(player = temp1, VORP = temp2, time = temp3, year = temp4)



###############################
### Saving my data as a csv ###
###############################

setwd('/Users/matty/Documents/BYU/undergrad_research/john/')

write.csv(career,'jordan_test.csv')

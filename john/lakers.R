library(tidyverse)


###################################
### Setting the right directory ###
###################################

setwd('/Users/matty/Documents/BYU/undergrad_research/server/')

###########################
### Loading in the Data ###
###########################

# Regular Season

lakers.post00 <- read.csv('lakers.post00.csv')      
lakers.post01 <- read.csv('lakers.post01.csv')      
lakers.post02 <- read.csv('lakers.post02.csv')     
lakers.post03 <- read.csv('lakers.post03.csv')      
lakers.post04 <- read.csv('lakers.post04.csv')    
lakers.post06 <- read.csv('lakers.post06.csv')      
lakers.post07 <- read.csv('lakers.post07.csv')       
lakers.post08 <- read.csv('lakers.post08.csv')
lakers.post09 <- read.csv('lakers.post09.csv')

lakers.reg00 <- read.csv('lakers.reg00.csv')      
lakers.reg01 <- read.csv('lakers.reg01.csv')      
lakers.reg02 <- read.csv('lakers.reg02.csv')     
lakers.reg03 <- read.csv('lakers.reg03.csv')      
lakers.reg04 <- read.csv('lakers.reg04.csv')    
lakers.reg06 <- read.csv('lakers.reg06.csv')      
lakers.reg07 <- read.csv('lakers.reg07.csv')       
lakers.reg08 <- read.csv('lakers.reg08.csv')
lakers.reg09 <- read.csv('lakers.reg09.csv')


##############################
### Creating my own data frame ###
##############################

michael <- data.frame()

michael.regular <- rbind(michael,lakers.reg00,
                         lakers.reg01,lakers.reg02,lakers.reg03,
                         lakers.reg04,lakers.reg06,lakers.reg07,
                         lakers.reg08,lakers.reg09)

michael.post <- rbind(michael,lakers.post00,
                      lakers.post01,lakers.post02,lakers.post03,
                      lakers.post04,lakers.post06,lakers.post07,
                      lakers.post08,lakers.post09)

michael.regular[,12] <- 'reg'

michael.post[,12] <- 'post'

michael <- rbind(michael,michael.regular,michael.post)

michael$team <- str_detect(michael$season_id, "^LAL")
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

write.csv(career,'kobe_test.csv')

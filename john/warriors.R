library(tidyverse)


###################################
### Setting the right directory ###
###################################

setwd('/Users/matty/Documents/BYU/undergrad_research/server/')

###########################
### Loading in the Data ###
###########################

# Regular Season

warriors.post13 <- read.csv('warriors.post13.csv')      
warriors.post15 <- read.csv('warriors.post15.csv')      
warriors.post16 <- read.csv('warriors.post16.csv')     
warriors.post17 <- read.csv('warriors.post17.csv')      
warriors.post18 <- read.csv('warriors.post18.csv')    


warriors.reg13 <- read.csv('warriors.reg13.csv')      
warriors.reg15 <- read.csv('warriors.reg15.csv')      
warriors.reg16 <- read.csv('warriors.reg16.csv')     
warriors.reg17 <- read.csv('warriors.reg17.csv')      
warriors.reg18 <- read.csv('warriors.reg18.csv')  


##############################
### Creating my own data frame ###
##############################

michael <- data.frame()

michael.regular <- rbind(michael,warriors.reg13,
                         warriors.reg15,warriors.reg16,warriors.reg17,
                         warriors.reg18)


michael.post <- rbind(michael,warriors.post13,
                      warriors.post15,warriors.post16,warriors.post17,
                      warriors.post18)

michael.regular[,12] <- 'reg'

michael.post[,12] <- 'post'

michael <- rbind(michael,michael.regular,michael.post)

michael$team <- str_detect(michael$season_id, "^GSW")
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

write.csv(career,'curry_test.csv')


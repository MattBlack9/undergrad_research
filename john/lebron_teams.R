library(tidyverse)


###################################
### Setting the right directory ###
###################################

setwd('/Users/matty/Documents/BYU/undergrad_research/server/')

###########################
### Loading in the Data ###
###########################

# Regular Season

caveliers.post10 <- read.csv('caveliers.post10.csv')      
caveliers.post15 <- read.csv('caveliers.post15.csv')
caveliers.post16 <- read.csv('caveliers.post16.csv')
caveliers.post17 <- read.csv('caveliers.post17.csv')
caveliers.post18 <- read.csv('caveliers.post18.csv')
heat.post11 <- read.csv('heat.post11.csv')
heat.post12 <- read.csv('heat.post12.csv')
heat.post13 <- read.csv('heat.post13.csv')
heat.post14 <- read.csv('heat.post14.csv')

caveliers.reg10 <- read.csv('caveliers.reg10.csv')      
caveliers.reg15 <- read.csv('caveliers.reg15.csv')
caveliers.reg16 <- read.csv('caveliers.reg16.csv')
caveliers.reg17 <- read.csv('caveliers.reg17.csv')
caveliers.reg18 <- read.csv('caveliers.reg18.csv')
heat.reg11 <- read.csv('heat.reg11.csv')
heat.reg12 <- read.csv('heat.reg12.csv')
heat.reg13 <- read.csv('heat.reg13.csv')
heat.reg14 <- read.csv('heat.reg14.csv')


##############################
### Creating my own data frame ###
##############################

michael <- data.frame()
jordan <- data.frame()

michael.regular <- rbind(michael,heat.reg11,heat.reg12,
                         heat.reg13,heat.reg14)

michael.post <- rbind(michael,heat.post11,heat.post12,
                      heat.post13,heat.post14)

lebron.regular <- rbind(michael,caveliers.reg10,
                        caveliers.reg15,caveliers.reg16,caveliers.reg17,
                        caveliers.reg18)

lebron.post <- rbind(michael,caveliers.post10,
                     caveliers.post15,caveliers.post16,caveliers.post17,
                     caveliers.post18)


michael.regular[,12] <- 'reg'

michael.post[,12] <- 'post'

michael <- rbind(michael,michael.regular,michael.post)

michael$team <- str_detect(michael$season_id, "^MIA")
michael <- filter(michael, team == T)

lebron.regular[,12] <- 'reg'

lebron.post[,12] <- 'post'

jordan <- rbind(jordan,lebron.regular,lebron.post)

jordan$team <- str_detect(jordan$season_id, "^CLE")
jordan <- filter(jordan, team == T)


temp1 <- michael$player
temp2 <- michael$VORP
temp3 <- michael$V12
temp4 <- michael$year

career <- data.frame(player = temp1, VORP = temp2, time = temp3, year = temp4)

temp5 <- jordan$player
temp6 <- jordan$VORP
temp7 <- jordan$V12
temp8 <- jordan$year

career2 <- data.frame(player = temp5, VORP = temp6, time = temp7, year = temp8)



###############################
### Saving my data as a csv ###
###############################

setwd('/Users/matty/Documents/BYU/undergrad_research/john/')

write.csv(career,'lebron_heat_test.csv')
write.csv(career2,'lebron_cav_test.csv')


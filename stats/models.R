library(tidyverse)
library(ggplot2)

###########################
### Loading in the data ###
###########################

### Setting the directory of the data ####

setwd('/Users/matty/Documents/BYU/undergrad_research/merging')

### Loading in the data ###

bulls <- read.csv('bulls.csv')
celtics <- read.csv('celtics.csv')
jazz <- read.csv('jazz.csv')
rockets <- read.csv('rockets.csv')
warriors <- read.csv('warriors.csv')
caveliers <- read.csv('caveliers.csv')
heat <- read.csv('heat.csv')
lakers <- read.csv('lakers.csv')
pistons <- read.csv('pistons.csv')
spurs <- read.csv('spurs.csv')

### Setting directory of the file ###

setwd('/Users/matty/Documents/BYU/undergrad_research/stats')


#############################
### Creating My Functions ###
#############################

### This function plots a scatter-plot and line ###

scatt <- function(data,years=NULL){
  if(is.null(years) == TRUE){
    ggplot(data)+
      geom_point(aes(x=RVORP,y=PVORP,color=win)) +
      scale_color_manual(name='Champions',values = c('black','#F0E442'),labels=c('No','Yes')) +
      geom_abline(intercept = 0,slope=1, color='red')+ 
      labs(title='Team Performance',x = 'Regular Season VORP',y='Post Season VORP',subtitle = 'Performance with y=x line')+
      xlim(-5,15)+
      ylim(-5,15)
  }
  else{
    temp <- filter(data, year == years)
    if(sum(temp$win) > 0){
      colors <- '#F0E442'
      champ <- 'Yes'
    }
    else{
      colors <- 'black'
      champ <- 'No'
      }
    ggplot(temp)+
      geom_point(aes(x=RVORP,y=PVORP,color=win)) +
      scale_color_manual(name='Champions',values =colors,labels=champ) +
      geom_abline(intercept = 0,slope=1, color='red') + 
      labs(title='Team Performance',x = 'Regular Season VORP',y='Post Season VORP',subtitle = 'Performance with y=x line')+
      xlim(-5,15)+
      ylim(-5,15)
  }
}


### This function will plot a player over years ###

player <- function(play, team){
  temp <- filter(team, player == play)
  minyear <- min(temp$year)
  ggplot(temp) +
    geom_point(aes(x = year, y= RVORP,color = 'Regular Season')) +
    geom_point(aes(x = year, y= PVORP,color = 'Post Season')) +
    geom_line(aes(x = year,y=RVORP,color = 'Regular Season')) +
    geom_line(aes(x = year,y=PVORP,color = 'Post Season')) +
    labs(title='Player Performance',x = 'Year',y='VORP',subtitle = 'Regular Season VORP vs Post Season VORP')+
    scale_color_manual(name='Time',values = c('#0072B2','#D55E00'))+
    scale_x_continuous(breaks = seq(min(temp$year), max(temp$year), by = 1))+
    ylim(-5,15)
}


### This will compare players through multiple years ###

reg <- function(player1,player2,team1,team2,player3=NULL,team3=NULL,player4=NULL,team4=NULL){
  if(is.null(player3) == TRUE){
    temp1 <- filter(team1,player==player1)
    temp2 <- filter(team2,player==player2)
    final1 <- temp1$PVORP - temp1$RVORP
    final2 <- temp2$PVORP - temp2$RVORP
    temp1 <- cbind(temp1,final1)
    temp2 <- cbind(temp2,final2)
    columns <- c('NA','player','year','RVORP','PVORP','win','real')
    names(temp1) <- columns
    names(temp2) <- columns
    temp1$xax <- seq_along(temp1[, 1])
    temp2$xax <- seq_along(temp2[, 1])
    
    df <- data.frame()
    df <- rbind(df,temp1,temp2)
    ggplot(df)+
      geom_point(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_point(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      geom_line(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_line(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      labs(x='Years made the Playoffs',y='Regular Season VORP')+
      scale_color_manual(name='Players',values = c('#0072B2','#D55E00'),labels=c(player1,player2))+
      scale_x_continuous(breaks = seq(1, 10, by = 1))+
      ylim(-5,15)
  }
  else if(is.null(player3) == FALSE & is.null(player4) == TRUE){
    temp1 <- filter(team1,player==player1)
    temp2 <- filter(team2,player==player2)
    temp3 <- filter(team3,player==player3)
    final1 <- temp1$PVORP - temp1$RVORP
    final2 <- temp2$PVORP - temp2$RVORP
    final3 <- temp3$PVORP - temp3$RVORP
    temp1 <- cbind(temp1,final1)
    temp2 <- cbind(temp2,final2)
    temp3 <- cbind(temp3,final3)
    columns <- c('NA','player','year','RVORP','PVORP','win','real')
    names(temp1) <- columns
    names(temp2) <- columns
    names(temp3) <- columns
    temp1$xax <- seq_along(temp1[, 1])
    temp2$xax <- seq_along(temp2[, 1])
    temp3$xax <- seq_along(temp3[,1])
    
    df <- data.frame()
    df <- rbind(df,temp1,temp2,temp3)
    ggplot(df)+
      geom_point(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_point(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      geom_line(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_line(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      geom_point(aes(x = xax, y= RVORP,color = player3), data = filter(df,player==player3)) +
      geom_line(aes(x = xax, y= RVORP, color = player3),data = filter(df,player==player3)) +
      labs(x='Years made the Playoffs',y='Regular Season VORP')+
      scale_color_manual(name='Players',values = c('#0072B2','#D55E00','#CC79A7'),labels=c(player1,player2,player3))+
      scale_x_continuous(breaks = seq(1, 10, by = 1))+
      ylim(-5,15)
  }
  else{
    temp1 <- filter(team1,player==player1)
    temp2 <- filter(team2,player==player2)
    temp3 <- filter(team3,player==player3)
    temp4 <- filter(team4,player==player4)
    final1 <- temp1$PVORP - temp1$RVORP
    final2 <- temp2$PVORP - temp2$RVORP
    final3 <- temp3$PVORP - temp3$RVORP
    final4 <- temp4$PVORP - temp4$RVORP
    temp1 <- cbind(temp1,final1)
    temp2 <- cbind(temp2,final2)
    temp3 <- cbind(temp3,final3)
    temp4 <- cbind(temp4,final4)
    columns <- c('NA','player','year','RVORP','PVORP','win','real')
    names(temp1) <- columns
    names(temp2) <- columns
    names(temp3) <- columns
    names(temp4) <- columns
    temp1$xax <- seq_along(temp1[, 1])
    temp2$xax <- seq_along(temp2[, 1])
    temp3$xax <- seq_along(temp3[,1])
    temp4$xax <- seq_along(temp4[,1])
    
    df <- data.frame()
    df <- rbind(df,temp1,temp2,temp3,temp4)
    ggplot(df)+
      geom_point(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_point(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      geom_line(aes(x = xax, y= RVORP,color = player1), data = filter(df,player==player1)) +
      geom_line(aes(x = xax, y= RVORP, color = player2),data = filter(df,player==player2)) +
      geom_point(aes(x = xax, y= RVORP,color = player3), data = filter(df,player==player3)) +
      geom_line(aes(x = xax, y= RVORP, color = player3),data = filter(df,player==player3)) +
      geom_point(aes(x = xax, y= RVORP,color = player4), data = filter(df,player==player4)) +
      geom_line(aes(x = xax, y= RVORP, color = player4),data = filter(df,player==player4)) +
      labs(x='Years made the Playoffs',y='Regular Season VORP')+
      scale_color_manual(name='Players',values = c('#0072B2','#D55E00','#CC79A7','#009E73'),labels=c(player4,player1,player2,player3))+
      scale_x_continuous(breaks = seq(1, 10, by = 1))+
      ylim(-5,15)
  }
}

reg('curryst01','malonka01',warriors,jazz)
reg('curryst01','malonka01',warriors,jazz,'thompkl01',warriors)
reg('curryst01','malonka01',warriors,jazz,'thompkl01',warriors,'bryanko01',lakers)


# If I have time, I will also make sense of all of the weird strings used for players.
# Lastly, I will create plots of all of the important players from every team in
# each decade.

############################
### Creating My Analyses ###
############################

player('stockjo01',jazz)
player('malonka01',jazz)

player('curryst01',warriors)
player('thompkl01',warriors)

player('jordami01',bulls)
player('pippesc01',bulls)

player('jamesle01',caveliers)
player('irvinky01',caveliers)

player('abdulka01',lakers)
player('johnsma02',lakers)

player('bryanko01',lakers)
player('onealsh01',lakers)

player('jamesle01',heat)
player('wadedw01',heat)

player('birdla01',celtics)






library(tidyverse)
library(ggplot2)

setwd('/Users/matty/Documents/BYU/STAT_RA')

playoffstats <- read.csv('playoffstats.csv')
regularstats <- read.csv('regularstats.csv')

for(i in 1:nrow(regularstats)){
  if(regularstats$player[i] %in% playoffstats$player){
    regularstats$playslater <- TRUE
  }
  else{
    regularstats$playslater <- FALSE
  }
}

playoff_players <- filter(regularstats, playslater == T)
unique <- unique(playoff_players$player)

regular_performance <- data.frame(player = unique, VORP = 0)
for(i in 1:length(unique)){
  temp <- filter(playoff_players, player == unique[i])
  performace <- mean(temp$VORP)
  for(j in 1:length(unique)){
    if(unique[i] == regular_performance$player[j]){
      regular_performance$VORP[j] <- performace
    }
  }
}

post_performance <- data.frame(player = unique, VORP = 0)
for(i in 1:length(unique)){
  temp <- filter(playoffstats, player == unique[i])
  performace <- mean(temp$VORP)
  for(j in 1:length(unique)){
    if(unique[i] == post_performance$player[j]){
      post_performance$VORP[j] <- performace
    }
  }
}

write_csv(post_performance, 'post_performance.csv')
write_csv(regular_performance, 'regular_performance.csv')

post_performance <- read.csv('post_performance.csv')
regular_performance <- read.csv('regular_performance.csv')

my_tibble <- data.frame(player = regular_performance$player, reg = regular_performance$VORP, 
                        post = post_performance$VORP)

my_tibble <- as.tibble(my_tibble)

my_tibble$calc <- my_tibble$post - my_tibble$reg



hist(my_tibble$calc,main='Post Season VORP - Regular Season VORP',xlab='VORP')


plot(my_tibble$reg,my_tibble$post)

my_tibble
drop_na(my_tibble)




summary(lm(my_tibble$post~my_tibble$reg))

summary(lm(my_tibble$reg~my_tibble$post))


mean(my_tibble$reg<my_tibble$post,na.rm = T)




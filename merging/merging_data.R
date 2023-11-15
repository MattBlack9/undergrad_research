library(tidyverse)

playoffstats1 <- read.csv('playoffstats1.csv')
playoffstats2 <- read.csv('playoffstats2.csv')
regularstats1 <- read.csv('regularstats1.csv')
regularstats4 <- read.csv('regularstats4.csv')

######################################################
### Checking to see if they play in the postseason ### 
######################################################

for(i in 1:nrow(regularstats1)){
  if(regularstats1$player[i] %in% playoffstats1$player){
    regularstats1$playslater <- TRUE
  }
  else if(regularstats1$player[i] %in% playoffstats2$player){
    regularstats1$playslater <- TRUE
  }
  else{
    regularstats1$playslater <- FALSE
  }
  progress <- (i/nrow(regularstats1)) * 100
  rounded_progress <- round(progress, 3)
  
  if (rounded_progress == 10) {
    print("10% done")
  } else if (rounded_progress==20){
    print("20% done")
  }
  else if (rounded_progress==30){
    print("30% done")
  }
  else if (rounded_progress==40){
    print("40% done")
  }
  else if (rounded_progress==50){
    print("50% done")
  }
  else if (rounded_progress==60){
    print("60% done")
  }
  else if (rounded_progress==70){
    print("70% done")
  }
  else if (rounded_progress==80){
    print("80% done")
  }
  else if (rounded_progress==90){
    print("90% done")
  }
}

print('regularstats1 done')

for(i in 1:nrow(regularstats4)){
  if(regularstats4$player[i] %in% playoffstats1$player){
    regularstats4$playslater <- TRUE
  }
  else if(regularstats4$player[i] %in% playoffstats2$player){
    regularstats4$playslater <- TRUE
  }
  else{
    regularstats4$playslater <- FALSE
  }
  progress <- (i/nrow(regularstats4)) * 100
  rounded_progress <- round(progress, 3)
  
  if (rounded_progress == 10) {
    print("10% done")
  } else if (rounded_progress==20){
    print("20% done")
  }
  else if (rounded_progress==30){
    print("30% done")
  }
  else if (rounded_progress==40){
    print("40% done")
  }
  else if (rounded_progress==50){
    print("50% done")
  }
  else if (rounded_progress==60){
    print("60% done")
  }
  else if (rounded_progress==70){
    print("70% done")
  }
  else if (rounded_progress==80){
    print("80% done")
  }
  else if (rounded_progress==90){
    print("90% done")
  }
}

print('regularstats4 done')


########################################################################
### Creating new data frames with those who played in the postseason ###
########################################################################

playoff_players <- filter(regularstats1, playslater == T)
temp_players <- filter(regularstats4, playslater == T)
playoff_players <- rbind(regularstats1, temp_players)
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

print('regular_performance done')


post_performance <- data.frame(player = unique, VORP = 0)
temp_post1 <- data.frame(player= unique, VORP = 0)
for(i in 1:length(unique)){
  temp <- filter(playoffstats1, player == unique[i])
  performace <- mean(temp$VORP)
  for(j in 1:length(unique)){
    if(unique[i] == post_performance$player[j]){
      post_performance$VORP[j] <- performace
    }
  }
}
for(i in 1:length(unique)){
  temp <- filter(playoffstats2, player == unique[i])
  performace <- mean(temp$VORP)
  for(j in 1:length(unique)){
    if(unique[i] == temp_post1$player[j]){
      temp_post1$VORP[j] <- performace
    }
  }
}

post_performance <- rbind(post_performance, temp_post1)

print('post_performance done')


#####################################################################
### This is creating the data frame that will be used in reserach ###
#####################################################################


my_tibble <- data.frame(player = regular_performance$player, reg = regular_performance$VORP, 
                        post = post_performance$VORP)

drop_na(my_tibble)

write_csv(my_tibble, 'data.csv')












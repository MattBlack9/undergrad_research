library(tidyverse)



#############################################
### Setting directory and reading in data ###
#############################################

johnnyboy <- read.csv('/Users/matty/Documents/BYU/undergrad_research/john/johnnyboy.csv')

setwd('/Users/matty/Documents/BYU/undergrad_research/linear/')


#########################
### Linear Regression ###
#########################

mean(johnnyboy$VORP)

plot(my_data$johnnyboy, my_data$vorp, xlab = "Johnnyboy", ylab = "VORP", main = "Scatterplot with Mean Line")

# Add points
points(my_data$vorp, col = "blue")

# Add mean line
abline(h = mean_vorp, col = "red") # Horizontal line at mean VORP
abline(v = mean_johnnyboy, col = "green") # Vertical line at mean Johnnyboy


plot(johnnyboy$VORP, main = "Scatterplot with Mean Line", xlab = "X", ylab = "Y")

# Add mean line
abline(h = mean(johnnyboy$VORP))
abline(v = mean_values["X"], col = "green") # Vertical line at mean X





























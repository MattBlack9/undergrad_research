library(tidyverse)
library(ggplot2)

setwd('/Users/matty/Documents/BYU/undergrad_research/stats')

data <- read.csv('data.csv')

data <- drop_na(data)


summary(lm(data$post~data$reg))

summary(lm(data$reg~data$post))


mean(data$reg<data$post,na.rm = T)

plot(data$reg,data$post)
abline(0,1)


ggplot(data)+
  geom_point(aes(x = reg, y = post)) +
  geom_smooth(aes(x = reg, y = post)) 





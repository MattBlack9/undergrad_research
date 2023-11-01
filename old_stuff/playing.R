gamesdf <- read.csv('gamesdf.csv')


column <- gamesdf[3543,6]
column

len <- nchar(column)

as.integer(substr(column,1,len-4)) > 1






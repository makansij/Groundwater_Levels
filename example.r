## Jordan Makansi 

library(ggplot2)
str(diamonds)
head(diamonds$cut)
head(diamonds)

p <- ggplot(diamonds, aes(diamonds))
p + geom_freqpoly()   # for some reason you have to run with control R!!!!!!!!!!!!!!!! 


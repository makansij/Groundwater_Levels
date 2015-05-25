# this is for prototyping 
library(ggplot2)

numb <- c(100,200,200,200,300)
df <- data.frame(matrix(unlist(numb), nrow=length(numb), byrow=T))

plt<- hist(df[[1]],breaks=10,main='some data stuff',lty=1, ylab='gheks')
jpeg('rplot.jpg')
plot(plt)
dev.off()

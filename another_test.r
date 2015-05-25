library(ggplot2)
Q <- read.table("E:\\UC_Berkeley_stuff\\Semesters\\Spring_2015\\Geostatistics_CEE_202B\\1-homework\\MaxFlow.txt", header=T, sep="\n")

p <- ggplot(Q,aes(X..Annual.maximum.flows.in.the.Colorado.River.at.Black.Canyon.from.1878.to.1929..m.3.s))
p + geom_histogram()
  
b <- ggplot(diamonds,aes(cut))
b+geom_histogram()
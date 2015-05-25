## Jordan Makansi

library(e1071)                  # load package for skewness 
library(gdata)                   # load gdata package 
library(pracma)                 # load pracma for trapz 
library(ggplot2)

Q <- read.table("E:\\UC_Berkeley_stuff\\Semesters\\Spring_2015\\Geostatistics_CEE_202B\\1-homework\\MaxFlow.txt", skip=1, sep="\n")
LOGQ <- log(Q)
 
## Problem 1)

# a.  Histogram 

histoQ <- hist(Q[[1]],breaks=10,main='HISTOGRAM of Q',lty=1, ylab='Occurrences')
histoLQ <- hist(LOGQ[[1]],breaks=10,main='HISTOGRAM of LOGQ',lty=1, ylab='Occurrences')

jpeg('histoQ.jpg')
plot(histoQ)
dev.off()

jpeg('histoLQ.jpg')
plot(histoLQ)
dev.off()


# b. Frequency diagram
freQ <- histoQ
freQ$counts <- freQ$counts/sum(freQ$counts)
freQ$xname <- "Frequency Diagram of Q"
plot(freQ)

jpeg('freQ.jpg')
plot(freQ)
dev.off()

freLQ <- histoLQ
freLQ$counts <- freLQ$counts/sum(freLQ$counts)
freLQ$xname <- "Frequency Diagram of LOGQ"
plot(freLQ)


jpeg('freLQ.jpg')
plot(freLQ)
dev.off()

#c. Normalized Frequency diagram:

a <- hist(Q[[1]],freq=FALSE,breaks=10,main='Normalized Frequency Diagram of Q',lty=1, ylab='Frequency Q')
b <- hist(LOGQ[[1]],freq=FALSE,breaks=10,main='Normalized Frequency Diagram of LOGQ',lty=1, ylab='Frequency LogQ')

jpeg('NormFreq.jpeg')
plot(a)
dev.off()

jpeg('NormFreLQ.jpeg')
plot(b)
dev.off()

#d. Normalized Frequency Polygons 

a$xname<-'Normalized Frequency Polygons Q'
b$xname<-'Normalized Frequency Polygons LogQ'

#norm freq polygons:

jpeg('NormFrQpoly.jpg')
plot(a$mids,a$density,type='b',xlab='Q',ylab='Frequency')
dev.off()

jpeg('NormFrLQpoly.jpg')
plot(b$mids,b$density,type='b',xlab='LogQ',ylab='Frequency')
dev.off()


#e. Describe how you arrived  at the optimal choice and why is it better?  

# I chose there to be 10 break points.  This shows the distribution of data without over-classifying it.
# If the bins were too big, there would be hardly any observable trend in the data.  If  the bins were
# too small, the data would be many gaps in the bins showing no values.

## another way to do this:  qplot(carat, data = diamonds, geom = "freqpoly", x=10)

#f.  The sample coefficient of variation

stdvQ = sd(Q[[1]])
stdvLOGQ = sd(LOGQ[[1]])

avgQ    = mean(Q[[1]])
avgLOGQ = mean(LOGQ[[1]])

covQ = stdvQ/avgQ
covLOGQ = stdvLOGQ/avgLOGQ 
print(' the coefficient of variance for Q is\n')
print(covQ)
print('\n the coefficient of variance for LOGQ is \n')
print(covLOGQ)

#g.  The skewness

skewQ = skewness(Q[[1]])
skewLOGQ = skewness(LOGQ[[1]])

print("the skew for Q is\n")
print(skewQ)
print(" \n the skew LOGQ is \n")
print(skewLOGQ)

## Problem 2)

# a.

h <- hist(cumsum(Q[[1]]),plot=FALSE)
h$counts <- cumsum(h$counts)
h$xname <- "Cumulative Frequency Diagram of Q"

jpeg('CDFQ.jpg')
plot(h)
dev.off()

g <- hist(cumsum(LOGQ[[1]]),plot=FALSE)
g$counts <- cumsum(g$counts)
g$xname <- "Cumulative Frequency Diagram of LOGQ"

jpeg('CDFLogQ.jpg')
plot(g)
dev.off()

#b. 

# for Q
print('this is the quantile')
quantile(Q[[1]])

print("\n this is the interquartile range")
v <- quantile(Q[[1]],0.25)
w <- quantile(Q[[1]],0.75)
print(w-v)

print("here are the quantiles for 0.05, 0.5, and 0.95, respectively:")
z <- quantile(Q[[1]],0.05)
k <- quantile(Q[[1]],0.5)
y <- quantile(Q[[1]],0.95)
print(z)
print(k)
print(y)

# this is LogQ
print('this is the quantile')
quantile(LOGQ[[1]])

print("\n this is the interquartile range")
v <- quantile(LOGQ[[1]],0.25)
w <- quantile(LOGQ[[1]],0.75)
print(w-v)

print("here are the quantiles for 0.05, 0.5, and 0.95, respectively:")
z <- quantile(LOGQ[[1]],0.05)
k <- quantile(LOGQ[[1]],0.5)
y <- quantile(LOGQ[[1]],0.95)
print(z)
print(k)
print(y)

#c.  s

jpeg('BoxQ.jpg')
s <- boxplot(Q[[1]])
dev.off()

jpeg('BoxLogQ.jpg')
p <- boxplot(LOGQ[[1]])
dev.off()


# Jordan Makansi 
# Homework 3 - Part 1 

library(gstat)
library(RandomFields)
library(lattice)
library(latticeExtra)
library(plyr)
library(outliers)

## COUNTOR PLOT 
load("E:/UC_Berkeley_stuff/Semesters/Spring_2015/Geostatistics_CEE_202B/3-homework/h3p1.Rdata")
#this loads into the workspace a bunch of different variables 
# one of those is "field", which contains the data that you need.  

print(levelplot(z~x+y,field))

# ##OUTLIERS REMOVED 
# 
# # divide into grid of 200 x 200 box each 
# 
# subdivisions <- c()
# 
# 
# subd1 <- field[ field$x >= 0 & field$x < 500 & field$y >= 0 & field$y <500 ,]
# subd2 <- field[ field$x >= 500 & field$x < 1000 & field$y >= 0 & field$y <500,]
# subd3 <- field[ field$x >= 1000 & field$x < 1500 & field$y >= 0 & field$y <500,]
# subd4 <- field[ field$x >= 1500 & field$x < 2000 & field$y >= 0 & field$y <500,]
# 
# subd5 <- field[ field$x >= 0 & field$x < 500 & field$y >= 500 & field$y <1000,]
# subd6 <- field[ field$x >= 500 & field$x < 1000 & field$y >= 500 & field$y <1000,]
# subd7 <- field[ field$x >= 1000 & field$x < 1500 & field$y >= 500 & field$y <1000,]
# subd8 <- field[ field$x >= 1500 & field$x < 2000 & field$y >= 500 & field$y <1000,]
# 
# subd9 <- field[ field$x >= 0 & field$x < 500 & field$y >= 1000 & field$y <1500,]
# subd10 <- field[ field$x >= 500 & field$x < 1000 & field$y >= 1000 & field$y <1500,]
# subd11 <- field[ field$x >= 1000 & field$x < 1500 & field$y >= 1000 & field$y <1500,]
# subd12 <- field[ field$x >= 1500 & field$x < 2000 & field$y >= 1000 & field$y <1500,]
# 
# subd13 <- field[ field$x >= 0 & field$x < 500 & field$y >= 1500 & field$y <2000,]
# subd14 <- field[ field$x >= 500 & field$x < 1000 & field$y >=  1500 & field$y <2000,]
# subd15 <- field[ field$x >= 1000 & field$x < 1500 & field$y >=  1500 & field$y <2000,]
# subd16 <- field[ field$x >= 1500 & field$x < 2000 & field$y >=  1500 & field$y <2000,]
# 
# subdvs <- list(subd1,subd2,subd3,subd4,subd5,subd6,subd7,subd8,subd9,subd10,subd11,subd12,
#                subd13,subd14,subd15,subd16,subd5)
# 
# 
# #attach(subdvs)
# #par(mfrow=c(4,4))
# 
# print(length(subd4[[3]]))
# 
# for (i in subdvs){
#   print(head(i))
#   mybp  <- boxplot(i[[3]])
#   print(mybp$out)
#   rm.outlier(i, fill = FALSE, median = FALSE, opposite = FALSE)
# }
# 
# print(length(subd4[[3]]))
# 
# rm.outlier(x, fill = FALSE, median = FALSE, opposite = FALSE)
# #v <- rm.outlier(field)
# 
# boxes <- function(field)
#   
#   # # Boxplot of MPG by Car Cylinders 
#   # boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 
#   #         xlab="Region", ylab="Values of Z") 
#   
#   boxplot(x~z,data=subd, main="notitle",xlab="Region",ylab="Values of Z")
# 
# ## RAW VARIOGRAM 
# g <- gstat(formula=z~1, locations=~x+y, data=field) 
# ## create object of class "gstat" with data
# 
# # Omni-directional variogram
# field.vgm <- variogram(g) # create method of class "gstatVariogram"
# plot(field.vgm ,main='Variogram of Field Data') # plot method for class "gstatVariogram"
# 
# # Anisotropic variogram 
# 
# #g <- gstat(formula=z~1, locations=~x+y, data=dat.aniso) 
# vgm.aniso <- variogram(g,alpha=c(0,30,60,90,120,150),tol.hor=5) 
# plot(vgm.aniso,main='Directional Variograms')
# 
# 
# ## HISTOGRAM
# 
# histoZ <- hist(field$z,breaks=10,main='HISTOGRAM of Z',lty=1, ylab='Occurrences')
# 
# # DETREND 
# 
# # PLOT TRENDLINE FOR X
# # For each x value, average the Z's to get Z versus x 
# zx <- aggregate(field$z,list(x=field$x),mean)  ## why am i getting two of the same column names 
# b = zx[,1]
# c = zx[,2]
# rename(zx,c("x"="z"))   #doesn't work?  
# fitx <- lm(c~b)
# plot(zx,ylab='z',xlab='x')
# abline(fitx)
# 
# # PLOT TRENDLINE FOR Y
# # For each y value, average the Z's to get Z versus y 
# zy <- aggregate(field$z,list(y=field$y),mean)  ## why am i getting two of the same column names 
# n = zy[,1]
# m = zy[,2]
# rename(zy,c("x"="z"))   # doesn't work?  
# fity <- lm(m~n)
# plot(zy,ylab='z',xlab='y')
# abline(fity)
# 
# 
# # DETREND 
# z = field$z
# x = field$x
# y = field$y 
# trend <- lm(z~x+y)
# 
# c = trend$coefficients[[1]]
# a = trend$coefficients[[2]]
# b = trend$coefficients[[3]]
# 
# #z_prime = z - (a*x + b*y +c)
# # SUBTRACT THE PREDICTED LINE 
# 
# Xs <- c()  
# Ys <- c()  
# Zs <- c()  
# 
# print('started the loop')
# for (i in 1:nrow(field)){
#   i = field[i,]
#   x=i$x
#   y=i$y
#   z=i$z
#   z_prime = z - (a*x+b*y+c)
#   Xs <- c(Xs,x)
#   Ys <- c(Ys,y)
#   Zs <- c(Zs,z_prime) 
# }
# result <- data.frame(Xs,Ys,Zs)
# # "result" is the new dataset with Z's detrended 
# print(levelplot(Zs~Xs+Ys,result))
# 
# 
# 
# # VARIOGRAMS WITH DETRENDED DATA
# 
# ## RAW VARIOGRAM 
# h <- gstat(formula=Zs~1, locations=~Xs+Ys, data=result) 
# ## create object of class "gstat" with data
# 
# # Omni-directional variogram
# result.vgm <- variogram(h) # create method of class "gstatVariogram"
# plot(result.vgm ,main='Variogram of Field Data (Detrended)') # plot method for class "gstatVariogram"
# 
# # Anisotropic variogram 
# 
# #g <- gstat(formula=z~1, locations=~x+y, data=dat.aniso) 
# vgm.aniso <- variogram(h,alpha=c(30,35,40,45,50,55),tol.hor=5)
# plot(vgm.aniso,main='Directional Variograms (Detrended)')
# 
# # FITTED VARIOGRAM (SPHERICAL)
# 
# # spherical model:
# vg.sph <- vgm(psill=0.6,model='Sph', range = 250)
# 
# # spherical fit:
# fit.sph <- fit.variogram(result.vgm , model = vg.sph)
# print(fit.sph) # display fitted parameters
# 
# plot(result.vgm$dist,result.vgm$gamma,xlab='lag distance',ylab='semivariance',main='Raw and Fitted Variograms')
# # lines(variogramLine(fit.exp, maxdist=225),lty=2,col='blue',lwd=2.5)
# lines(variogramLine(fit.sph, maxdist=800),lty=3, col='red',lwd=2.5)
# legend('topleft',c('Raw data','Sph fit'),col=c('black','red'),pch=c(1,NA),lty=c(NA,3),lwd=c(NA,2.5))
# 
# # ANISOTROPIC, FITTED VARIOGRAM 
# 
# vgm.aniso <- variogram(h,alpha=c(40),tol.hor=5)
# plot(vgm.aniso,main='Variogram at 40 degrees')
# 
# # spherical model:
# vg.sph <- vgm(psill=0.6,model='Sph', range = 250)
# 
# # spherical fit:
# fit.sph <- fit.variogram(vgm.aniso, model = vg.sph)
# print(fit.sph) # display fitted parameters
# 
# plot(vgm.aniso$dist,vgm.aniso$gamma,xlab='lag distance',ylab='semivariance',main='Raw and Fitted Variograms at 40 degrees')
# # lines(variogramLine(fit.exp, maxdist=225),lty=2,col='blue',lwd=2.5)
# lines(variogramLine(fit.sph, maxdist=800),lty=3, col='red',lwd=2.5)
# legend('topleft',c('Raw data','Sph fit'),col=c('black','red'),pch=c(1,NA),lty=c(NA,3),lwd=c(NA,2.5))
# 
# # ALTERNATIVE MODEL
# 
# # exponential model:
# vg.exp <- vgm(psill=0.61,model='Exp', range = 800)
# fit.exp <- fit.variogram(result.vgm , model = vg.exp)
# print(fit.exp) # display fitted parameters
# 
# plot(result.vgm$dist,result.vgm$gamma,xlab='lag distance',ylab='semivariance',main='Raw and Fitted Variograms to Exponential model with range = 800')
# lines(variogramLine(fit.sph, maxdist=800),lty=3, col='red',lwd=2.5)
# legend('topleft',c('Raw data','Exponential fit'),col=c('black','red'),pch=c(1,NA),lty=c(NA,3),lwd=c(NA,2.5))

load("E:/UC_Berkeley_stuff/Semesters/Spring_2015/Geostatistics_CEE_202B/3-homework/h3p2.Rdata")

print(levelplot(z~x+y,meas))

library(sp)
library(lattice) # required for trellis.par.set():
trellis.par.set(sp.theme()) # sets color ramp to bpy.colors()
 
#data(meas)
coordinates(meas)=~x+y

plot(meas,xlab="West East",ylab=" South North")
axis(1,xaxp=c(0,2000,4),yaxp=c(0,2000,4))
axis(2,xaxp=c(0,2000,4),yaxp=c(0,2000,4))
axis(3,xaxp=c(0,2000,4),yaxp=c(0,2000,4))
axis(4,xaxp=c(0,2000,4),yaxp=c(0,2000,4))

# UNIVARIATE ANALYSIS 

uni <- data.frame(meas$z)
boxplot(uni)
print(uni$out)

# select measurements by giving coordinates
seq1 <- seq(200,800,20)
seq2 <- seq(1200,1900,20)
seq3 <- seq(1500,2000,20)
seq4 <- seq(100,500,20)
seq5 <- seq(1200,1550,20)

x1 <- sample(seq1,15)
y1 <- sample(seq2,15)

x2 <- sample(seq3,5)
y2 <- sample(seq3,5)

x3 <- sample(seq4,15)
y3 <- sample(seq1,15)

x4 <- sample(seq5,15)
y4 <- sample(seq1,15)

l <- data.frame(x=c(x1,x2,x3,x4),y=c(y1,y2,y3,y4),z=NA)

for  (i in (1:nrow(l))) {
  l$z[i]=field$z[field$x == l$x[i] & field$y == l$y[i]]
}

# convert to dataframe before merging 
m <- as.data.frame(meas)
samples <- rbind(m,l)

#print(z~x+y,samples)
#plot(samples)#,xlab="West East",ylab=" South North")

plot(samples$x,samples$y)
points(meas$x,meas$y,pch=15)

sample_u <- data.frame(samples$z)
boxplot(sample_u,main="Boxplot of 60 measured values")
print(sample_u$out)

# VARIOGRAM MODEL FOR SAMPLES 

v <- gstat(formula=z~1, locations=~x+y, data=samples) 
## ^^create object of class "gstat" with data

raw.vgm <- variogram(v) # create method of class "gstatVariogram"
plot(raw.vgm,main='Variogram of Raw Data of Samples') # plot method for class "gstatVariogram"

# exponential model:
vg.exp <- vgm(psill=1.5,model='Exp', range = 50)
fit.exp <- fit.variogram(raw.vgm, model = vg.exp)
print(fit.exp) # display fitted parameters

plot(raw.vgm$dist,raw.vgm$gamma,xlab='lag distance',ylab='semivariance',main='Raw and Fitted Variograms for Samples')
lines(variogramLine(fit.exp, maxdist=800),lty=3, col='red',lwd=2.5)
legend('topleft',c('Raw data','Exp fit'),col=c('black','red'),pch=c(1,NA),lty=c(NA,3),lwd=c(NA,2.5))

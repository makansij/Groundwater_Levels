# Jordan Makansi
# Question 2 Homework 4  

library(RandomFields)
library(pracma) # needed inside Hfield()
library(iterators)
library(gstat)

load("E:/UC_Berkeley_stuff/Semesters/Spring_2015/Geostatistics_CEE_202B/4-homework/samples")


#  Uncomment this if you'd like to prototype a small samples data.frame  
#############
# x <- seq(0,2000,by=20)
# y <- seq(0,2000,by=20)
# 
# x = sample(x,10,replace=T)
# y = sample(y,10,replace=T)
# z = sample(0.532:3.7,10,replace=T)
# 
# samples = data.frame(x,y,z)
###############

# detrend the samples: 
print(mean(samples$z))

#create object of class gstat
h <- gstat(formula=z~1, locations=~x+y, data=samples)
samples.vgm <- variogram(h) # create method of class "gstatVariogram"
plot(samples.vgm,main='Variogram of Samples NOT detrended') # plot method for class "gstatVariogram"
samples = samples[-2,]

# DETREND 
z = samples$z
x = samples$x
y = samples$y 
trend <- lm(z~x+y)

c = trend$coefficients[[1]]
a = trend$coefficients[[2]]
b = trend$coefficients[[3]]

#z_prime = z - (a*x + b*y +c)
# SUBTRACT THE PREDICTED LINE 

Xs <- c()  
Ys <- c()  
Zs <- c()  
# 
# print('started the loop')
# for (i in 1:nrow(samples)){
#   i = samples[i,]
#   x=i$x
#   y=i$y
#   z=i$z
#   z_prime = z - (a*x+b*y+c)
#   Xs <- c(Xs,x)
#   Ys <- c(Ys,y)
#   Zs <- c(Zs,z_prime) 
# }

# sampled <- data.frame(Xs=Xs,Ys=Ys,Zs=Zs)
# print(sampled)
# print('the length of sampled is')
# print(length(sampled[[1]]))
# # "result" is the new dataset with Z's detrended 
# # print(levelplot(Zs~Xs+Ys,sampled))


# define the domain or kriging estimation

x <- seq(0,2000,by=20)
y <- seq(0,2000,by=20)

# make data frame with prediction locations 
pred.grid <- data.frame(x=rep(x,times=length(y)),y=rep(y,each=length(x)))

#create object of class gstat
g <- gstat(formula=Zs~1, locations=~Xs+Ys, data=sampled)
sampled.vgm <- variogram(g) # create method of class "gstatVariogram"
plot(sampled.vgm,main='Variogram of Samples hopefully detrended') # plot method for class "gstatVariogram"

# 1st model from HW 3:  spherical 
vg.sph <- vgm(psill=1.0,model='Sph', range = 500)
fit.sph <- fit.variogram(sampled.vgm, model = vg.sph)
sk1 <- krige(formula=Zs~1, locations=~Xs+Ys, data=sampled, newdata=pred.grid, model=fit.sph)#, beta=0)

# 2nd model from HW 3:  exponential
vg.exp <- vgm(psill=0.61,model='Exp', range = 800)
fit.exp <- fit.variogram(samples.vgm , model = vg.exp)
sk2 <- krige(formula=z~1, locations=~x+y, data=samples, newdata=pred.grid, model=fit.exp, beta=mean(sampled$Zs))

# 3rd model from HW 3:  exponential with different parameters
vg.exp2 <- vgm(psill=1.5,model='Exp', range = 50)
fit.exp2 <- fit.variogram(samples.vgm, model = vg.exp2)
sk3 <- krige(formula=z~1, locations=~x+y, data=samples, newdata=pred.grid, model=fit.exp2, beta=mean(sampled$Zs))


cross_val <- function(vg_model){
  #Input:
  # -vg_model, e.g. vg.sph
  #Output:
  # -zstar and varsk, which are predicted values and the variances  
  zstar = c()
  varsk = c()
  for (i in (1:length(sampled$Zs))){
    pred_z  = sampled[i,1:2]  # prediction location
    tmpdata = sampled[-i,]  # the data with that location removed 
    kout <- krige(formula=Zs~1, locations=~Xs+Ys, data=tmpdata, newdata=pred_z, model=vg_model)
    zstar = c(zstar,kout$var1.pred)  # collect the predicted values 
    varsk = c(varsk,kout$var1.var)  # collect the variance 
  }
  result = list(zstar, varsk)
  return (result)
}

get_error <- function(zstar){
  #Input:
  # -zstar data in the form of a list of estimated points
  #Output  
  # - error between zstar and sampled variogram
  error = (zstar - sampled$Zs)^2
  return (error)
}

error_vs_variance <- function(results,names){
  # Input:
  # -list of results where each element in the list is the output of cross_val 
  #Output:
  # - plots of error versus variance 
  for (i in 1:length(results)){
    zstar = results[[i]][[1]]  # list of z values  
    varsk = results[[i]][[2]]  # list of variances
    error = get_error(zstar)
    name = paste0("error_versus_variance_",names[[i]],".jpg",sep="")
    jpeg(name)
    plot(error,varsk, main=names[[i]])
    dev.off()
  }
  return (error)
}

zstar_vs_z <- function(results,names){
  # Input:
  # -list of results where each element in the list is the output of cross_val 
  #Output:
  # - plots of z (sampled) versus z_star 
  # - doesn't return anything 
  for (i in 1:length(results)){
    zstar = results[[i]][[1]]  # list of z values  
    name = paste0("Zstar_versus_Z_",names[[i]],".jpg",sep="")
    jpeg(name)
    plot(zstar,sampled$Zs,main=names[[i]])  
    dev.off()
  }
}

#---------------------------- cross validation ---------------------------

result_sph = cross_val(vg.sph)
result_exp =  cross_val(vg.exp)
result_exp2 = cross_val(vg.exp2)
results = list(result_sph,result_exp,result_exp2)


# finally, plot the results
names = list("spherical", "exponential","alternative exponential")
error_vs_variance(results,names)
zstar_vs_z(results,names)


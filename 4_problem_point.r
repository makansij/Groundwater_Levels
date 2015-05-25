library(gstat)
library(RandomFields)
library(lattice)
library(latticeExtra)

# First compute correlation, given r 

getCorr <- function(r){
  # this function takes in "r" in kilometers, and spits back correlation.
  # based on spherical covariance model, and scale length 13.3km
  
  term1 = 1
  term2 = 1.5*(r/(40/3))
  term3 = 0.5*(r/(40/3))^3
  rho =  (term1-term2+term3)
  if (rho > 1) {
    print('yes rho is greater than 1')
    print(rho)  
    print('this is r')
    print(r)
  }
  return (rho)
}

fc <- function(z0,z,rho){
  # this function is the conditional probability of z   
  # it evaluates at z, rho is correlation, and z0 is the variable of integration 
  exponent = (-1/(2*3*(1-rho^2)))*((z-rho*z0)^2)
  cpdf = (1/sqrt(3*2*pi*(1-rho^2)))*exp(exponent)
  #print(z0)
  if (z0==2.5){
    return (cpdf)
  }
  else{
    return (0)
  }
}

fccz <- function(z0,z,r){
  # this function gets integrated over z0 (this must be the first input)
  # r is length used to find the correlation
  # pdf is probability density function of z0   
  # l is the scaling (c = b-a ) in eq. 3.34 on page 63 in pdf 
  
  rho = getCorr(r)
  cpdf = fc(z0,z,rho)
  return (cpdf)
}

fccz0 <- function(z0_min,z0_max,z,r){
  # this is where the integration actually happens 
  # l = 0.5 because the gaps are 2.0 to 2.5 between 2.5 and 3.0.  
  
  print(0.5,subdivisions = 5)
  result = integrate(fccz,z0_min,z0_max,z,r,subdivisions = 50, stop.on.error = TRUE)
  
  print(result)
  return (result)
}

# range of r-values:
r_vect <- c(0.25,0.5,1.0,2.0,5.0,10.0)

# Now, we need a vector of z-values, and a vector of evaluations of the integral:
z_vect <- seq(-3,3,0.01)
# since the mean is zero, range from -5 to 5 


for (r in r_vect){
  integral <- c()
  for (z in z_vect){
    
    part1 <- fccz0(2.0,3.0,z,r)
    
    result <- as.numeric(part1[1])
    integral <- c(integral,result)
  }
  plot(z_vect, integral ,main=paste("Conditional at point pdf of r=",toString(r)),xlab='z',ylab='pdf') # show data locations
}


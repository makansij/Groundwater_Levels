# Jordan Makansi
# Question 1 

library(RandomFields)
library(pracma)   # needed inside Hfield()
library(iterators)

## functions to compute H(x,y) with heterogenous K-field (1-D or 2-D)
## load the 4 functions below into your workspace and call:

#H <- Hfield(k=k, h0=50, hl = 30, dx=1, dy=1)
#where k is the K-field (NOT log(K)!) (i.e. exponentiated output from random field generator)

# h0 is the constant head value at x=0 ("beginning" of domain)
# hl is the constant head value at x=L ("end" of domain)
# dx and dy are the (uniform) grid spacing in x and y
# dy = 1 for a 1-D field 

## more detailed example (1-D):

# define model for field simulation:
mod <- RMexp(var=1.5, scale=5) + RMtrend(mean=0)

a <- list(0.5,1,2.5,10,50)

#1-D grid:
# L <- a*5   
# dx <- L/100
# x <- seq(0,L,by=dx)  # this determines grid  and  grid spacing 

# generate field for Y=LOG(K):
Y <-  RFsimulate(mod, x=x, spConform=FALSE)

## Question 1 part a

# for each "a", generate a simulator.
# for each simulator, generate 50 fields 
 
field_array = c()
perm_array = c()

n = 50

for (i in a){
  for (j in seq(1,n)){
    L <- i*5  # I_y is 5 (hard coded)
    dx <- L/100
    x <- seq(0,L,by=dx)  # this determines grid  and  grid spacing 
    Y <- RFsimulate(mod, x=x, spConform=FALSE)
    H <- Hfield(k=exp(Y), h0=70, hl=50, dx=dx, dy=1, oneD=TRUE)    # solves the flow equation 
    # ...using the functions from below
    field_array = c(field_array,list(H))
    perm_array = c(perm_array,list(exp(Y)))
  }
}

field_matrix = matrix(field_array,nrow=length(a),ncol=n,byrow=TRUE)
perm_matrix = matrix(perm_array,nrow=length(a),ncol=n,byrow=TRUE)
# perm_matrix is used for part c 

histo_field <- function(field_matrix){
  
  num_rows = size(field_matrix)[1]
  num_cols = size(field_matrix)[2]
  
  print( "the number of rows is ")
  print(num_rows)
  print("the number of cols is")
  print(num_cols)
  
  irow <- iapply(field_matrix, 1)
  for (i in 1:num_rows){

    b = nextElem(irow)
    L_4 = c()
    L_2 = c()
    for (j in b){

      L_4 = c(L_4,j[25])
      L_2 = c(L_2,j[50])
    }    
    png(file=paste0('E:/UC_Berkeley_stuff/Semesters/Spring_2015/Geostatistics_CEE_202B/4-homework/histogram_of_L2_' , a[i] , '.png' ))
    hist_L2 <- hist(L_2,breaks=10,main=paste("HISTOGRAM OF L/2 FOR a = ",a[i],sep="",collapse=NULL))    
    #print(hist_L2)
    dev.off()

    png(file=paste0('E:/UC_Berkeley_stuff/Semesters/Spring_2015/Geostatistics_CEE_202B/4-homework/histogram_of_L4_' , a[i] , '.png' ))
    hist_L4 <- hist(L_4,breaks=10,main=paste("HISTOGRAM OF L/4 FOR a = ",a[i],sep="",collapse=NULL))    
    #print(hist_L4)
    dev.off()
  }
}

## Question 1 part b (histograms)
# this question uses the function defined above called "histo_field" 

histo_field(field_matrix)

## Question 1 part c
# for each field, compute the flux of water through the aquifer 

print('calling the flux matrix function')

q_matrix = flux_matrix(field_matrix,perm_matrix)
q_matrix = matrix(q_matrix,nrow=length(a),ncol=n,byrow=TRUE)

histo_flux(q_matrix)

# this function plots the flux histograms 
histo_flux <- function(q_matrix){
  
  num_rows = size(q_matrix)[1]
  num_cols = size(q_matrix)[2]
  
  print("the number of rows is")
  print(num_rows)
  print("the number of cols is")
  print(num_cols)
  
  irow <- iapply(q_matrix, 1)
  for (i in 1:num_rows){    
    fluxes=q_matrix[i,]
    png(file=paste0('E:/UC_Berkeley_stuff/Semesters/Spring_2015/Geostatistics_CEE_202B/4-homework/flux_histogram_of_L2_' , a[i] , '.png' ))
    hist_flux <- hist(fluxes,breaks=10,main=paste("Flux HISTOGRAM OF L/2 FOR a = ",a[i],sep="",collapse=NULL))    
    dev.off()
  }
}


# here we construct the flux matrix using the function flux_field

flux_matrix <- function(field_matrix,perm_matrix){
  q_matrix <- c()
  a = list(0.5,1,2.5,10,50)
  for (i in seq(1,dim(field_matrix)[1])){
    for (j in seq(1,dim(field_matrix)[2])){
      d_x = (as.double(a[i]))/20
      field_H = field_matrix[i,j]
      perm = perm_matrix[i,j]
      
      q = flux_field(field_H,perm,d_x)
      q_matrix <- c(q_matrix,q)
      
      }
    }
  return (q_matrix)
  }

flux_field <- function(field_H,perm,d_x){
  # Input:
  #   - field_H (list of H values)
  #   - perm (list of K values)
  #   - d_x is the grid size, which depends on a and L 
  # for a single field compute the flux here 
  q <- c()
  for (i in seq(1,length(field_H)-1)){
    # perm and field_H should be the same length
    d_H = field_H[[1]][i]-field_H[[1]][i+1]
    K_x = mean(c(perm[[1]][i],perm[[1]][i+1]))
    flux = -(K_x*(d_H/d_x))
    q <- c(q,flux)
    }
  print(q)
  return (q) 
  }





# ~~~~~~~~~~ Functions provided by Brad ~~~~~~~~~~~~~


# define a function that computes the flux for two arrays of H and K 
Y <-  RFsimulate(mod, x=x, spConform=FALSE)

# compute H
H <- Hfield(k=exp(Y), h0=70, hl=50, dx=dx, dy=1, oneD=TRUE)

## plot K & H
plot(x,H,type='l',ylim=c(0,70),main='H(x) and K(x)')
lines(x,exp(Y))

## 2-D example:
mod <- RMexp(var=0.3, scale=7) + RMtrend(mean=-1)
dx <- 1
dy <- 1
x <- seq(0,60,by=dx)
y <- seq(0,20,by=dy)

# simulate Y:
Y2D <- RFsimulate(mod, x=x, y=y, grid=TRUE, spConform=FALSE)
#compute H:
H2D <- Hfield(exp(Y2D), h0=70, hl=50, dx=dx, dy=dy, oneD=FALSE)
#plot H:
levelplot(H2D)


Hfield <- function(k,h0,hl,dx,dy=1,oneD=FALSE){
  ### function to compute head-field given K-field (k) 
  ###(uniformly spaced dx-by-dy grid cells)
  ### and constant-head BC at x=0 (h0) and at x=L (hl)
  
  if(oneD){
    require(pracma)
    k <- repmat(as.matrix(k),1,11)
  }
  
  kt=t(k)
  ul <- femsolve(kt,(h0-hl),dx,dy)
  
  Uu <- chol(ul$K)
  tmp <- backsolve(Uu, backsolve(Uu, ul$rhs, transpose = TRUE))
  h <- hl + t( head.transform(matrix(tmp,ncol=(ncol(kt)+1),nrow=(nrow(kt)+1) ) ) )
  if(oneD){
    h <- h[,6]
  }
  return(h)
}

femsolve<-function(field,b.head,dx,dy){
  nx <- length(field[,1]);
  ny <- length(field[1,]);
  
  K<-stiff(field,nx,ny,dx,dy)  
  rhs<-matrix(0,ncol=1,nrow=((nx+1)*(ny+1)));  #<-
  rhs[c(1:(nx+1))]<-b.head;  #<-
  
  bc<-c(c(1:(nx+1)),c((nx+1)*ny+1):((nx+1)*(ny+1)));
  bcn<-setdiff(c(1:((nx+1)*(ny+1))),bc)
  
  K[bc,]<-0; 
  
  rhs[bcn]<-rhs[bcn]-K[bcn,bc]%*%rhs[bc]; 
  K[,bc]<-0; 
  
  diag(K[bc,bc])<-1; 
  return(list(K=K,rhs=rhs))
}   


stiff <-function(field,nx,ny,dx,dy){ 
  # Calculate the stiffness matrix
  K<-matrix(0,ncol=(nx+1)*(ny+1),nrow=(nx+1)*(ny+1))#<-
  a<-rbind(c(2, -2, -1, 1),c(-2, 2, 1, -1),c(-1, 1, 2, -2),c(1, -1, -2, 2));
  b<-rbind(c(2, 1, -1, -2),c(1, 2, -2, -1),c(-1, -2, 2, 1),c(-2, -1, 1, 2));
  sk<-dy/dx/6*a+dx/dy/6*b;
  
  for(m in 1:(nx*ny)){
    n1<-floor((m-1)/nx)+m; n2=n1+1; n3=n2+nx+1; n4=n1+nx+1;
    n<-c(n1, n2, n3, n4);	
    K[n,n]<-K[n,n]+sk*field[m];
  }
  K
}


head.transform <- function(x){
  c <- dim(x)-1
  x0 <- matrix(NA,nrow=c[1],ncol=c[2])
  for (i in 1:nrow(x0)){
    for (j in 1:ncol(x0)){
      x0[i,j] <- (x[i,j]+x[i+1,j]+x[i,j+1]+x[i+1,j+1])/4}}
  x0
}

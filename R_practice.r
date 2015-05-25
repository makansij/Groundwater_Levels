# Jordan's file for practicing
# Make a plot of x,y data 

# Jordan first practice problem
a <- sample(1:100,10)
b <- sample(1:100,10)

plot(a,b)  # remember that plot will plot x,y data if you provide it like plot(x,y)

# Jordan second practice problem 
# this generates a random Gaussian field:
# "rnorm" stands for "random generation" 
X <- rnorm(500, mean=5, sd =7)
plot(X)

# Problem 3 
# use the iris dataframe and plot histograms of each of the attributes

histomakr <- function(Dframe){
  for (i in 1:length(attributes(iris)[[1]])){
    print(i)
    hgram <- hist(iris[[i]])
  }
}

histomakr(iris)
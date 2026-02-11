#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################
options(scipen = 10)

set.seed(123)
test_data <- rcauchy(1000, location = 0, scale = 1)

vector <- c() 

KS_test <- function(data) {
  ECDF <- ecdf(data) 
  empiricalCDF <- ECDF(data) 
  D <- max(abs(empiricalCDF - pnorm(data))) 
  for(i in 1:length(data)) { 
   vector[i] <- exp((-(2 * i - 1)^2 * pi^2)/(8 * D^2))
  }
  sum_vector <- sum(vector) 
  scaling_factor <- sqrt(2*pi)/D
  p <- sum_vector * scaling_factor 
  return(c(D, p))
}

KS_test(test_data)

#A test statistic of 0.1347281 indicates that the largest difference 
#between our empirical cumulative distribution function and a normal 
#distribution function is 13.47%. The interpretation of the test-statistic is 
#based on the specific context of the data and has no universal rules, 
#but it could indicate a moderate disagreement between the distributions.

#The p-value is very small, well below the commonly used critical value of 0.05.
#Therefore, we can reject the null hypothesis that the empirical CDF for our
#data matches the normal distribution.

#####################
# Problem 2
#####################

#The seed is set and data is created
set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

lm(y ~ x, data)
summary(lm(y ~ x, data))$sigma^2

#The linear.like function is defined; it takes 3 arguments, theta, y, and x.
#Within the function:
#The variable n is assigned to the number of rows in x
#The variable k is assigned to the number of columns in x
#The variable beta is assigned to the regression coefficients, which are extracted from within theta
#The variable sigma2 is assigned to the squared element in the k+1 position of theta
#The variable e is assigned to y minus the matrix multiplication between x and beta
#The previously calculated variables are used in the log likelihood equation and the calculated
#result is assigned to the variable logl
#The function returns -logl, it returns the negative log to account for the fact that the optim
#function finds the minimum of the function and we want the maximum of the function



linear.lik <- function(theta, y , x){
  n <- nrow(x)
  k <- ncol(x)
  beta <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e <- y - x%*%beta
  logl <- -.5*n*log(2*pi) -.5*n*log(sigma2) - ((t(e) %*% e)/(2*sigma2))
  return(-logl)
}

#The optim function is used with our linear.lik function, the initial parameters all set to one, 
#a hessian matrix is computed, our x and y values are set, and the method is set to BFGS; the
#result is assigned to the parameter linear.MLE and the parameters of linear.MLE are printed

linear.MLE <- optim(fn=linear.lik, par=c(1,1,1), hessian=
TRUE, y=data$y, x=cbind(1, data$x), method = "BFGS")

linear.MLE$par

residual_variance <- (-1.4390716)^2
residual_variance







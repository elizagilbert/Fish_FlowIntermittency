#Read me#######
#the purpose of this script is to start to understand how to run a functional regression
#regressing drying metric "hydrographs" agains annual fish relative abundances

#Libraries ####
library(tidyverse)
library(fda)

#Data ####
datdry <- read.csv("Data/Processed/RGDryCovariates_ByReach.csv")

#following vignette Intro Basics of FDA
#splines
nbasis <- 15 #more basis functions you use the wigglyier the curve will be 
             #there might be something with this that it represents knots if using "breaks"
rangeval <- c(0,8) #I don't understand what this does
spline.basis <- create.bspline.basis(rangeval, nbasis) #the rangeval changes the params
coefs <- diag(rep(1,64))
spline.fd <- fd(coefs, spline.basis)
plot(spline.fd, xlab = "t", ylab = "phi(t)")

coefs <- matrix(rnorm(700),15,3)
threespline.fd <- fd(coefs, spline.basis)
plot(threespline.fd, xlab = "t", ylab = "f(t)")

#under vignette Spline Function
#We usually elect to keep the order fixed, and add breakpoints as needed to get the required flexibility.
#norder <- 3 is a third order polynomial that has two curves - number of curves is order - 1 so 5 order has 4 curves

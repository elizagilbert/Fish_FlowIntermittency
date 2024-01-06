#Read me#######
#the purpose of this script is to learn FDA 
#using fda.usc from these two cites https://rpubs.com/moviedo/fda_usc_regression 
#https://www.r-users.gal/sites/default/files/fda_usc.pdf

#TRY THIS CITE
# https://rviews.rstudio.com/2021/05/04/functional-data-analysis-in-r/

#TRY THIS ONE TOO
#https://rpubs.com/moviedo/fda_usc_introduction 

#there is also an fda package but the book I downloaded doesn't seem all that user friendly
#at the moment so starting with fda.usc

#Libraries ####
library(tidyverse)
library(fda.usc)

#data
data("aemet") #data for 73 weather stations and daily summaries so dimensions of 73 by 365 (30 years of data)
class(aemet$temp) #the class is an fdata 

#data of a matric of nxm which contains a set of n curves discretized in m points or argvals
#argvals that are locations of the discretization points, by default {t1 = 1,..., tm = m}
#rangevals that are rangevals of discretization points
#names (optional) that list with the three components: main, an overall title,, xlab, a title for 
#the x axis and ylab, a title for the y axis

names(aemet$temp)
dim(aemet$temp)
head(aemet$temp$argvals) #location of the descritized points -  0.5 1.5 2.5 3.5 4.5 5.5
aemet$temp$rangeval #range of discreatized points 0 365
aemet$temp$names

range(aemet$temp)
is.fdata(aemet$df)
is.fdata(aemet$temp)

par(mfrow = c(1,2))
col1 = ifelse(aemet$df$latitude < 31, "red", "blue")
plot(aemet$df[, c("longitude", "latitude")], col = col1, lwd =2)
plot(aemet$temp, col = col1, lwd =2)

#fdata() converts data.frame to an object of class fdata
#fdata2fd converts fdata to fd object using the basis representation
temp.fd = fdata2fd(aemet$temp, type.basis = "fourier", nbasis = 15)
temp.fdata = fdata(temp.fd)
class(temp.fd)
class(temp.fdata)
names(temp.fd)
names(temp.fdata)

#smoothing raw data by fixed basis and kernel
par(mfrow = c(1,3))
plot(aemet$temp)
plot(min.basis(aemet$temp, lambda = 1000)$fdata.est, main = "Bspline basis") #doesn't work min.basis depreciated
plot(min.np(aemet$temp)$fdata.est, main = "Kernel smooth") #doesn't work min.np depreciate

par(mfrow = c(1, 1))
plot(aemet$temp)
pc <- create.pc.basis(aemet$temp, l = 1:2)

temp <- create.pc.basis(aemet$temp, l = 1:2)
plot(temp[[1]])

data(tecator)
basis.pc<-create.pc.basis(tecator$absorp.fdata,c(1,4,5))
plot(basis.pc$basis,col=1)
summary(basis.pc)
basis.pls<-create.pls.basis(tecator$absorp.fdata,y=tecator$y[,1],c(1,4,5))
summary(basis.pls)
plot(basis.pls$basis,col=2)
summary(basis.pls)

basis.fd<-create.fdata.basis(tecator$absorp.fdata,c(1,4,5),
                             type.basis="fourier")
plot(basis.pc$basis)
basis.fdata<-create.fdata.basis(tecator$absorp.fdata,c(1,4,5),
                                type.basis="fourier",class.out="fdata")
plot(basis.fd,col=2,lty=1)
lines(basis.fdata,col=3,lty=1)

#data driven basis: Principla components
pc <- fdata2pc(aemet$temp)
summary(pc)

#a different data set
#115 curves of NOx levels measured every hour in Barcelona
par(mfrow = c(1, 2))
data(poblenou)
dd <- as.integer(poblenou$df$day.week)
working = poblenou$nox[poblenou$df$day.festive == 0 & dd < 6]
nonworking = poblenou$nox[poblenou$df$day.festive == 1 | dd > 5]

depth.mode(working, draw = T)
depth.mode(nonworking, draw = T)

#finding outliers
out = outliers.depth.trim(working, dfunc = depth.FM, nb = 100, smo = 0.1, trim = 0.05)
out2 = outliers.depth.trim(nonworking, dfunc = depth.FM, nb = 100, smo = 0.1,
                           trim = 0.05)

#functional regression models
data(tecator)
par(mfrow = c(1, 3))
fat15 <- ifelse((y <- tecator$y$Fat) < 15, 2, 4)
boxplot(y, main = "Fat")
plot((X <- tecator$absorp), col = fat15, main = "Spectrometric: X")
plot((X.d1 <- fdata.deriv(tecator$absorp, 1)), col = fat15, main = "Derviative: X.d1")

summary(fregre.basis(X.d1, y))

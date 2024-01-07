#https://rviews.rstudio.com/2021/05/04/functional-data-analysis-in-r/?_gl=1*3zjbwy*_ga*MTQyMzQ3MzIxNS4xNjk1NDc4NTc0*_ga_2C0WZ1JHG0*MTcwNDU2ODUzMi44LjAuMTcwNDU2ODUzMi4wLjAuMA..

library(fda)
library(tidyverse)

set.seed(999)
n_obs <- 80
time_span <- 100
time <- sort(runif(n_obs, 0, time_span))
Wiener <- cumsum(rnorm(n_obs))
y_obs <- Wiener +rnorm(n_obs, 0, 0.5) #rnorm (number, mean, sd)

time_basis <- seq(0, time_span, 1) #seq 0 to 100 by 1s
knots <- seq(0, time_span, 5) #seq 0 to 100 by 5s
n_knots <- (length(knots))
n_order <- 4 #order of basis functions: cubic bspline order = 3+1
n_basis = length(knots) + n_order-2 #from Ramsay, p.35 the number of basis
 #functions = order + number of interior knots (interior knots are always 2 less than
 #total) because outer and inner not counted
basis <-  create.bspline.basis(c(min(time_basis),max(time_basis)),n_basis,n_order,knots)

  #paramters ends up being the number of knots - 2
  #nbasis is 23 this means there are 23 vectors

#use eval.basis() to evalute the basis functions at the times (n_obs)
#where our data curve was observed. The matrix PHI will
#contain the values of the 23 basis functions evaluated at 80 points
#randomly placed between 0-100

PHI <- eval.basis(evalarg = time, basisobj = basis) #so 80 rows evaluation
 #and 23 columns basis functions

matplot(time,PHI,type='l',lwd=1,lty=1, xlab='time',ylab='basis',cex.lab=1,cex.axis=1)
for (i in 1:n_knots)
{
  abline(v=knots[i], lty=2, lwd=1)
}

#computing coefficients by hand
M = ginv(t(PHI) %*% PHI) %*% t(PHI)
c_hat = M %*% Wiener
y_hat = PHI %*% c_hat
df <- as.data.frame(y_hat) %>% rename(y_hat = 1) #had to change some of the code from the blog
p2 <- df %>% ggplot() + 
  geom_line(aes(x = time, y = Wiener), col = "grey") +
  geom_point(aes(x = time, y = y_obs)) +
  geom_line(aes(x = time, y = y_hat), col = "red")
p2 + ggtitle("Original curve and least squares estimate") + 
  xlab("time") + ylab("f(time)")

#compute variance by hand
# estimate the variance of noise
## SSE = (Y - Xb)'(Y - Xb)
SSE = t(y_hat-y_obs)%*%(y_hat-y_obs)
sigma2 = SSE/(n_obs-n_basis)

# estimate the variance of the fitted curve
# H is the Hat matrix H
# H = X*inv(X'X)*X``
H = PHI %*% M
varYhat = diag(H %*% H * matrix(sigma2,n_obs,n_obs))

# 95% confidence interval

y_hat025 = y_hat-1.96*sqrt(varYhat)
y_hat975 = y_hat+1.96*sqrt(varYhat)


#for plotting
df <- mutate(df, y_hat025 = y_hat025,
             y_hat975 = y_hat975)

#names(df) <- c("time","Wiener","y_hat", "y_hat025", "y_hat975")
p3 <- df %>% ggplot() + 
  geom_line(aes(x = time, y = Wiener), col = "grey") +
  geom_point(aes(x = time, y = y_obs)) +
  geom_line(aes(x = time, y = y_hat), col = "red") +
  geom_line(aes(x = time, y_hat025), col = "green") +
  geom_line(aes(x = time, y_hat975), col = "green") 
p3 + ggtitle("Estimated curve with error bars") + 
  xlab("time") + ylab("f(time)")


#finally with the fda package
#do the work of estimaitng the coefficients and function values
#with a single line of code using smooth.basis()
#this takes the arguments argvals (the times we want to use for 
#evaluation as a vecotr), the y (the observed values) and fdParobj (
#an fda object containing the bais elements)

Wiener_obj <- smooth.basis(argvals = time, y = y_obs, fdParobj = basis)


plot(time, Wiener, type = "l", xlab = "time", ylab = "f(time)", 
     main = "Comparison of fda package and naive smoothing estimates", col = "grey")
lines(time,y_hat,type = "l",col="red")
lines(Wiener_obj, lwd = 1, col = "blue")

splinebasis <- create.bspline.basis(c(0,10), 13) #default order =4, 9 interior knots so 13 basis


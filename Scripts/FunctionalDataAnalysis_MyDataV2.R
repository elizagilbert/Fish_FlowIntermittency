#read me ####
#the purpose of this code is to understand the curves in the drying metrics


#libraries ####
# install.packages("devtools")
# require(devtools)
# install_version("fda", version = "5.1.4") #need to install this version or things get weird with the CV and lambda

library(tidyverse)
library(lubridate)
library(fda) #seems like it messes up some tidy functions like "complete" and maybe "select"
library(reshape2)
library(wesanderson) #colors

#wrangle fish ####
datfish <- read.csv("Data/Processed/RGFishCPUE_RM.csv") %>% 
  group_by(Species_Codes, Year, Reach) %>% 
  summarise(MnCPUE = (mean(CPUE_m, na.rm = T)*1000)) %>% 
  ungroup() %>% 
  mutate(LogMean = log10(MnCPUE+0.001))

#wrangle drying ####
dat_drying <- read.csv("C:/Users/eigilbert/OneDrive - DOI/Documents/UNM/RiverDrying/Chapter1/Scratch_Chap1_R/Data/Processed/2010_2021_WetDryTenths.csv") %>% 
  mutate(Reach = case_when(RMTenthDry < 116 ~ "San Acacia",
                           TRUE ~ "Isleta"),
         Date = as.Date(Date, format = "%Y-%m-%d"))

#Extent dry - Isleta
#need it to be a matrix with years named as columns and days as rows 
ExtIsleta_Yr <- dat_drying %>%
  dplyr::select(!X) %>% 
  filter(DryRM == 0 & Reach == "San Acacia") %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  tidyr::complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0), Year = year(Date), MnDay = format(Date, format = "%b%d")) %>% 
  ungroup() %>% 
  select(Year, ExtentDry, MnDay) %>% 
  filter(MnDay != "Feb29") %>% 
  pivot_wider(names_from = Year, values_from = ExtentDry) %>% 
  column_to_rownames(var = "MnDay") %>% 
  as.matrix()

ExtIsleta_Irrig <- dat_drying %>%
  dplyr::select(!X) %>% 
  filter(DryRM == 0 & Reach == "San Acacia") %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  tidyr::complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0), Year = year(Date), MnDay = format(Date, format = "%b%d")) %>% 
  ungroup() %>% 
  filter(between(month(Date),6,10)) %>% 
  select(Year, ExtentDry, MnDay) %>% 
  filter(MnDay != "Feb29") %>% 
  pivot_wider(names_from = Year, values_from = ExtentDry) %>% 
  column_to_rownames(var = "MnDay") %>% 
  as.matrix()

#fda ####
#1)get log abundance and put into named number ####
Carcar_Isleta <- datfish %>% 
  filter(Species_Codes == "PLAGRA" & Reach == "San Acacia") %>% 
  select(Year, LogMean) %>% 
  pull(LogMean, Year) #makes a named number using Year to name row

ny <- length(Carcar_Isleta) #length of y data

#2) create functional covariate for predictor data ####
 
  #for irrigation season
ExtIsleta_basis <- create.bspline.basis(c(1,153), norder = 4, breaks = seq(1,153,3)) #don't add penalties or lambda to keep data information
plot(ExtIsleta_basis)

ExtIsleta_smooth <- smooth.basis(seq(1,153,1), ExtIsleta_Irrig, ExtIsleta_basis)
ExtIsleta_smooth_fd <- ExtIsleta_smooth$fd

plot(ExtIsleta_smooth_fd)

 #for the entire year
ExtIsleta_basis <- create.bspline.basis(c(0365), norder = 4, breaks = seq(0,365,5)) #don't add penalties or lambda to keep data information
plot(ExtIsleta_basis)

ExtIsleta_smooth <- smooth.basis(day.5, ExtIsleta_Yr, ExtIsleta_basis)
ExtIsleta_smooth_fd <- ExtIsleta_smooth$fd

plot(ExtIsleta_smooth_fd)


#3) create list of functional covarites
ExtIsleta_list <- vector("list", 2)
ExtIsleta_list[[1]] <- rep(1,ny)
ExtIsleta_list[[2]] <- ExtIsleta_smooth_fd

#4) create basis list
conbasis <-  create.constant.basis(c(0,365))
betabasis <- ExtIsleta_basis #used same basis as before

betalist1 <- vector("list", 2)
betalist1[[1]] <- conbasis
betalist1[[2]] <- betabasis

#5) Choose Smoothing Parameters using cross-validation ####
# 
# loglam = seq(1,15,0.5)
# nlam   = length(loglam)
# SSE.CV = rep(NA,nlam)
# for (ilam in 1:nlam) {
#   print(paste("log lambda =", loglam[ilam]))
#   lambda     = 10^(loglam[ilam])
#   betalisti  = betalist1
#   betalisti[[2]] = fdPar(betabasis, 2, lambda)
#   fRegi          = fRegress.CV(Carcar_Isleta, ExtIsleta_list, betalisti)
#   SSE.CV[ilam]   = fRegi$SSE.CV
# }
# 
# par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
# plot(loglam, SSE.CV, type="b", lwd=2,
#      xlab="log smoothing parameter lambda",
#      ylab="Cross-validation score", cex.lab=2,cex.axis=2)

loglam        = seq(1,15,0.5)
nlam          = length(loglam)
dfsave        = rep(NA,nlam)
names(dfsave) = loglam
gcvsave       = dfsave
for (ilam in 1:nlam) {
  cat(paste('log10 lambda =',loglam[ilam],'\n'))
  lambda        = 10^loglam[ilam]
  fdParobj      = fdPar(betabasis, 2, lambda)
  smoothlist    = smooth.basis(seq(1,153,1), ExtIsleta_Irrig,
                               fdParobj)
  dfsave[ilam]  = smoothlist$df
  gcvsave[ilam] = sum(smoothlist$gcv)
}

plot(loglam, gcvsave, type='b', lwd=2, ylab='GCV Criterion',
     xlab=expression(log[10](lambda)) )


#6) Run regression with lambda ####
lambda = 10^4
betafdPar  = fdPar(betabasis, 2, lambda)

betalist2 <- betalist1
betalist2[[2]] <- betafdPar  

AnnCarcarExtIsleta <- fRegress(Carcar_Isleta, ExtIsleta_list, betalist2)

#significance ####
#7) Get parameters and assess significance of drying

betaest_Carcar_ExtIsleta <- AnnCarcarExtIsleta$betaestlist #getting beta(t)
chat2_Carcar_ExtIsleta <- AnnCarcarExtIsleta$yhatfdobj #getting fitted value yhat
print(AnnCarcarExtIsleta$df) #getting effective degrees of freedom

# F test for the overall effect of x_i(t) #####
# H0: y = alpha + \epsilon
# H1: y = alpha + \int [beta(t)x(t)]dt + epsilon

(SSE0 <-  sum((Carcar_Isleta - mean(Carcar_Isleta))^2)) # sum squared residuals for the null model y = alpha + \epsilon
(SSE2 <- sum((Carcar_Isleta - chat2_Carcar_ExtIsleta)^2)) #sum squared residuals

nbasis <- ExtIsleta_smooth$fd$basis$nbasis
(Fratio = ((SSE0-SSE2)/(nbasis-1)/(SSE2/(ny-nbasis))))
(Fratio2  <-  ((SSE0-SSE2)/(AnnCarcarExtIsleta$df-1))/(SSE2/(ny-AnnCarcarExtIsleta$df)))

# 95% quantile 
(qFratio <- qf(0.95,AnnCarcarExtIsleta$df-1,ny-AnnCarcarExtIsleta$df))

Fratio2 > qFratio #if false no significant, if true significant

# calculate the p-value
1-pf(Fratio2,AnnCarcarExtIsleta$df-1,ny-AnnCarcarExtIsleta$df)
# p-value is 0.12 indicating that x_i(t) does not have a significant effect on y_i

##This part was in the Ramsay book and not in Jiguo Cao's script or lecture
# F.res = Fperm.fd(Carcar_Isleta, ExtIsleta_list, betalist2)
# F.res$Fobs
# F.res$qval

#plotting ####
#8) plot beta(t)
plot(betaest_Carcar_ExtIsleta[[2]]$fd)

# plot yhat vs. y

par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
plot(chat2_Carcar_ExtIsleta, Carcar_Isleta, lwd=2,cex.lab=2,cex.axis=2)
abline(lm(Carcar_Isleta~chat2_Carcar_ExtIsleta), lty='dashed', lwd=2)
abline(0,1,lty=1, lwd=2,col="red")

#9) confidence intervals ####
# fitted residuals
resid   = Carcar_Isleta - chat2_Carcar_ExtIsleta

# estimate sigma^2
SigmaE. = sum(resid^2)/(12-AnnCarcarExtIsleta$df)
SigmaE  = SigmaE.*diag(rep(1,12))

# for smoothing temperature chat = a matrix * y
y2cMap  = ExtIsleta_smooth$y2cMap

# # obtain point-wise standard error for beta(t)
stderrList <-  fRegress.stderr(AnnCarcarExtIsleta, y2cMap, SigmaE) 

betafdPar      = betaest_Carcar_ExtIsleta[[2]]
betafd         = betafdPar$fd
betastderrList = stderrList$betastderrlist
betastderrfd   = betastderrList[[2]]

plot(betafd, xlab="Day", ylab="Drying Metric Coeff.")
 lines(betafd+2*betastderrfd, lty=2, lwd=2)
 lines(betafd-2*betastderrfd, lty=2, lwd=2)



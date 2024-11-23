#read me ####
#the purpose of this code is to understand the curves in the drying metrics


#libraries ####
install.packages("devtools")
require(devtools)
install_version("fda", version = "5.1.4") #need to install this version or things get weird with the CV and lambda

library(tidyverse)
library(lubridate)
library(fda) #seems like it messes up some tidy functions like "complete" and maybe "select"
library(reshape2)
library(wesanderson) #colors

#wrangle fish ####
datfish <- read.csv("Data/Processed/RGFishCPUE_RM.csv") %>% 
  group_by(Species_Codes, Year, Reach) %>% 
  summarise(MnCPUE = (mean(CPUE_m, na.rm = T)*10)) %>% 
  ungroup() %>% 
  mutate(LogMean = log(MnCPUE+0.001)) %>% 
  filter(between(Year, 2010,2021))

#wrangle drying ####
dat_drying <- read.csv("Data/Processed/2010_2021_WetDryTenths.csv") %>% 
  mutate(Reach = case_when(RMTenthDry < 116 ~ "San Acacia",
                           TRUE ~ "Isleta"),
         Date = as.Date(Date, format = "%Y-%m-%d"))

#visualzing data #####
temp <- dat_drying %>%
  dplyr::select(!X) %>% 
  filter(DryRM == 0) %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  tidyr::complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0), Year = year(Date), DOY = yday(Date)) %>% 
  ungroup() %>% 
  filter(between(month(Date),4,10)) %>% 
  ggplot(aes(x = ExtentDry))+
  geom_density()+
  facet_grid(vars(Year))+
  scale_y_log10()+
  theme_classic()


   #extent dry
dat_drying %>%
  dplyr::select(!X) %>% 
  filter(DryRM == 0) %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  tidyr::complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0), Year = year(Date), DOY = yday(Date)) %>% 
  ungroup() %>% 
  filter(between(month(Date),4,10)) %>% 
  ggplot(aes(x = DOY, y = ExtentDry, color = as.factor(Year)))+
  geom_line(linewidth = 1)+
  facet_grid(vars(Reach))+
  theme_classic()

  #change extent dry
dat_drying %>%
  dplyr::select(!X) %>% 
  filter(DryRM == 0) %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  tidyr::complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0), Year = year(Date), DOY = yday(Date)) %>% 
  mutate(ExtentChng = ExtentDry - lag(ExtentDry, default = ExtentDry[1])) %>% 
  ungroup() %>% 
  filter(between(month(Date),4,10)) %>% 
  ggplot(aes(x = DOY, y = ExtentChng, color = as.factor(Year)))+
  geom_line(size = 1)+
  facet_grid(vars(Reach))+
  theme_classic()

    #mile days
dat_drying %>%  
  select(!X) %>%  
  filter(Date >= "2010-01-01") %>%  
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1, TRUE ~ 0)) %>%  
  arrange(Date, RMTenthDry) %>%  
  group_by(year(Date), Reach) %>%  
  mutate(MD = cumsum(DryRM2)/10) %>%  
  ungroup() %>%  
  filter(between(month(Date), 4, 10)) %>%  
  group_by(Reach, Date) %>%  
  summarise(Daily_MaxMileDays = max(MD)) %>%  
  ungroup() %>% 
  filter(between(month(Date),4,10)) %>% 
  mutate(Year = year(Date), DOY = yday(Date)) %>% 
  ggplot(aes(x = DOY, y = Daily_MaxMileDays, color = as.factor(Year)))+
  geom_line(size = 1)+
  facet_grid(vars(Reach))+
  theme_classic()

# matrices that should be copied into loop #####
#need it to be a matrix with years named as columns and days as rows 
ExtIsleta_Irrig <- dat_drying %>%
  dplyr::select(!X) %>% 
  filter(DryRM == 0 & Reach == reach) %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  tidyr::complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0), Year = year(Date), MnDay = format(Date, format = "%b%d"),
         ExtentDry = log(ExtentDry+0.001)) %>% 
  ungroup() %>% 
  filter(between(month(Date),4,10)) %>% 
  select(Year, ExtentDry, MnDay) %>% 
  filter(MnDay != "Feb29") %>% 
  pivot_wider(names_from = Year, values_from = ExtentDry) %>% 
  column_to_rownames(var = "MnDay") %>% 
  as.matrix()

ExtChngIsleta_Irrig <- dat_drying %>%
  dplyr::select(!X) %>% 
  filter(DryRM == 0 & Reach == reach) %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  tidyr::complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0), Year = year(Date), MnDay = format(Date, format = "%b%d")) %>% 
  mutate(ExtentChng = ExtentDry - lag(ExtentDry, default = ExtentDry[1])) %>% 
  ungroup() %>% 
  filter(between(month(Date),4,10)) %>% 
  select(Year, ExtentChng, MnDay) %>% 
  filter(MnDay != "Feb29") %>% 
  pivot_wider(names_from = Year, values_from = ExtentChng) %>% 
  column_to_rownames(var = "MnDay") %>% 
  as.matrix()

MileDays_Irrig <- dat_drying %>%  
  select(!X) %>%  
  filter(Date >= "2010-01-01" & Reach == reach) %>%  
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1, TRUE ~ 0)) %>%  
  arrange(Date, RMTenthDry) %>%  
  group_by(year(Date), Reach) %>%  
  mutate(MD = cumsum(DryRM2)/10) %>%  
  ungroup() %>%  
  filter(between(month(Date), 4, 10)) %>%  
  mutate(Year = year(Date), MnDay = format(Date, format = "%b%d")) %>% 
  group_by(Year, Date) %>%  
  mutate(Daily_MaxMileDays = max(MD)) %>% 
  ungroup() %>% 
  select("Year", "MnDay", "Daily_MaxMileDays") %>% 
  distinct(Year, MnDay, Daily_MaxMileDays) %>% 
  pivot_wider(names_from = Year, values_from = Daily_MaxMileDays) %>% 
  column_to_rownames(var = "MnDay") %>% 
  as.matrix()

#function to prepare data #####
prepare_data <- function(reach, species_code, step_size){
  
  #change entire code for each metric from Extent, ExtentChng, and Mile Days from lines above
  DryingMetric <- dat_drying %>%
    dplyr::select(!X) %>% 
    filter(DryRM == 0 & Reach == reach) %>% 
    group_by(Reach, Date) %>% 
    summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
    tidyr::complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
    mutate(ExtentDry = replace_na(ExtentDry, 0), Year = year(Date), MnDay = format(Date, format = "%b%d")) %>% 
    mutate(ExtentChng = ExtentDry - lag(ExtentDry, default = ExtentDry[1])) %>% 
    ungroup() %>% 
    filter(between(month(Date),4,10)) %>% 
    select(Year, ExtentChng, MnDay) %>% 
    filter(MnDay != "Feb29") %>% 
    pivot_wider(names_from = Year, values_from = ExtentChng) %>% 
    column_to_rownames(var = "MnDay") %>% 
    as.matrix()
  
  #fda ####
  #1)get log abundance and put into named number ####
  Carcar_Isleta <- datfish %>% 
    filter(Species_Codes == species_code & Reach == reach) %>% 
    select(Year, LogMean) %>% 
    pull(LogMean, Year) #makes a named number using Year to name row
  
  ny <- length(Carcar_Isleta) #length of y data
  
#2) create functional covariate for predictor data ####

  #for irrigation season
ExtIsleta_basis <- create.bspline.basis(c(1,214), norder = 4, breaks = seq(1,214,step_size)) #don't add penalties or lambda to keep data information
#plot(ExtIsleta_basis)

ExtIsleta_smooth <- smooth.basis(seq(1,214,1), DryingMetric, ExtIsleta_basis)
ExtIsleta_smooth_fd <- ExtIsleta_smooth$fd

#plot(ExtIsleta_smooth_fd)

 #for the entire year
# ExtIsleta_basis <- create.bspline.basis(c(0365), norder = 4, breaks = seq(0,365,21)) #don't add penalties or lambda to keep data information
# plot(ExtIsleta_basis)
# 
# ExtIsleta_smooth <- smooth.basis(day.5, ExtIsleta_Yr, ExtIsleta_basis)
# ExtIsleta_smooth_fd <- ExtIsleta_smooth$fd
# 
# plot(ExtIsleta_smooth_fd)


#3) create list of functional covarites ####
ExtIsleta_list <- vector("list", 2)
ExtIsleta_list[[1]] <- rep(1,ny)
ExtIsleta_list[[2]] <- ExtIsleta_smooth_fd

#4) create basis list
conbasis <-  create.constant.basis(c(1,214)) #THIS HAS TO CHANGE WITH THE DATE RANGE ##
betabasis <- ExtIsleta_basis #used same basis as before

betalist1 <- vector("list", 2)
betalist1[[1]] <- conbasis
betalist1[[2]] <- betabasis

#5) Choose Smoothing Parameters using cross-validation ####
# 
loglam = seq(1,15,0.5)
nlam   = length(loglam)
SSE.CV = rep(NA,nlam)
for (ilam in 1:nlam) {
  print(paste("log lambda =", loglam[ilam]))
  lambda     = 10^(loglam[ilam])
  betalisti  = betalist1
  betalisti[[2]] = fdPar(betabasis, 2, lambda)
  fRegi          = fRegress.CV(Carcar_Isleta, ExtIsleta_list, betalisti)
  SSE.CV[ilam]   = fRegi$SSE.CV
}

# par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
# plot(loglam, SSE.CV, type="b", lwd=2,
#      xlab="log smoothing parameter lambda",
#      ylab="Cross-validation score", cex.lab=2,cex.axis=2)

#find the index of the minimum SSE.CV
min.index <- which.min(SSE.CV)
#corresponding loglam vlaue
best.loglam <- loglam[min.index]


return(list(Carcar_Isleta = Carcar_Isleta, ExtIsleta_list = ExtIsleta_list, best_loglam = best.loglam, betabasis=betabasis,
            betalist1 = betalist1, ExtIsleta_smooth = ExtIsleta_smooth, ny = ny))
}

#function to run regresssion ####
#6) Run regression with lambda 
run_analysis <- function(data){
  #Extract elements
  
  Carcar_Isleta <- data$Carcar_Isleta
  ExtIsleta_list <- data$ExtIsleta_list
  lambda1 <- data$best_loglam
  betabasis <- data$betabasis
  betalist1 <- data$betalist1
  ExtIsleta_smooth <- data$ExtIsleta_smooth
  ny <- data$ny
  
  
  lambda = 10^lambda1
  
  betafdPar  = fdPar(betabasis, 2, lambda)
  
  betalist2 <- betalist1
  betalist2[[2]] <- betafdPar  
  
  AnnCarcarExtIsleta <- fRegress(Carcar_Isleta, ExtIsleta_list, betalist2)
  
  #significance
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
  
  TF_Fratio <- Fratio2 > qFratio #if false no significant, if true significant
  Dif_Fratio <- round((Fratio2 - qFratio),3)
  
  # calculate the p-value
  pval <- round(1-pf(Fratio2,AnnCarcarExtIsleta$df-1,ny-AnnCarcarExtIsleta$df),3)
  # p-value is 0.12 indicating that x_i(t) does not have a significant effect on y_i
  
  # r-squared (1- sum residuals squared /total sum squares) 
  r2 <- round(1-(SSE2/SSE0),3)
  
  
  return(list(TF_Fratio = TF_Fratio, Fratio2 = Fratio2, qFratio = qFratio, pval = pval, r2 = r2, Dif_Fratio=Dif_Fratio, best_loglam = data$best_loglam))
}

reaches <- c("San Acacia", "Isleta")
species_codes <- c( "CARCAR", "CYPCAR" ,"CYPLUT" ,"GAMAFF" ,"HYBAMA", "ICTPUN", "PIMPRO", "PLAGRA")
step_size <- c(5)  # Example step sizes

results_table <- data.frame(Reach = character(), Species_Code = character(), step_size = numeric(), 
                            TF_Fratio = numeric(), Fratio2 = numeric(), qFratio = numeric(),
                            P_Value = numeric(), r2 = numeric(), best_loglam = numeric(),
                            lambda1 = numeric())

start.time <- Sys.time()
for (reach in reaches) {
  for (species_code in species_codes) {
    for (sz in step_size) {
      tryCatch({
        # Prepare data with step_size passed as an argument
        data <- prepare_data(reach, species_code, sz)
        
        # Run analysis
        analysis_results <- run_analysis(data)
        
        # Store results in the table
        results_table <- rbind(results_table, c(reach, species_code, sz,
                                                analysis_results$TF_Fratio,
                                                analysis_results$Fratio2,
                                                analysis_results$qFratio,
                                                analysis_results$pval,
                                                analysis_results$r2,
                                                analysis_results$Dif_Fratio,
                                                data$best_loglam))
      }, error = function(e) {
        cat("Error in processing:", reach, species_code, sz, "\n")
        print(e)
      })
    }
  }
}

# Adjust the results_table to include the step_size column
colnames(results_table) <- c("Reach_Extent", "Species_Code", "step_size", "TF_Fratio", "Fratio2", 
                             "qFratio", "P_Value", "R2", "Diff_Fratio", "lambda") #change Reach name 

write.csv(results_table, "FDA_Data/ExtentChange.csv")

end.time <- Sys.time()
time8 <- round(end.time - start.time,2) #14.4 min
library(beepr)
beep(1)

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
SigmaE. = sum(resid^2)/(ny-AnnCarcarExtIsleta$df)
SigmaE  = SigmaE.*diag(rep(1,ny))

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



#Read me ####
#modeling the effect of river drying metrics on fish cpue using
#hurdle models to be able to incorporate all of the zeros
#using Reach as an interaction because we know there are population differences between reaches
#not using Year as a random effect because annual variation is already in drying measure
#using site as a random effect because of spatial differences but testing this whether
#this is needed using loo

#Libraries ####
library(tidyverse)
library(lubridate)
library(brms)
library(beepr)
library(loo)

#Data ####
datfish_rm<- read.csv("Data/Processed/RGFishCPUE_RM.csv")
datdry <- read.csv("Data/Processed/RGDryCovariates_ByReach_Irrig.csv")

#Wrangle data ####
DatFishDry_RM <- datfish_rm %>% 
  left_join(datdry) %>% 
  mutate(Species_Codes = as.factor(Species_Codes), Year = as.factor(Year), Reach = as.factor(Reach),
         RM_Start = as.factor(RM_Start))

#Priors ####
get_prior(bf(CPUE_m ~  Species_Codes*Max_Extent*Reach + (1|Year) +(1|RM_Start), 
             hu~  Species_Codes*Max_Extent*Reach + (1|Year) +(1|RM_Start)),
          family = hurdle_lognormal(),
          data = DatFishDry_RM)

p1 <- c(set_prior("normal(0,10)", class = "b"))

#hurdle max extent #####
#running integer Pro, integer fish/100 m fish, max tree depth and 5 chains 
start.time <- Sys.time()
mod_maxextent_yr_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_Extent*Reach + (1|Year) +(1|RM_Start), 
                      hu~ 0+ Species_Codes*Max_Extent*Reach + (1|Year) +(1|RM_Start)),
                   family = hurdle_lognormal(),
                   prior = p1,
                   data=DatFishDry_RM,
                   chains = 4,
                   warmup = 500,
                   iter=4000,
                   sample_prior = TRUE,
                   cores = 4)

beep(1)
end.time <- Sys.time()
time1 <- round(end.time - start.time,2) 

saveRDS(mod_maxextent_yr_rm, "Models/FreshwaterMOdels/mod_maxextent_yr_rm.rds")

Reloo_mod_maxextent_yr_rm <- loo(mod_maxextent_yr_rm, reloo = T) 
saveRDS(Reloo_mod_maxextent_yr_rm, "Models/FreshwaterMOdels/Reloo_mod_maxextent_yr_rm.rds")
Reloo_maxextent_no_rm <- readRDS("Models/FreshwaterMOdels/Reloo_maxextent_no_rm.rds")
Reloo_maxextent_rm <- readRDS("Models/FreshwaterMOdels/Reloo_maxextent_rm.rds")

loo_compare(Reloo_maxextent_no_rm, Reloo_maxextent_rm, Reloo_mod_maxextent_yr_rm)

start.time <- Sys.time()
mod_Pro_maxextent_nosite<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMax_Extent*Reach , 
                               hu~ 0+ Species_Codes*ProMax_Extent*Reach),
                            family = hurdle_lognormal(),
                            prior = p1,
                            data=DatFishDry_RM_Pro,
                            chains = 3,
                            warmup = 500,
                            iter=4000,
                            sample_prior = TRUE,
                            cores = 4)

beep(1)
end.time <- Sys.time()
time1 <- round(end.time - start.time,2) 

#save models #
saveRDS(mod_Pro_maxextent, "Models/mod_Pro_maxextent.rds")

#hurdle mean extent #####

start.time <- Sys.time()
mod_Pro_meanextent<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMean_Extent*Reach + (1|Year) + (1|RM_Start), 
                      hu~ 0+ Species_Codes*ProMean_Extent*Reach + (1|Year) + (1|RM_Start)),
                   family = hurdle_lognormal(),
                   prior = p1,
                   data=DatFishDry_RM_Pro,
                   chains = 3,
                   warmup = 500,
                   iter=4000,
                   sample_prior = TRUE,
                   cores = 4)

beep(1)
end.time <- Sys.time()
time8 <- round(end.time - start.time,2) #14.4 min

#save models #
saveRDS(mod_Pro_meanextent, "Models/mod_Pro_meanextent.rds")

#hurdle sd extent #####
start.time <- Sys.time()
mod_Pro_sdextent<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProSD_Extent*Reach + (1|Year) + (1|RM_Start), 
                       hu~ 0+Species_Codes*ProSD_Extent*Reach + (1|Year) + (1|RM_Start)),
                    family = hurdle_lognormal(),
                    prior = p1,
                    data=DatFishDry_RM_Pro,
                    chains = 3,
                    warmup = 500,
                    iter=4000,
                    sample_prior = TRUE,
                    cores = 4)

beep(1)
end.time <- Sys.time()
time7 <- round(end.time - start.time,2) #15.8 min

#save models #
saveRDS(mod_Pro_sdextent, "Models/mod_Pro_sdextent.rds")

#hurdle max change #####
start.time <- Sys.time()
mod_Pro_maxchng<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMax_Change*Reach + (1|Year) + (1|RM_Start), 
                     hu~ 0+Species_Codes*ProMax_Change*Reach + (1|Year) + (1|RM_Start)),
                  family = hurdle_lognormal(),
                  prior = p1,
                  data=DatFishDry_RM_Pro,
                  chains = 3,
                  warmup = 500,
                  iter=4000,
                  sample_prior = TRUE,
                  cores = 4)

beep(1)
end.time <- Sys.time()
time6 <- round(end.time - start.time,2) #11.5 min
#save models #
saveRDS(mod_Pro_maxchng, "Models/mod_Pro_maxchng.rds")

#hurdle mean change #####
#DID NOT CONVERGE!
# start.time <- Sys.time()
# mod_Pro_meanchng<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMean_Change*Reach + (1|Year) + (1|RM_Start),
#                     hu~ 0+Species_Codes*ProMean_Change*Reach + (1|Year) + (1|RM_Start)),
#                  family = hurdle_lognormal(),
#                  prior = p1,
#                  data=DatFishDry_RM_Pro,
#                  chains = 3,
#                  warmup = 500,
#                  iter=4000,
#                  sample_prior = TRUE,
#                  cores = 4)
# 
# beep(1)
# end.time <- Sys.time()
# time5 <- round(end.time - start.time,2) #2.27 hrs
# #save models #
# saveRDS(mod_Pro_meanchng, "Models/mod_Pro_meanchng.rds")

#hurdle sd change #####
start.time <- Sys.time()
mod_Pro_sdchng<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProSD_Change*Reach + (1|Year) + (1|RM_Start), 
                     hu~ 0+Species_Codes*ProSD_Change*Reach + (1|Year) + (1|RM_Start)),
                  family = hurdle_lognormal(),
                  prior = p1,
                  data=DatFishDry_RM_Pro,
                  chains = 3,
                  warmup = 500,
                  iter=4000,
                  sample_prior = TRUE,
                  cores = 4)

beep(1)
end.time <- Sys.time()
time4 <- round(end.time - start.time,2) #13.05 min
#save models #
saveRDS(mod_Pro_sdchng, "Models/mod_Pro_sdchng.rds")




#hurdle max MD ####
#maxMD doesn't converge
start.time <- Sys.time()
mod_Pro_maxMD<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_ProDays*Reach + (1|Year) + (1|RM_Start),
                      hu~ 0+ Species_Codes*Max_ProDays*Reach + (1|Year) + (1|RM_Start)),
                   family = hurdle_lognormal(),
                   prior = p1,
                   data=DatFishDry_RM_Pro,
                   chains = 3,
                   warmup = 500,
                   iter=4000,
                   sample_prior = TRUE,
                   cores = 4)

beep(1)
end.time <- Sys.time()
time10 <- round(end.time - start.time,2) #36.83 mins

#save models #
saveRDS(mod_Pro_maxMD, "Models/mod_Pro_maxMD.rds")

#hurdle mean MD ####
start.time <- Sys.time()
mod_Pro_meanMD<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_ProDays*Reach + (1|Year) + (1|RM_Start), 
                  hu~ 0+Species_Codes*Mean_ProDays*Reach + (1|Year) + (1|RM_Start)),
               family = hurdle_lognormal(),
               prior = p1,
               data=DatFishDry_RM_Pro,
               chains = 3,
               warmup = 500,
               iter=4000,
               sample_prior = TRUE,
               cores = 4,
               control = list(max_treedepth = 13))

beep(1)
end.time <- Sys.time()
time11 <- round(end.time - start.time,2) #47 minutes

#save models #
saveRDS(mod_Pro_meanMD, "Models/mod_Pro_meanMD.rds")

#hurdle sd MD ####
start.time <- Sys.time()
mod_Pro_sdMD<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProSD_ProDays*Reach + (1|Year) + (1|RM_Start), 
                   hu~ 0+Species_Codes*ProSD_ProDays*Reach + (1|Year) + (1|RM_Start)),
                family = hurdle_lognormal(),
                prior = p1,
                data=DatFishDry_RM_Pro,
                chains = 3,
                warmup = 500,
                iter=4000,
                sample_prior = TRUE,
                cores = 4,
              control = list(max_treedepth = 13))

beep(1)
end.time <- Sys.time()
time13 <- round(end.time - start.time,2) #34 minutes

#save models #
saveRDS(mod_Pro_sdMD, "Models/mod_Pro_sdMD.rds")

# #model check ####
# #r-hat (need to be <1.01 indicating chains converged)
# summary(mod_meanMD) #okay - no divergent transitions for max, mean, sd extent or change, mean and sd MD
#                   #did not converge maxMD
# 
# #pp_check #
# #max,mean, sd extent or change have same okayness with a little bit of wonkyness
# pmod <- pp_check(mod_Pro_maxextent, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
# pmod+scale_x_continuous(trans="log10")
# 
# p <- pp_check(mod_maxextent)
# p+scale_x_continuous(trans="log10")
# 
# t <- pp_check(mod_sdextent, type = "boxplot")
# t+scale_y_continuous(trans = "log10")
# 
# t2 <- pp_check(mod_sdextent, type = "hist")
# t2+scale_x_continuous(trans="log10")
# 
# pp_check(mod_sdextent, type="stat_grouped", stat = "mean", group = "Species_Codes")
# 
# 
# #pp_check #
# #works in as much as just one peak with a little wonkyness, not any better or worse than nested
# pmod_mod_meanextent <- pp_check(mod_meanextent, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
# pmod_mod_meanextent+scale_x_continuous(trans="log10")
# 
# p <- pp_check(mod_maxextent)
# p+scale_x_continuous(trans="log10")
# 
# 
# #Read in models ####
MaxExtent <- readRDS("Models/mod_Pro_maxextent.rds")
MeanExtent <- readRDS("Models/mod_Pro_meanextent.rds")
SDExtent <- readRDS("Models/mod_Pro_sdextent.rds")

MaxChng <- readRDS("Models/mod_Pro_maxchng.rds")
#MeanChng <- readRDS("Models/Mean_Change.rds")
SDChng <- readRDS("Models/mod_Pro_sdchng.rds")

MaxMD <- readRDS("Models/mod_Pro_maxMD.rds")
MeanMD <- readRDS("Models/mod_Pro_meanMD.rds")
SDMD <- readRDS("Models/mod_Pro_sdMD.rds")
# 
# #Loo ####
#  #moment_match = true breaks R
# 
LooMaxExtent <- loo(MaxExtent, save_psis = T)  #1 observation pareto_k >0.7
LooMeanExtent <- loo(MeanExtent, save_psis = T) #1 observation pareto_k >0.7

LooSDExtent <- loo(SDExtent, save_psis = T) #no problems
saveRDS(LooSDExtent, "Models/LooSDExtent.rds")

LooMaxChng <- loo(MaxChng, save_psis = T) #3 observation pareto_k >0.7
LooSDChng <- loo(SDChng, save_psis = T) #3 observation pareto_k >0.7

LooMaxMD <- loo(MaxMD, save_psis = T) #1 observation pareto_k >0.7
LooMeanMD <- loo(MeanMD, save_psis = T) #1 observation pareto_k >0.7
LooSDMD <- loo(SDMD, save_psis = T) #1 observation pareto_k >0.7

# 
#   # Let's you see what observation are bad and how many in each category
# print(LooSDMD)
# plot(LooSDMD)
# 
# #reloo ####
# #set cores 
options(mc.cores = 4)

start.time <- Sys.time()
# Reloo_MaxExtent_Pro <- loo(MaxExtent, reloo = T) #reloo to run models without the bad observations; 3.38 hrs; worked
#   saveRDS(Reloo_MaxExtent_Pro, "Models/Reloo_MaxExtent_Pro.rds")
# 
# Reloo_MeanExtent_Pro <- loo(MeanExtent, reloo = T) #reloo to run models without the bad observations; <1 min with 4 cores; worked
#   saveRDS(Reloo_MeanExtent_Pro, "Models/Reloo_MeanExtent_Pro.rds")

# Reloo_MaxChng_Pro <- loo(MaxChng, reloo = T) #reloo to run models without the bad observations: 6.5 hrs; worked
  # saveRDS(Reloo_MaxChng_Pro, "Models/Reloo_MaxChng_Pro.rds")

# Reloo_MeanChng <- loo(MeanChng, reloo = T) #base model doesn't converge
# saveRDS(Reloo_MeanChng, "Models/Reloo_MeanChng.rds")

# Reloo_SDChng_Pro <- loo(SDChng, reloo = T) #reloo to run models without the bad observations: 24 mins; worked
#   saveRDS(Reloo_SDChng_Pro, "Models/Reloo_SDChn_Prog.rds")

# Reloo_MaxMD_Pro <- loo(MaxMD, reloo = T) # 1 hr
#   saveRDS(Reloo_MaxMD_Pro, "Models/Reloo_MaxMD_Pro.rds")

# Reloo_MeanMD_Pro <- loo(MeanMD, reloo = T) #reloo to run models without the bad observations; 3 hrs with 4 cores; worked
#   saveRDS(Reloo_MeanMD_Pro, "Models/Reloo_MeanMD_Pro.rds")

# Reloo_SDMD_Pro <- loo(SDMD, reloo = T) #reloo to run models without the bad observations; 2 hrs with 4 cores; worked
#   saveRDS(Reloo_SDMD_Pro, "Models/Reloo_SDMD_Pro.rds")
  
end.time <- Sys.time()
round(end.time - start.time,2)
# 
# #didn't need to reloo SDExtent because it had no problems 
# 
# print(Reloo_MaxExtent) #use print to see if any pareto k >0.7
# 
# #Read in reloo models ####
Reloo_MaxExtent <- readRDS("Models/Reloo_MaxExtent_Pro.rds")
Reloo_MeanExtent <- readRDS("Models/Reloo_MeanExtent_Pro.rds")

LooSDExtent <- readRDS("Models/LooSDExtent.rds") 

Reloo_MaxChng <- readRDS("Models/Reloo_MaxChng_Pro.rds")
Reloo_SDChng <- readRDS("Models/Reloo_SDChn_Prog.rds")

# 
Reloo_MaxMD <- readRDS("Models/Reloo_MaxMD_Pro.rds")
Reloo_MeanMD <- readRDS("Models/Reloo_MeanMD_Pro.rds")
RelooSDMD <- readRDS("Models/Reloo_SDMD_Pro.rds")
 
# 
# #compare models loo #####
#   #Extent
# loo_compare(Reloo_MaxExtent, Reloo_MeanExtent, Reloo_SDExtent)
# 
#   #Change
# loo_compare(Reloo_MaxChng, Reloo_MeanChng, Reloo_SDChng)
# 
#   #Mile Days
# loo_compare(Reloo_MeanMD, LooSDMD)
# 
#   #all
loo_compare(Reloo_MaxExtent, Reloo_MeanExtent, LooSDExtent, Reloo_MaxChng, Reloo_SDChng,
            Reloo_MaxMD, Reloo_MeanMD, RelooSDMD)

#results in p_waic that says to use loo
# WAIC(MaxExtent)
# WAIC(MeanExtent) 
# WAIC(SDExtent) 
# WAIC(MaxChng)
# WAIC(SDChng) 
# WAIC(MaxMD) 
# WAIC(MeanMD) 
# WAIC(SDMD) 


# 
# #Predicting ####
# #https://www.google.com/search?sca_esv=594252889&rlz=1C1GCEA_enUS1082US1082&tbm=vid&q=understanding+hurdle+model+output&sa=X&ved=2ahUKEwjdxO7zw7KDAxWtMUQIHbIBBBQQ8ccDegQIDBAJ&biw=1920&bih=953&dpr=1#fpstate=ive&vld=cid:1531d1f9,vid:7tYbxkI1FNA,st:0
# ggpredict(mod, terms = ..., type = "zi_prob")
# 
# conditional_effects(MaxExtent)
# 
# #general drying trends analysis
# 
# library(fpp3)
# library(tsibbledata)
# library(fable)
# library(forcats)
# 
# maxExtent_IS <- datdry %>% 
#   filter(Reach == "Isleta") %>% 
#   select(Max_Extent) %>% 
#   ts(start = 2010, frequency = 1)
# 
# plot(maxExtent_IS)
# 
# # Calculate the 3-year moving average
# moving_avg <- stats::filter(maxExtent_IS, rep(1/3, 3), sides = 2)
# 
# # Plot the original time series and the moving average
# plot(maxExtent_IS, type = "l", col = "blue", lwd = 2, xlab = "Year", ylab = "GDP Growth (%)", main = "3-Year Moving Average")
# lines(moving_avg, col = "red", lwd = 2)
# legend("topright", legend = c("Original", "3-Year MA"), col = c("blue", "red"), lty = 1, lwd = 2)

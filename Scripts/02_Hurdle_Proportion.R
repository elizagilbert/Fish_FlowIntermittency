#Read me ####
#predicting october fish densities using proportion of habitat dried
#use RGFishCPUE_RM, which is species >10% and not >5% as 5% won't converge

#Libraries ####
library(tidyverse)
library(lubridate)
library(brms)
library(beepr)
library(loo)

#Data ####
datfish_rm<- read.csv("Data/Processed/RGFishCPUE_RM.csv")
datdry_pro <- read.csv("Data/Processed/ProDryCovariates_ByReach.csv")

#Wrangle data ####
DatFishDry_RM_Pro <- datfish_rm %>% 
  left_join(datdry_pro) %>% 
  mutate(Species_Codes = as.factor(Species_Codes), Year = as.factor(Year), Reach = as.factor(Reach),
         RM_Start = as.factor(RM_Start))

#Priors ####
get_prior(bf(CPUE_m ~  Species_Codes*ProMax_Extent*Reach + (1|RM_Start), 
             hu~  Species_Codes*ProMax_Extent*Reach + (1|RM_Start)),
          family = hurdle_lognormal(),
          data = DatFishDry_RM_Pro)

get_prior(bf(CPUE_m ~  Species_Codes*ProMax_Extent*Reach, 
             hu~  Species_Codes*ProMax_Extent*Reach),
          family = hurdle_lognormal(),
          data = DatFishDry_RM_Pro)

p1 <- c(set_prior("normal(0,10)", class = "b"))

#hurdle max extent #####
start.time <- Sys.time()
mod_Pro_maxextent<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMax_Extent*Reach + (1|RM_Start),
                          hu~ 0+ Species_Codes*ProMax_Extent*Reach + (1|RM_Start)),
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
time1 <- round(end.time - start.time,2) #15 minutes

start.time <- Sys.time()
mod_Pro_maxextent_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMax_Extent*Reach,
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
time2 <- round(end.time - start.time,2) #16 min
#There were 3 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 2: Examine the pairs() plot to diagnose sampling problems

#save models #
saveRDS(mod_Pro_maxextent, "HurdleModels/Proportion/mod_Pro_maxextent_rm.rds")
saveRDS(mod_Pro_maxextent_no_rm, "HurdleModels/Proportion/mod_Pro_maxextent_no_rm.rds")

#hurdle mean extent #####

start.time <- Sys.time()
mod_Pro_meanextent_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMean_Extent*Reach + (1|RM_Start), 
                              hu~ 0+ Species_Codes*ProMean_Extent*Reach + (1|RM_Start)),
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
time8 <- round(end.time - start.time,2) #1.8 hrs

start.time <- Sys.time()
mod_Pro_meanextent_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMean_Extent*Reach, 
                                 hu~ 0+ Species_Codes*ProMean_Extent*Reach),
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
time9 <- round(end.time - start.time,2) #17 min

#save models #
saveRDS(mod_Pro_meanextent_rm, "HurdleModels/Proportion/mod_Pro_meanextent_rm.rds")
saveRDS(mod_Pro_meanextent_no_rm, "HurdleModels/Proportion/mod_Pro_meanextent_no_rm.rds")

#hurdle sd extent #####
start.time <- Sys.time()
mod_Pro_sdextent_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProSD_Extent*Reach + (1|RM_Start), 
                            hu~ 0+Species_Codes*ProSD_Extent*Reach  + (1|RM_Start)),
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
time7 <- round(end.time - start.time,2) #14 min

start.time <- Sys.time()
mod_Pro_sdextent_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProSD_Extent*Reach, 
                               hu~ 0+Species_Codes*ProSD_Extent*Reach),
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
time1 <- round(end.time - start.time,2) #14 min

#save models #
saveRDS(mod_Pro_sdextent_rm, "HurdleModels/Proportion/mod_Pro_sdextent_rm.rds")
saveRDS(mod_Pro_sdextent_no_rm, "HurdleModels/Proportion/mod_Pro_sdextent_no_rm.rds")

#hurdle max change #####
start.time <- Sys.time()
mod_Pro_maxchng_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMax_Change*Reach + (1|RM_Start), 
                           hu~ 0+Species_Codes*ProMax_Change*Reach + (1|RM_Start)),
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
time1 <- round(end.time - start.time,2) #1.17 hrs

start.time <- Sys.time()
mod_Pro_maxchng_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMax_Change*Reach, 
                              hu~ 0+Species_Codes*ProMax_Change*Reach),
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
time7 <- round(end.time - start.time,2) #12 min
#save models #
saveRDS(mod_Pro_maxchng_rm, "HurdleModels/Proportion/mod_Pro_maxchng_rm.rds")
saveRDS(mod_Pro_maxchng_no_rm, "HurdleModels/Proportion/mod_Pro_maxchng_no_rm.rds")

#hurdle mean change #####
start.time <- Sys.time()
mod_Pro_meanchng_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMean_Change*Reach + (1|RM_Start),
                            hu~ 0+Species_Codes*ProMean_Change*Reach + (1|RM_Start)),
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
time5 <- round(end.time - start.time,2) #2.46

start.time <- Sys.time()
mod_Pro_meanchng_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProMean_Change*Reach,
                               hu~ 0+Species_Codes*ProMean_Change*Reach),
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
time6 <- round(end.time - start.time,2) #1.55
#save models #
saveRDS(mod_Pro_meanchng_rm, "HurdleModels/Proportion/mod_Pro_meanchng_rm.rds")
saveRDS(mod_Pro_meanchng_no_rm, "HurdleModels/Proportion/mod_Pro_meanchng_no_rm.rds")

#hurdle sd change #####
start.time <- Sys.time()
mod_Pro_sdchng_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProSD_Change*Reach + (1|RM_Start), 
                          hu~ 0+Species_Codes*ProSD_Change*Reach + (1|RM_Start)),
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
time4 <- round(end.time - start.time,2) #30 min

start.time <- Sys.time()
mod_Pro_sdchng_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProSD_Change*Reach, 
                             hu~ 0+Species_Codes*ProSD_Change*Reach),
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
time9 <- round(end.time - start.time,2) #23
#save models #
saveRDS(mod_Pro_sdchng_rm, "HurdleModels/Proportion/mod_Pro_sdchng_rm.rds")
saveRDS(mod_Pro_sdchng_no_rm, "HurdleModels/Proportion/mod_Pro_sdchng_no_rm.rds")




#hurdle max MD ####
#maxMD doesn't converge
start.time <- Sys.time()
mod_Pro_maxMD_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_ProDays*Reach  + (1|RM_Start),
                         hu~ 0+ Species_Codes*Max_ProDays*Reach + (1|RM_Start)),
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
time10 <- round(end.time - start.time,2) 

start.time <- Sys.time()
mod_Pro_maxMD_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_ProDays*Reach,
                            hu~ 0+ Species_Codes*Max_ProDays*Reach),
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
saveRDS(mod_Pro_maxMD_rm, "HurdleModels/Proportion/mod_Pro_maxMD_rm.rds")
saveRDS(mod_Pro_maxMD_no_rm, "HurdleModels/Proportion/mod_Pro_maxMD_no_rm.rds")

#hurdle mean MD ####
start.time <- Sys.time()
mod_Pro_meanMD_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_ProDays*Reach  + (1|RM_Start), 
                          hu~ 0+Species_Codes*Mean_ProDays*Reach + (1|RM_Start)),
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
time11 <- round(end.time - start.time,2) 

start.time <- Sys.time()
mod_Pro_meanMD_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_ProDays*Reach, 
                             hu~ 0+Species_Codes*Mean_ProDays*Reach),
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
time2 <- round(end.time - start.time,2) 

#save models #
saveRDS(mod_Pro_meanMD_rm, "HurdleModels/Proportion/mod_Pro_meanMD_rm.rds")
saveRDS(mod_Pro_meanMD_no_rm, "HurdleModels/Proportion/mod_Pro_meanMD_no_rm.rds")

#hurdle sd MD ####
start.time <- Sys.time()
mod_Pro_sdMD_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProSD_ProDays*Reach + (1|RM_Start), 
                        hu~ 0+Species_Codes*ProSD_ProDays*Reach + (1|RM_Start)),
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
time13 <- round(end.time - start.time,2) 

start.time <- Sys.time()
mod_Pro_sdMD_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*ProSD_ProDays*Reach, 
                           hu~ 0+Species_Codes*ProSD_ProDays*Reach),
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
time3 <- round(end.time - start.time,2) 

#save models #
saveRDS(mod_Pro_sdMD_rm, "HurdleModels/Proportion/mod_Pro_sdMD_rm.rds")
saveRDS(mod_Pro_sdMD_no_rm, "HurdleModels/Proportion/mod_Pro_sdMD_no_rm.rds")

#read in models ####
  #extent
MaxExtent_rm <- readRDS("HurdleModels/Proportion/mod_Pro_maxextent_rm.rds") #okay r-hat, no divergent transitions, ppcheck generally okay
MaxExtent_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_maxextent_no_rm.rds") #okay r-hat, no divergent transitions, ppcheck generally okay

MeanExtent_rm <- readRDS("HurdleModels/Proportion/mod_Pro_meanextent_rm.rds") #okay r-hat, no divergent transitions, ppcheck generally okay
MeanExtent_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_meanextent_no_rm.rds") #okay r-hat, no divergent transitions, ppcheck generally okay

SDExtent_rm <- readRDS("HurdleModels/Proportion/mod_Pro_sdextent_rm.rds") #okay r-hat, no divergent transitions, ppcheck generally okay
SDExtent_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_sdextent_no_rm.rds") #okay r-hat, no divergent transitions, ppcheck generally okay

 #change
MaxChange_rm <- readRDS("HurdleModels/Proportion/mod_Pro_maxchng_rm.rds") 
MaxChange_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_maxchng_no_rm.rds") 

MeanChange_rm <- readRDS("HurdleModels/Proportion/mod_Pro_meanchng_rm.rds") 
MeanChange_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_meanchng_no_rm.rds") 

SDChange_rm <- readRDS("HurdleModels/Proportion/mod_Pro_sdchng_rm.rds") 
SDChange_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_sdchng_no_rm.rds") 

  #Mile days (MD)
MaxMD_rm <- readRDS("HurdleModels/Proportion/mod_Pro_maxMD_rm.rds") 
MaxMD_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_maxMD_no_rm.rds") 

MeanMD_rm <- readRDS("HurdleModels/Proportion/mod_Pro_meanMD_rm.rds") 
MeanMD_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_meanMD_no_rm.rds") 

SDMD_rm <- readRDS("HurdleModels/Proportion/mod_Pro_sdMD_rm.rds") 
SDMD_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_meanMD_no_rm.rds") 

#model check ####
#r-hat (need to be <1.01 indicating chains converged)
summary(SDExtent_no_rm) 

#pp_check #
pmod <- pp_check(SDExtent_no_rm, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pmod+scale_x_continuous(trans="log10")

p <- pp_check(SDExtent_no_rm)
p+scale_x_continuous(trans="log10")

t <- pp_check(SDExtent_no_rm, type = "boxplot")
t+scale_y_continuous(trans = "log10")

t2 <- pp_check(SDExtent_no_rm, type = "hist")
t2+scale_x_continuous(trans="log10")

pp_check(SDExtent_no_rm, type="stat_grouped", stat = "mean", group = "Species_Codes")
 
#Loo ####
#"moment_match = true" breaks R
   #extent
LooMaxExtent_rm <- loo(MaxExtent_rm, save_psis = T)  #3 observation pareto_k >0.7
LooMaxExtent_no_rm <- loo(MaxExtent_no_rm, save_psis = T)  #2 observation pareto_k >0.7

LooMeanExtent_rm <- loo(MeanExtent_rm, save_psis = T) #no problems
saveRDS(LooMeanExtent_rm, "HurdleModels/Proportion/Reloo/LooMeanExtent_rm.rds")
LooMeanExtent_no_rm <- loo(MeanExtent_no_rm, save_psis = T)  #1 observation pareto_k >0.7

LooSDExtent_rm <- loo(SDExtent_rm, save_psis = T)  #1 observation pareto_k >0.7
LooSDExtent_no_rm <- loo(SDExtent_no_rm, save_psis = T)  #no problems
saveRDS(LooSDExtent_no_rm, "HurdleModels/Proportion/Reloo/LooSDExtent_no_rm.rds")

#change
LooMaxChange_rm <- loo(MaxChange_rm, save_psis = T)  # 3 observation pareto k>0.7
LooMaxChange_no_rm <- loo(MaxChange_no_rm, save_psis = T)  # 3 observation pareto k>0.7

LooMeanChange_rm <- loo(MeanChange_rm, save_psis = T) # 1 observation pareto k>0.7
LooMeanChange_no_rm <- loo(MeanChange_no_rm, save_psis = T)  #2 observations pareto k>0.7

LooSDChange_rm <- loo(SDChange_rm, save_psis = T)  # 2 observation pareto k>0.7
LooSDChange_no_rm <- loo(SDChange_no_rm, save_psis = T)   # 3 observation pareto k>0.7

#MileDays (MD)
LooMaxMD_rm <- loo(MaxMD_rm, save_psis = T)  # 1 observation pareto k>0.7
LooMaxMD_no_rm <- loo(MaxMD_no_rm, save_psis = T)  # 1 observation pareto k>0.7

LooMeanMD_rm <- loo(MeanMD_rm, save_psis = T) # 2 observation pareto k>0.7
LooMeanMD_no_rm <- loo(MeanMD_no_rm, save_psis = T) # 2 observation pareto k>0.7 

LooSDMD_rm <- loo(SDMD_rm, save_psis = T)   # 2 observation pareto k>0.7
LooSDMD_no_rm <- loo(SDMD_no_rm, save_psis = T)   # 2 observation pareto k>0.7

# see what observation are bad and how many in each category
print(LooMaxExtent_rm)
plot(LooMaxExtent_rm)
# 
#reloo ####
#this is rerunning model removing observation with pareto_k >0.7
#set cores 
options(mc.cores = 4)
 #extent
start.time <- Sys.time()
Reloo_LooMaxExtent_rm <- loo(MaxExtent_rm, reloo = T) #
Reloo_LooMaxExtent_no_rm <- loo(MaxExtent_no_rm, reloo = T) #
Reloo_LooMeanExtent_no_rm <- loo(MeanExtent_no_rm, reloo = T) 
Reloo_LooSDExtent_rm <- loo(SDExtent_rm, reloo = T) #

saveRDS(Reloo_LooMaxExtent_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMaxExtent_rm.rds")
saveRDS(Reloo_LooMaxExtent_no_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMaxExtent_no_rm.rds")
saveRDS(Reloo_LooMeanExtent_no_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMeanExtent_no_rm.rds")
saveRDS(Reloo_LooSDExtent_rm, "HurdleModels/Proportion/Reloo/Reloo_LooSDExtent_rm.rds")

end.time <- Sys.time()
round(end.time - start.time,2)
  #change
start.time <- Sys.time()
Reloo_LooMaxChange_rm <- loo(MaxChange_rm, reloo = T) #
Reloo_LooMaxChange_no_rm <- loo(MaxChange_no_rm, reloo = T) #
Reloo_LooMeanChange_rm <- loo(MeanChange_rm, reloo = T) 
Reloo_LooMeanChange_no_rm <- loo(MeanChange_no_rm, reloo = T) 
Reloo_LooSDChange_rm <- loo(SDChange_rm, reloo = T) #
Reloo_LooSDChange_no_rm <- loo(SDChange_no_rm, reloo = T) #
end.time <- Sys.time()
round(end.time - start.time,2)

saveRDS(Reloo_LooMaxChange_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMaxChange_rm.rds")
saveRDS(Reloo_LooMaxChange_no_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMaxChange_no_rm.rds")
saveRDS(Reloo_LooMeanChange_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMeanChange_rm.rds")
saveRDS(Reloo_LooMeanChange_no_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMeanChange_no_rm.rds")
saveRDS(Reloo_LooSDChange_rm, "HurdleModels/Proportion/Reloo/Reloo_LooSDChange_rm.rds")
saveRDS(Reloo_LooSDChange_no_rm, "HurdleModels/Proportion/Reloo/Reloo_LooSDChange_no_rm.rds")


  #Mile Days (MD)
start.time <- Sys.time()
Reloo_LooMaxMD_rm <- loo(MaxMD_rm, reloo = T) #
Reloo_LooMaxMD_no_rm <- loo(MaxMD_no_rm, reloo = T) #
Reloo_LooMeanMD_rm <- loo(MeanMD_rm, reloo = T) 
Reloo_LooMeanMD_no_rm <- loo(MeanMD_no_rm, reloo = T) 
Reloo_LooSDMD_rm <- loo(SDMD_rm, reloo = T) #
Reloo_LooSDMD_no_rm <- loo(SDMD_no_rm, reloo = T) #

saveRDS(Reloo_LooMaxMD_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMaxMD_rm.rds")
saveRDS(Reloo_LooMaxMD_no_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMaxMD_no_rm.rds")
saveRDS(Reloo_LooMeanMD_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMeanMD_rm.rds")
saveRDS(Reloo_LooMeanMD_no_rm, "HurdleModels/Proportion/Reloo/Reloo_LooMeanMD_no_rm.rds")
saveRDS(Reloo_LooSDMD_rm, "HurdleModels/Proportion/Reloo/Reloo_LooSDMD_rm.rds")
saveRDS(Reloo_LooSDMD_no_rm, "HurdleModels/Proportion/Reloo/Reloo_LooSDMD_no_rm.rds")

end.time <- Sys.time()
round(end.time - start.time,2)
# 
# #didn't need to reloo SDExtent because it had no problems 
# 
# print(Reloo_MaxExtent) #use print to see if any pareto k >0.7
# 
#Read in reloo models ####
  #extent
Reloo_MaxExtent_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMaxExtent_rm.rds")
Reloo_MaxExtent_no_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMaxExtent_no_rm.rds")

LooMeanExtent_rm <- readRDS("HurdleModels/Proportion/Reloo/LooMeanExtent_rm.rds")
Reloo_MeanExtent_no_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMeanExtent_no_rm.rds")

Reloo_SDExtent_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooSDExtent_rm.rds")
LooSDExtent_no_rm <- readRDS("HurdleModels/Proportion/Reloo/LooSDExtent_no_rm.rds")
 
 #check pareto k
print(Reloo_MaxExtent_rm)
print(Reloo_MaxExtent_no_rm)
print(LooMeanExtent_rm)
print(Reloo_MeanExtent_no_rm)
print(Reloo_SDExtent_rm)
print(LooSDExtent_no_rm)

  #change
Reloo_MaxChange_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMaxChange_rm.rds")
Reloo_MaxChange_no_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMaxChange_no_rm.rds")

Reloo_MeanChange_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMeanChange_rm.rds")
Reloo_MeanChange_no_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMeanChange_no_rm.rds")

Reloo_SDChange_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooSDChange_rm.rds")
Reloo_SDChange_no_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooSDChange_no_rm.rds")

#check pareto k
print(Reloo_MaxChange_rm)
print(Reloo_MaxChange_no_rm)
print(Reloo_MeanChange_rm)
print(Reloo_MeanChange_no_rm)
print(Reloo_SDChange_rm)
print(Reloo_SDChange_no_rm)

  #mile days (MD)
Reloo_MaxMD_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMaxMD_rm.rds")
Reloo_MaxMD_no_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMaxMD_no_rm.rds")

Reloo_MeanMD_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMeanMD_rm.rds")
Reloo_MeanMD_no_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooMeanMD_no_rm.rds")

Reloo_SDMD_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooSDMD_rm.rds")
Reloo_SDMD_no_rm <- readRDS("HurdleModels/Proportion/Reloo/Reloo_LooSDMD_no_rm.rds")

#check pareto k
print(Reloo_MaxMD_rm)
print(Reloo_MaxMD_no_rm)
print(Reloo_MeanMD_rm)
print(Reloo_MeanMD_no_rm)
print(Reloo_SDMD_rm)
print(Reloo_SDMD_no_rm)

#compare models loo #####

folder_path <- "HurdleModels/Proportion/Reloo/"
file_names <- list.files(path = folder_path, full.names = F)

loo_compare(LooMeanExtent_rm,       LooSDExtent_no_rm,      Reloo_MaxChange_no_rm, 
            Reloo_MaxChange_rm,     Reloo_MaxExtent_no_rm,  Reloo_MaxExtent_rm,    
            Reloo_MaxMD_no_rm,      Reloo_MaxMD_rm,         Reloo_MeanChange_no_rm,
            Reloo_MeanChange_rm,    Reloo_MeanExtent_no_rm, Reloo_MeanMD_no_rm,    
            Reloo_MeanMD_rm,        Reloo_SDChange_no_rm,   Reloo_SDChange_rm,     
            Reloo_SDExtent_rm,      Reloo_SDMD_no_rm,       Reloo_SDMD_rm)

#top model is SDMD with and without rm, all others have ELPD differences with
#respect to standard error >4 and poor predictive performance

#Using WAIC on models themselves says p_waic estimates >0.4 recommend using loo


# 
#Predicting ####
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

#Read me ####
<<<<<<< HEAD
#modeling the effect of river drying metrics on fish cpue using
#hurdle models to be able to incorporate all of the zeros
#using Reach as an interaction because we know there are population differences between reaches
#not using Year as a random effect because annual variation is already in drying measure
#using site as a random effect because of spatial differences but testing this whether
#this is needed using loo
=======
#Purpose of this script is to use model comparison to determine which river drying
#regime metric best predicts October fish densities in the Rio Grande
#also assessing whether using site as a random effect is needed or can be removed.
#in a different script tried to model response using proportion of reach dried but those
#models failed to converge in a strong way

>>>>>>> origin/main

#Libraries ####
library(tidyverse)
library(lubridate)
library(brms)
library(beepr)
library(loo)

#Data ####
<<<<<<< HEAD
datfish_rm<- read.csv("Data/Processed/RGFishCPUE_RM.csv")
datdry <- read.csv("Data/Processed/RGDryCovariates_ByReach_Irrig.csv")
=======
datfish_rm<- read.csv("Data/Processed/Greater5Per_RGFishCPUE_RM.csv")
datdry <- read.csv("Data/Processed/RGDryCovariates_ByReach_Irrig.csv")

>>>>>>> origin/main

#Wrangle data ####
DatFishDry_RM <- datfish_rm %>% 
  left_join(datdry) %>% 
  mutate(Species_Codes = as.factor(Species_Codes), Year = as.factor(Year), Reach = as.factor(Reach),
         RM_Start = as.factor(RM_Start))

#Priors ####
<<<<<<< HEAD
get_prior(bf(CPUE_m ~  Species_Codes*Max_Extent*Reach + (1|Year) +(1|RM_Start), 
             hu~  Species_Codes*Max_Extent*Reach + (1|Year) +(1|RM_Start)),
          family = hurdle_lognormal(),
          data = DatFishDry_RM)
=======
get_prior(bf(CPUE_m ~  Species_Codes*Max_Extent*Reach + (1|RM_Start), 
             hu~  Species_Codes*Max_Extent*Reach + (1|RM_Start)),
          family = hurdle_lognormal(),
          data = DatFishDry_RM)

get_prior(bf(CPUE_m ~  Species_Codes*Max_Extent*Reach, 
             hu~  Species_Codes*Max_Extent*Reach),
          family = hurdle_lognormal(),
          data = DatFishDry_RM)
>>>>>>> origin/main

p1 <- c(set_prior("normal(0,10)", class = "b"))

#hurdle max extent #####
#running integer Pro, integer fish/100 m fish, max tree depth and 5 chains 
start.time <- Sys.time()
<<<<<<< HEAD
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

=======
mod_maxextent_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_Extent*Reach + (1|RM_Start), 
                           hu~ 0+ Species_Codes*Max_Extent*Reach + (1|RM_Start)),
                        family = hurdle_lognormal(),
                        prior = p1,
                        data=DatFishDry_RM,
                        chains = 3,
                        warmup = 500,
                        iter=4000,
                        sample_prior = TRUE,
                        cores = 4)
>>>>>>> origin/main
beep(1)
end.time <- Sys.time()
time1 <- round(end.time - start.time,2) 

<<<<<<< HEAD
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
=======
start.time <- Sys.time()
mod_maxextent_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_Extent*Reach, 
                            hu~ 0+ Species_Codes*Max_Extent*Reach),
                         family = hurdle_lognormal(),
                         prior = p1,
                         data=DatFishDry_RM,
                         chains = 3,
                         warmup = 500,
                         iter=4000,
                         sample_prior = TRUE,
                         cores = 4)
beep(1)
end.time <- Sys.time()
time2 <- round(end.time - start.time,2) 
>>>>>>> origin/main

#save models #
saveRDS(mod_maxextent_rm, "Models/mod_maxextent_rm.rds")
saveRDS(mod_maxextent_no_rm, "Models/mod_maxextent_no_rm.rds")

#hurdle mean extent #####
start.time <- Sys.time()
mod_meanextent_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_Extent*Reach  + (1|RM_Start), 
                      hu~ 0+ Species_Codes*Mean_Extent*Reach + (1|RM_Start)),
                   family = hurdle_lognormal(),
                   prior = p1,
                   data=DatFishDry_RM,
                   chains = 3,
                   warmup = 500,
                   iter=4000,
                   sample_prior = TRUE,
                   cores = 4)

beep(1)
end.time <- Sys.time()
time8 <- round(end.time - start.time,2) 

start.time <- Sys.time()
mod_meanextent_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_Extent*Reach, 
                          hu~ 0+ Species_Codes*Mean_Extent*Reach),
                       family = hurdle_lognormal(),
                       prior = p1,
                       data=DatFishDry_RM,
                       chains = 3,
                       warmup = 500,
                       iter=4000,
                       sample_prior = TRUE,
                       cores = 4)
beep(1)
end.time <- Sys.time()
time9 <- round(end.time - start.time,2)

#save models #
saveRDS(mod_meanextent_rm, "Models/mod_meanextent_rm.rds")
saveRDS(mod_meanextent_no_rm, "Models/mod_meanextent_no_rm.rds")

#hurdle sd extent #####
start.time <- Sys.time()
mod_sdextent_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*SD_Extent*Reach + (1|RM_Start), 
                       hu~ 0+Species_Codes*SD_Extent*Reach  + (1|RM_Start)),
                    family = hurdle_lognormal(),
                    prior = p1,
                    data=DatFishDry_RM,
                    chains = 3,
                    warmup = 500,
                    iter=4000,
                    sample_prior = TRUE,
                    cores = 4)

beep(1)
end.time <- Sys.time()
time7 <- round(end.time - start.time,2) 

start.time <- Sys.time()
mod_sdextent_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*SD_Extent*Reach, 
                     hu~ 0+Species_Codes*SD_Extent*Reach),
                  family = hurdle_lognormal(),
                  prior = p1,
                  data=DatFishDry_RM,
                  chains = 3,
                  warmup = 500,
                  iter=4000,
                  sample_prior = TRUE,
                  cores = 4)
beep(1)
end.time <- Sys.time()
time5 <- round(end.time - start.time,2) 

#save models #
saveRDS(mod_sdextent_rm, "Models/mod_sdextent_rm.rds")
saveRDS(mod_sdextent_no_rm, "Models/mod_sdextent_no_rm.rds")

#hurdle max change #####
start.time <- Sys.time()
mod_maxchng_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_Change*Reach + (1|RM_Start), 
                     hu~ 0+Species_Codes*Max_Change*Reach  + (1|RM_Start)),
                  family = hurdle_lognormal(),
                  prior = p1,
                  data=DatFishDry_RM,
                  chains = 3,
                  warmup = 500,
                  iter=4000,
                  sample_prior = TRUE,
                  cores = 4)
beep(1)
end.time <- Sys.time()
time6 <- round(end.time - start.time,2)

start.time <- Sys.time()
mod_maxchng_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_Change*Reach, 
                    hu~ 0+Species_Codes*Max_Change*Reach),
                 family = hurdle_lognormal(),
                 prior = p1,
                 data=DatFishDry_RM,
                 chains = 3,
                 warmup = 500,
                 iter=4000,
                 sample_prior = TRUE,
                 cores = 4)
beep(1)
end.time <- Sys.time()
time7 <- round(end.time - start.time,2)

#save models #
saveRDS(mod_maxchng_rm, "Models/mod_maxchng_rm.rds")
saveRDS(mod_maxchng_no_rm, "Models/mod_maxchng_no_rm.rds")

#hurdle mean change #####
start.time <- Sys.time()
mod_meanchng_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_Change*Reach + (1|RM_Start),
                    hu~ 0+Species_Codes*Mean_Change*Reach + (1|RM_Start)),
                 family = hurdle_lognormal(),
                 prior = p1,
                 data=DatFishDry_RM,
                 chains = 3,
                 warmup = 500,
                 iter=4000,
                 sample_prior = TRUE,
                 cores = 4)

beep(1)
end.time <- Sys.time()
time5 <- round(end.time - start.time,2) 

start.time <- Sys.time()
mod_meanchng_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_Change*Reach,
                     hu~ 0+Species_Codes*Mean_Change*Reach),
                  family = hurdle_lognormal(),
                  prior = p1,
                  data=DatFishDry_RM,
                  chains = 3,
                  warmup = 500,
                  iter=4000,
                  sample_prior = TRUE,
                  cores = 4)

beep(1)
end.time <- Sys.time()
time4 <- round(end.time - start.time,2)

#save models #
saveRDS(mod_meanchng_rm, "Models/mod_meanchng_rm.rds")
saveRDS(mod_meanchng_no_rm, "Models/mod_meanchng_no_rm.rds")

#hurdle sd change #####
start.time <- Sys.time()
mod_sdchng_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*SD_Change*Reach + (1|RM_Start), 
                     hu~ 0+Species_Codes*SD_Change*Reach + (1|RM_Start)),
                  family = hurdle_lognormal(),
                  prior = p1,
                  data=DatFishDry_RM,
                  chains = 3,
                  warmup = 500,
                  iter=4000,
                  sample_prior = TRUE,
                  cores = 4)
beep(1)
end.time <- Sys.time()
time4 <- round(end.time - start.time,2)

start.time <- Sys.time()
mod_sdchng_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*SD_Change*Reach, 
                   hu~ 0+Species_Codes*SD_Change*Reach),
                family = hurdle_lognormal(),
                prior = p1,
                data=DatFishDry_RM,
                chains = 3,
                warmup = 500,
                iter=4000,
                sample_prior = TRUE,
                cores = 4)
beep(1)
end.time <- Sys.time()
time3 <- round(end.time - start.time,2)

#save models #
saveRDS(mod_sdchng_rm, "Models/mod_sdchng_rm.rds")
saveRDS(mod_sdchng_no_rm, "Models/mod_sdchng_no_rm.rds")



#hurdle max MD ####
start.time <- Sys.time()
mod_maxMD_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_MileDays*Reach + (1|RM_Start),
                      hu~ 0+ Species_Codes*Max_MileDays*Reach + (1|RM_Start)),
                   family = hurdle_lognormal(),
                   prior = p1,
                   data=DatFishDry_RM,
                   chains = 3,
                   warmup = 500,
                   iter=4000,
                   sample_prior = TRUE,
                   cores = 4)
beep(1)
end.time <- Sys.time()
time1 <- round(end.time - start.time,2)

start.time <- Sys.time()
mod_maxMD_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_MileDays*Reach,
                  hu~ 0+ Species_Codes*Max_MileDays*Reach),
               family = hurdle_lognormal(),
               prior = p1,
               data=DatFishDry_RM,
               chains = 3,
               warmup = 500,
               iter=4000,
               sample_prior = TRUE,
               cores = 4)
beep(1)
end.time <- Sys.time()
time2 <- round(end.time - start.time,2)

#save models #
saveRDS(mod_maxMD_rm, "Models/mod_maxMD_rm.rds")
saveRDS(mod_maxMD_no_rm, "Models/mod_maxMD_no_rm.rds")

#hurdle mean MD ####
start.time <- Sys.time()
mod_meanMD_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_MileDays*Reach + (1|RM_Start), 
                  hu~ 0+Species_Codes*Mean_MileDays*Reach + (1|RM_Start)),
               family = hurdle_lognormal(),
               prior = p1,
               data=DatFishDry_RM,
               chains = 3,
               warmup = 500,
               iter=4000,
               sample_prior = TRUE,
               cores = 4,
               control = list(max_treedepth = 13))
beep(1)
end.time <- Sys.time()
time2 <- round(end.time - start.time,2)

start.time <- Sys.time()
mod_meanMD_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_MileDays*Reach, 
                   hu~ 0+Species_Codes*Mean_MileDays*Reach),
                family = hurdle_lognormal(),
                prior = p1,
                data=DatFishDry_RM,
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
saveRDS(mod_meanMD_rm, "Models/mod_meanMD_rm.rds")
saveRDS(mod_meanMD_no_rm, "Models/mod_meanMD_no_rm.rds")

#hurdle sd MD ####
start.time <- Sys.time()
mod_sdMD_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*SD_MileDays*Reach + (1|RM_Start), 
                   hu~ 0+Species_Codes*SD_MileDays*Reach + (1|RM_Start)),
                family = hurdle_lognormal(),
                prior = p1,
                data=DatFishDry_RM,
                chains = 3,
                warmup = 500,
                iter=4000,
                sample_prior = TRUE,
                cores = 4,
              control = list(max_treedepth = 13))
beep(1)
end.time <- Sys.time()
time3 <- round(end.time - start.time,2)

start.time <- Sys.time()
mod_sdMD_no_rm<-brm(bf(CPUE_m ~ 0+ Species_Codes*SD_MileDays*Reach, 
                 hu~ 0+Species_Codes*SD_MileDays*Reach),
              family = hurdle_lognormal(),
              prior = p1,
              data=DatFishDry_RM,
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
saveRDS(mod_sdMD_rm, "Models/mod_sdMD_rm.rds")
saveRDS(mod_sdMD_no_rm, "Models/mod_sdMD_no_rm.rds")

#model check ####
#r-hat (need to be <1.01 indicating chains converged)
summary(mod_meanMD) #okay - no divergent transitions for max, mean, sd extent or change, mean and sd MD
                  #did not converge maxMD

#pp_check #
#max,mean, sd extent or change have same okayness with a little bit of wonkyness
pmod <- pp_check(mod_Pro_maxextent, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pmod+scale_x_continuous(trans="log10")

p <- pp_check(mod_maxextent)
p+scale_x_continuous(trans="log10")

t <- pp_check(mod_sdextent, type = "boxplot")
t+scale_y_continuous(trans = "log10")

t2 <- pp_check(mod_sdextent, type = "hist")
t2+scale_x_continuous(trans="log10")

pp_check(mod_sdextent, type="stat_grouped", stat = "mean", group = "Species_Codes")


#pp_check #
#works in as much as just one peak with a little wonkyness, not any better or worse than nested
pmod_mod_meanextent <- pp_check(mod_meanextent, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pmod_mod_meanextent+scale_x_continuous(trans="log10")

p <- pp_check(mod_maxextent)
p+scale_x_continuous(trans="log10")


#Read in models ####
MaxExtent <- readRDS("Models/MaxExtent.rds")
MeanExtent <- readRDS("Models/Mean_Extent.rds")
SDExtent <- readRDS("Models/SD_Extent.rds")

MaxChng <- readRDS("Models/Max_Change.rds")
#MeanChng <- readRDS("Models/Mean_Change.rds")
SDChng <- readRDS("Models/SD_Change.rds")

MeanMD <- readRDS("Models/Mean_MileDays.rds")
SDMD <- readRDS("Models/SD_MileDays.rds")

#Loo ####
 #moment_match = true breaks R

LooMaxExtent <- loo(MaxExtent, save_psis = T)  #2 observation pareto_k >0.7
LooMeanExtent <- loo(MeanExtent, save_psis = T) #1 observation pareto_k >0.7
LooSDExtent <- loo(SDExtent, save_psis = T) #1 observation pareto_k >0.7

LooMaxChng <- loo(MaxChng, save_psis = T) #3 observation pareto_k >0.7
#LooMeanChng <- loo(MeanChng, save_psis = T) #1 observation pareto_k >0.7
LooSDChng <- loo(SDChng, save_psis = T) #2 observation pareto_k >0.7

LooMeanMD <- loo(MeanMD, save_psis = T) #1 observation pareto_k >0.7
LooSDMD <- loo(SDMD, save_psis = T) #no problems
saveRDS(LooSDMD, "Models/LooSDMD.rds")

  # Let's you see what observation are bad and how many in each category
print(LooSDMD)
plot(LooSDMD)

#reloo ####
#set cores 
options(mc.cores = 4)

start.time <- Sys.time()
Reloo_MaxExtent <- loo(MaxExtent, reloo = T) #reloo to run models without the bad observations; 2 hrs; worked
  saveRDS(Reloo_MaxExtent, "Models/Reloo_MaxExtent.rds")

Reloo_MeanExtent <- loo(MeanExtent, reloo = T) #reloo to run models without the bad observations; 16.61 min with 4 cores; worked
  saveRDS(Reloo_MeanExtent, "Models/Reloo_MeanExtent.rds")

Reloo_SDExtent <- loo(SDExtent, reloo = T) #reloo to run models without the bad observations:10 mins; worked
  saveRDS(Reloo_SDExtent, "Models/Reloo_SDExtent.rds")
  
  

Reloo_MaxChng <- loo(MaxChng, reloo = T) #reloo to run models without the bad observations: 2 hrs; worked
saveRDS(Reloo_MaxChng, "Models/Reloo_MaxChng.rds")

# Reloo_MeanChng <- loo(MeanChng, reloo = T) #reloo to run models without the bad observations: 2 hrs; worked
# saveRDS(Reloo_MeanChng, "Models/Reloo_MeanChng.rds")

Reloo_SDChng <- loo(SDChng, reloo = T) #reloo to run models without the bad observations: 24 mins; worked
saveRDS(Reloo_SDChng, "Models/Reloo_SDChng.rds")



Reloo_MeanMD <- loo(MeanMD, reloo = T) #reloo to run models without the bad observations; 2 hrs with 4 cores; worked
  saveRDS(Reloo_MeanMD, "Models/Reloo_MeanMD.rds")
  
end.time <- Sys.time()
round(end.time - start.time,2) 

#didn't need to reloo SDMD because it had no problems 

print(Reloo_MaxExtent) #use print to see if any pareto k >0.7

#Read in reloo models ####
Reloo_MaxExtent <- readRDS("Models/Reloo_MaxExtent.rds")
Reloo_MeanExtent <- readRDS("Models/Reloo_MeanExtent.rds")
Reloo_SDExtent <- readRDS("Models/Reloo_SDExtent.rds")

Reloo_MaxChng <- readRDS("Models/Reloo_MaxChng.rds")
Reloo_MeanChng <- readRDS("Models/Reloo_MeanChng.rds")
Reloo_SDChng <- readRDS("Models/Reloo_SDChng.rds")

Reloo_MeanMD <- readRDS("Models/Reloo_MeanMD.rds")
LooSDMD <- readRDS("Models/LooSDMD.rds")


#compare models loo #####
  #Extent
loo_compare(Reloo_MaxExtent, Reloo_MeanExtent, Reloo_SDExtent)

  #Change
loo_compare(Reloo_MaxChng, Reloo_MeanChng, Reloo_SDChng)

  #Mile Days
loo_compare(Reloo_MeanMD, LooSDMD)

  #all
loo_compare(Reloo_MaxExtent, Reloo_MeanExtent, Reloo_SDExtent,
            Reloo_MaxChng, Reloo_MeanChng, Reloo_SDChng,
            Reloo_MeanMD, LooSDMD)

#Predicting ####
#https://www.google.com/search?sca_esv=594252889&rlz=1C1GCEA_enUS1082US1082&tbm=vid&q=understanding+hurdle+model+output&sa=X&ved=2ahUKEwjdxO7zw7KDAxWtMUQIHbIBBBQQ8ccDegQIDBAJ&biw=1920&bih=953&dpr=1#fpstate=ive&vld=cid:1531d1f9,vid:7tYbxkI1FNA,st:0
ggpredict(mod, terms = ..., type = "zi_prob")

conditional_effects(MaxExtent)

#general drying trends analysis

library(fpp3)
library(tsibbledata)
library(fable)
library(forcats)

maxExtent_IS <- datdry %>% 
  filter(Reach == "Isleta") %>% 
  select(Max_Extent) %>% 
  ts(start = 2010, frequency = 1)

plot(maxExtent_IS)

# Calculate the 3-year moving average
moving_avg <- stats::filter(maxExtent_IS, rep(1/3, 3), sides = 2)

# Plot the original time series and the moving average
plot(maxExtent_IS, type = "l", col = "blue", lwd = 2, xlab = "Year", ylab = "GDP Growth (%)", main = "3-Year Moving Average")
lines(moving_avg, col = "red", lwd = 2)
legend("topright", legend = c("Original", "3-Year MA"), col = c("blue", "red"), lty = 1, lwd = 2)

summary(mod_maxextent_rm)
summary(mod_maxextent_no_rm)
pp_check(mod_maxextent_rm)
pp_check(mod_maxextent_no_rm)

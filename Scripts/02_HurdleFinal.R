#Read me ####
#I'm getting confused on what works and doesn't work and
#this is script is to get my brain wrapped around things for final model
#I'm interacting Reach because I know there is a difference between reaches
#I am not nesting random effects because RMs were not random each year
#I am using lognormal because Poisson and Negbinomial did not converege

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
get_prior(bf(CPUE_m ~  Species_Codes*Max_Extent*Reach + (1|Year) + (1|RM_Start), 
             hu~  Species_Codes*Max_Extent*Reach + + (1|Year) + (1|RM_Start)),
          family = hurdle_lognormal(),
          data = DatFishDry_RM)

p1 <- c(set_prior("normal(0,10)", class = "b"))




#hurdle max extent #####
start.time <- Sys.time()
mod_maxextent<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_Extent*Reach + (1|Year) + (1|RM_Start), 
                           hu~ 0+ Species_Codes*Max_Extent*Reach + (1|Year) + (1|RM_Start)),
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

#save models #
saveRDS(mod_maxextent, "Models/MaxExtent.rds")


#hurdle mean extent #####

start.time <- Sys.time()
mod_meanextent<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_Extent*Reach + (1|Year) + (1|RM_Start), 
                      hu~ 0+ Species_Codes*Mean_Extent*Reach + (1|Year) + (1|RM_Start)),
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
time8 <- round(end.time - start.time,2) #14.4 min

#save models #
saveRDS(mod_meanextent, "Models/Mean_Extent.rds")

#hurdle sd extent #####
start.time <- Sys.time()
mod_sdextent<-brm(bf(CPUE_m ~ 0+ Species_Codes*SD_Extent*Reach + (1|Year) + (1|RM_Start), 
                       hu~ 0+Species_Codes*SD_Extent*Reach + (1|Year) + (1|RM_Start)),
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
time7 <- round(end.time - start.time,2) #15.8 min

#save models #
saveRDS(mod_sdextent, "Models/SD_Extent.rds")

#hurdle max change #####
start.time <- Sys.time()
mod_maxchng<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_Change*Reach + (1|Year) + (1|RM_Start), 
                     hu~ 0+Species_Codes*Max_Change*Reach + (1|Year) + (1|RM_Start)),
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
time6 <- round(end.time - start.time,2) #11.5 min
#save models #
saveRDS(mod_maxchng, "Models/Max_Change.rds")

#hurdle mean change #####
#DONT RERUN- I CANT' GET IT TO CONVERGE AGAIN
# start.time <- Sys.time()
# mod_meanchng<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_Change*Reach + (1|Year) + (1|RM_Start),
#                     hu~ 0+Species_Codes*Mean_Change*Reach + (1|Year) + (1|RM_Start)),
#                  family = hurdle_lognormal(),
#                  prior = p1,
#                  data=DatFishDry_RM,
#                  chains = 3,
#                  warmup = 500,
#                  iter=4000,
#                  sample_prior = TRUE,
#                  cores = 4)
# 
# beep(1)
# end.time <- Sys.time()
# time5 <- round(end.time - start.time,2) #1.64 hrs
#save models #
#saveRDS(mod_meanchng, "Models/Mean_Change.rds")

#hurdle sd change #####
start.time <- Sys.time()
mod_sdchng<-brm(bf(CPUE_m ~ 0+ Species_Codes*SD_Change*Reach + (1|Year) + (1|RM_Start), 
                     hu~ 0+Species_Codes*SD_Change*Reach + (1|Year) + (1|RM_Start)),
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
time4 <- round(end.time - start.time,2) #13.05 min
#save models #
saveRDS(mod_sdchng, "Models/SD_Change.rds")




#hurdle max MD ####
#maxMD doesn't converge
# start.time <- Sys.time()
# mod_maxMD<-brm(bf(CPUE_m ~ 0+ Species_Codes*Max_MileDays*Reach + (1|Year) + (1|RM_Start),
#                       hu~ 0+ Species_Codes*Max_MileDays*Reach + (1|Year) + (1|RM_Start)),
#                    family = hurdle_lognormal(),
#                    prior = p1,
#                    data=DatFishDry_RM,
#                    chains = 3,
#                    warmup = 500,
#                    iter=4000,
#                    sample_prior = TRUE,
#                    cores = 4)
# 
# beep(1)
# end.time <- Sys.time()
# time1 <- round(end.time - start.time,2)

#save models #
#saveRDS(mod_maxMD, "Models/Max_MileDays.rds")
#hurdle mean MD ####
start.time <- Sys.time()
mod_meanMD<-brm(bf(CPUE_m ~ 0+ Species_Codes*Mean_MileDays*Reach + (1|Year) + (1|RM_Start), 
                  hu~ 0+Species_Codes*Mean_MileDays*Reach + (1|Year) + (1|RM_Start)),
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
time2 <- round(end.time - start.time,2) #47 minutes

#save models #
saveRDS(mod_meanMD, "Models/Mean_MileDays.rds")

#hurdle sd MD ####
start.time <- Sys.time()
mod_sdMD<-brm(bf(CPUE_m ~ 0+ Species_Codes*SD_MileDays*Reach + (1|Year) + (1|RM_Start), 
                   hu~ 0+Species_Codes*SD_MileDays*Reach + (1|Year) + (1|RM_Start)),
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
time3 <- round(end.time - start.time,2) #34 minutes

#save models #
saveRDS(mod_sdMD, "Models/SD_MileDays.rds")

#model check ####
#r-hat (need to be <1.01 indicating chains converged)
summary(mod_meanMD) #okay - no divergent transitions for max, mean, sd extent or change, mean and sd MD
                  #did not converge maxMD

#pp_check #
#max,mean, sd extent or change have same okayness with a little bit of wonkyness
pmod <- pp_check(mod_sdextent, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
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

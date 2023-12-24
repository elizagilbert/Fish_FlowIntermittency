#read me####
#the purpose of this script is to run poisson and negbinomial models
#to assess extent and change dry models predicting the number 
#of fish per some unit sampled (need to get to an integer)

#libraries ####
library(tidyverse)
library(brms)
library(beepr)
library(loo)
library(bayesplot)
library(rstanarm)

#set cores ####
options(mc.cores = 4)

#Data ####
datfish_rm<- read.csv("Data/Processed/RGFishCPUE_RM.csv")
datdry <- read.csv("Data/Processed/RGDryCovariates_ByReach.csv")

#Data wrangle ####
FishDry_RM <- datfish_rm %>% 
  left_join(datdry) %>% 
  mutate(Species_Codes = as.factor(Species_Codes), Year = as.factor(Year),
         Mn_CPUE_1000m = round((Mn_CPUE_m*1000),0))

#Fitting models ####

start.time <- Sys.time()

mod_max_ext_lg <- brm(bf(Mn_CPUE_m ~ Species_Codes*Max_Extent*Reach + (1|Year) + (1|Year:Reach), 
                         hu~ Species_Codes*Max_Extent*Reach + (1|Year) + (1|Year:Reach)),
                      family = hurdle_lognormal(),
                      prior = p1 <- c(set_prior("normal(0,10)", class = "b")),
                      data=FishDry_RM,
                      chains = 3,
                      warmup = 500,
                      iter=4000,
                      sample_prior = TRUE,
                      cores = 4)

saveRDS(mod_max_ext_lg, "ModelOutput/mod_max_ext_lg.rds")



end.time <- Sys.time()
print(round(end.time - start.time,2))
beep(1)



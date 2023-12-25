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
datdry <- read.csv("Data/Processed/RGDryCovariates_ByReach.csv")

#zeros in data ####
NumZeros <- datfish_rm %>% 
  group_by(Year, Species_Codes) %>% 
  count(CPUE_m == 0) 

#Wrangle data ####
DatFishDry_RM <- datfish_rm %>% 
  left_join(datdry) %>% 
  mutate(Species_Codes = as.factor(Species_Codes), Year = as.factor(Year), Reach = as.factor(Reach),
         RM_Start = as.factor(RM_Start), CPUE_1000m = round((CPUE_m*1000),0))

#Priors ####
get_prior(bf(CPUE_m ~  Species_Codes*Max_Extent*Reach + (1|Year) + (1|RM_Start), 
             hu~  Species_Codes*Max_Extent*Reach + + (1|Year) + (1|RM_Start)),
          family = hurdle_lognormal(),
          data = DatFishDry_RM)

p1 <- c(set_prior("normal(0,10)", class = "b"))


#hurdle lognormal #####
  #crossed random effects-Reach as interaction
start.time <- Sys.time()
mlognormal_reach_crossed<-brm(bf(CPUE_m ~ Species_Codes*Max_Extent*Reach + (1|Year) +(1|RM_Start), 
                           hu~ Species_Codes*Max_Extent*Reach + (1|Year) +(1|RM_Start)),
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
print(round(end.time - start.time,2)) #15.4 min

#r-hat ####
  #need to be <1.01 indicating chains converged
summary(mlognormal_reach_crossed) #okay - no divergent transitions

#pp_check #####
  #lognormal - mlognormal_reach_crossed
  #works in as much as just one peak with a little wonkyness, not any better or worse than nested
pmod3 <- pp_check(mlognormal_reach_crossed, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pmod3+scale_x_continuous(trans="log10")

#save models ####
saveRDS(mlognormal_reach_crossed, "Models/MaxExtent_HurdModLogNorm.rds")

#loo best model ####
  # suggested moment_match = T because pareto_k >0.7 but that breaks R
  # using save_psis = T, it says 3 observations with pareto_k >0.7
LooBestModel <- loo(mlognormal_reach_crossed, save_psis = T) 

  # 2 bad and 1 very bad of 1584
print(LooBestModel)
plot(LooBestModel)

#reloo best model ####
start.time <- Sys.time()

Reloo_MaxExtent <- loo(mlognormal_reach_crossed, reloo = T) #reloo to run models without the bad observations
saveRDS(Reloo_MaxExtent, "Models/Reloo_MaxExtent.rds")

end.time <- Sys.time()
print(round(end.time - start.time,2)) #3 hrs
beep(1)

print(Reloo_MaxExtent) #use print to see if any pareto k >0.7...this reloo worked


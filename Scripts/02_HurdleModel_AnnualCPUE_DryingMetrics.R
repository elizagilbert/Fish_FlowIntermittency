#Read me ####
#the purpose of this code is to estimate CPUE for Rio Grande fishes
#based on river drying metrics
#here we use a hurdle model to account for the zeros in the data

#Libraries ####
library(tidyverse)
library(brms)
library(beepr)
library(loo)
library(bayesplot)
library(rstanarm)
library(lme4)

#set cores ####
options(mc.cores = 4)

#Data ####
datfish_rm<- read.csv("Data/Processed/RGFishCPUE_RM.csv")
datdry <- read.csv("Data/Processed/RGDryCovariates_ByReach.csv")

#Data wrangle ####
# Dry_irrig <- datdry %>%
#   mutate(Month = month(Date)) %>% 
#   filter(between(Month, 4, 10)) %>% 
#   group_by(Reach, year(Date)) %>% 
#   summarise(mn_extent = mean(ExtentDry), mn_chng = mean(ChngExtentDry), 
#             mn_sum = mean(SumDaysDry), max_sum = max(SumDaysDry), 
#             max_extent = max(ExtentDry), max_chng = max(ChngExtentDry)) %>% 
#   rename(Year = 2) %>% 
#   ungroup()

FishDry_RM <- datfish_rm %>% 
  left_join(datdry) %>% 
  mutate(Species_Codes = as.factor(Species_Codes), Year = as.factor(Year),
         RM_Start = as.factor(RM_Start), Reach = as.factor(Reach),
         CPUE_1000m = round((CPUE_m*1000),0))

#Hurdle modeling####

#priors
get_prior(bf(CPUE_m ~  Species_Codes*Max_Extent, 
             hu ~  Species_Codes*Max_Extent),
          family = hurdle_lognormal(),
          data = FishDry_RM)


p1 <- c(set_prior("normal(0,10)", class = "b"))

start.time <- Sys.time()

  #max extent log normal
m1<-brm(bf(CPUE_1000m ~ Species_Codes*Max_Extent + (1|Year), 
           hu ~ Species_Codes*Max_Extent + (1|Year)),
        family = hurdle_lognormal(),
        prior = p1,
        data=FishDry_RM,
        chains = 3,
        warmup = 500,
        iter=4000,
        sample_prior = TRUE,
        cores = 4)

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2)) 

saveRDS(m1, "ModelOutput/MaxExtent_HurdModLog.rds")

  #max extent negbinomial
m2<-brm(bf(CPUE_1000m ~ Species_Codes*Max_Extent*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), 
           hu ~ Species_Codes*Max_Extent*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)),
        family = hurdle_negbinomial(),
        prior = p1,
        data=FishDry_RM,
        chains = 3,
        warmup = 500,
        iter=4000,
        sample_prior = TRUE,
        cores = 4)

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2)) #18.37min

saveRDS(m2, "ModelOutput/MaxExtent_HurdModNeg.rds")
  
  #mn extent
start.time <- Sys.time()
m2<-brm(bf(CPUE ~ Species_Codes*mn_extent*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), 
           hu~ Species_Codes*mn_extent*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)),
        family = hurdle_lognormal(),
        prior = p1,
        data=FishDry_RM,
        chains = 3,
        warmup = 500,
        iter=4000,
        sample_prior = TRUE,
        cores = 4,
        save_pars = save_pars(all=T))

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2)) #15.92 minutes

saveRDS(m2, "ModelOutput/MnExtent_HurdMod.rds") 

start.time <- Sys.time()
  #max sum
m3<-brm(bf(CPUE ~ Species_Codes*max_sum*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), 
           hu~ Species_Codes*max_sum*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)),
        family = hurdle_lognormal(),
        prior = p1,
        data=FishDry_RM,
        chains = 3,
        warmup = 500,
        iter=4000,
        sample_prior = TRUE,
        cores = 4)

beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2)) # 2.05 hrs

saveRDS(m3, "ModelOutput/MaxSum_HurdMod.rds")


start.time <- Sys.time()
  #mn sum
m4<-brm(bf(CPUE ~ Species_Codes*mn_sum*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), 
           hu~ Species_Codes*mn_sum*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)),
        family = hurdle_lognormal(),
        prior = p1,
        data=FishDry_RM,
        chains = 3,
        warmup = 500,
        iter=4000,
        sample_prior = TRUE,
        cores = 4)

saveRDS(m4, "ModelOutput/MnSum_HurdMod.rds")

  #mn change
m5<-brm(bf(CPUE ~ Species_Codes*mn_chng*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), 
           hu~ Species_Codes*mn_chng*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)),
        family = hurdle_lognormal(),
        prior = p1,
        data=FishDry_RM,
        chains = 3,
        warmup = 500,
        iter=4000,
        sample_prior = TRUE,
        cores = 4)

saveRDS(m5, "ModelOutput/MnChng_HurdMod.rds")

  #max change
m6<-brm(bf(CPUE ~ Species_Codes*max_chng*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), 
           hu~ Species_Codes*max_chng*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)),
        family = hurdle_lognormal(),
        prior = p1,
        data=FishDry_RM,
        chains = 3,
        warmup = 500,
        iter=4000,
        sample_prior = TRUE,
        cores = 4)

saveRDS(m6, "ModelOutput/MaxChng_HurdMod.rds")
end.time <- Sys.time()
print(round(end.time - start.time,2)) #3.55 hrs

#read in models ####
mod_maxExtent <- readRDS("ModelOutput/MaxExtent_HurdMod.rds")
mod_mnExtent <- readRDS("ModelOutput/MnExtent_HurdMod.rds")
mod_maxChng <- readRDS("ModelOutput/MaxChng_HurdMod.rds")
mod_mnChng <- readRDS("ModelOutput/MnChng_HurdMod.rds")
mod_maxSum <- readRDS("ModelOutput/MaxSum_HurdMod.rds")
mod_mnSum <- readRDS("ModelOutput/MnSum_HurdMod.rds")

#model fits ####
  #visual pp_check
  #overall Carcar and cypcar and pimpro maybe not so good
  #bad model fits for max Sum

  #max Extent  
pp_maxExtent <- pp_check(m1, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pp_maxExtent+scale_x_continuous(trans="log10")

  #mn Extent 
pp_mnExtent <- pp_check(mod_mnExtent, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pp_mnExtent+scale_x_continuous(trans="log10")

  #max Change 
pp_maxChng <- pp_check(mod_maxChng, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pp_maxChng+scale_x_continuous(trans="log10")

  #mn Change  
pp_mnChng <- pp_check(mod_mnChng, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pp_mnChng+scale_x_continuous(trans="log10")
  
  #max Sum - bad model fits!!!! 
pp_maxSum <- pp_check(mod_maxSum, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pp_maxSum+scale_x_continuous(trans="log10", limits = c(1e-10, 1e10))

  #mn Sum 
pp_mnSum <- pp_check(mod_mnSum, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pp_mnSum+scale_x_continuous(trans="log10")

  #r-hats, need to be <1.01 indicating chains converged
summary(mod_maxExtent) # is 1.00
summary(mod_mnExtent) # is 1.00
summary(mod_maxChng) # is 1.00
summary(mod_mnChng) # is 1.00
summary(mod_maxSum) # model did not converge
summary(mod_mnSum) #is 1.75 parts of model did not converge - says run more iterations or set stronger priors

#model comparisons ####

#think about WAIC (the widely applicable or Watanabe-Akaike information criterion; Watanabe, 2010) 
#be viewed as an improvement on the deviance information criterion (DIC) for Bayesian models
# WAIC(mod_brm_Qlty, mod_brm_Age)
# WAIC SE
# mod_brm_Qlty 128.24 17.52
# mod_brm_Age 151.24 20.60
# mod_brm_Qlty - mod_brm_Age -23.00 6.32
# 
# The model with quality has a smaller WAIC. The Rhat and effective sample size look good, so I think it’s mixing well. And I think I’m using fairly classic and uninformative priors 
# 

#k fold cross-validation if I can't get loo or WAIC to work
#supposedly it is more computationally intensive
# https://paul-buerkner.github.io/brms/reference/kfold.html
# Perform exact K-fold cross-validation by refitting the model K
# times each leaving out one-K
# th of the original data.
# 
# # S3 method for brmsfit - Varanti suggested random pulls and I'm not sure if this does that
# kfold(x, ..., compare = TRUE, K = 10, Ksub = NULL,
#       folds = NULL, group = NULL, exact_loo = NULL, resp = NULL,
#       model_names = NULL, save_fits = FALSE)
# 
# kfold(x, ...)
# may be an issue because of hierarchical nature of data and would have to sample
# with the hierarchy in mind.  I don't know if the kfold function does that

#below is pareto smoothed importance sampling leave-one-out cross-validation that calculates
#the expected log pointwise predictive density (elpd)

loo_maxExtent <- loo(mod_maxExtent, save_psis = T) # suggested because 4 observation with pareto_k >0.7 in model moment_match = T breaks R
print(loo_maxExtent)  # but elpd_loo should not be considered reliable because the estimated effective number of parameters (p_loo)
                      #should be smaller or similar to the total number of parameters in the model
                      #here it is 255.7 but in the loo_mnChng that works p_loo is 235.2 so I don't really understand
                      #what are the number of parameters? I think it is about 64 parameters - maybe I should remove reach?
plot(loo_maxExtent)     
yrep <- posterior_predict(mod_maxExtent)
ppc_loo_pit_overlay(y = FishDry_RM$CPUE, yrep = yrep, lw = weights(loo_maxExtent$psis_object)) 
                      #excessive number of values close to 0.75 indicate the model is under-dispersed compared to the data
                      #and we should consider a model that allows for greater dispersion - uses "bayesplot" library
                      #negative binomal model is often used for overdispersed count data, unlike Poisson allows the 
                      #conditional mean and variance of y to differ

#doesn't work because requires integer response for poisson or negative binomial
fit2 <- update(mod_maxExtent, family = hurdle_poisson) #poisson and negative binomial require integer response

  #####---
loo_mnExtent <- loo(mod_mnExtent, save_psis = T) #because 1 observation with pareto_k >0.7 in model 
                                                                      #suggested moment_match could not find function "prep_call_sampler
start.time <- Sys.time()
loo_mnExtent <- loo(mod_mnExtent, save_psis = T, reloo = T)
end.time <- Sys.time()
print(round(end.time - start.time,2))
beep(1)

saveRDS(loo_mnExtent, "Loo/loo_mnExtent.rds")

start.time <- Sys.time()
loo_maxExtent <- loo(mod_maxExtent, save_psis = T, reloo = T)
saveRDS(loo_maxExtent, "Loo/loo_maxExtent.rds")

loo_maxChng <- loo(mod_maxChng, save_psis = T, reloo = T)
saveRDS(loo_maxChng, "Loo/loo_maxChng.rds")

end.time <- Sys.time()
print(round(end.time - start.time,2))

print(loo_maxExtent) #it worked!!!!!
plot(loo_mnExtent)     
yrep_2 <- posterior_predict(mod_maxExtent)
ppc_loo_pit_overlay(y = FishDry_RM$CPUE, yrep = yrep_2, lw = weights(loo_mnExtent$psis_object))
                      #excessive number of values close to 0.75 indicate the model is under-dispersed compared to the data

  #refit model for each of problematic k values - this didn't work, 
  #it should have said leaving out the observation
if (any(pareto_k_values(loo_mnExtent) > 0.7)) {
  loo_mnExtent <- loo(mod_mnExtent, save_psis = TRUE, k_threshold = 0.7)
}
print(loo_mnExtent)
plot(loo_mnExtent, label_points = T)

  ####---
loo_maxChng <- loo(mod_maxChng, save_psis = T) #suggested because 3 observation with pareto_k >0.7 in model moment_match = T breaks R
print(loo_maxChng)
plot(loo_maxChng)     
yrep_3 <- posterior_predict(mod_maxChng)
ppc_loo_pit_overlay(y = FishDry_RM$CPUE, yrep = yrep_3, lw = weights(loo_maxChng$psis_object))
                      #excessive number of values close to 0.75 indicate the model is under-dispersed compared to the data

loo_mnChng <- loo(mod_mnChng, save_psis = T) #this one succeeds
print(loo_mnChng) #because no k > 1 able to compute Monte Carlo SE of expected log predictive density (elpd_loo)
plot(loo_mnChng)     
yrep_4 <- posterior_predict(mod_mnChng)
ppc_loo_pit_overlay(y = FishDry_RM$CPUE, yrep = yrep_4, lw = weights(loo_mnChng$psis_object))
                      #I don't know why this works
                      #excessive number of values close to 0.75 indicate the model is under-dispersed compared to the data

#comparing models with loo anyways
  #elpd is expected log predictive density
loo_compare(loo_maxExtent, loo_mnExtent, loo_maxChng, loo_mnChng)

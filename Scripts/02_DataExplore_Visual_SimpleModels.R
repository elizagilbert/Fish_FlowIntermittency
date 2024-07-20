#Read me ####
#The purpose of this script is to explore the data
#visually and with some basic models

#Libraries ####
library(tidyverse)
library(lubridate)
library(brms)
library(beepr)
library(lme4)
library(MASS)
library(performance)

#Data ####
datfish_rm<- read.csv("Data/Processed/RGFishCPUE_RM.csv")
datdry <- read.csv("Data/Processed/RGDryCovariates_ByReach_Irrig.csv")

#data distributions ####

  #checking over dispersion (i.e., sd greater than mean)
  #true for all fish
datfish_rm %>% 
  group_by(Species_Codes) %>% 
  summarise(mnCpue = mean(CPUE_m), sdCpue = sd(CPUE_m))

  #histogram
datfish_rm %>% 
  ggplot(aes(x = (CPUE_m)*1000))+
  geom_histogram(binwidth = 10)+
  facet_wrap(vars(Species_Codes), scales = "free")

#species means per reach
fishmean <- datfish_rm %>% 
  group_by(Species_Codes, Reach) %>% 
  summarise(MnCpue = round(mean(CPUE_m),4), MinCpue = round(min(CPUE_m),4),
            MaxCpue = round(max(CPUE_m),4))

annualfishmean <- datfish_rm %>% 
  group_by(Species_Codes, Reach, Year) %>% 
  summarise(MeanAnCpue = round(mean(CPUE_m),4)) %>% 
  ungroup() %>% 
  group_by(Species_Codes, Reach) %>% 
  summarise(MnCpue = round(mean(MeanAnCpue),4), MinCpue = round(min(MeanAnCpue),10),
            MaxCpue = round(max(MeanAnCpue),4))

#Visualize fish ####
#visualization of annual density
datfish_rm %>% 
  group_by(Species_Codes, Year) %>% 
  summarise(MnCpue = mean(CPUE_m)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Year, y = MnCpue))+
  geom_line()+
  facet_wrap(vars(Species_Codes))+
  theme_bw()+
  scale_x_continuous(breaks = seq(2010, 2022, by = 1))+
  scale_y_continuous(trans = "log10", labels = scales::number_format(accuracy = 0.01, decimal.mark = "."))+
  theme(axis.text.x=element_text(angle=60,hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

datfish_rm %>%   
  group_by(Species_Codes, Year) %>% 
  summarise(MnCpue = mean(CPUE_m)) %>% 
  ungroup() %>% 
  ggplot(aes(x = log(MnCpue)))+
  geom_histogram()+
  facet_wrap(vars(Species_Codes), scales = "free")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60,hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#Wrangle fish and drying mean by rm ####
DatFishDry_RM <- datfish_rm %>% 
  left_join(datdry) %>% 
  mutate(Species_Codes = as.factor(Species_Codes), Year = as.factor(Year),
         RM_Start = as.factor(RM_Start), CPUE_1000m = round((CPUE_m*1000),0))

DatFishDry_RMNoZeros <- DatFishDry_RM %>% 
  group_by(Reach, Year, Species_Codes) %>% 
  mutate(MnCPUE = mean(CPUE_m)) %>% 
  distinct(Year, Reach, Species_Codes, .keep_all = T) %>% 
  ungroup() %>% 
  mutate(MnCPUE_Plus1 = MnCPUE+0.0000001)

#Modeling ####

#checking for zero inflation in count model
#have to use poisson
m <- glm(CPUE_1000m ~ Species_Codes*Reach*Max_Extent, family = poisson, data = DatFishDry_RM) #'library 'MASS'
check_zeroinflation(m) #package 'performance'

#regular Bayes
get_prior(MnCPUE_Plus1 ~ Species_Codes*Max_Extent + (1 | Year/Reach),
          data = DatFishDry_RMNoZeros)

p2 = c(set_prior("normal(0,1)", class = "b"))

start.time <- Sys.time()
mod1 <- brm(MnCPUE_Plus1 ~  0 + Species_Codes*Max_Extent + (1 | Year/Reach), 
                data = DatFishDry_RMNoZeros,
                chains = 3,
                warmup = 200,
                iter = 1000,
                sample_prior = TRUE,
                family = lognormal(),
                prior = p2,
                cores = 4)
beep(1)
end.time <- Sys.time()
print(round(end.time - start.time,2))

mod2 <- brm(MnCPUE_Plus1 ~  0 + Species_Codes*Max_Extent + (1|Reach) + (1|Year), 
            data = DatFishDry_RMNoZeros,
            chains = 3,
            warmup = 500,
            iter = 4000,
            sample_prior = TRUE,
            family = lognormal(),
            prior = p2,
            cores = 4,
            control = list(max_treedepth = 15, adapt_delta = 0.9))

beep(sound = 3)

print(summary(mod1), digits = 10)
conditional_effects(mod1)

pmod1 <- pp_check(mod1, ndraws = 100, type = "dens_overlay_grouped", group = "Species_Codes")
pmod1+scale_x_continuous(trans="log10")

p4 <- pp_check(m1)
p4+scale_x_continuous(trans="log10")
ce <- conditional_effects(mod1)
saveRDS(mod1, "ModelOutput/MaxSpec.rds")
mod1 <- readRDS("ModelOutput/MaxE_YearReach.rds")
mod2 <- readRDS("ModelOutput/MaxSpec.rds")

plot(mod2)

conditional_effects(mod1)

#plot conditional effects wrapped by species
for_pl <- ce$`Max_Extent:Species_Codes`
sum_pl <- summary(mod1)
for_pl_est <- sum_pl$fixed %>% 
  slice(9:16) %>% 
  select(Estimate) %>% 
  rename(ModCoeff = Estimate) %>% 
  mutate(ModCoeff = round(ModCoeff,4)) %>% 
  mutate(Species_Codes = c("CARCAR", "CYPCAR", "CYPLUT", "GAMAFF", "HYBAMA", "ICTPUN", "PIMPRO", "PLAGRA")) 

to_plot <- for_pl %>% 
  left_join(for_pl_est, by = "Species_Codes")

MaxExtent_All <- ggplot(to_plot, aes(x = Max_Extent, y = estimate__)) +
  geom_line(color = "blue", size = 1)+
  geom_ribbon(aes(ymin=for_pl$lower__, ymax=for_pl$upper__),
              linetype = 2, alpha = 0.1)+
  facet_wrap(vars(Species_Codes), scales = "free")+
  geom_text(aes(x = 15, y = 1, label = paste0("Coeff =  ", ModCoeff)))+
  ylab("Model Catch Per Unit Effort") + xlab("Max annual extent of dry river miles")+
  theme_bw()

MaxExtent_YearReachAdd<- ggplot(to_plot, aes(x = Max_Extent, y = estimate__)) +
  geom_line(color = "blue", size = 1)+
  geom_ribbon(aes(ymin=for_pl$lower__, ymax=for_pl$upper__),
              linetype = 2, alpha = 0.1)+
  facet_wrap(vars(Species_Codes), scales = "free")+
  geom_text(aes(x = 10, y = 12.5, label = paste0("Coeff =  ", ModCoeff)))+
  ylab("Model Catch Per Unit Effort") + xlab("Max annual extent of dry river miles")+
  theme_bw()



#Modeling hurdle ####
get_prior(bf(CPUE_m ~  Species_Codes*Max_Extent*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), 
          hu~  Species_Codes*Max_Extent*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)),
          family = hurdle_lognormal(),
          data = DatFishDry_RM)


p1 <- c(set_prior("normal(0,10)", class = "b"))

#Random effects
#From Chapt GPT
#if each year's sample consists of a random selection of sites, 
#but the sites are not shared across different years (i.e., each year has its own set of sites), 
#then you have a nested random effects design.

#nesting assumes hierarchy - schools group classes; classes group students
#years group reaches; reaches group sites

#explicitly written as (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)
#when one factor appears only within a particular level
#a RMs are within a given reach within a given year

start.time <- Sys.time()

m1<-brm(bf(CPUE_m ~ Species_Codes*Max_Extent + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), 
           hu~ Species_Codes*Max_Extent + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)),
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
print(round(end.time - start.time,2))

summary(m1)

#this is crossing when one random factor is separately nested in the other
#can be used when there is partial crossing, some years may share the same RM
#crossed is when two or more variables can be used to create distinct groupings
m4<-brm(bf(CPUE ~ 0+ Species_Codes*max_extent*Reach + (1|Year) +(1|Reach) +(1|RM_Start), 
           hu~ Species_Codes*max_extent*Reach + (1|Year) +(1|Reach) +(1|RM_Start)),
        family = hurdle_lognormal(),
        prior = p1,
        data=DatFishDry_RM,
        chains = 3,
        warmup = 500,
        iter=4000,
        sample_prior = TRUE,
        cores = 4)
beep(sound=3)

p4 <- pp_check(m4)
p4+scale_x_continuous(trans="log10")

saveRDS(m2, "Temp/Hurdle3Way_RMPlusYear.rds")
m1 <- readRDS("Temp/Hurdle3Way_RMPlusYear.rds")
m2 <- readRDS("Temp/HurdleIs_RMPlusYear.rds")
m3 <- readRDS("Temp/HurdleSanA_RMPlusYear.rds")


p4 <- pp_check(m1)
p4+scale_x_continuous(trans="log10")

summary(m2)
conditional_effects(m1, effects = "max_extent:Species_Codes")
conditional_effects(m1, effects = "max_extent:Species_Codes", dpar="hu")
conditional_effects(m1, effects = "max_extent:Species_Codes", dpar="mu")

conditions <- make_conditions(m4, "Species_Codes")

conditional_effects(m4, "max_extent:Reach", conditions = conditions)

ce <- conditional_effects(m3)
for_pl <- ce$`max_extent:Species_Codes`

ggplot(for_pl, aes(x = max_extent, y = estimate__)) +
  geom_line(color = "blue", size = 1)+
  geom_ribbon(aes(ymin=for_pl$lower__, ymax=for_pl$upper__),
              linetype = 2, alpha = 0.1)+
  facet_wrap(vars(Species_Codes), scales = "free")+
  geom_text(aes(x = 15, y = 1, label = paste0("Coeff =  ", ModCoeff)))+
  ylab("Model Catch Per Unit Effort") + xlab("Max annual extent of dry river miles")+
  theme_bw()


ranef(m2)


for_pl <- ce$`Species_Codes:Reach`

to_plot <- for_pl %>% 
  left_join(for_pl_est, by = "Species_Codes")

ggplot(for_pl, aes(x = max_extent, y = estimate__)) +
  geom_line(color = "blue", size = 1)+
  geom_ribbon(aes(ymin=for_pl$lower__, ymax=for_pl$upper__),
              linetype = 2, alpha = 0.1)+
  facet_wrap((Species_Codes~Reach), scales = "free")+
  ylab("Hurdle model Catch Per Unit Effort") + xlab("Max annual extent of dry river miles")+
  theme_bw()

summary(lmer(CPUE ~ (1|Year) + (1|Reach) + (1|RM_Start), data = DatFishDry_RM)) #crossed
summary(lmer(CPUE ~ (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), data = DatFishDry_RM)) #nested same output as 4; I think this is correct
summary(lmer(CPUE ~ (1|Year/Reach) + (1|RM_Start:Reach), data = DatFishDry_RM)) #nested and crossed
summary(lmer(CPUE ~ (1|Year/Reach/RM_Start), data = DatFishDry_RM)) #I think this is nested same output as 2

with(DatFishDry_RM, table(RM_Start, Reach)) %>% 
  image(
    col = grey.colors(10, start = 1, end = 0), axes = F, xlab = "RM_Start", ylab = "Reach"
  )

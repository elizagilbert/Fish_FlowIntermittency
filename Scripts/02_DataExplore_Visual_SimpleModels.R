#Read me ####
#The purpose of this script is to explore the data
#visually and with some basic models

#Libraries ####
library(tidyverse)
library(lubridate)
library(brms)
library(beepr)
library(lme4)

#Data ####
datfish_rm<- read.csv("Data/Processed/RGFishCPUE_RM.csv")
datfish <- read.csv("Data/Processed/RGFishCPUE.csv")
datdry <- read.csv("Data/Processed/RGDryCovariates_ByReach.csv")

#Wrangle drying ####
#annual statistics
ggplot(datdry, aes(x = log(SumDaysDry)))+
  geom_histogram()+
  facet_wrap(vars(Reach))

sum_dry_irrig <- datdry %>%
  mutate(Month = month(Date)) %>% 
  filter(between(Month, 3, 10)) %>% 
  group_by(Reach, year(Date)) %>% 
  summarise(mn_extent = mean(ExtentDry), mn_chng = mean(ChngExtentDry), 
            mn_sum = mean(SumDaysDry), max_sum = max(SumDaysDry), 
            max_extent = max(ExtentDry), max_chng = max(ChngExtentDry)) %>% 
  rename(Year = 2)

sum_dry_irrig %>% 
  select(Year, mn_chng, mn_extent, mn_sum) %>% 
  pivot_longer(cols = "mn_chng":"mn_sum", names_to = "Stat", values_to = "Val") %>% 
  ggplot()+
  geom_point(aes(x = Year, y = Val))+
  geom_line(aes(x = Year, y = Val))+
  facet_wrap((Stat~Reach), scales = "free", nrow = 3)+
  theme_bw()+
  scale_x_continuous(breaks = seq(2010, 2022, by = 1))+
  scale_y_continuous(trans = "log10")+
  theme(axis.text.x=element_text(angle=60,hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

sum_dry_irrig %>% 
  select(Year, mn_chng, mn_extent, mn_sum) %>% 
  pivot_longer(cols = "mn_chng":"mn_sum", names_to = "Stat", values_to = "Val") %>% 
  ggplot()+
  geom_histogram(aes(x = log(Val)), bins = 40)+
  facet_wrap((Stat~Reach), scales = "free", nrow = 3)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60,hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Visualize fish ####
#visualization of annual density
datfish %>% 
  ggplot(aes(x = Year, y = MnCpue))+
  geom_line()+
  facet_grid(Species_Codes ~ Reach, scales = "free_y")+
  theme_bw()+
  scale_x_continuous(breaks = seq(2010, 2022, by = 1))+
  scale_y_continuous(trans = "log10")+
  theme(axis.text.x=element_text(angle=60,hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

datfish %>% 
  ggplot(aes(x = log(MnCpue+0.001)))+
  geom_histogram()+
  facet_grid((Species_Codes~Reach), scales = "free")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=60,hjust=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Wrangle fish and drying mean by year####
DatFishDryAll <- datfish %>% 
  left_join(sum_dry_irrig) %>% 
  filter(Year != 2022) %>% 
  mutate(z_extent = scale(mn_extent), z_chng = scale(mn_chng), 
         z_sum = scale(mn_sum), abs_chng = abs(mn_chng),
         Species_Codes = as.factor(Species_Codes), Year = as.factor(Year),
         Reach = as.factor(Reach))

DatFishDry <- datfish %>% 
  left_join(sum_dry_irrig) %>% 
  filter(Year != 2022 & MnCpue != 0) %>% 
  mutate(sc_extent = scale(mn_extent), sc_chng = scale(mn_chng), 
         sc_sum = scale(mn_sum), abs_chng = abs(mn_chng),
         Species_Codes = as.factor(Species_Codes), Year = as.factor(Year),
         Reach = as.factor(Reach))

DatFishDryIs <- DatFishDry %>% 
  filter(Reach == "Isleta") %>% 
  filter(Year != 2022 & MnCpue != 0) %>% 
  mutate(sc_extent = scale(mn_extent), sc_chng = scale(mn_chng), 
         sc_sum = scale(mn_sum), abs_chng = abs(mn_chng))

DatFishDryIs_hybama <- DatFishDry %>% 
  filter(Reach == "Isleta" & Species_Codes == "HYBAMA") %>% 
  filter(Year != 2022 & MnCpue != 0) %>% 
  mutate(sc_extent = scale(mn_extent), sc_chng = scale(mn_chng), 
         sc_sum = scale(mn_sum), abs_chng = abs(mn_chng))

DatFishDryIs_plagra <- DatFishDry %>% 
  filter(Reach == "Isleta" & Species_Codes == "PLAGRA") %>% 
  filter(Year != 2022 & MnCpue != 0) %>% 
  mutate(sc_extent = scale(mn_extent), sc_chng = scale(mn_chng), 
         sc_sum = scale(mn_sum), abs_chng = abs(mn_chng))

DatFishDryIs_cyplut <- DatFishDry %>% 
  filter(Reach == "Isleta" & Species_Codes == "CYPLUT") %>% 
  filter(Year != 2022 & MnCpue != 0) %>% 
  mutate(sc_extent = scale(mn_extent), sc_chng = scale(mn_chng), 
         sc_sum = scale(mn_sum), abs_chng = abs(mn_chng))

DatFishDrySanA <- DatFishDry %>% 
  filter(Reach == "San Acacia") %>% 
  filter(Year != 2022 & MnCpue != 0) %>% 
  mutate(sc_extent = scale(mn_extent), sc_chng = scale(mn_chng), 
         sc_sum = scale(mn_sum), abs_chng = abs(mn_chng))

#Wrangle fish and drying mean by rm ####
DatFishDry_RM <- datfish_rm %>% 
  left_join(sum_dry_irrig) %>% 
  filter(Year != 2022) %>% 
  mutate(Species_Codes = as.factor(Species_Codes), Year = as.factor(Year),
         RM_Start = as.factor(RM_Start))

#Visualize fish drying ####
DatFishDry_RM %>% 
  filter(CPUE != 0) %>% 
  ggplot(aes(x = mn_extent, y = CPUE))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(Reach ~ Species_Codes, scales = "free_y")+
  scale_y_continuous(trans = "log10", labels = scales::comma)

DatFishDry_RM %>% 
  ggplot(aes(x = Year, y = RM_Start)) + 
  geom_point()

DatFishDry_RM %>% 
  ggplot(aes(x = max_chng, y = CPUE))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(Reach ~ Species_Codes, scales = "free_y")+
  scale_y_continuous(trans = "log10", labels = scales::comma)

DatFishDry %>% 
  ggplot(aes(x = mn_sum, y = MnCpue))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(Reach ~ Species_Codes, scales = "free_y")+
  scale_y_continuous(trans = "log10", labels = scales::comma)

DatFishDry %>% 
  ggplot(aes(x = max_sum, y = MnCpue))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(Reach ~ Species_Codes, scales = "free_y")+
  scale_y_continuous(trans = "log10", labels = scales::comma)

DatFishDry %>% 
  ggplot(aes(x = max_chng, y = MnCpue))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(Reach ~ Species_Codes, scales = "free_y")+
  scale_y_continuous(trans = "log10", labels = scales::comma)

#Modeling Lognormal Bayes ####

#regular Bayes
get_prior(MnCpue ~ Species_Codes*max_extent + (1 | Year/Reach),
          data = DatFishDry)

p2 = c(set_prior("normal(0,1)", class = "b"))

mod1 <- brm(MnCpue ~  0 + Species_Codes*max_extent, 
                data = DatFishDrySanA,
                chains = 3,
                warmup = 500,
                iter = 4000,
                sample_prior = TRUE,
                family = lognormal(),
                prior = p2,
                cores = 4)
beep(sound = 3)

mod2 <- brm(MnCpue ~  0 + Species_Codes*max_extent + (1|Reach) + (1|Year), 
            data = DatFishDry,
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
conditional_effects(mod2)


p4 <- pp_check(mod2)
p4+scale_x_continuous(trans="log10")
ce <- conditional_effects(mod1)
saveRDS(mod1, "ModelOutput/MaxSpec.rds")
mod1 <- readRDS("ModelOutput/MaxE_YearReach.rds")
mod2 <- readRDS("ModelOutput/MaxSpec.rds")

plot(mod2)

conditional_effects(mod1)

#plot conditional effects wrapped by species
for_pl <- ce$`max_extent:Species_Codes`
sum_pl <- summary(mod2)
for_pl_est <- sum_pl$fixed %>% 
  slice(9:16) %>% 
  select(Estimate) %>% 
  rename(ModCoeff = Estimate) %>% 
  mutate(ModCoeff = round(ModCoeff,4)) %>% 
  mutate(Species_Codes = c("CARCAR", "CYPCAR", "CYPLUT", "GAMAFF", "HYBAMA", "ICTPUN", "PIMPRO", "PLAGRA")) 

to_plot <- for_pl %>% 
  left_join(for_pl_est, by = "Species_Codes")

MaxExtent_All <- ggplot(to_plot, aes(x = max_extent, y = estimate__)) +
  geom_line(color = "blue", size = 1)+
  geom_ribbon(aes(ymin=for_pl$lower__, ymax=for_pl$upper__),
              linetype = 2, alpha = 0.1)+
  facet_wrap(vars(Species_Codes), scales = "free")+
  geom_text(aes(x = 15, y = 1, label = paste0("Coeff =  ", ModCoeff)))+
  ylab("Model Catch Per Unit Effort") + xlab("Max annual extent of dry river miles")+
  theme_bw()

MaxExtent_YearReachAdd<- ggplot(to_plot, aes(x = max_extent, y = estimate__)) +
  geom_line(color = "blue", size = 1)+
  geom_ribbon(aes(ymin=for_pl$lower__, ymax=for_pl$upper__),
              linetype = 2, alpha = 0.1)+
  facet_wrap(vars(Species_Codes), scales = "free")+
  geom_text(aes(x = 10, y = 12.5, label = paste0("Coeff =  ", ModCoeff)))+
  ylab("Model Catch Per Unit Effort") + xlab("Max annual extent of dry river miles")+
  theme_bw()



#Modeling hurdle ####
get_prior(bf(CPUE ~  Species_Codes*max_extent*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), 
          hu~  Species_Codes*max_extent*Reach + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)),
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

m1<-brm(bf(CPUE ~ Species_Codes*max_extent + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start), 
           hu~ Species_Codes*max_extent + (1|Year) + (1|Year:Reach) + (1|Year:Reach:RM_Start)),
        family = hurdle_lognormal(),
        prior = p1,
        data=DatFishDry_RM,
        chains = 3,
        warmup = 500,
        iter=4000,
        sample_prior = TRUE,
        cores = 4)

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


p4 <- pp_check(m3)
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

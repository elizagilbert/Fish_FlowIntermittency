#Read me ####
#The purpose of this script is to develop figures for the manuscript

#Libraries ####
library(tidyverse)
library(lubridate)
library(plyr) #this overwrites a number of functions from dplyr so need to call it specifically from dplyr
library(brms)
library(scales)
library(gridExtra)

#Data ####
datfish_rm<- read.csv("Data/Processed/RGFishCPUE_RM.csv")
dat_drying <- read.csv("C:/Users/eigilbert/OneDrive - DOI/Documents/UNM/RiverDrying/Chapter1/Scratch_Chap1_R/Data/Processed/2010_2021_WetDryTenths.csv") %>% 
  mutate(Reach = case_when(RMTenthDry < 116 ~ "San Acacia",
                           TRUE ~ "Isleta"),
         Date = as.Date(Date, format = "%Y-%m-%d"))

#fish ####
#cpue data showing zero inflation ##
newlabels<- c("CYPCAR" = "Common Carp", "CYPLUT" = "Red Shiner",
              "PLAGRA" = "Flathead Chub", "ICTPUN" = "Channel Catfish", 
              "HYBAMA" = "Rio Grande Silvery Minnow",
              "PIMPRO" = "Fathead Minnow", "GAMAFF" = "Mosquitofish", "CARCAR" = "River Carpsucker")

  #histogram
tiff("Figures/RawCPUE_Species.jpg", units= "in", width = 7.5, height = 6, res = 600)
datfish_rm %>% 
  mutate(across(Species_Codes, ~factor(., levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                                   "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" ))),
         Reach = as.factor(Reach)) %>% 
  ggplot(aes(x = (CPUE_m*100), fill = Reach))+
  geom_histogram()+
  facet_wrap(vars(Species_Codes), scales = "free", labeller = labeller(Species_Codes= newlabels))+
  xlab (expression(paste("Density (#/", m^2, ")", sep="")))+ 
  ylab("Count")+
  theme_classic()+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme(
    legend.position = c(1,0), # bottom right position
    legend.justification = c(1, 0), # bottom right justification
    legend.box.margin = margin(5, l = 5, unit = "mm") # small margin
  )
dev.off()

datfish_rm %>% 
  ggplot(aes(x = CPUE_m, fill = Reach))+
  geom_density()

#drying ####

#daily change in river miles dry (extent - # river miles)
ExtentChngDry_Irrig <- dat_drying %>%
  dplyr::select(!X) %>% 
  filter(DryRM == 0) %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = (sum(DryRM == 0)/10)*1.6) %>% 
  ungroup() %>% 
  group_by(Reach) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0),
         ChngExtentDry = ExtentDry - lag(ExtentDry, default = ExtentDry[1])) %>% 
  ungroup() %>% 
  filter(between(month(Date), 4, 11))

#cummulative days and length of drying
KiloMDays_Irrig <- dat_drying %>% 
  dplyr::select(!X) %>% 
  filter(Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(Reach, year(Date)) %>% 
  mutate(MD = (cumsum(DryRM2)/10)*1.6) %>% 
  ungroup() %>% 
  filter(between(month(Date), 4, 11)) %>% 
  group_by(Reach,year(Date), Date) %>% 
  summarise(Daily_KiloMDayes = max(MD)) %>% 
  ungroup() %>% 
  rename(Year = 2) %>% 
  select(!Date)

newlabels2<- c("ExtentDry" = "Extent dry", "ChngExtentDry" = "Change in extent dry", "Daily_KiloMDayes" = "Kilometer-days dry")

p1 <- cbind(ExtentChngDry_Irrig, KiloMDays_Irrig) %>% 
  pivot_longer(cols = c("ExtentDry", "ChngExtentDry", "Daily_KiloMDayes"), names_to = "Metric", values_to = "values") %>% 
  mutate(DOY = yday(Date)) %>%
  filter(Year >= 2016) %>% 
  mutate(across(Metric, ~factor(., levels=c("ExtentDry", "ChngExtentDry", "Daily_KiloMDayes")))) %>% 
  ggplot(aes(x = DOY, y = values, color = Reach))+
  geom_line(size = 1)+
  facet_grid(Metric~Year, scales = "free", labeller = labeller(Metric= newlabels2))+
  theme_classic()+
  xlab("Day of year") + ylab("Kilometers")+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))

p2 <- cbind(ExtentChngDry_Irrig, KiloMDays_Irrig) %>% 
  pivot_longer(cols = c("ExtentDry", "ChngExtentDry", "Daily_KiloMDayes"), names_to = "Metric", values_to = "values") %>% 
  mutate(DOY = yday(Date)) %>%
  filter(Year < 2016) %>% 
  mutate(across(Metric, ~factor(., levels=c("ExtentDry", "ChngExtentDry", "Daily_KiloMDayes")))) %>% 
  ggplot(aes(x = DOY, y = values, color = Reach))+
  geom_line(size = 1)+
  facet_grid(Metric~Year, scales = "free", labeller = labeller(Metric= newlabels2))+
  theme_classic()+
  xlab("Day of year") + ylab("Kilometers")+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme(legend.position = "none", axis.text.x = element_blank()) + xlab("")
  
#line plots
tiff("Figures/DailyDryingMetrics.jpg", units= "in", width = 8, height = 10, res = 600)
grid.arrange(p2,p1)
dev.off()

#hurdle conditional effects ####
m3 <- readRDS("Models/MaxExtent.rds")
ce <- conditional_effects(m3)
for_pl <- ce$`Max_Extent:Species_Codes`

 #I think this is overall effects
tiff("Figures/Hurdleoverall_MaxExtent.jpg", units= "in", width = 8, height = 6, res = 600)
for_pl %>% 
  mutate(across(Species_Codes, ~factor(., levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                                   "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" )))) %>% 
  ggplot(aes(x = Max_Extent, y = estimate__)) +
  geom_line(color = "blue", size = 1)+
  geom_ribbon(aes(ymin=for_pl$lower__, ymax=for_pl$upper__),
              linetype = 2, alpha = 0.1)+
  facet_wrap(vars(Species_Codes), scales = "free", labeller = labeller(Species_Codes= newlabels) )+
  ylab("Model Catch Per Unit Effort") + xlab("Max annual extent of dry river miles")+
  theme_classic()
dev.off()

 #I think this is just when fish present - "mu"
ce2 <- conditional_effects(m3, effects = "Max_Extent:Species_Codes", dpar="mu")
for_pl2 <- ce2$`Max_Extent:Species_Codes`

tiff("Figures/HurdleMu_MaxExtent.jpg", units= "in", width = 8, height = 6, res = 600)
for_pl2 %>% 
  mutate(across(Species_Codes, ~factor(., levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                                   "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" )))) %>% 
  ggplot(aes(x = Max_Extent, y = estimate__)) +
  geom_line(color = "blue", size = 1)+
  geom_ribbon(aes(ymin=for_pl2$lower__, ymax=for_pl2$upper__),
              linetype = 2, alpha = 0.1)+
  facet_wrap(vars(Species_Codes), scales = "free", labeller = labeller(Species_Codes= newlabels) )+
  ylab("Model Catch Per Unit Effort - 'mu'") + xlab("Max annual extent of dry river miles")+
  theme_classic()
dev.off()

  #"hu" #####
MaxExtent <- readRDS("Models/MaxExtent.rds")

conditions <- make_conditions(MaxExtent, "Species_Codes")

ce3 <- conditional_effects(MaxExtent, "Max_Extent:Reach", conditions = conditions, dpar="hu")
for_pl3 <- ce3$`Max_Extent:Reach` %>% 
  mutate(Max_Extent = Max_Extent*1.6) 

for_pl3 <- for_pl3[!(for_pl3$Reach == "Isleta" & for_pl3$Max_Extent > 40 ),] 


tiff("Figures/HurdleHu_MaxExtent.jpg", units= "in", width = 8, height = 6, res = 600)
for_pl3 %>% 
  mutate(Species_Codes = factor(Species_Codes, levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                     "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" ))) %>% 
  ggplot(aes(x = Max_Extent, y = (1-estimate__), color = Reach, fill = Reach)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin=(1-for_pl3$lower__), ymax=1-(for_pl3$upper__), group = Reach),
              linetype = 1, alpha = 0.6)+
  facet_wrap(vars(Species_Codes), labeller = labeller(Species_Codes= newlabels) )+
  ylab(expression(paste("1- ", italic("hu")))) + xlab(expression(paste("Maximum ", italic("Extent"), "(km)")))+
  theme_classic()+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme(
    legend.position = c(1,0), # bottom right position
    legend.justification = c(1, 0), # bottom right justification
    legend.box.margin = margin(5, l = 5, unit = "mm") # small margin
  )
dev.off()


conditional_effects(m3)

#hurdle pp_epred ####

mod_maxextent <- readRDS("Models/MaxExtent.rds")
newdata <-  crossing(Species_Codes = c("CYPLUT", "PLAGRA", "ICTPUN", "CYPCAR", "HYBAMA", "PIMPRO", "CARCAR", "GAMAFF"),
                     Reach = c("San Acacia", "Isleta"), Max_Extent = seq(0,40,5)) 

pp_epredp <- as.data.frame(posterior_epred(mod_maxextent,
                                           re_formula = NA,
                                           ndraws = 500,
                                           summary = T,
                                           newdata = newdata))

columnnames <- newdata %>% 
  mutate(Reach = gsub(' ', '', Reach)) %>% 
  mutate(names = paste0(Species_Codes, "_", Reach, "_", Max_Extent))
colnames(pp_epredp) <- columnnames$names

pp_epredp_long <- pp_epredp %>% 
  pivot_longer(1:144, names_to = "Names", values_to = "Values") %>% 
  separate(Names, c("Species", "Reach", "Max_Extent")) %>% 
  mutate(Max_Extent = as.numeric(Max_Extent))

stats_epredp <- ddply(pp_epredp_long, c("Species", "Reach", "Max_Extent"), summarise,
                    mean = mean(Values),
                    CI_L = quantile(Values, probs = 0.025),
                    CI_U = quantile(Values, probs = 0.975)) %>% 
  mutate(Max_Extent = as.numeric(Max_Extent*1.6), 
         Species = factor(Species, levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                               "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" )))

stats_epredp <- stats_epredp[!(stats_epredp$Reach == "Isleta" & stats_epredp$Max_Extent > 40 ),] 

newlabels<- c("CYPLUT" = "Red Shiner", "CYPCAR" = "Common Carp", 
              "PLAGRA" = "Flathead Chub", "ICTPUN" = "Channel Catfish", 
              "HYBAMA" = "RG Silvery Minnow",
              "PIMPRO" = "Fathead Minnow", "GAMAFF" = "Mosquitofish", "CARCAR" = "River Carpsucker")

tiff("Figures/Predicted_MaxExtent.jpg", units= "in", width = 8, height = 6, res = 600)
stats_epredp %>% 
  ggplot(aes(x = Max_Extent, y = mean, color = Reach, fill = Reach))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin=CI_L, ymax=CI_U, group = Reach), alpha = 0.6, size = 0.5)+
  facet_wrap(vars(Species), scales = "free_y", labeller = labeller(Species= newlabels))+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme_classic()+
  scale_y_continuous(trans = "log10", labels = comma)+
  ylab(expression(paste(italic("mu")))) + xlab(expression(paste("Maximum ", italic("Extent"), "(km)")))+
  theme(
    legend.position = c(1,0), # bottom right position
    legend.justification = c(1, 0), # bottom right justification
    legend.box.margin = margin(5, l = 5, unit = "mm") # small margin
  )
dev.off()            




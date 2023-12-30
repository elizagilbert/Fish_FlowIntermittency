#Read me ####
#The purpose of this script is to develop figures for the manuscript

#Libararies ####
library(tidyverse)
library(lubridate)

#Data ####
datfish_rm<- read.csv("Data/Processed/RGFishCPUE_RM.csv")
dat_drying <- read.csv("C:/Users/eigilbert/OneDrive - DOI/Documents/UNM/RiverDrying/Chapter1/Scratch_Chap1_R/Data/Processed/2010_2021_WetDryTenths.csv") %>% 
  mutate(Reach = case_when(RMTenthDry < 116 ~ "San Acacia",
                           TRUE ~ "Isleta"),
         Date = as.Date(Date, format = "%Y-%m-%d"))

#fish data ###
 #cpue data showing zero inflation ####
newlabels<- c("CYPCAR" = "Common Carp", "CYPLUT" = "Red Shiner",
              "PLAGRA" = "Flathead Chub", "ICTPUN" = "Channel Catfish", 
              "HYBAMA" = "Rio Grande Silvery Minnow",
              "PIMPRO" = "Fathead Minnow", "GAMAFF" = "Mosquitofish", "CARCAR" = "River Carpsucker")

  #histogram
tiff("Figures/RawCPUE_Species.jpg", units= "in", width = 7.5, height = 6, res = 600)
datfish_rm %>% 
  mutate(across(Species_Codes, ~factor(., levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                                   "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" )))) %>% 
  ggplot(aes(x = (CPUE_m)*100))+
  geom_histogram(binwidth = 1)+
  facet_wrap(vars(Species_Codes), scales = "free", labeller = labeller(Species_Codes= newlabels))+
  xlab("CPUE") + ylab("Count")+
  theme_classic()
dev.off()

#daily drying metrics ####

#daily change in river miles dry (extent - # river miles)
ExtentChngDry_Irrig <- dat_drying %>%
  select(!X) %>% 
  filter(DryRM == 0) %>% 
  group_by(Date) %>% 
  summarise(ExtentDry = (sum(DryRM == 0)/10)*1.6) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0),
         ChngExtentDry = ExtentDry - lag(ExtentDry, default = ExtentDry[1])) %>% 
  ungroup() %>% 
  filter(between(month(Date), 4, 11))

#cummulative days and length of drying
KiloMDays_Irrig <- dat_drying %>% 
  select(!X) %>% 
  filter(Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date)) %>% 
  mutate(MD = (cumsum(DryRM2)/10)*1.6) %>% 
  ungroup() %>% 
  filter(between(month(Date), 4, 11)) %>% 
  group_by(year(Date), Date) %>% 
  summarise(Daily_KiloMDayes = max(MD)) %>% 
  ungroup() %>% 
  rename(Year = 1) %>% 
  select(!Date)

newlabels2<- c("ExtentDry" = "Extent dry", "ChngExtentDry" = "Change in extent dry", "Daily_KiloMDayes" = "Kilometer-days dry")

  #line plots
tiff("Figures/DailyDryingMetrics.jpg", units= "in", width = 8, height = 6, res = 600)
cbind(ExtentChngDry_Irrig, KiloMDays_Irrig) %>% 
  pivot_longer(cols = c("ExtentDry", "ChngExtentDry", "Daily_KiloMDayes"), names_to = "Metric", values_to = "values") %>% 
  mutate(DOY = yday(Date)) %>%
  filter(Year >= 2018) %>% 
  mutate(across(Metric, ~factor(., levels=c("ExtentDry", "ChngExtentDry", "Daily_KiloMDayes")))) %>% 
  ggplot(aes(x = DOY, y = values))+
  geom_line()+
  facet_grid(Metric~Year, scales = "free", labeller = labeller(Metric= newlabels2))+
  theme_classic()+
  xlab("Day of year") + ylab("Value")
dev.off()
  

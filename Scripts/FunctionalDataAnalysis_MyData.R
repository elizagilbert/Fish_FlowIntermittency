#Read me ####
#the purpose of this code is to understand the curves in the drying metrics

#Libraries ####
library(tidyverse)
library(lubridate)
#library(fda) #seems like it messes up some tidy functions like "complete"

#data ####
dat2 <- read.csv("C:/Users/eigilbert/OneDrive - DOI/Documents/UNM/RiverDrying/Chapter1/Scratch_Chap1_R/Data/Processed/2010_2021_WetDryTenths.csv")
dat_drying <- dat2 %>% 
  mutate(Reach = case_when(RMTenthDry < 116 ~ "San Acacia",
                           TRUE ~ "Isleta"),
         Date = as.Date(Date, format = "%Y-%m-%d"))

#wrangle ####
#daily change in river miles dry (extent - # river miles)
ExtentChngDry_Irrig <- dat_drying %>%
  select(!X) %>% 
  filter(DryRM == 0) %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0),
         ChngExtentDry = ExtentDry - lag(ExtentDry, default = ExtentDry[1])) %>% 
  ungroup() %>% 
  filter(between(month(Date), 4, 10))

MileDays_Irrig <- dat_drying %>% 
  select(!X) %>% 
  filter(Date >= "2010-01-01") %>% 
  mutate(DryRM2 = case_when(DryRM == 0 ~ 1,
                            TRUE ~ 0)) %>% 
  arrange(Date, RMTenthDry) %>% 
  group_by(year(Date), Reach) %>% 
  mutate(MD = cumsum(DryRM2)/10) %>% 
  ungroup() %>% 
  filter(between(month(Date), 4, 10)) %>% 
  group_by(Reach, Date) %>% 
  summarise(Daily_MaxMileDays = max(MD)) %>% 
  ungroup() 

DryingDat <- ExtentChngDry_Irrig %>% 
  full_join(MileDays_Irrig, by = c("Date", "Reach")) %>% 
  mutate(DOY = yday(Date)) %>% 
  select(Date, DOY, Reach, ExtentDry, ChngExtentDry, Daily_MaxMileDays)

#simple visualizing yearly curves ####
DryingDat %>% 
  mutate(Year = as.factor(year(Date))) %>% 
  ggplot(aes(x = DOY, y = ExtentDry, color = Year))+
  geom_line()+
  facet_wrap(vars(Reach))

DryingDat %>% 
  mutate(Year = as.factor(year(Date))) %>% 
  ggplot(aes(x = DOY, y = ChngExtentDry, color = Year))+
  geom_line()+
  facet_wrap(vars(Reach))

DryingDat %>% 
  mutate(Year = as.factor(year(Date))) %>% 
  ggplot(aes(x = DOY, y = Daily_MaxMileDays, color = Year))+
  geom_line()+
  facet_wrap(vars(Reach))

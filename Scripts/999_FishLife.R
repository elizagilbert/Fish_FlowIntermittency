library(tidyverse)

#Data Drying ####
dat2 <- read.csv("C:/Users/eigilbert/OneDrive - DOI/Documents/UNM/RiverDrying/Chapter1/Scratch_Chap1_R/Data/Processed/2010_2021_WetDryTenths.csv")
dat_drying <- dat2 %>% 
  mutate(Reach = case_when(RMTenthDry < 116 ~ "San Acacia",
                           TRUE ~ "Isleta"),
         Date = as.Date(Date, format = "%Y-%m-%d"))

#Wrangle Drying ####

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
  filter(between(month(Date), 4, 10), Reach == "Isleta") %>% 
  select(Date, ExtentDry)

write.csv(ExtentChngDry_Irrig, "Data/Processed/ForIHA_ExtentIsleta.csv", row.names = F)

         
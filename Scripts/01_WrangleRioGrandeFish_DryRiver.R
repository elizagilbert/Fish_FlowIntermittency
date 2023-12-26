#Read me ###
#The purpose of this script it to wrangle the Rio Grande fish data from Rob Dudley


#Libraries ####
library(tidyverse)
library(lubridate)


#Data Fish ####
dat <- read.csv("Data/Raw/RGSM_Pop_Mon_Query_Monthly_Station_USBR (EIG).csv")

#Wrangle Fish ####
  #converting to an integer catch per unit effort (CPUE) for Isleta and San Acacia reaches
dat_fish <- dat %>% 
  mutate(Date = as.Date(Date.Collected, format = "%m/%d/%Y"),
         CPUE_m = SumOfSPEC/Effort.m.2,
         Year = year(Date)) %>% 
  filter(Reach != "Angostura", Year != 2022) %>% 
  select(Reach, Station, Year, Date, RM_Start, Species_Codes, CPUE_m)

  #inserting zeros to locations where a given species wasn't captured
stations <- unique(dat_fish$Station) #length = 229
species_code <- unique(dat_fish$Species_Codes) #length = 24
  
station_species <- expand.grid(stations, species_code) %>% #should be 5038 once remove dry sites
  rename(Station = 1, Species_Codes = 2) %>% 
  filter(Species_Codes != "SITE DRY" &  Species_Codes != "Site Dry")

AllStation_Species <- station_species %>%  #should be 4580 once remove no fish records
  left_join(dat_fish, by = c("Station", "Species_Codes")) %>% 
  mutate(CPUE_m = coalesce(CPUE_m, 0)) %>% 
  group_by(Station) %>% 
  fill(c("Reach", "Year", "Date", "RM_Start"), .direction = "up") %>% 
  fill(c("Reach", "Year", "Date", "RM_Start"), .direction = "down") %>% 
  filter(Species_Codes != "NO FISH" &  Species_Codes != "No Fish Collected") %>% 
  ungroup() 


  #removing species whose frequency was <15% 
  #Stations with NA are those that were dry
temp2 <- AllStation_Species %>% 
  group_by(Reach, Species_Codes) %>% 
  count(CPUE_m == 0) %>% 
  rename(Grp = 3) %>% 
  pivot_wider(names_from = Grp, values_from = n) %>% 
  rename(NotZero = 3, IsZero = 4) %>% 
  mutate(NotZero = coalesce(NotZero,0)) %>% 
  mutate(Percent = NotZero/(NotZero + IsZero)*100) %>% 
  drop_na() %>% 
  filter(Percent >= 15)


CommonSpecies_Dat <- AllStation_Species %>% 
  right_join(temp2, by = c("Species_Codes", "Reach")) %>% 
  select(RM_Start, Reach, Year, Species_Codes, CPUE_m) %>% 
  mutate(RM_Start = round(RM_Start,0)) 


#write data
write.csv(CommonSpecies_Dat, "Data/Processed/RGFishCPUE_RM.csv", row.names = F)


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
  filter(between(month(Date), 4, 10)) %>% 
  group_by(Reach, year(Date)) %>% 
  summarise(Max_Extent = max(ExtentDry), Mean_Extent = mean(ExtentDry), SD_Extent = sd(ExtentDry),
            Max_Change = max(ChngExtentDry), Mean_Change = mean(ChngExtentDry), SD_Change = sd(ChngExtentDry)) %>% 
  ungroup() %>% 
  rename(Year = 2)

#cummulative days and length of drying
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
  group_by(Reach, year(Date), Date) %>% 
  summarise(Daily_MaxMileDayes = max(MD)) %>% 
  ungroup() %>% 
  group_by(Reach, year(Date)) %>% 
  summarise(Max_MileDays = max(Daily_MaxMileDayes), Mean_MileDays = mean(Daily_MaxMileDayes), 
            SD_MileDays = sd(Daily_MaxMileDayes)) %>% 
  ungroup() %>% 
  rename(Year = 2)

#combine covariates to save 
DryCovariates_ByReach <- ExtentChngDry_Irrig %>% 
  left_join(MileDays_Irrig)

write.csv(DryCovariates_ByReach, "Data/Processed/RGDryCovariates_ByReach_Irrig.csv", row.names = F)


dat <- read.csv("Data/Processed/RGDryCovariates_ByReach_Irrig.csv")

dat %>% 
  pivot_longer(cols = Max_Extent:Max_MileDays, names_to = "metric", values_to = "Summary") %>% 
  ggplot(aes(x = Year, y = (Summary+0.000001))) +
  geom_line()+
  facet_grid(vars(metric), scales = "free")+
  scale_y_continuous(trans = "log", labels = scales::number_format(accuracy = 0.01, decimal.mark = "."))+
  ylab("Summary") + xlab("Year")+
  theme_classic()

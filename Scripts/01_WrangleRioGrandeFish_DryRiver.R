#Read me ###
#The purpose of this script it to wrangle the Rio Grande fish data from Rob Dudley


#Libraries ####
library(tidyverse)
library(lubridate)
library(corrr)


#Data Fish ####
dat <- read.csv("Data/Raw/RGSM_Pop_Mon_Query_Monthly_Station_USBR (EIG).csv")

#Wrangle Fish ####
  #converting to catch per unit effort (CPUE) for Isleta and San Acacia reaches
dat_fish <- dat %>% 
  mutate(Date = as.Date(Date.Collected, format = "%m/%d/%Y"),
         CPUE_m = SumOfSPEC/Effort.m.2,
         Year = year(Date)) %>% 
  filter(Reach != "Angostura", between(Year, 2010, 2021)) %>% 
  filter(Station != "RKD16-144" &  Station != "RKD19-134" &     #removing isolated pools 
           Station != "RKD20-138" &  Station != "RKD20-139") %>% 
  select(Reach, Station, Year, Date, RM_Start, Species_Codes, CPUE_m)

stations <- unique(dat_fish$Station) #length = 205 
species_code <- unique(dat_fish$Species_Codes) #length = 22 includes 2 site dry species codes
  
   #expanding grid so I can add zeros to all species when not caught and removing sites dry
   #should be 4100 once remove site dry (n=2)
station_species <- expand.grid(stations, species_code) %>% 
  rename(Station = 1, Species_Codes = 2) %>% 
  filter(Species_Codes != "SITE DRY" &  Species_Codes != "Site Dry") 

  #adding in zeros and removing the no fish designations,
AllStation_Species <- station_species %>%  
  left_join(dat_fish, by = c("Station", "Species_Codes")) %>% 
  mutate(CPUE_m = coalesce(CPUE_m, 0)) %>% 
  group_by(Station) %>% 
  fill(c("Reach", "Year", "Date", "RM_Start"), .direction = "up") %>% 
  fill(c("Reach", "Year", "Date", "RM_Start"), .direction = "down") %>% 
  filter(Species_Codes != "NO FISH" &  Species_Codes != "No Fish Collected") %>%  
  ungroup() 

  #removing species whose frequency was <5%, stations with NA are those that were dry 
  #and were dropped from data set
temp2 <- AllStation_Species %>% 
  group_by(Species_Codes) %>% 
  count(CPUE_m == 0) %>% 
  rename(Grp = 2) %>% 
  pivot_wider(names_from = Grp, values_from = n) %>% 
  rename(NotZero = 2, IsZero = 3) %>% 
  mutate(NotZero = coalesce(NotZero,0)) %>% 
  mutate(Percent = NotZero/(NotZero + IsZero)*100) %>% 
  drop_na() %>% 
  filter(Percent >= 10)


CommonSpecies_Dat <- AllStation_Species %>% 
  right_join(temp2, by = c("Species_Codes")) %>% 
  select(RM_Start, Reach, Year, Species_Codes, CPUE_m) %>% 
  mutate(RM_Start = round(RM_Start,0)) %>% 
  na.omit()


#write data
write.csv(CommonSpecies_Dat, "Data/Processed/RGFishCPUE_RM.csv", row.names = F)


#Data Drying ####
dat2 <- read.csv("Data/Processed/2010_2021_WetDryTenths.csv")
dat_drying <- dat2 %>% 
  mutate(Reach = case_when(RMTenthDry < 116 ~ "San Acacia",
                           TRUE ~ "Isleta"),
         Date = as.Date(Date, format = "%Y-%m-%d"))
#CV function
cv <- function(x, na.rm = FALSE)  sd(x, na.rm = na.rm)/mean(x, na.rm = na.rm)

#Wrangle Drying ####

#Extent and daily change in river miles dry (spatial - # river miles)
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
  summarise(Max_Extent = max(ExtentDry), Mean_Extent = mean(ExtentDry), 
            SD_Extent = sd(ExtentDry), CV_Extent = cv(ExtentDry),
            Max_Change = max(ChngExtentDry), Mean_Change = mean(ChngExtentDry), 
            SD_Change = sd(ChngExtentDry), CV_Change = cv(ChngExtentDry)) %>% 
  ungroup() %>% 
  rename(Year = 2)

#cummulative days of drying (spatial and temporal)
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
            SD_MileDays = sd(Daily_MaxMileDayes), CV_MileDays = cv(Daily_MaxMileDayes)) %>% 
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



#correlation drying regimes ####
correlation_matrix_is <- dat %>%
  mutate(Year = as.factor(Year)) %>%
  filter(Reach == "Isleta") %>% 
  select(where(is.numeric)) %>%
  correlate() %>% 
  mutate(across(everything(), ~ ifelse(. > 0.5, ., NA)))



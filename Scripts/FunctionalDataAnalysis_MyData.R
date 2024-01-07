#Read me ####
#the purpose of this code is to understand the curves in the drying metrics

#Libraries ####
library(tidyverse)
library(lubridate)
library(fda) #seems like it messes up some tidy functions like "complete" and maybe "select"

#data ####
dat2 <- read.csv("C:/Users/eigilbert/OneDrive - DOI/Documents/UNM/RiverDrying/Chapter1/Scratch_Chap1_R/Data/Processed/2010_2021_WetDryTenths.csv")
dat_drying <- dat2 %>% 
  mutate(Reach = case_when(RMTenthDry < 116 ~ "San Acacia",
                           TRUE ~ "Isleta"),
         Date = as.Date(Date, format = "%Y-%m-%d"))

#wrangle ####
#daily change in river miles dry (extent - # river miles)
ExtentChngDry_Irrig <- dat_drying %>%
  dplyr::select(!X) %>% 
  filter(DryRM == 0) %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  tidyr::complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0),
         ChngExtentDry = ExtentDry - lag(ExtentDry, default = ExtentDry[1])) %>% 
  ungroup() %>% 
  filter(between(month(Date), 4, 10))

MileDays_Irrig <- dat_drying %>% 
  dplyr::select(!X) %>% 
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
  dplyr::select(Date, DOY, Reach, ExtentDry, ChngExtentDry, Daily_MaxMileDays)

#simple visualizing yearly curves ####

 #Extent
DryingDat %>% 
  mutate(Year = as.factor(year(Date))) %>% 
  ggplot(aes(x = DOY, y = ExtentDry, color = Year))+
  geom_line()+
  geom_point()+
  facet_wrap(vars(Reach))

 #Change Extent
DryingDat %>% 
  mutate(Year = as.factor(year(Date))) %>% 
  ggplot(aes(x = DOY, y = ChngExtentDry, color = Year))+
  geom_line()+
  facet_wrap(vars(Reach))

  #max miles
DryingDat %>% 
  mutate(Year = as.factor(year(Date))) %>% 
  ggplot(aes(x = DOY, y = Daily_MaxMileDays, color = Year))+
  geom_line()+
  facet_wrap(vars(Reach))

#fda####
fda_extent <- ExtentChngDry_Irrig %>% 
  filter(Reach == "Isleta" & between(Date, as.Date("2020-04-05"), as.Date("2020-10-31"))) %>%  #has to be divisible by 5
  dplyr:: select(Date, ExtentDry)


time_span <- length(fda_extent$Date) #this has to be divisible by 5
time_basis <- seq(0, time_span, 1) #seq 0 to 100 by 1s - if I do this as date, I have to add "1" as another date 
knots <- seq(0, time_span, 5) #seq 0 to 100 by 5s
n_knots <- (length(knots))
n_order <- 4 #order of basis functions: cubic bspline order = 3+1
n_basis = length(knots) - 2 + n_order #from Ramsay, p.35 the number of basis functions = order + number of interior knots 
                                    #(interior knots are always 2 less than total) because outer and inner not counted
basis <-  create.bspline.basis(c(min(time_basis),max(time_basis)),n_basis,n_order,knots)

#plotting basis

plot(basis)

#use eval.basis() to evalute the basis functions at the times (n_obs)
#where our data curve was observed. The matric PHI will
#contain the values of the 45 basis functions evaluated at 210 points
#placed between min and max dates

PHI <- eval.basis(evalarg = fda_extent$ExtentDry, basisobj = basis) #so 210 rows evaluation and 45 columns basis functions

knots_dates <- seq(min(fda_extent$Date), max(fda_extent$Date), by = '5 days')

matplot(fda_extent$Date,PHI, type='l', lwd=2, xlab='time',ylab='basis',cex.lab=1,cex.axis=1)

for (i in 1:knots_dates)
{
  abline(v=knots_dates[i], lty=2, lwd=0.5)
}


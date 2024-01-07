#Read me ####
#the purpose of this code is to understand the curves in the drying metrics

#Libraries ####
library(tidyverse)
library(lubridate)
library(fda) #seems like it messes up some tidy functions like "complete" and maybe "select"
library(reshape2)
library(wesanderson) #colors

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

#fda 2020 only ####
fda_extent <- ExtentChngDry_Irrig %>% 
  filter(Reach == "Isleta" & between(Date, as.Date("2020-04-05"), as.Date("2020-10-31"))) %>%  #has to be divisible by 5
  dplyr:: select(Date, ExtentDry)

time_span <- length(fda_extent$Date) #this has to be divisible by 5
time_basis <- seq(0, time_span, 1) #seq 0 to timespan by 1s 
knots <- seq(0, time_span, 5) #seq 0 to timespan by 5s
n_knots <- (length(knots))
n_order <- 4 #order of basis functions: cubic bspline order = 3+1
n_basis = length(knots) - 2 + n_order #from Ramsay, p.35 the number of basis functions = order + number of interior knots 
              #(interior knots are always 2 less than total) because outer and inner not counted
basis_my <-  create.bspline.basis(c(min(time_basis),max(time_basis)),n_basis,n_order,knots)
plot(basis_my)

time2 <- seq(0,(length(fda_extent$Date)-1), 1)

#estimating the coefficients and function values with a single line of code using smooth.basis()
#this takes the arguments argvals (the times we want to use for evaluation as a vecotr), the y (the observed values) 
#and fdParobj (an fda object containing the basis elements)
extent_obj <- smooth.basis(argvals = time2, y = fda_extent$ExtentDry, fdParobj = basis_my)

plot(time2, fda_extent$ExtentDry, type = "l", xlab = "time", ylab = "f(time)", 
     main = "fda package smoothing estimates", col = "grey")
lines(extent_obj, lwd = 1, col = "blue")

#fda all years ####

#Step 2 functional smoothing #####
Edat_df <- ExtentChngDry_Irrig %>%   
  mutate(Year = year(Date), DOY = yday(Date),
         DOY_2 = case_when(Year == 2012 | Year == 2016 | Year == 2020 ~ DOY-1,
                   TRUE ~ DOY)) %>% 
  group_by(Year) %>% 
  filter(Reach == "Isleta" & between(DOY_2, 95,304)) %>%   #has to be divisible by 5
  pivot_wider(id_cols = DOY_2, names_from = Year, values_from = ExtentDry)


time_span1 <- length(Edat_df$DOY_2) #this has to be divisible by 5
time_basis1 <- seq(0, time_span1, 1) #seq 0 to timespan by 1s 
knots1 <- seq(0, time_span1, 5) #seq 0 to timespan by 5s
n_knots1 <- (length(knots1))
n_order1 <- 4 #order of basis functions: cubic bspline order = 3+1
n_basis1 = length(knots1) - 2 + n_order1 #from Ramsay, p.35 the number of basis functions = order + number of interior knots 
#(interior knots are always 2 less than total) because outer and inner not counted
basis_my1 <-  create.bspline.basis(c(min(time_basis1),max(time_basis1)),n_basis1,n_order1,knots1)
plot(basis_my1)

#need matrix
Edat_mat <- as.matrix(Edat_df %>% 
                        dplyr::select(!DOY_2))

#setup smoothing parameters - not sure how you choose the Lfdobj or the lambda
#fdobj: is the basis functions to use
#Lfdobj: is the derivative degree to smooth
#lambda: is the smoothing penalty
smoothPar = fdPar(fdobj = basis_my1, Lfdobj=2, lambda=1)

#smoothed functional data
dat_fd = smooth.basis(argvals = time2, y = Edat_mat, fdParobj = smoothPar)

'-----------------------------------------------------------------------------'
obs_fd = eval.fd(evalarg = time2, fdobj = dat_fd$fd)

obs_fd = as.data.frame(obs_fd)
obs_fd$DayOfYear = Edat_df$DOY_2
obs_fd_df = melt(obs_fd, measure.vars = 1:12)
colnames(obs_fd_df) = c("DayOfYear", "Year", "ExtentDry")

pal <- rev(wes_palette("Zissou1", n=length(unique(obs_fd_df$Year)), type = "continuous"))

ggplot(data=obs_fd_df) + 
  geom_line(aes(x=DayOfYear, y=ExtentDry, colour=Year), 
            linewidth=1, na.rm = T) +
  scale_color_manual(values = pal) +
  theme_classic(base_size = 14)+
  xlab("Day of Year - Isleta")

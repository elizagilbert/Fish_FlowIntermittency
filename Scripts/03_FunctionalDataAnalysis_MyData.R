#Read me ####
#the purpose of this code is to understand the curves in the drying metrics


#Libraries ####
# install.packages("devtools")
# require(devtools)
# install_version("fda", version = "5.1.4") #need to install this version or things get weird with the CV and lambda

library(tidyverse)
library(lubridate)
library(fda) #seems like it messes up some tidy functions like "complete" and maybe "select"
library(reshape2)
library(wesanderson) #colors

#Wrangle fish ####
datfish <- read.csv("Data/Processed/RGFishCPUE_RM.csv") %>% 
  group_by(Species_Codes, Year, Reach) %>% 
  summarise(MnCPUE = mean(CPUE_m, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(LogMean = log10(MnCPUE+0.001))

Carcar_Isleta <- datfish %>% 
  filter(Species_Codes == "CARCAR" & Reach == "Isleta") %>% 
  select(Year, LogMean) %>% 
  pull(LogMean, Year) #makes a named number using Year to name row

#wrangle drying ####
dat_drying <- read.csv("C:/Users/eigilbert/OneDrive - DOI/Documents/UNM/RiverDrying/Chapter1/Scratch_Chap1_R/Data/Processed/2010_2021_WetDryTenths.csv") %>% 
  mutate(Reach = case_when(RMTenthDry < 116 ~ "San Acacia",
                           TRUE ~ "Isleta"),
         Date = as.Date(Date, format = "%Y-%m-%d"))

#Extent dry - Isleta
#need it to be a matrix with years named as columns and days as rows 
ExtIsleta_Yr <- dat_drying %>%
  dplyr::select(!X) %>% 
  filter(DryRM == 0 & Reach == "Isleta") %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10) %>% 
  tidyr::complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0), Year = year(Date), MnDay = format(Date, format = "%b%d")) %>% 
  ungroup() %>% 
  select(Year, ExtentDry, MnDay) %>% 
  filter(MnDay != "Feb29") %>% 
  pivot_wider(names_from = Year, values_from = ExtentDry) %>% 
  column_to_rownames(var = "MnDay") %>% 
  as.matrix()

#setting up fda ####
ny <- length(Carcar_Isleta) #length of y data



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
Edat_mat <- as.matrix(Edat_df %>% dplyr::select(!DOY_2))

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

#scalar on function regression CARCAR ####

#obtain the annual abundance of fish at Isleta - # years = 12
abund_CARCAR <- datfish %>% 
  filter(Species_Codes == "CARCAR" & Reach == "Isleta") %>% 
  select(Year, MnCPUE) %>% 
  group_by(Year) %>% 
  summarise(MnCPUE = mean(CPUE_m, na.rm = T))

CARCAR <- Carcar_Isleta %>% pull(MnCPUE, Year) #make a named numeric


#define the bspline basis functions
basis_my1 <-  create.bspline.basis(c(min(time_basis1),max(time_basis1)),n_basis1,n_order1,knots1) #from above

#smooth predictor data
smoothPar <-  fdPar(fdobj = basis_my1, Lfdobj=2, lambda=1) #from above
smoothPar <- smoothPar$fd

templist       <-  vector("list",2)
templist[[1]]  <-  rep(1,12) # the first functional covariate
templist[[2]]  <-  smoothPar  # the second functional covariate

#create a constant basis for the intercept  
conbasis   = create.constant.basis(c(0,210)) #this might not be right

plot(conbasis)
  
#combine constant and basis functions into list for regression
betalist1  <-  vector("list",2)
betalist1[[1]] <-  conbasis
betalist1[[2]] <-  basis_my1
  
# fit the functional linear model 
fRegressList1  <-  fRegress(CARCAR,templist,betalist1) #bad xfdlist which is the templist 
 #incorrect number of replications in XFDLIST for covariate #2
 #I think I need to figure out how to names each year in the basis to get the 12 replicates of 12 years
 #In Cao's and in Ramsay the object is called day.5 and I need to figure out how it was 
 #made and what is in it
names(fRegressList1)
betaestlist1   <-  fRegressList1$betaestlist
length(betaestlist1)
  
  
  

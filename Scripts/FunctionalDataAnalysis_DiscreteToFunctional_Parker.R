library(fda)
library(patchwork)
library(wesanderson)
library(lubridate)
library(reshape2)

#https://matthew-parker.rbind.io/post/2021-04-24-fda/#
#https://rforpoliticalscience.com/2020/07/26/make-wes-anderson-themed-graphs-with-wesanderson-package-in-r/

#data wrangle and plotting ####
rawdat = read.csv("Data/LearningFDA/Health_Regional_Archive_(Public_View) (1).csv")

dat <- rawdat %>% 
  dplyr::select(SummaryDate = Last_Updated, Province, CaseCount, Tests) %>% #maybe fda screws up "select"
  group_by(Province) %>% 
  mutate(Date = round_date(ymd_hms(SummaryDate), unit = "day")) %>% 
  dplyr::select(-SummaryDate) %>% 
  group_by(Province, Date) %>% 
  summarise(CaseCounts = sum(CaseCount, na.rm = T), Tests = sum(Tests, na.rm = T)) %>% 
  filter(Date <= as.Date("2021-04-30")) #if I don't filter there are NAs for 3 of the 5 provinces

colnames(dat)
unique(dat$Province)

dat_clean <- dat %>% 
  filter(Province %in% c("AB", "BC", "ON", "MB", "QC"))
unique(dat_clean$Province)

pal <- rev(wes_palette("Zissou1", n=length(unique(dat_clean$Province)), type = "continuous"))


plot1 <- ggplot(data=dat_clean) + 
  geom_line(aes(x=Date, y=CaseCounts, color=Province), size=1, na.rm = T) + 
  scale_color_manual(values = pal) + 
  theme_classic(base_size = 14)

plot2 <- ggplot(data = dat_clean)+
  geom_line(aes(x = Date, y = Tests, color = Province), size =1, na.rm = T)+
  scale_color_manual(values = pal)+
  theme_classic(base_size = 14)

plot1 + plot2  

#step 1: functional basis ####
#basis functions used to represent complex curves as simple linear combinations
#of known curves
#example a 45 degree line with sine curves is made up of 
#basis 1 which is y=x a straight line and basis 2 which is y = sin(x), which is the sinusoidal curve
#together you have a 45 degree line with a sine curve
#here there are 2 basis functions

#using a fourier basis for hourly temperature data over many days
  #here there is no data, you are just creating the basis functions as a composition
  #of sin and cosine
basis_fourier <- create.fourier.basis(rangeval = c(0,1), nbasis = 4)
  #why is rangeval 0,1, why does this create 5 nbasis, why is parameter = 1?
  #I changed the nbasis from 4 to 5 and it didn't change the nbasis....I think
  #it has to do with the default that if you chose an even number it will always
  #make it odd because it has to pair sin/cos and add a constant
  #the parameter might be the "order" argument....

#evaluate the basis functions at discrete points - still not using any data
pts <- seq(0,2, by = 0.01) 
basis_obs <- eval.basis(pts, basis_fourier)   
  #matplot plots the column of one matrix against another
  #functional periods is the pts sequence or time or T
  #Fourier Basis Functions is the evlauation of the function at each time point
  #should be 5 colors = one is a constant at "1" and the other are the
  #4 paired sin/cosines
matplot(pts, basis_obs, type = "l", lwd = 2, cex =3,
        xlab = "Functional Periods",
        ylab = "Fourier Basis Functions") 
  
#another common set of functions are the B-Splines (which is what was used by 
#Stewart-Koster for hydrograph and fish response. They used 6 splines, I think)

#create 10 B-splines basis functions
basis_bs = create.bspline.basis(rangeval = c(0,1), nbasis = 10, norder = 5)
 #so it looks like the norder does give the number of parameters and maybe in the
 #fourier it defaults to one if no order is given or maybe you don't use order 
 #in fourier.  There are 10 basis functions and there are 10 "names"
 #there are 201 rows because that is the number of pts in pts

#evaluate basis function at discrete points
  #not totally sure why you evaluate unless this is just a data check
  #and to see what the functions look like
pts <- seq(0,1,by = 0.01)
basis_obs <- eval.basis(pts, basis_bs) #why 11 rows, ah because there are 11 pts in pts2
matplot(pts, basis_obs, type = 'l', lwd = 2, cex = 3, 
        xlab = "Time", 
        ylab = "B-spline Basis Functions") #should be 10 colors and I think there are, I checked with nbasis =5 and there were 5 colors

#moving forward will be using the B-spline function for the actual data
basis_bs <- create.bspline.basis(c(as_date(min(dat_clean$Date)), #the rangeval is being the dates...not the range of the test or case numbers...
                                   as_date(max(dat_clean$Date))), #does it have to do with the domain, horizontal being time...? even though range is the y axis?
                                 nbasis = 10, norder = 5) #not sure why chosing 10 basis or order 5....
  #here with dates a rangeval, the paramters are 5 for the order but there numbers are evenly spaced dates

#evaluate the basis function at discrete points - the plot does this automatically
plot(basis_bs) #plots the 5 orders or 5 paramter locations, I think there should be 10 colors but can't tell

#Step 2 functional smoothing #####

#need a matrix with columns = subjects (provinces) rows = replicates (dates)
#for me I think I subjects are years and rows are dates
Mdat = dat_clean %>% 
  pivot_wider(id_cols = Date, 
              names_from = Province, 
              values_from = CaseCounts) %>% #this is not test but case counts
  data.matrix()

Mdat = Mdat[,-1] #don't want date, just know that rows are number of dates

#setup smoothing parameters - not sure how you choose the Lfdobj or the lambda
#fdobj: is the basis functions to use
#Lfdobj: is the derivative degree to smooth
#lambda: is the smoothing penalty

smoothPar = fdPar(fdobj = basis_bs, Lfdobj=2, lambda=1)

#smoothed functional data
dat_fd = smooth.basis(argvals = as_date(unique(dat_clean$Date)), y = Mdat, fdParobj = smoothPar)
  #something is messed up here are there are NAs in the for the first 3 provinces


#step 3: function data representation ####
#the data is now in functional form using basis functions and smoothing

#visualize smoothed data
 #something still messed up because the smoothed lines don't look like the blog
  #but just moving on to keep learning - I checked that I did the code right
  #might be something with the csv...data different from what the blogger used
  #it was the data, I had to filter it down to 4/2021 because the data had all
  #the way through 1 January 2022.
dates  = seq(min(dat_fd$argvals), max(dat_fd$argvals), by = 'days')
dates2 = seq(min(dat_fd$argvals), max(dat_fd$argvals), by = 'quarter')
obs_fd = eval.fd(evalarg = dates, fdobj = dat_fd$fd)

obs_fd = as.data.frame(obs_fd)
obs_fd$Dates = dates
obs_fd_df = melt(obs_fd, measure.vars = 1:5)
colnames(obs_fd_df) = c("Date", "Province", "CaseCounts")

ggplot(data=obs_fd_df) + 
  geom_line(aes(x=Date, y=CaseCounts, colour=Province), 
            size=1, na.rm = T) +
  scale_color_manual(values = pal) +
  theme_classic(base_size = 14)

#can now look at how case counts are changing with time 
#using derivative functions

# we evaluate the function, using Lfdobj to specify the first derivative
div1_obs1_fd = eval.fd(evalarg = dates, fdobj = dat_fd$fd, Lfdobj = 1)

div1_obs_fd = as.data.frame(div1_obs1_fd)
div1_obs_fd$Dates = dates
div1_obs_fd_df = melt(div1_obs_fd, measure.vars = 1:5)
colnames(div1_obs_fd_df) = c("Date", "Province", "CaseCounts")

ggplot(data=div1_obs_fd_df) + 
  geom_line(aes(x=Date, y=CaseCounts, colour=Province),
            size=1, na.rm = T) +
  scale_color_manual(values = pal) +
  ylab("Rate of Increase in Case Counts") +
  theme_classic(base_size = 14)


#can get test data into functional form
# we need a matrix with columns = subjects (provinces), rows = replicates (dates)
Mdat2 = dat_clean %>% 
  pivot_wider(id_cols = Date,
              names_from = Province, 
              values_from = Tests) %>%
  data.matrix()

Mdat2 = Mdat2[,-1]

# setup smoothing parameters
smoothPar = fdPar(fdobj = basis_bs, Lfdobj=2, lambda=1)

# smoothed functional data
test1_fd = smooth.basis(argvals = as_date(unique(dat_clean$Date)), y = Mdat2, fdParobj = smoothPar)

# visualize the tests data:
test_fd = eval.fd(evalarg = dates, fdobj = test1_fd$fd)
test_fd = as.data.frame(test_fd)
test_fd$Dates = dates
test_fd_df = melt(test_fd, measure.vars = 1:5)
colnames(test_fd_df) = c("Date", "Province", "Tests")

ggplot(data=test_fd_df) + 
  geom_line(aes(x=Date, y=Tests, colour=Province), 
            size = 1, na.rm = T) +
  ylab("Tests Administered") +
  scale_color_manual(values = pal) +
  theme_classic(base_size = 14)

#can compare functional data representation agains original discrete data counterparts
plot3 = ggplot(data=dat_clean) + 
  geom_line(aes(x=Date, y=CaseCounts, color=Province), 
            size=1, na.rm = T) + 
  scale_color_manual(values = pal) + 
  theme_classic(base_size = 11) +
  ggtitle("Discrete Case Counts")
plot4 = ggplot(data=obs_fd_df) +
  geom_line(aes(x=Date, y=CaseCounts, colour=Province), 
            size=1, na.rm = T) +
  scale_color_manual(values = pal) +
  theme_classic(base_size = 11) +
  ggtitle("Functional Case Counts")

plot3 + plot4















#Read me ####
#The purpose of this script is to develop figures for the manuscript

#Libraries ####
library(tidyverse)
library(lubridate)
library(gridExtra)
library(broom)

#Data ####
datfish_rm<- read.csv("Data/Processed/RGFishCPUE_RM.csv")
dat_drying <- read.csv("C:/Users/eigilbert/OneDrive - DOI/Documents/UNM/RiverDrying/Chapter1/Scratch_Chap1_R/Data/Processed/2010_2021_WetDryTenths.csv") %>% 
  mutate(Reach = case_when(RMTenthDry < 116 ~ "San Acacia",
                           TRUE ~ "Isleta"),
         Date = as.Date(Date, format = "%Y-%m-%d"))
datfish_yr <- datfish_rm %>% 
  group_by(Reach, Species_Codes, Year) %>% 
  summarise(MeanCPUE = mean(CPUE_m))

#general summaries ####
datfish_rm %>% 
  group_by(Reach, Species_Codes) %>% 
  summarise(Meanab = round(mean(CPUE_m),2), 
            maxab = round(max(CPUE_m),2))





#fish ####
#cpue data showing zero inflation ##
newlabels<- c("CYPCAR" = "Common Carp", "CYPLUT" = "Red Shiner",
              "PLAGRA" = "Flathead Chub", "ICTPUN" = "Channel Catfish", 
              "HYBAMA" = "Rio Grande Silvery Minnow",
              "PIMPRO" = "Fathead Minnow", "GAMAFF" = "Western Mosquitofish", "CARCAR" = "River Carpsucker")

  #histogram
tiff("Figures/RawCPUE_Species.jpg", units= "in", width = 7.5, height = 6, res = 600)
datfish_rm %>% 
  mutate(across(Species_Codes, ~factor(., levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                                   "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" ))),
         Reach = as.factor(Reach)) %>% 
  ggplot(aes(x = (CPUE_m*100), fill = Reach))+
  geom_histogram()+
  facet_wrap(vars(Species_Codes), scales = "free", labeller = labeller(Species_Codes= newlabels))+
  xlab (expression(paste("Abundance (individuals/", m^2, ")", sep="")))+ 
  ylab("Count")+
  theme_classic()+
  scale_color_manual(name = "River reach", values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("upper", "lower"))+
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
  select(!X) %>% 
  filter(DryRM == 0) %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10*1.6) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0),
         ChngExtentDry = ExtentDry - lag(ExtentDry, default = ExtentDry[1])) %>% 
  ungroup() %>% 
  filter(between(month(Date), 4, 10)) %>% 
  mutate(Year = year(Date)) 


temp <- ExtentChngDry_Irrig %>% 
  group_by(Reach, Year) %>% 
  summarise(MeanExt = round(mean(ExtentDry),2), 
            MaxExt = round(max(ExtentDry),2),
            SDExt = round(sd(ExtentDry),2),
            MeanChgExt = round(mean(ChngExtentDry),2), 
            MaxChgExt = round(max(ChngExtentDry),2),
            SDChgExt = round(sd(ChngExtentDry),2))


#cummulative days and length of drying
MileDays_Irrig <- dat_drying %>%
  select(!X) %>% 
  filter(DryRM == 0) %>% 
  group_by(Reach, Date) %>% 
  summarise(ExtentDry = sum(DryRM == 0)/10*1.6) %>% 
  complete(Date = seq.Date(as.Date("2010-01-01"), as.Date("2021-12-31"), by = "day")) %>% 
  ungroup() %>% 
  filter(between(month(Date), 4, 10)) %>% 
  mutate(ExtentDry = replace_na(ExtentDry, 0),
         Year = year(Date)) %>% 
  group_by(Reach, Year) %>% 
  mutate(MD = cumsum(ExtentDry)) %>% 
  ungroup() 

temp3 <- MileDays_Irrig %>% 
  group_by(Reach, Year) %>% 
  summarise(meanmax = mean(MD), maxmax = max(MD), sdmax = sd(MD))

test <- cbind(temp, temp3)
test2 <- temp %>% 
  full_join(temp3, by = c("Year", "Reach"))

write.csv(test2, "Data/Processed/AnnualRiverDryingStatistics.csv", row.names = F)

temp4 <- MileDays_Irrig %>% 
  group_by(Year, Reach) %>% 
  slice(match(TRUE, MD >0)) %>% 
  mutate(Jday = yday(Date)) %>% 
  ungroup() %>% 
  group_by(Reach) %>% 
  summarise(MnJday = mean(Jday), MinJday = min(Jday), MaxJday = max(Jday)) %>% 
  mutate(Mnday = as.Date(MnJday, origin="1970-01-01"),
         Minday = as.Date(MinJday, origin="1970-01-01"),
         Maxday = as.Date(MaxJday, origin="1970-01-01"))


  

newlabels2<- c("ExtentDry" = "a)  magnitude", "ChngExtentDry" = "b)  rate of change", 
               "MD" = "c)  duration")

p1 <- ExtentChngDry_Irrig %>% 
  left_join(MileDays_Irrig) %>% 
  pivot_longer(cols = c("ExtentDry", "ChngExtentDry", "MD"), names_to = "Metric", values_to = "values") %>% 
  mutate(DOY = yday(Date)) %>%
  filter(Year >= 2016) %>% 
  mutate(across(Metric, ~factor(., levels=c("ExtentDry", "ChngExtentDry", "MD")))) %>% 
  ggplot(aes(x = Date, y = values, color = Reach))+
  geom_line(size = 1)+
  facet_grid(Metric~Year, scales = "free", labeller = labeller(Metric= newlabels2))+
  theme_classic()+
  xlab("") + ylab("Kilometers")+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")
  

p2 <- ExtentChngDry_Irrig %>% 
  left_join(MileDays_Irrig) %>% 
  pivot_longer(cols = c("ExtentDry", "ChngExtentDry", "MD"), names_to = "Metric", values_to = "values") %>% 
  mutate(DOY = yday(Date)) %>%
  filter(Year < 2016) %>% 
  mutate(across(Metric, ~factor(., levels=c("ExtentDry", "ChngExtentDry", "MD")))) %>% 
  ggplot(aes(x = Date, y = values, color = Reach))+
  geom_line(size = 1)+
  facet_grid(Metric~Year, scales = "free", labeller = labeller(Metric= newlabels2))+
  theme_classic()+
  xlab("") + ylab("Kilometers")+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

tiff("Figures/DailyDryingMetrics.jpg", units= "in", width = 12, height = 6, res = 600)
ExtentChngDry_Irrig %>% 
  left_join(MileDays_Irrig) %>% 
  pivot_longer(cols = c("ExtentDry", "ChngExtentDry", "MD"), names_to = "Metric", values_to = "values") %>% 
  mutate(DOY = yday(Date)) %>%
  filter(Year >= 2010) %>% 
  mutate(across(Metric, ~factor(., levels=c("ExtentDry", "ChngExtentDry", "MD")))) %>% 
  ggplot(aes(x = Date, y = values, color = Reach))+
  geom_line(size = 1)+
  facet_grid(Metric~Year, scales = "free", labeller = labeller(Metric= newlabels2))+
  theme_classic()+
  xlab("") + ylab("Kilometers")+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")
dev.off()

#line plots
tiff("Figures/DailyDryingMetrics.jpg", units= "in", width = 8, height = 10, res = 600)
grid.arrange(p2,p1)
dev.off()





#annual trends ###
# Conversion vector for species codes
species_conversion <- c("CYPCAR" = "Common Carp", "CYPLUT" = "Red Shiner",
                        "PLAGRA" = "Flathead Chub", "ICTPUN" = "Channel Catfish", 
                        "HYBAMA" = "Rio Grande Silvery Minnow",
                        "PIMPRO" = "Fathead Minnow", "GAMAFF" = "Western Mosquitofish", 
                        "CARCAR" = "River Carpsucker")

# Create a sorted version of the conversion vector by species names
species_conversion_sorted <- species_conversion[order(species_conversion)]

# Reorder the Species_Codes factor levels in datfish_yr based on the sorted species names
datfish_yr <- datfish_yr %>%
  mutate(Species_Codes = factor(Species_Codes, levels = names(species_conversion_sorted)))

# Conversion function for labeller
species_labeller <- function(labels) {
  labels <- as.character(labels)
  return(species_conversion[labels])
}

tiff("Figures/TrendsCPUE_Species.jpg", units= "in", width = 7.5, height = 6, res = 600)
datfish_yr %>% 
  ggplot(aes(x = Year, y = log10(MeanCPUE + 0.001), color = Species_Codes)) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(vars(Reach), labeller = labeller(Species_Codes = species_labeller)) +
  scale_color_manual(name = "", labels = species_conversion_sorted, values = c(
    "CARCAR" = "#ffe119", "CYPCAR" = "#e6194B", "CYPLUT" = "#911eb4", "GAMAFF" = "#f032e6",
    "HYBAMA" = "#42d4f4", "ICTPUN" = "#4363d8", "PIMPRO" = "#f58231", "PLAGRA" = "#3cb44b"
  )) +
  theme_classic() +
  theme(legend.position = "bottom")+
  ylab("Mean Density (Log10+0.001)")+ xlab("")
dev.off()

# Fit linear models and extract slopes
slopes <- datfish_yr %>%
  group_by(Species_Codes, Reach) %>%
  do(tidy(lm(log10(MeanCPUE + 0.001) ~ Year, data = .))) %>%
  filter(term == "Year") %>%
  select(Species_Codes, Reach, slope = estimate)

# Print the slopes
print(slopes)

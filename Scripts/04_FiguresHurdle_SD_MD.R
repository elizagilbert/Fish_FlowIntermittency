#Read me ####
#The purpose of this script is to develop figures for the manuscript

#Libraries ####
library(tidyverse)
library(lubridate)
library(plyr) #this overwrites a number of functions from dplyr so need to call it specifically from dplyr
library(brms)
library(scales)
library(gridExtra)

#fish ####
newlabels<- c("CYPCAR" = "Common Carp", "CYPLUT" = "Red Shiner",
              "PLAGRA" = "Flathead Chub", "ICTPUN" = "Channel Catfish", 
              "HYBAMA" = "RG Silvery Minnow",
              "PIMPRO" = "Fathead Minnow", "GAMAFF" = "Western Mosquitofish", "CARCAR" = "River Carpsucker")

#"hu" #####
mod_SDMD_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_sdMD_no_rm.rds")

conditions <- make_conditions(mod_SDMD_no_rm, "Species_Codes")

ce3 <- conditional_effects(mod_SDMD_no_rm, "ProSD_ProDays:Reach", conditions = conditions, dpar="hu")
for_pl3 <- ce3$`ProSD_ProDays:Reach` %>% 
  mutate(ProSD_ProDays = ProSD_ProDays*1.6) #check this, it is turning it into km/days

temp <- for_pl3 %>% 
  mutate(ProbOccur = 1 - estimate__, ProbOccurLwr = 1 - lower__, ProbOccurUpper = 1 - upper__) 

tiff("Figures/HurdleHu_SDMD_no_rm.jpg", units= "in", width = 8, height = 6, res = 600)
for_pl3 %>% 
  mutate(Species_Codes = factor(Species_Codes, levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                                        "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" ))) %>% 
  ggplot(aes(x = ProSD_ProDays, y = (1-estimate__), color = Reach, fill = Reach)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin=(1-for_pl3$lower__), ymax=1-(for_pl3$upper__), group = Reach),
              linetype = 1, alpha = 0.6)+
  facet_wrap(vars(Species_Codes), labeller = labeller(Species_Codes= newlabels) )+
  ylab(expression(paste("Probability of occurrence (1- ", italic("hu)")))) + 
  xlab(expression(paste("SD ", italic("Accumulation"), " (km-days)")))+
  theme_classic()+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme(
    legend.position = c(1,0), # bottom right position
    legend.justification = c(1, 0), # bottom right justification
    legend.box.margin = margin(5, l = 5, unit = "mm") # small margin
  )
dev.off()

#hurdle pp_epred ####

mod_SDMD_no_rm <- readRDS("HurdleModels/Proportion/mod_Pro_sdMD_no_rm.rds")
newdata <-  crossing(Species_Codes = c("CYPLUT", "PLAGRA", "ICTPUN", "CYPCAR", "HYBAMA", "PIMPRO", "CARCAR", "GAMAFF"),
                     Reach = c("San Acacia", "Isleta"), ProSD_ProDays = seq(0,20,5)) 

pp_epredp <- as.data.frame(posterior_epred(mod_SDMD_no_rm,
                                           re_formula = NA,
                                           ndraws = 500,
                                           summary = T,
                                           newdata = newdata))

columnnames <- newdata %>% 
  mutate(Reach = gsub(' ', '', Reach)) %>% 
  mutate(names = paste0(Species_Codes, "_", Reach, "_", ProSD_ProDays))
colnames(pp_epredp) <- columnnames$names

pp_epredp_long <- pp_epredp %>% 
  pivot_longer(1:80, names_to = "Names", values_to = "Values") %>% 
  separate(Names, c("Species", "Reach", "ProSD_ProDays")) %>% 
  mutate(ProSD_ProDays = as.numeric(ProSD_ProDays))

stats_epredp <- ddply(pp_epredp_long, c("Species", "Reach", "ProSD_ProDays"), summarise,
                      mean = mean(Values),
                      CI_L = quantile(Values, probs = 0.025),
                      CI_U = quantile(Values, probs = 0.975)) %>% 
  mutate(ProSD_ProDays = as.numeric(ProSD_ProDays*1.6), 
         Species = factor(Species, levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                            "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" )))

newlabels<- c("CYPLUT" = "Red Shiner", "CYPCAR" = "Common Carp", 
              "PLAGRA" = "Flathead Chub", "ICTPUN" = "Channel Catfish", 
              "HYBAMA" = "RG Silvery Minnow",
              "PIMPRO" = "Fathead Minnow", "GAMAFF" = "Mosquitofish", "CARCAR" = "River Carpsucker")

tiff("Figures/Predicted_Proportion_SDMD.jpg", units= "in", width = 8, height = 6, res = 600)
stats_epredp %>% 
  ggplot(aes(x = ProSD_ProDays, y = mean, color = Reach, fill = Reach))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin=CI_L, ymax=CI_U, group = Reach), alpha = 0.6, size = 0.5)+
  facet_wrap(vars(Species), scales = "free_y", labeller = labeller(Species= newlabels))+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme_classic()+
  scale_y_continuous(trans = "log10", labels = comma)+
  ylab(expression(paste(italic("Density")))) + xlab(expression(paste("SD ", italic("Accumulation"), " (km-days)")))+
  theme(
    legend.position = c(1,0), # bottom right position
    legend.justification = c(1, 0), # bottom right justification
    legend.box.margin = margin(5, l = 5, unit = "mm") # small margin
  )
dev.off()            




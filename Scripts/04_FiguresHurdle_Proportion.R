#Libraries ####
library(tidyverse)
library(lubridate)
library(plyr) #this overwrites a number of functions from dplyr so need to call it specifically from dplyr
library(brms)
library(scales)
library(gridExtra)

#Data 
MaxExtent <- readRDS("Models/mod_Pro_maxextent.rds")

#set up 
newlabels<- c("CYPCAR" = "Common Carp", "CYPLUT" = "Red Shiner",
              "PLAGRA" = "Flathead Chub", "ICTPUN" = "Channel Catfish", 
              "HYBAMA" = "Rio Grande Silvery Minnow",
              "PIMPRO" = "Fathead Minnow", "GAMAFF" = "Western Mosquitofish", "CARCAR" = "River Carpsucker")

conditions <- make_conditions(MaxExtent, "Species_Codes")

#marginal effects

ce3 <- conditional_effects(MaxExtent, "ProMax_Extent:Reach", conditions = conditions, dpar="hu")
for_pl3 <- ce3$`ProMax_Extent:Reach`

#plot
tiff("Figures/HurdleHu_MaxExtent_Proportion.jpg", units= "in", width = 8, height = 6, res = 600)
for_pl3 %>% 
  mutate(Species_Codes = factor(Species_Codes, levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                                        "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" ))) %>% 
  ggplot(aes(x = ProMax_Extent, y = (1-estimate__), color = Reach, fill = Reach)) +
  geom_line(size = 1)+
  geom_ribbon(aes(ymin=(1-for_pl3$lower__), ymax=1-(for_pl3$upper__), group = Reach),
              linetype = 1, alpha = 0.6)+
  facet_wrap(vars(Species_Codes), labeller = labeller(Species_Codes= newlabels) )+
  ylab(expression(paste("Probability of occurrence (1- ", italic("hu)")))) + 
  xlab(expression(paste("Maximum spatial ", italic("Extent"), "(proportion of dried river)")))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_classic()+
  xlim(c(0,1))+
  scale_color_manual(name = "River reach", values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("upper", "lower"))+
  theme(
    legend.position = c(1,0), # bottom right position
    legend.justification = c(1, 0), # bottom right justification
    legend.box.margin = margin(5, l = 5, unit = "mm") # small margin
  )
dev.off()

#hurdle overall abundance pp_epred ####

#set up
mod_maxextent <- readRDS("Models/mod_Pro_maxextent.rds")
newdata <-  crossing(Species_Codes = c("CYPLUT", "PLAGRA", "ICTPUN", "CYPCAR", "HYBAMA", "PIMPRO", "CARCAR", "GAMAFF"),
                     Reach = c("San Acacia", "Isleta"), ProMax_Extent = seq(0,1,.10)) 
newlabels<- c("CYPCAR" = "Common Carp", "CYPLUT" = "Red Shiner",
              "PLAGRA" = "Flathead Chub", "ICTPUN" = "Channel Catfish", 
              "HYBAMA" = "Rio Grande Silvery Minnow",
              "PIMPRO" = "Fathead Minnow", "GAMAFF" = "Western Mosquitofish", "CARCAR" = "River Carpsucker")

#simulate from distribution

pp_epredp <- as.data.frame(posterior_epred(mod_maxextent,
                                           re_formula = NA,
                                           ndraws = 500,
                                           summary = T,
                                           newdata = newdata))

columnnames <- newdata %>% 
  mutate(Reach = gsub(' ', '', Reach)) %>% 
  mutate(names = paste0(Species_Codes, "_", Reach, "_", ProMax_Extent))
colnames(pp_epredp) <- columnnames$names

pp_epredp_long <- pp_epredp %>% 
  pivot_longer(1:176, names_to = "Names", values_to = "Values") %>% 
  separate(Names, c("Species", "Reach", "Max_Extent")) %>% 
  mutate(Max_Extent = as.numeric(Max_Extent)) %>% 
  dplyr::filter(!is.na(Values)) %>% 
  dplyr::filter(is.finite(Values))

stats_epredp <- ddply(pp_epredp_long, c("Species", "Reach", "Max_Extent"), summarise,
                      mean = mean(Values),
                      CI_L = quantile(Values, probs = 0.025),
                      CI_U = quantile(Values, probs = 0.975)) %>% 
          mutate(Species = factor(Species, levels=c("CARCAR", "CYPCAR", "CYPLUT", 
                                                    "GAMAFF", "HYBAMA", "ICTPUN", "PIMPRO" ,"PLAGRA" )))

#plot

tiff("Figures/Predicted_MaxExtent.jpg", units= "in", width = 8, height = 6, res = 600)
stats_epredp %>% 
  ggplot(aes(x = Max_Extent, y = mean, color = Reach, fill = Reach))+
  geom_line(linewidth = 1)+
  geom_ribbon(aes(ymin=CI_L, ymax=CI_U, group = Reach), alpha = 0.6, size = 0.5)+
  facet_wrap(vars(Species), scales = "free_y", labeller = labeller(Species= newlabels))+
  scale_color_manual(values = c("darkgrey", "black"), aesthetics = c("color", "fill"),
                     labels = c("Upper", "Lower"))+
  theme_classic()+
  scale_y_continuous(trans = "log10", labels = comma)+
  ylab("Mean abundance") + xlab(expression(paste("Flow intermittency: maximum ", italic("Extent"), "(km)")))+
  theme(
    legend.position = c(1,0), # bottom right position
    legend.justification = c(1, 0), # bottom right justification
    legend.box.margin = margin(5, l = 5, unit = "mm") # small margin
  )
dev.off()            


#testing to understand model output
#hurdle conditional effects ####
m3 <- readRDS("Models/mod_Pro_maxextent.rds")
ce <- conditional_effects(m3)
for_pl <- ce$`ProMax_Extent:Species_Codes`

#I think this is overall effects
tiff("Figures/Hurdleoverall_MaxExtent.jpg", units= "in", width = 8, height = 6, res = 600)
for_pl %>% 
  mutate(across(Species_Codes, ~factor(., levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                                   "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" )))) %>% 
  ggplot(aes(x = ProMax_Extent, y = estimate__)) +
  geom_line(color = "blue", size = 1)+
  geom_ribbon(aes(ymin=for_pl$lower__, ymax=for_pl$upper__),
              linetype = 2, alpha = 0.1)+
  facet_wrap(vars(Species_Codes), scales = "free", labeller = labeller(Species_Codes= newlabels) )+
  ylab("Model Catch Per Unit Effort") + xlab("Max annual extent of dry river miles")+
  theme_classic()
dev.off()

#I think this is just when fish present - "mu"
ce2 <- conditional_effects(m3, effects = "Max_Extent:Species_Codes", dpar="mu")
for_pl2 <- ce2$`Max_Extent:Species_Codes`

tiff("Figures/HurdleMu_MaxExtent.jpg", units= "in", width = 8, height = 6, res = 600)
for_pl2 %>% 
  mutate(across(Species_Codes, ~factor(., levels=c("ICTPUN" ,"CYPCAR", "PIMPRO", "PLAGRA" ,  
                                                   "GAMAFF" , "CYPLUT", "HYBAMA" , "CARCAR" )))) %>% 
  ggplot(aes(x = Max_Extent, y = estimate__)) +
  geom_line(color = "blue", size = 1)+
  geom_ribbon(aes(ymin=for_pl2$lower__, ymax=for_pl2$upper__),
              linetype = 2, alpha = 0.1)+
  facet_wrap(vars(Species_Codes), scales = "free", labeller = labeller(Species_Codes= newlabels) )+
  ylab("Model Catch Per Unit Effort - 'mu'") + xlab("Max annual extent of dry river miles")+
  theme_classic()
dev.off()

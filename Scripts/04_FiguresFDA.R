#read me ####
#script is to plot the species/reaches that had signicant FDA results for
#daily extent dry

#libraries ####
library(tidyverse)
library(cowplot)

#data ####

#extent
carcar_san_a <- read.csv("FDA_Data/Coefs_SanAcacia_CARCAR_Extent.csv")%>%
  mutate(Time = as.Date(Time, origin="1970-01-01"))
pimpro_is <- read.csv("FDA_Data/Coefs_Isleta_PIMPRO_Extent.csv")%>%
  mutate(Time = as.Date(Time, origin="1970-01-01"))
plagra_is <- read.csv("FDA_Data/Coefs_Isleta_PLAGRA_Extent.csv")%>%
  mutate(Time = as.Date(Time, origin="1970-01-01"))
gamaff_is <- read.csv("FDA_Data/Coefs_Isleta_GAMAFF_Extent.csv")%>%
  mutate(Time = as.Date(Time, origin="1970-01-01"))
cyplut_is <- read.csv("FDA_Data/Coefs_Isleta_CYPLUT_Extent.csv") %>%
  mutate(Time = as.Date(Time, origin="1970-01-01"))
cyplut_san_a <- read.csv("FDA_Data/Coefs_SanAcacia_CYPLUT_Extent.csv")%>%
  mutate(Time = as.Date(Time, origin="1970-01-01"))


#plotting code
#carcar - san acacia
pl_car_sana<- carcar_san_a %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("Coefficient")+ xlab("")+
  ylim(-0.005, 0.005)+
  theme_classic()+
  ggtitle("Common Carp (Lower)")+
  theme(plot.title = element_text(size = 10), axis.text.x=element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")


  #pimpro - isleta
pl_pim_is <- pimpro_is %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("")+ xlab("")+
  ylim(-0.01, 0.030)+
  theme_classic()+
  ggtitle("Fathead Minnow (Upper)")+
  theme(plot.title = element_text(size = 10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

  #plagra - isleta
pl_pla_is <-plagra_is %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("Coefficient")+ xlab("")+
  scale_y_continuous(breaks = seq(-0.04, 0.08, 0.02))+
  theme_classic()+
  ylim(-0.015, 0.005)+
  ggtitle("Flathead Chub (Upper)")+
  theme(plot.title = element_text(size = 10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")


  #pimpro - isleta
pl_gam_is <-gamaff_is %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("")+ xlab("")+
  scale_y_continuous(limits = c(-0.04, 0.08), breaks = c(-0.04, -.02, 0.0, 0.02, 0.04, 0.06, 0.08))+
  theme_classic()+
  ggtitle("Western Mosquitofish (Upper)")+
  theme(plot.title = element_text(size = 10), axis.text.x=element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")


  #cyplut - isleta
pl_cyp_is <-cyplut_is %>%  
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("")+ xlab("")+
  ylim(-0.01, 0.01)+
  theme_classic()+
  ggtitle("Red Shiner (Upper)")+
  theme(plot.title = element_text(size = 10), axis.text.x=element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")


  #cyplut - san acacia
pl_cyp_sana <-cyplut_san_a %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("")+ xlab("")+
  ylim(-0.03, 0.02)+
  theme_classic()+
  ggtitle("Red Shiner (Lower)")+
  theme(plot.title = element_text(size = 10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")



tiff("Figures/FDA_Extent.jpg", units= "in", width = 8, height = 6, res = 600)
plot_grid(pl_car_sana, pl_pim_is, pl_pla_is, pl_gam_is, pl_cyp_is, pl_cyp_sana)
dev.off()

#read me ####
#script is to plot the species/reaches that had signicant FDA results for
#daily extent dry

#libraries ####
library(tidyverse)
library(cowplot)

#data ####
#md
pimpro_is <- read.csv("FDA_Data/Coefs_Isleta_PIMPRO_MD.csv")%>% 
  mutate(Time = as.Date(Time, origin="1970-01-01"))
plagra_is <- read.csv("FDA_Data/Coefs_Isleta_PLAGRA_MD.csv") %>% 
  mutate(Time = as.Date(Time, origin="1970-01-01"))

gamaff_is <- read.csv("FDA_Data/Coefs_Isleta_GAMAFF_MD.csv")%>% 
  mutate(Time = as.Date(Time, origin="1970-01-01"))
gamaff_san <- read.csv("FDA_Data/Coefs_SanAcacia_GAMAFF_MD.csv")%>% 
  mutate(Time = as.Date(Time, origin="1970-01-01"))


cyplut_is <- read.csv("FDA_Data/Coefs_Isleta_CYPLUT_MD.csv")%>% 
  mutate(Time = as.Date(Time, origin="1970-01-01"))
cyplut_san_a <- read.csv("FDA_Data/Coefs_SanAcacia_CYPLUT_MD.csv")%>% 
  mutate(Time = as.Date(Time, origin="1970-01-01"))

#plotting code

  #cyplut - isleta
pl_pim_is <- pimpro_is %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("")+ xlab("")+
  ylim(-0.001, 0.0010)+
  theme_classic()+
  ggtitle("Fathead Minnow (Upper)")+
  theme(plot.title = element_text(size = 10), axis.text.x=element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(labels = scales::label_number())

  #gamaff - isleta
pl_pla_is <-plagra_is %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("Coefficient")+ xlab("")+
  scale_y_continuous(breaks = seq(-0.04, 0.08, 0.02))+
  theme_classic()+
  ylim(-0.0005, 0.00025)+
  ggtitle("Flathead Chub (Upper)")+
  theme(plot.title = element_text(size = 10), axis.text.x=element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")


  #pimpro - isleta
pl_gam_is <-gamaff_is %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("")+ xlab("")+
  scale_y_continuous(limits = c(-0.01, 0.1))+
  theme_classic()+
  ggtitle("Mosquitofish (Upper)")+
  theme(plot.title = element_text(size = 10), axis.text.x=element_blank())+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")


  #plagra - isleta
pl_gam_sana <-gamaff_san %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("")+ xlab("")+
  ylim(-0.005, 0.01)+
  theme_classic()+
  ggtitle("Western Mosquitofish (Lower)")+
  theme(plot.title = element_text(size = 10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")


  #cyplut - san acacia
pl_cyp_is <-cyplut_is %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("")+ xlab("")+
  ylim(-0.0004, 0.0004)+
  theme_classic()+
  ggtitle("Red Shiner (Upper)")+
  theme(plot.title = element_text(size = 10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(labels = scales::label_number())

 #carcar - san acacia
pl_cyp_sana<- cyplut_san_a %>% 
  ggplot(aes(x = Time, y = Coef))+
  geom_line(size = 1)+
  geom_line(aes(x=Time, y = UpperCI), color = "grey", linewidth = 1)+
  geom_line(aes(x=Time, y = LowerCI), color = "grey", linewidth = 1)+
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1)+
  ylab("Coefficient")+ xlab("")+
  ylim(-0.0001, 0.0001)+
  theme_classic()+
  ggtitle("Red Shiner (Lower)")+
  theme(plot.title = element_text(size = 10))+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(labels = scales::label_number())

tiff("Figures/FDA_MD.jpg", units= "in", width = 8, height = 6, res = 600)
plot_grid(pl_pim_is, pl_pla_is, pl_gam_is, pl_gam_sana, pl_cyp_is, pl_cyp_sana)
dev.off()

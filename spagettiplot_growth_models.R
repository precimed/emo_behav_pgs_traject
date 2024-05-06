#spagettiplot_growth_models
library(phenotools)#this script is based on v 0.2.7
library(tidyverse)
library(readr)

####check_basic_model####
#
library(readr)
dat<-read_csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
#plot based on script from: https://www.alexcernat.com/estimating-and-visualizing-change-in-time-using-latent-growth-models-with-r/
int_d<-dat%>%select(ID_2445,sex,by_cat,int18m,int3yr,int5yr)
#sample 
int_d<-int_d%>%sample_n(10000)
table(int_d$by_cat)
table(int_d$sex)

int_long <- gather(data=int_d, key=age, value=score, int18m:int5yr, factor_key=TRUE)
int_long

int_mean_traject<-ggplot(int_long, aes(age, score, group = ID_2445)) + 
  geom_line(alpha = 0.4) + # add individual line with transparency
  stat_summary( # add average line
    aes(group = 1),
    fun = mean,
    geom = "line",
    size = 1,
    color = "red"
  ) +
  labs(y = "Emotional difficulty score") +
  theme(axis.title.x = element_blank(),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70", size=0.7),
        axis.text = element_text(size=10,colour ="black"),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        strip.background = element_rect(fill="white",colour="white"),
        strip.text = element_blank(),
        legend.position = "none")
int_mean_traject

ggsave("int_mean_traject_10000.jpeg",int_mean_traject,unit = "cm", height = 6, width =9)

#plot for behavioral problems
ext_d<-dat%>%select(ID_2445,sex,by_cat,ext18m,ext3yr,ext5yr)

ext_d<-ext_d%>%sample_n(10000)
table(ext_d$by_cat)
table(ext_d$sex)

ext_long <- gather(data=ext_d, key=age, value=score, ext18m:ext5yr, factor_key=TRUE)
ext_long

ext_mean_traject<-ggplot(ext_long, aes(age, score, group = ID_2445)) + 
  geom_line(alpha = 0.4) + # add individual line with transparency
  stat_summary( # add average line
    aes(group = 1),
    fun = mean,
    geom = "line",
    size = 1,
    color = "red"
  ) +
  labs(y = "Behavioral difficulty score") +
  theme(axis.title.x = element_blank(),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70", size=0.7),
        axis.text = element_text(size=10,colour ="black"),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        strip.background = element_rect(fill="white",colour="white"),
        strip.text = element_blank(),
        legend.position = "none")
ext_mean_traject

ggsave("ext_mean_traject_10000.jpeg",ext_mean_traject,unit = "cm", height = 6, width =9)


library(readxl)
or_pgs<- read_excel("./output/or_pgs_rew.xlsx")

or_pgs$OR<-as.numeric(or_pgs$OR)
or_pgs$`Lower 2.5`<-as.numeric(or_pgs$`Lower 2.5`)
or_pgs$`Upper 2.5`<-as.numeric(or_pgs$`Upper 2.5`)
library(tidyverse)
or_pgs<-or_pgs%>%rename(conf.low=`Lower 2.5`)
or_pgs<-or_pgs%>%rename(conf.high=`Upper 2.5`)
or_pgs<-or_pgs%>%rename(PGS=Trait)

pd <- position_dodge(0.6)

or_pgs$PGS<-fct_rev(as.factor(or_pgs$PGS))

or_pgs$PGS <- factor(or_pgs$PGS,      # Reordering group factor levels
                     levels = c("ANX","DEP","BD","NEUR"))

or_pgs$Comparision <- factor(or_pgs$Comparision,      # Reordering group factor levels
                             levels = c("P1 vs. P2","P3 vs. P2","P4 vs. P2","P5 vs. P2","P3 vs. P1","P4 vs. P1","P5 vs. P1","P4 vs. P3","P5 vs. P3","P5 vs. P4"))
or_pgs$Comparision<-fct_rev(or_pgs$Comparision)

or_pgs_full<-or_pgs%>%mutate(Comparision=recode(Comparision,"P1 vs. P2"="Profile1 vs. Profile2",
                                                "P3 vs. P2"="Profile3 vs. Profile2",
                                                "P4 vs. P2"="Profile4 vs. Profile2",
                                                "P5 vs. P2"="Profile5 vs. Profile2",
                                                "P3 vs. P1"="Profile3 vs. Profile1",
                                                "P4 vs. P1"="Profile4 vs. Profile1",
                                                "P5 vs. P1"="Profile5 vs. Profile1",
                                                "P4 vs. P3"="Profile4 vs. Profile3",
                                                "P5 vs. P3"="Profile5 vs. Profile3",
                                                "P5 vs. P4"="Profile5 vs. Profile4"))
#head(or_pgs_full)

plot<-ggplot(or_pgs, aes(x=OR, y=Comparision, label=Comparision, shape=PGS, color=PGS)) + 
  geom_point(position=pd, size = 2, show.legend = TRUE) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), 
                width=0.45, position=pd, size=1) +
  geom_vline(xintercept = 1, linetype = "dashed")+scale_shape_manual(values = c(3, 16, 17, 15))+
  scale_color_manual(values=c("#d01c8b", "#7b3294", "#e66101", "#4dac26"))+
  ylab("Developmental profiles")+
  xlab("Odds Ratio per standard deviation change in PGS")+
  theme(legend.text.align=1)+ theme_bw()+
  theme(strip.background = element_rect(fill="orange"),legend.margin=margin(t=-10),
        legend.position = "bottom",axis.text.x = element_text(family="arial",size=10, color="black"), 
        axis.text.y = element_text(family="arial",size=10,color="black"),legend.title = element_text(family="arial",size=10,color="black"),
        legend.text = element_text(family="arial",size=10,color="black"),
        axis.title=element_text(family="arial",size=10, face="bold",color="black"),
        strip.text = element_text(family="arial",size = 10),
        axis.title.x = element_text(family="arial",size=10,color="black"),axis.title.y= element_text(family="arial",size=10,color="black"), 
        strip.text.y = element_blank())
plot

ggsave("./output/pgs_lpa_or_all_in_one_20x18.jpeg",plot,dpi=600, height = 20, width = 18, units = "cm")

###plot OR separated

or_pgs_anx<-or_pgs%>%subset(PGS=="ANX")
or_pgs_dep<-or_pgs%>%subset(PGS=="DEP")
or_pgs_bd<-or_pgs%>%subset(PGS=="BD")
or_pgs_neur<-or_pgs%>%subset(PGS=="NEUR")

####increase size
plot_anx<-ggplot(or_pgs_anx, aes(x=OR, y=Comparision, label=Comparision, shape=PGS, color=PGS)) + 
  geom_point(position=pd, size = 3.3, show.legend = TRUE) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), 
                width=0.5, position=pd, size=1.3) +
  geom_vline(xintercept = 1, linetype = "dashed")+scale_shape_manual(values = 3)+
  scale_color_manual(values="#d01c8b")+
  xlab("Odds Ratio per standard deviation change in PGS (log scale)")+
  theme(legend.text.align=1)+ theme_bw()+scale_x_continuous(trans = "log10", minor_breaks = seq(0.75,1.5,by=0.125),
                                                            breaks = seq(0.75,1.5,by=0.25),
                                                            limits = c(0.75,1.55),
                                                            expand = c(0,0))+
  theme(strip.background = element_rect(fill="orange"),
        legend.position = "bottom",axis.text.x = element_text(family="arial",size=14, color="black"), 
        axis.text.y = element_text(family="arial",size=14,color="black"),legend.title = element_text(family="arial",size=14,color="black"),
        legend.text = element_text(family="arial",size=14,color="black"),
        axis.title=element_text(family="arial",size=14, face="bold",color="black"),
        strip.text = element_text(family="arial",size = 14),
        axis.title.x = element_text(family="arial",size=14,color="black"),legend.margin=margin(t=-7),axis.title.y= element_blank(), 
        strip.text.y = element_blank())
plot_anx
ggsave("or_lpa_anx_rew2.jpeg",plot_anx,dpi=600, height = 15, width = 20, units = "cm")
  # axis.title.x = element_text(family="arial",size=14,color="black",vjust = -0.2),

plot_dep<-ggplot(or_pgs_dep, aes(x=OR, y=Comparision, label=Comparision, shape=PGS, color=PGS)) + 
  geom_point(position=pd, size = 3.3, show.legend = TRUE) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), 
                width=0.5, position=pd, size=1.3) +
  geom_vline(xintercept = 1, linetype = "dashed")+scale_shape_manual(values = 16)+
  scale_color_manual(values="#7b3294")+
  ylab("Developmental profiles")+
  xlab("Odds Ratio per standard deviation change in PGS (log scale)")+
  theme(legend.text.align=1)+ theme_bw()+scale_x_continuous(trans = "log10", minor_breaks = seq(0.75,1.5,by=0.125),
                                                            breaks = seq(0.75,1.5,by=0.25),
                                                            limits = c(0.75,1.55),
                                                            expand = c(0,0))+
  theme(strip.background = element_rect(fill="orange"),legend.margin=margin(t=-7),
        legend.position = "bottom",axis.text.x = element_text(family="arial",size=14, color="black"), 
        axis.text.y = element_text(family="arial",size=14,color="black"),legend.title = element_text(family="arial",size=14,color="black"),
        legend.text = element_text(family="arial",size=14,color="black"),
        axis.title=element_text(family="arial",size=14, face="bold",color="black"),
        strip.text = element_text(family="arial",size = 14),
        axis.title.x = element_text(family="arial",size=14,color="black"),axis.title.y= element_blank(), 
        strip.text.y = element_blank())
plot_dep
ggsave("or_lpa_dep_rew.jpeg",plot_dep,dpi=600, height = 15, width = 20, units = "cm")


plot_bd<-ggplot(or_pgs_bd, aes(x=OR, y=Comparision, label=Comparision, shape=PGS, color=PGS)) + 
  geom_point(position=pd, size = 3.3, show.legend = TRUE) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), 
                width=0.5, position=pd, size=1.3) +
  geom_vline(xintercept = 1, linetype = "dashed")+scale_shape_manual(values = c(17))+
  scale_color_manual(values=c("#e66101"))+
  ylab("Developmental profiles")+
  xlab("Odds Ratio per standard deviation change in PGS (log scale)")+
  theme(legend.text.align=1)+ theme_bw()+scale_x_continuous(trans = "log10", minor_breaks = seq(0.75,1.5,by=0.125),
                                                            breaks = seq(0.75,1.5,by=0.25),
                                                            limits = c(0.75,1.55),
                                                            expand = c(0,0))+
  theme(strip.background = element_rect(fill="orange"),legend.margin=margin(t=-7),
        legend.position = "bottom",axis.text.x = element_text(family="arial",size=14, color="black"), 
        axis.text.y = element_text(family="arial",size=14,color="black"),legend.title = element_text(family="arial",size=14,color="black"),
        legend.text = element_text(family="arial",size=14,color="black"),
        axis.title=element_text(family="arial",size=14, face="bold",color="black"),
        strip.text = element_text(family="arial",size = 14),
        axis.title.x = element_text(family="arial",size=14,color="black"),axis.title.y= element_blank(), 
        strip.text.y = element_blank())
plot_bd
ggsave("or_lpa_bd_rew.jpeg",plot_bd,dpi=600, height = 15, width = 20, units = "cm")

plot_neur<-ggplot(or_pgs_neur, aes(x=OR, y=Comparision, label=Comparision, shape=PGS, color=PGS)) + 
  geom_point(position=pd, size = 3.3, show.legend = TRUE) +
  geom_errorbar(aes(xmin=conf.low, xmax=conf.high), 
                width=0.5, position=pd, size=1.3) +
  geom_vline(xintercept = 1, linetype = "dashed")+scale_shape_manual(values = 15)+
  scale_color_manual(values="#4dac26")+
  ylab("Developmental profiles")+
  xlab("Odds Ratio per standard deviation change in PGS (log scale)")+
  theme(legend.text.align=1)+ theme_bw()+scale_x_continuous(trans = "log10", minor_breaks = seq(0.75,1.5,by=0.125),
                                                            breaks = seq(0.75,1.5,by=0.25),
                                                            limits = c(0.75,1.55),
                                                            expand = c(0,0))+
  theme(strip.background = element_rect(fill="orange"),legend.margin=margin(t=-7),
        legend.position = "bottom",axis.text.x = element_text(family="arial",size=14, color="black"), 
        axis.text.y = element_text(family="arial",size=14,color="black"),legend.title = element_text(family="arial",size=14,color="black"),
        legend.text = element_text(family="arial",size=14,color="black"),
        axis.title=element_text(family="arial",size=14, face="bold",color="black"),
        strip.text = element_text(family="arial",size = 14),
        axis.title.x = element_text(family="arial",size=14,color="black"),axis.title.y= element_blank(), 
        strip.text.y = element_blank())
plot_neur

ggsave("or_lpa_neur_rew.jpeg",plot_neur,dpi=600, height = 15, width = 20, units = "cm")


library(ggpubr)
combi_reg<-ggarrange(plot_anx,plot_dep,plot_bd,plot_neur,ncol = 2, nrow = 2)
ggsave("./output/or_lpa_combi.jpeg",combi_reg,dpi=600, height = 32, width = 34, units = "cm")
ggsave("./output/or_lpa_combi.tiff",combi_reg,dpi=600, height = 32, width = 34, units = "cm")


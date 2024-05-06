#plot developmental profiles

library(MplusAutomation)
library(tidyverse)
mplusOutput2 <- readModels("./mplus/output/step3", recursive=TRUE)
  #plot for all models collectivly minor changes in trajects. 

source("./scripts/adapted_functions.R")
mplusOutput <- readModels("./mplus/output/step1", recursive=TRUE)
  
ext <- MyplotGrowthMixtures(mplusOutput, bw = FALSE, rawdata = FALSE,
                            estimated = TRUE, poly = FALSE, alpha_range = c(0, 0.1),
                            growth_variables = c("i1","s1"), time_scale = c(1.5,3,5), jitter_lines = NULL,
                            coefficients = "unstandardized") %>% 
  mutate(Pheno = "ext")%>% 
  filter(Title =="lpa_5c")

int <- MyplotGrowthMixtures(mplusOutput, bw = FALSE, rawdata = FALSE,
                            estimated = TRUE, poly = FALSE, alpha_range = c(0, 0.1),
                            growth_variables = c("i2","s2"), time_scale = c(1.5,3,5), jitter_lines = NULL,
                            coefficients = "unstandardized") %>% 
  mutate(Pheno = "int")%>% 
  filter(Title =="lpa_5c")


trajs <- ext %>% 
  bind_rows(int)%>%
  mutate_at(vars(Value,Lci,Uci), list(~case_when(Pheno=="ext" ~ ./16,
                                                 Pheno=="int" ~ ./10))) %>% 
  mutate(Pheno=recode(Pheno, "ext"="Behavioral","int"="Emotional"))  %>% 
  mutate(Pheno=factor(Pheno, levels=c("Behavioral","Emotional")),
         Class=factor(Class, ordered = F)) %>% 
  mutate_at(vars(Value,Lci,Uci,Time), list(~as.numeric(.))) %>% 
  droplevels()


classcounts<- cbind(data.frame(mplusOutput$..mplus.output.step1.lpa_5c.out$class_counts$mostLikely),
                    data.frame(mplusOutput$..mplus.output.step1.lpa_5c.out$class_counts$avgProbs.mostLikely)) %>% 
  rename("prob_Class1"=X1,
         "prob_Class2"=X2,
         "prob_Class3"=X3,
         "prob_Class4"=X4,
         "prob_Class5"=X5)
classcounts

profile_names <- list(
  '1'="Profile 1\n(4.82%)",
  '2'="Profile 2\n(85.04%)",
  '3'="Profile 3\n(4.85%)",
  '4'="Profile 4\n(4.19%)",
  '5'="Profile 5\n(1.10%)"
)
profile_labeller <- function(variable,value){
  return(profile_names[value])
}

ests <- map(mplusOutput, function(x){
  data.frame(x$parameters$ci.unstandardized) %>% 
    filter(paramHeader %in% "Means",
           param %in% c("HYP_C_8YR","INA_C_8YR","ODD_C_8YR","CD_C_8YR","DEP_C_8YR","ANX_C_8YR"))%>%
    mutate_at(vars(est,low2.5,up2.5), list(~case_when(param=="DEP_C_8YR" ~ ./(2*13),
                                                      param=="ANX_C_8YR" ~ ./(2*5),
                                                      param=="CD_C_8YR" ~ ./(3*8),
                                                      param=="ODD_C_8YR" ~ ./(3*8),
                                                      param=="HYP_C_8YR" ~ ./(3*9),
                                                      param=="INA_C_8YR" ~ ./(3*9)))) %>% 
    mutate(var=factor(param, levels=c("DEP_C_8YR",
                                      "ANX_C_8YR",
                                      "CD_C_8YR",
                                      "ODD_C_8YR",
                                      "HYP_C_8YR",
                                      "INA_C_8YR"),
                      labels = c("DEP","ANX","CD","ODD","HYP","INAT")),
           Class=LatentClass)%>% 
    mutate(Pheno=factor(ifelse(param %in% c("DEP_C_8YR","ANX_C_8YR"), "Emotional", "Behavioral")))%>%
    mutate_at(vars(est,low2.5,up2.5), list(~as.numeric(.))) %>%
    mutate(Class=factor(Class),
           Title= x$output[(grep("lpa_", x$output))][[3]]) %>% 
    droplevels()
}) %>% 
  purrr::reduce(bind_rows)
ests<-ests%>%subset(Title=="lpa_5c")

#write.csv(ests,"N:/durable/users/norarba/FLA2_rew/ests.csv",row.names=F)

p2 <- ggplot(data=trajs, aes(x=Time, y=Value, group=interaction(Class,Pheno)))+
  geom_point(aes(colour=Pheno,shape=Pheno),size=3.5,alpha=1)+
  geom_line(aes(colour=Pheno),size=1.8,alpha=1)+
  geom_ribbon(aes(ymin=Lci,ymax=Uci, fill= Pheno), alpha=0.4)+
  geom_hline(yintercept = 0, linetype=5, colour="grey70", size=1)+
  facet_grid(Class~.)+
  scale_shape_manual("Domain",values=c("Behavioral"=15,"Emotional"=16))+
  coord_cartesian(xlim=c(1.2,5.5), ylim=c(-0.05,0.6))+
  #scale_x_continuous(breaks=c(1,2,3,4,5))+
  scale_x_continuous(breaks=c(1.5,3.0,5.0))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70", size=0.7),
        axis.text = element_text(size=14),
        panel.background = element_rect(fill = "white", colour = "white"),
        #      panel.spacing = unit(0.2, "lines"),
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        strip.background = element_rect(fill="white",colour="white"),
        strip.text = element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        legend.position = "none")+
  guides(fill= FALSE,
         shape= FALSE, colour= guide_legend(title="Domain",override.aes = list(size=6)))
p2

p4<-p2+scale_color_manual(values=c("#fc9272","#3182bd")) + scale_fill_manual(values=c("#fc9272","#3182bd"))
p4#figure with name for reference


ggsave("240427_traject_25cmx9cm.jpeg",p4,unit = "cm", height = 20, width =10)

p3 <- ggplot()+
  geom_hline(yintercept = 0, linetype=5, colour="grey70", size=1)+
  geom_point(data=ests, aes(x=var, y=est, group=Class, colour=Pheno, shape=Pheno),size=4,alpha=1)+
  geom_errorbar(data=ests,aes(x=var,ymin=low2.5,ymax=up2.5, colour=Pheno), alpha=0.4, size=1.8,width=0)+
  scale_shape_manual("Domain",values=c("Behavioral"=15,"Emotional"=16))+
  facet_grid(Class~., labeller=profile_labeller)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70", size=0.7),
        axis.text = element_text(size=14),
        panel.background = element_rect(fill = "white", colour = "white"),
        #      panel.spacing = unit(0.2, "lines"),
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        strip.background = element_rect(fill="white",colour="white"),
        strip.text = element_blank(),
        legend.position = "none")+scale_color_manual(values=c("#fc9272","#3182bd")) + 
  scale_fill_manual(values=c("#fc9272","#3182bd"))+coord_cartesian(ylim=c(-0.05,0.6))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6))
p3#figure with name for reference

ggsave("240427_midchild_25cmx9cm.jpeg",p3,unit = "cm", height = 25, width =9)


####plot without text for more adjustable aestetics.  
p2 <- ggplot(data=trajs, aes(x=Time, y=Value, group=interaction(Class,Pheno)))+
  geom_point(aes(colour=Pheno,shape=Pheno),size=3.8,alpha=1)+
  geom_line(aes(colour=Pheno),size=1.8,alpha=1)+
  geom_ribbon(aes(ymin=Lci,ymax=Uci, fill= Pheno), alpha=0.4)+
  geom_hline(yintercept = 0, linetype=5, colour="grey70", size=1)+
  facet_grid(Class~.)+
  scale_shape_manual("Domain",values=c("Behavioral"=15,"Emotional"=16))+
  coord_cartesian(xlim=c(1.2,5.5), ylim=c(-0.05,0.6))+
  #scale_x_continuous(breaks=c(1,2,3,4,5))+
  scale_x_continuous(breaks=c(1.5,3.0,5.0))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70", size=0.7),
        axis.text = element_text(size=14),
        panel.background = element_rect(fill = "white", colour = "white"),
        #      panel.spacing = unit(0.2, "lines"),
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        strip.background = element_rect(fill="white",colour="white"),
        strip.text = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")+
  guides(fill= FALSE,
         shape= FALSE, colour= guide_legend(title="Domain",override.aes = list(size=6)))
p2

p4<-p2+scale_color_manual(values=c("#fc9272","#3182bd")) + scale_fill_manual(values=c("#fc9272","#3182bd"))
p4#figure with name for reference

ggsave("240427_traject_25cmx9cm_notext.jpeg",p4,unit = "cm", height = 25, width =9,dpi=1000)

tiff("240427_traject_25cmx9cm_notext.tiff", res = 1000, unit = "cm",
     height = 25, width =9)

p4
dev.off()

p3 <- ggplot()+
  geom_hline(yintercept = 0, linetype=5, colour="grey70", size=1)+
  geom_point(data=ests, aes(x=var, y=est, group=Class, colour=Pheno, shape=Pheno),size=3.8,alpha=1)+
  geom_errorbar(data=ests,aes(x=var,ymin=low2.5,ymax=up2.5, colour=Pheno), alpha=0.4, size=1.8,width=0)+
  scale_shape_manual("Domain",values=c("Behavioral"=15,"Emotional"=16))+
  facet_grid(Class~., labeller=profile_labeller)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70", size=0.7),
        axis.text = element_text(size=14),
        panel.background = element_rect(fill = "white", colour = "white"),
        #      panel.spacing = unit(0.2, "lines"),
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        strip.background = element_rect(fill="white",colour="white"),
        strip.text = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")+scale_color_manual(values=c("#fc9272","#3182bd")) + 
  scale_fill_manual(values=c("#fc9272","#3182bd"))+coord_cartesian(ylim=c(-0.05,0.6))+
  scale_y_continuous(breaks=c(0.0,0.2,0.4,0.6))
p3#

ggsave("240427_midchild_25cmx9cm_notext.jpeg",p3,unit = "cm", height = 25, width =9,dpi=1000)

tiff("240427_midchild_25cmx9cm_notext.tiff", res = 1000, unit = "cm",
     height = 25, width =9)

p3
dev.off()

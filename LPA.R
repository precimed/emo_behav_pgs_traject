#R-project: FLA2_rew

#load relevant packages

library(MplusAutomation)
library(tidyverse)

#read in the data 
new_dat<-read.csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
new_dat<-new_dat %>% 
  rename("anyemo" = "anyemo_diag","dep"="dep_diag","anx"="anx_diag","bd"="bd_diag")

new_dat2<-new_dat%>%select(BARN_NR,preg_id,sex,anyemo,dep,anx,bd,
                   m_id,f_id,birth_yr,cbcl_ext_c_18m,cbcl_ext_c_5yr,cbcl_ext_c_3yr,
                   cbcl_int_c_18m,cbcl_int_c_5yr,cbcl_int_c_3yr,rsdbd_ina_c_8yr,rsdbd_hyp_c_8yr,rsdbd_cd_c_8yr,rsdbd_odd_c_8yr,
                   scared_anx_c_8yr,smfq_dep_c_8yr,adjpgs_dep,adjpgs_anx,adjpgs_bd,adjpgs_neur)

table(new_dat2$anyemo)
library(psych)
describe(new_dat2$adjpgs_anx)
sum(is.na(new_dat2$adjpgs_anx))

new_dat2 <- new_dat2 %>% mutate_at(c("BARN_NR","birth_yr","cbcl_ext_c_18m","cbcl_ext_c_5yr","cbcl_ext_c_3yr",
                                     "cbcl_int_c_18m","cbcl_int_c_5yr","cbcl_int_c_3yr","rsdbd_ina_c_8yr","rsdbd_hyp_c_8yr","rsdbd_cd_c_8yr","rsdbd_odd_c_8yr",
                                     "scared_anx_c_8yr","smfq_dep_c_8yr","adjpgs_dep","adjpgs_anx","adjpgs_bd","adjpgs_neur"), as.numeric)

new_dat3 <- new_dat2 %>% 
  rename_with(~str_remove(.,"cbcl_|rsdbd_|scared_|smfq_")) %>%
  mutate(sex=case_when(sex==1~0,
                       sex==2~1),
         IID=paste(preg_id,BARN_NR,sep="")) %>% 
  mutate(across(matches("_id"), factor)) %>% 
  drop_na(m_id)

library(jtools)
library(tidyverse)
library(tidyverse)
library(psych)
sjPlot::tab_df(describe(new_dat3),
               digits = 2,
               title = "Descriptive statistics LPA", #give your tables title
               file = "N:/durable/users/norarba/FLA2_rew/QC/Descriptives.doc",
               show.rownames =TRUE)# 
str(new_dat3)
new_dat3$IID<-as.numeric(new_dat3$IID)

prepareMplusData(new_dat3, "./mplus/data/data_for_mplus.dat")

sum(is.na(new_dat3$anyemo))
sum(is.na(new_dat3$anx))
sum(is.na(new_dat3$dep))
sum(is.na(new_dat3$bd))

# Run LPAs for profile enumeration in Mplus, using externally created .inp scripts (STEP1)

filepath1 <- "./mplus/scripts/step1"

runModels(filepath1, logFile="alllpa.txt", recursive =F,replaceOutfile="always", Mplus_command = "C:/Program Files/Mplus/Mplus" )#
writeLines(readLines(paste(filepath1,"/alllpa.txt",sep="")))
  

file.copy(from=paste0(filepath1,"/",list.files(filepath1)[str_detect(list.files(filepath1),".inp",negate=T)]),
         to="./mplus/output/step1",
         overwrite = TRUE, recursive = F,
         copy.mode = TRUE)

junk <- dir(path=filepath1, pattern=".out|.dat") 
file.remove(paste0(filepath1,"/",junk))

# Read

mplusOutput <- readModels("./mplus/output/step1", recursive=TRUE)
mySummaries <- readModels("./mplus/output/step1", recursive=TRUE, what="summaries")

# condense results and summaries into one single df each

summaries <-
  lapply(mySummaries, function(x) {
    x$summaries
  }) %>%  bind_rows() %>% as.data.frame()


row.names(summaries) <- NULL

summaries <- summaries %>% 
  select(Model = Title, Parameters, LL, AIC,BIC,aBIC,AICC, Entropy,T11_VLMR_2xLLDiff,T11_VLMR_PValue  ) %>% 
  mutate(Model = str_sub(Model,start=-2)) %>% 
  filter(Model %in% c("1c","2c","3c","4c","5c","6c","7c","8c"))
 
summaries

write.table(summaries, "./mplus/output/step1/lpa_model_fitting.txt", quote = F, row.names = F,sep = ",")
library(arsenal)
write2word(summaries, "N:/durable/users/norarba/FLA2_rew/mplus/output/step1/lpa_model_fitting2.doc")


# Explore interpretability of models
source("./scripts/adapted_functions.R")

mplusOutput <- readModels("./mplus/output/step1", recursive=TRUE)

ext <- MyplotGrowthMixtures(mplusOutput, bw = FALSE, rawdata = FALSE,
                            estimated = TRUE, poly = FALSE, alpha_range = c(0, 0.1),
                            growth_variables = c("i1","s1"), time_scale = c(1.5,3,5), jitter_lines = NULL,
                            coefficients = "unstandardized") %>% 
  mutate(Pheno = "ext") 
int <- MyplotGrowthMixtures(mplusOutput, bw = FALSE, rawdata = FALSE,
                            estimated = TRUE, poly = FALSE, alpha_range = c(0, 0.1),
                            growth_variables = c("i2","s2"), time_scale = c(1.5,3,5), jitter_lines = NULL,
                            coefficients = "unstandardized") %>% 
  mutate(Pheno = "int")

trajs <- ext %>% 
  bind_rows(int)%>%
  mutate_at(vars(Value,Lci,Uci), list(~case_when(Pheno=="ext" ~ ./16,
                                                 Pheno=="int" ~ ./10))) %>% 
  mutate(Pheno=recode(Pheno, "ext"="Behavioural","int"="Emotional"))  %>% 
  mutate(Pheno=factor(Pheno, levels=c("Behavioural","Emotional")),
         Class=factor(Class, ordered = F)) %>% 
  mutate_at(vars(Value,Lci,Uci,Time), list(~as.numeric(.))) %>% 
  droplevels() #

profile_names <- list(
  '1'="Profile 1",
  '2'="Profile 2",
  '3'="Profile 3",
  '4'="Profile 4"
) 
profile_labeller <- function(variable,value){
  return(profile_names[value])
}

p2 <- ggplot(data=trajs, aes(x=Time, y=Value, group=interaction(Class,Pheno)))+
  geom_point(aes( colour=Class),size=3.5,alpha=1)+
  geom_line( aes( colour=Class, linetype=Pheno),size=1.2,alpha=1)+
  geom_ribbon(aes(ymin=Lci,ymax=Uci, fill= Class), alpha=0.4)+
  geom_hline(yintercept = 0, linetype=5, colour="grey70", size=1)+
  facet_grid(Title~Class)+
  theme(axis.title.y = element_text(margin = margin(t = -20, r = 30, b = 0, l = 0)),
        axis.title.x = element_blank(),
        text =element_text(size = 19.5),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70", size=0.7),
        axis.text = element_text(size=20),
        axis.text.x.top = element_text(angle= 0,size=12, margin = margin(t = 0, r = 0, b = , l = 0), hjust=0.5),
        panel.background = element_rect(fill = "grey95", colour = "white"),
        #      panel.spacing = unit(0.2, "lines"),
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(size =18, face="bold"),
        strip.background = element_rect(fill="white",colour="white"),
        strip.placement = "outside",
        strip.text.x = element_text(angle=0,colour="slateblue4", face="bold", size=14.5))+
  coord_cartesian(xlim=c(1.2,5.3), ylim=c(-0.05,0.6))+
  scale_x_continuous(name="Age (yrs)", breaks=c(1,2,3,4,5), position = "top")+
  scale_y_continuous(name="Mean score on scale\n(scaled 0-1)", breaks=c(0.0,0.2,0.4,0.6))+
  guides(fill= FALSE,
         shape= FALSE, colour= guide_legend(title="Class",override.aes = list(size=6))) 
p2#

ggsave("traject_eb.jpeg",p2,units = "cm", height = 30, width = 30)

ests <- map(mplusOutput, function(x){
  
  data.frame(x$parameters$unstandardized) %>% 
    filter(paramHeader %in% "Means",
           param %in% c("HYP_C_8YR","INA_C_8YR","ODD_C_8YR","CD_C_8YR","DEP_C_8YR","ANX_C_8YR"))%>%
    mutate_at(vars(est), list(~case_when(param=="DEP_C_8YR" ~ ./(2*13),
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
    mutate(Pheno=factor(ifelse(param %in% c("DEP_C_8YR","ANX_C_8YR"), "Emotional", "Behavioural")))%>% 
    mutate(Class=factor(Class),
           Title= x$output[(grep("lpa_", x$output))][[3]]) %>% 
    droplevels()
}) %>% 
  purrr::reduce(bind_rows )



p3 <- ggplot()+
  geom_hline(yintercept = 0, linetype=5, colour="grey70", size=1)+
  geom_point(data=ests, aes(x=var, y=est, group=interaction(Class,var), colour=Class),position=position_dodge(1),size=3.5,alpha=1)+
  geom_errorbar(data=ests,aes(x=var,ymin=est-1.96*se,ymax=est+1.96*se, colour=Class),position=position_dodge(1), alpha=0.4, size=1.8,width=0)+
  scale_shape_manual("Domain",values=c("Behavioural"=16,"Emotional"=17,"8-year (see axis)"=15))+
  facet_grid(Title~Class)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 30, b =10 , l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size=16),
        axis.line = element_line(colour="grey70", size=0.7),
        axis.ticks = element_line(colour="grey70", size=0.7),
        axis.text = element_text(size=20),
        axis.text.x = element_text(angle=90, size=12, vjust=0.5),
        text =element_text(size = 19.5),
        panel.background = element_rect(fill = "grey95", colour = "white"),
        panel.spacing = unit(0.2, "lines"),
        panel.grid.major = element_line(colour="grey90"),
        panel.grid.minor = element_line(colour="grey90"),
        legend.position = "bottom",
        legend.justification = "right",
        legend.direction = "horizontal",
        legend.title = element_text(size =18, face="bold"),
        legend.key= element_rect(fill=NA),
        strip.background = element_blank()) +
  coord_cartesian( ylim=c(-0.05,0.6))+
  scale_y_continuous(name="Mean score on scale\n(scaled 0-1)", breaks=c(0.0,0.2,0.4,0.6))+
  scale_x_discrete(name="", breaks=c("DEP","ANX","CD","ODD","HYP","INAT"), position="bottom")+
  guides(fill=FALSE,colour= guide_legend(title="Class",override.aes = list(size=6), reverse=F)) 
p3 


ggsave("midchild.jpeg",p3,units = "cm", height = 30, width = 30)

# Select best-fitting model, and get classcounts from there 
classcounts<- cbind(data.frame(mplusOutput$..mplus.output.step1.lpa_5c.out$class_counts$mostLikely),
                    data.frame(mplusOutput$..mplus.output.step1.lpa_5c.out$class_counts$avgProbs.mostLikely)) %>% 
  rename("prob_Class1"=X1,
         "prob_Class2"=X2,
         "prob_Class3"=X3,
         "prob_Class4"=X4,
         "prob_Class5"=X5)
classcounts
#minimal class count >1%, ok to proceed. 

write.table(classcounts, "./mplus/output/step1/lpa_classcounts.txt", quote = F, row.names = F,sep = ",")
write2word(classcounts, "N:/durable/users/norarba/FLA2_rew/mplus/output/step1/lpa_classcounts.doc")

mplusOutput <- readModels("./mplus/output/step1", recursive=TRUE)# 
library(tidyverse)
library(dplyr)
newdat <- mplusOutput$..mplus.output.step1.lpa_5c.out$savedata 
newdat<-newdat%>%dplyr::mutate(by_cat = case_when(BIRTH_YR < quantile(newdat$BIRTH_YR, c(0.33,0.66))[[1]] ~ 0,
                            BIRTH_YR > quantile(newdat$BIRTH_YR, c(0.33,0.66))[[2]] ~ 2,
                            TRUE ~1))#

prepareMplusData(newdat, "./mplus/data/data_for_mplus_step3.dat")
 
# Extract the logits to specify measurement error for profile assignment (STEP2)
logits <- mplusOutput$..mplus.output.step1.lpa_5c.out$class_counts$logitProbs.mostLikely

# Put the logits into the right place in the step3 inp file
filepath2 <- "./mplus/scripts/step3"

fileConn<-file(paste0(filepath2,"/lpa_5c_step3_dep.inp"), open="r")
tmp <- readLines(fileConn, warn=F)
close(fileConn)

tmp_new <- c(tmp[1:min(grep("!logits from step2 start",tmp))],
         paste0("!
%c#1%
[cc#1@",logits[1,1]," cc#2@",logits[1,2]," cc#3@",logits[1,3]," cc#4@",logits[1,4],"];
[ANYEMO$1] (d1);
%c#2%
[cc#1@",logits[2,1]," cc#2@",logits[2,2]," cc#3@",logits[2,3]," cc#4@",logits[2,4],"];
[ANYEMO$1] (d2);
%c#3%
[cc#1@",logits[3,1]," cc#2@",logits[3,2]," cc#3@",logits[3,3]," cc#4@",logits[3,4],"];
[ANYEMO$1] (d3);
%c#4%
[cc#1@",logits[4,1]," cc#2@",logits[4,2]," cc#3@",logits[4,3]," cc#4@",logits[4,4],"];
[ANYEMO$1] (d4);
%c#5%
[cc#1@",logits[5,1]," cc#2@",logits[5,2]," cc#3@",logits[5,3]," cc#4@",logits[5,4],"];
[ANYEMO$1] (d5);
!"), tmp[min(grep("!logits from step2 end",tmp)):length(tmp)])

if(length(tmp_new)>0){
fileconnWT <-file(paste0(filepath2,"/lpa_5c_step3_dep.inp"), open="wt")
writeLines(tmp_new,fileconnWT)
close(fileconnWT)
} 

#
fileConn<-file(paste0(filepath2,"/lpa_5c_step3_anx.inp"), open="r")
tmp <- readLines(fileConn, warn=F)
close(fileConn)

tmp_new <- c(tmp[1:min(grep("!logits from step2 start",tmp))],
             paste0("!
%c#1%
[cc#1@",logits[1,1]," cc#2@",logits[1,2]," cc#3@",logits[1,3]," cc#4@",logits[1,4],"];
[ANYEMO$1] (d1);
%c#2%
[cc#1@",logits[2,1]," cc#2@",logits[2,2]," cc#3@",logits[2,3]," cc#4@",logits[2,4],"];
[ANYEMO$1] (d2);
%c#3%
[cc#1@",logits[3,1]," cc#2@",logits[3,2]," cc#3@",logits[3,3]," cc#4@",logits[3,4],"];
[ANYEMO$1] (d3);
%c#4%
[cc#1@",logits[4,1]," cc#2@",logits[4,2]," cc#3@",logits[4,3]," cc#4@",logits[4,4],"];
[ANYEMO$1] (d4);
%c#5%
[cc#1@",logits[5,1]," cc#2@",logits[5,2]," cc#3@",logits[5,3]," cc#4@",logits[5,4],"];
[ANYEMO$1] (d5);
!"), tmp[min(grep("!logits from step2 end",tmp)):length(tmp)])

if(length(tmp_new)>0){
  fileconnWT <-file(paste0(filepath2,"/lpa_5c_step3_anx.inp"), open="wt")
  writeLines(tmp_new,fileconnWT)
  close(fileconnWT)
} 


###

fileConn<-file(paste0(filepath2,"/lpa_5c_step3_neurot.inp"), open="r")
tmp <- readLines(fileConn, warn=F)
close(fileConn)

tmp_new <- c(tmp[1:min(grep("!logits from step2 start",tmp))],
             paste0("!
%c#1%
[cc#1@",logits[1,1]," cc#2@",logits[1,2]," cc#3@",logits[1,3]," cc#4@",logits[1,4],"];
[ANYEMO$1] (d1);
%c#2%
[cc#1@",logits[2,1]," cc#2@",logits[2,2]," cc#3@",logits[2,3]," cc#4@",logits[2,4],"];
[ANYEMO$1] (d2);
%c#3%
[cc#1@",logits[3,1]," cc#2@",logits[3,2]," cc#3@",logits[3,3]," cc#4@",logits[3,4],"];
[ANYEMO$1] (d3);
%c#4%
[cc#1@",logits[4,1]," cc#2@",logits[4,2]," cc#3@",logits[4,3]," cc#4@",logits[4,4],"];
[ANYEMO$1] (d4);
%c#5%
[cc#1@",logits[5,1]," cc#2@",logits[5,2]," cc#3@",logits[5,3]," cc#4@",logits[5,4],"];
[ANYEMO$1] (d5);
!"), tmp[min(grep("!logits from step2 end",tmp)):length(tmp)])

if(length(tmp_new)>0){
  fileconnWT <-file(paste0(filepath2,"/lpa_5c_step3_neurot.inp"), open="wt")
  writeLines(tmp_new,fileconnWT)
  close(fileconnWT)
} 

####
fileConn<-file(paste0(filepath2,"/lpa_5c_step3_bd.inp"), open="r")
tmp <- readLines(fileConn, warn=F)
close(fileConn)

tmp_new <- c(tmp[1:min(grep("!logits from step2 start",tmp))],
             paste0("!
%c#1%
[cc#1@",logits[1,1]," cc#2@",logits[1,2]," cc#3@",logits[1,3]," cc#4@",logits[1,4],"];
[ANYEMO$1] (d1);
%c#2%
[cc#1@",logits[2,1]," cc#2@",logits[2,2]," cc#3@",logits[2,3]," cc#4@",logits[2,4],"];
[ANYEMO$1] (d2);
%c#3%
[cc#1@",logits[3,1]," cc#2@",logits[3,2]," cc#3@",logits[3,3]," cc#4@",logits[3,4],"];
[ANYEMO$1] (d3);
%c#4%
[cc#1@",logits[4,1]," cc#2@",logits[4,2]," cc#3@",logits[4,3]," cc#4@",logits[4,4],"];
[ANYEMO$1] (d4);
%c#5%
[cc#1@",logits[5,1]," cc#2@",logits[5,2]," cc#3@",logits[5,3]," cc#4@",logits[5,4],"];
[ANYEMO$1] (d5);
!"), tmp[min(grep("!logits from step2 end",tmp)):length(tmp)])

if(length(tmp_new)>0){
  fileconnWT <-file(paste0(filepath2,"/lpa_5c_step3_bd.inp"), open="wt")
  writeLines(tmp_new,fileconnWT)
  close(fileconnWT)
} 

fileConn<-file(paste0(filepath2,"/lpa_5c_step3_anyemod.inp"), open="r")
tmp <- readLines(fileConn, warn=F)
close(fileConn)

tmp_new <- c(tmp[1:min(grep("!logits from step2 start",tmp))],
             paste0("!
%c#1%
[cc#1@",logits[1,1]," cc#2@",logits[1,2]," cc#3@",logits[1,3]," cc#4@",logits[1,4],"];
[ANYEMO$1] (d1);
%c#2%
[cc#1@",logits[2,1]," cc#2@",logits[2,2]," cc#3@",logits[2,3]," cc#4@",logits[2,4],"];
[ANYEMO$1] (d2);
%c#3%
[cc#1@",logits[3,1]," cc#2@",logits[3,2]," cc#3@",logits[3,3]," cc#4@",logits[3,4],"];
[ANYEMO$1] (d3);
%c#4%
[cc#1@",logits[4,1]," cc#2@",logits[4,2]," cc#3@",logits[4,3]," cc#4@",logits[4,4],"];
[ANYEMO$1] (d4);
%c#5%
[cc#1@",logits[5,1]," cc#2@",logits[5,2]," cc#3@",logits[5,3]," cc#4@",logits[5,4],"];
[ANYEMO$1] (d5);
!"), tmp[min(grep("!logits from step2 end",tmp)):length(tmp)])

if(length(tmp_new)>0){
  fileconnWT <-file(paste0(filepath2,"/lpa_5c_step3_anyemod.inp"), open="wt")
  writeLines(tmp_new,fileconnWT)
  close(fileconnWT)
} 

# Run the LPA + auxiliary model(s) with fixed profile assignment and calculated error (STEP3)

runModels(filepath2, logFile="alllpa.txt", recursive =F,replaceOutfile="always", Mplus_command = "C:/Program Files/Mplus/Mplus" )

file.copy(from=paste0(filepath2,"/",list.files(filepath2)[str_detect(list.files(filepath2),".inp",negate=T)]),
          to="./mplus/output/step3",
          overwrite = TRUE, recursive = F,
          copy.mode = TRUE)

####READ INN  FINAL GROWTH MODELS
library(MplusAutomation)
library(tidyverse)

mplusOutput2 <- readModels("./mplus/output/step3", recursive=TRUE)#


classcounts_3step_dep<- cbind(data.frame(mplusOutput2$..mplus.output.step3.lpa_5c_step3_dep.out$class_counts$mostLikely),
                              data.frame(mplusOutput2$..mplus.output.step3.lpa_5c_step3_dep.out$class_counts$avgProbs.mostLikely)) %>% 
  rename("prob_Class1"=X1,
         "prob_Class2"=X2,
         "prob_Class3"=X3,
         "prob_Class4"=X4,
         "prob_Class5"=X5)
classcounts_3step_dep

classcounts_3step_anx<- cbind(data.frame(mplusOutput2$..mplus.output.step3.lpa_5c_step3_anx.out$class_counts$mostLikely),
                          data.frame(mplusOutput2$..mplus.output.step3.lpa_5c_step3_anx.out$class_counts$avgProbs.mostLikely)) %>% 
  rename("prob_Class1"=X1,
         "prob_Class2"=X2,
         "prob_Class3"=X3,
         "prob_Class4"=X4,
         "prob_Class5"=X5)
classcounts_3step_anx

classcounts_3step_neurot<- cbind(data.frame(mplusOutput2$..mplus.output.step3.lpa_5c_step3_neurot.out$class_counts$mostLikely),
                              data.frame(mplusOutput2$..mplus.output.step3.lpa_5c_step3_neurot.out$class_counts$avgProbs.mostLikely)) %>% 
  rename("prob_Class1"=X1,
         "prob_Class2"=X2,
         "prob_Class3"=X3,
         "prob_Class4"=X4,
         "prob_Class5"=X5)
classcounts_3step_neurot

classcounts_3step_bd<- cbind(data.frame(mplusOutput2$..mplus.output.step3.lpa_5c_step3_bd.out$class_counts$mostLikely),
                              data.frame(mplusOutput2$..mplus.output.step3.lpa_5c_step3_bd.out$class_counts$avgProbs.mostLikely)) %>% 
  rename("prob_Class1"=X1,
         "prob_Class2"=X2,
         "prob_Class3"=X3,
         "prob_Class4"=X4,
         "prob_Class5"=X5)
classcounts_3step_bd

classcounts_3step_anyemod<- cbind(data.frame(mplusOutput2$..mplus.output.step3.lpa_5c_step3_anyemod.out$class_counts$mostLikely),
                             data.frame(mplusOutput2$..mplus.output.step3.lpa_5c_step3_anyemod.out$class_counts$avgProbs.mostLikely)) %>% 
  rename("prob_Class1"=X1,
         "prob_Class2"=X2,
         "prob_Class3"=X3,
         "prob_Class4"=X4,
         "prob_Class5"=X5)
classcounts_3step_anyemod

#all the same numbers 


# effect estimates

dx_res <-map(mplusOutput2, function(x){
x$output[(grep("New/Additional Parameters",x$output)[[2]]+1):
    (grep("CONFIDENCE INTERVALS FOR THE LOGISTIC REGRESSION ODDS RATIO RESULTS",x$output)-3)] %>% 
    as.data.frame() %>% 
    `colnames<-`("all") %>% 
    separate(col=all, into=c("comparison","null","lower_0.5","lower_2.5","lower_5","est","upper_5","upper_2.5","upper_.5"), sep = "     ") %>% 
    filter(str_detect(comparison, "OR")) %>% 
    mutate(analysis="outcome_anyEmoDx") %>% 
    select(analysis, comparison, est, lower_2.5, upper_2.5)
}) %>% 
  purrr::reduce(bind_rows)
dx_res

library(arsenal)
#write2word(dx_res, "N:/durable/users/norarba/FLA2_rew/mplus/output/step3/anyemo_OR.doc")
#install.packages("ReporteRs")
#library(ReporteRs)
#doc <- docx()
    library(officer)
    library(flextable)
    library(magrittr)
    ft <- flextable(data = dx_res) 
    ft
    # Create a temp file
    pt <- tempfile(fileext = ".docx")
    # Create a docx file
    read_docx() %>% 
      body_add_flextable(ft) %>% 
      print(target = pt)
    
    # open word document
    browseURL(pt)


#Effects of covariates on profile assignment reference class (2 here)
cov_res <-map(mplusOutput2, function(x){
  tmp <- x$output[(grep("Parameterization using Reference Class 2",x$output)[[4]]+1):
             (grep("MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES",x$output)-3)] 
  
  # This splits the tmp character output and puts it as a data frame
  tmp2 <- tmp <- tmp %>% 
       .[str_detect(tmp,"#", negate=T)] %>% 
       as.data.frame() %>% 
       `colnames<-`("all") %>% 
       separate(col=all, into=c("covariate","null","lower_0.5","lower_2.5","lower_5","est","upper_5","upper_2.5","upper_.5"), sep = "     ")
  
  # Find rows with PRS and shift tem by one column
  tmp2[grep(tmp2$covariate, pattern = "PGS"),3:9] <- tmp2[grep(tmp2$covariate, pattern = "PGS"), 2:8]
  
  # drop null column which is column 2
  tmp2 <- tmp2[, c(1,3:9)]
  
  # Drop all NA
  tmp <- drop_na(tmp2)
  # tmp <- tmp %>% 
  #   .[str_detect(tmp,"#", negate=T)] %>% 
  #   as.data.frame() %>% 
  #   `colnames<-`("all")  %>% 
  #  separate(col=all, into=c("covariate","null","lower_0.5","lower_2.5","lower_5","est","upper_5","upper_2.5","upper_.5"), sep = "     ") %>% 
  #  drop_na()
  
  tmp %>% slice_head(n= length(unique(tmp$covariate))*4)
}) %>% 
  purrr::reduce(bind_rows) #%>% 
  #mutate(comparison= c(rep(c("OR12","OR32","OR42","OR52"), each=2),
                       #c("OR12","OR32","OR42","OR52"))) #%>% 
  #select(covariate,comparison, sensitivity, est, lower_2.5, upper_2.5)
cov_res
 
str(cov_res)
cov_res2 <-subset(cov_res, covariate!="    SEX" & covariate!="    BY_CAT")#note spacing 
cov_res2


write.table(cov_res2, file = "N:/durable/users/norarba/FLA2_rew/mplus/output/step3/prs_OR_ref2.txt", sep = ",", quote = FALSE, row.names = F)

####
#Effects of covariates on profile assignment ref 1
cov_res <-map(mplusOutput2, function(x){
  tmp <- x$output[(grep("Parameterization using Reference Class 1",x$output)[[4]]+1):
                    (grep("MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES",x$output)-3)] 
  
  # This splits the tmp character output and puts it as a data frame
  tmp2 <- tmp <- tmp %>% 
    .[str_detect(tmp,"#", negate=T)] %>% 
    as.data.frame() %>% 
    `colnames<-`("all") %>% 
    separate(col=all, into=c("covariate","null","lower_0.5","lower_2.5","lower_5","est","upper_5","upper_2.5","upper_.5"), sep = "     ")
  
  # Find rows with PRS and shift tem by one column
  tmp2[grep(tmp2$covariate, pattern = "PGS"),3:9] <- tmp2[grep(tmp2$covariate, pattern = "PGS"), 2:8]
  
  # drop null column which is column 2
  tmp2 <- tmp2[, c(1,3:9)]
  
  # Drop all NA
  tmp <- drop_na(tmp2)
  # tmp <- tmp %>% 
  #   .[str_detect(tmp,"#", negate=T)] %>% 
  #   as.data.frame() %>% 
  #   `colnames<-`("all")  %>% 
  #  separate(col=all, into=c("covariate","null","lower_0.5","lower_2.5","lower_5","est","upper_5","upper_2.5","upper_.5"), sep = "     ") %>% 
  #  drop_na()
  
  tmp %>% slice_head(n= length(unique(tmp$covariate))*4)
}) %>% 
  purrr::reduce(bind_rows) #%>% 
#mutate(comparison= c(rep(c("OR12","OR32","OR42","OR52"), each=2),
#c("OR12","OR32","OR42","OR52"))) #%>% 
#select(covariate,comparison, sensitivity, est, lower_2.5, upper_2.5)
cov_res

str(cov_res)
cov_res2 <-subset(cov_res, covariate!="    SEX" & covariate!="    BY_CAT")#note spacing
cov_res2
write.table(cov_res2, file = "N:/durable/users/norarba/FLA2_rew/mplus/output/step3/prs_OR_ref1.txt", sep = ",", quote = FALSE, row.names = F)

####

#Effects of covariates on profile assignment ref 3
cov_res <-map(mplusOutput2, function(x){
  tmp <- x$output[(grep("Parameterization using Reference Class 3",x$output)[[4]]+1):
                    (grep("MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES",x$output)-3)] 
  
  # This splits the tmp character output and puts it as a data frame
  tmp2 <- tmp <- tmp %>% 
    .[str_detect(tmp,"#", negate=T)] %>% 
    as.data.frame() %>% 
    `colnames<-`("all") %>% 
    separate(col=all, into=c("covariate","null","lower_0.5","lower_2.5","lower_5","est","upper_5","upper_2.5","upper_.5"), sep = "     ")
  
  # Find rows with PRS and shift tem by one column
  tmp2[grep(tmp2$covariate, pattern = "PGS"),3:9] <- tmp2[grep(tmp2$covariate, pattern = "PGS"), 2:8]
  
  # drop null column which is column 2
  tmp2 <- tmp2[, c(1,3:9)]
  
  # Drop all NA
  tmp <- drop_na(tmp2)
  # tmp <- tmp %>% 
  #   .[str_detect(tmp,"#", negate=T)] %>% 
  #   as.data.frame() %>% 
  #   `colnames<-`("all")  %>% 
  #  separate(col=all, into=c("covariate","null","lower_0.5","lower_2.5","lower_5","est","upper_5","upper_2.5","upper_.5"), sep = "     ") %>% 
  #  drop_na()
  
  tmp %>% slice_head(n= length(unique(tmp$covariate))*4)
}) %>% 
  purrr::reduce(bind_rows) #%>% 
#mutate(comparison= c(rep(c("OR12","OR32","OR42","OR52"), each=2),
#c("OR12","OR32","OR42","OR52"))) #%>% 
#select(covariate,comparison, sensitivity, est, lower_2.5, upper_2.5)
cov_res

str(cov_res)
cov_res2 <-subset(cov_res, covariate!="    SEX" & covariate!="    BY_CAT")#note spacing
cov_res2

write.table(cov_res2, file = "N:/durable/users/norarba/FLA2_rew/mplus/output/step3/prs_OR_ref3.txt", sep = ",", quote = FALSE, row.names = F)

#Effects of covariates on profile assignment ref 4
cov_res <-map(mplusOutput2, function(x){
  tmp <- x$output[(grep("Parameterization using Reference Class 4",x$output)[[4]]+1):
                    (grep("MODEL COMMAND WITH FINAL ESTIMATES USED AS STARTING VALUES",x$output)-3)] 
  
  # This splits the tmp character output and puts it as a data frame
  tmp2 <- tmp <- tmp %>% 
    .[str_detect(tmp,"#", negate=T)] %>% 
    as.data.frame() %>% 
    `colnames<-`("all") %>% 
    separate(col=all, into=c("covariate","null","lower_0.5","lower_2.5","lower_5","est","upper_5","upper_2.5","upper_.5"), sep = "     ")
  
  # Find rows with PRS and shift tem by one column
  tmp2[grep(tmp2$covariate, pattern = "PGS"),3:9] <- tmp2[grep(tmp2$covariate, pattern = "PGS"), 2:8]
  
  # drop null column which is column 2
  tmp2 <- tmp2[, c(1,3:9)]
  
  # Drop all NA
  tmp <- drop_na(tmp2)
  # tmp <- tmp %>% 
  #   .[str_detect(tmp,"#", negate=T)] %>% 
  #   as.data.frame() %>% 
  #   `colnames<-`("all")  %>% 
  #  separate(col=all, into=c("covariate","null","lower_0.5","lower_2.5","lower_5","est","upper_5","upper_2.5","upper_.5"), sep = "     ") %>% 
  #  drop_na()
  
  tmp %>% slice_head(n= length(unique(tmp$covariate))*4)
}) %>% 
  purrr::reduce(bind_rows) #%>% 
#mutate(comparison= c(rep(c("OR12","OR32","OR42","OR52"), each=2),
#c("OR12","OR32","OR42","OR52"))) #%>% 
#select(covariate,comparison, sensitivity, est, lower_2.5, upper_2.5)
cov_res

str(cov_res)
cov_res2 <-subset(cov_res, covariate!="    SEX" & covariate!="    BY_CAT")#note spacing 
cov_res2


write.table(cov_res2, file = "N:/durable/users/norarba/FLA2_rew/mplus/output/step3/prs_OR_ref4.txt", sep = ",", quote = FALSE, row.names = F)

####dubble check all numbers with numbers in Mplus output files. 


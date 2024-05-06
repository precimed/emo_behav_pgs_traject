#LGM_FLA2
#R-project: FLA2_rew
#install.packages("tidyimpute", repos = "file://tsd-evs/shared/R/cran")
require('lavaan')#
#require('semPlot')
#install.packages("semPlot")
library(semPlot)
require('psych')
require('tidyverse')
require('lme4')

library(phenotools)#this script is based on v 0.2.7
library(tidyverse)
library(readr)
####CHECK - NO MISSING VARIABLES FOR BIRTH_YR, SEX OR PGS
#=> missing=fiml.x = missin=fiml for main analyses. 
#Use fiml.x in sens analyses to account for power loss as explanation. 


####check_basic_model####
#
library(readr)
dat<-read_csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
dat$sex<-as.character(dat$sex)
dat$by_cat<-as.character(dat$by_cat)
str(dat$adjpgs_bd)
str(dat$sex)
str(dat$by_cat)
dat$sex<-recode(dat$sex,`1`=0,`2`=1)
table(dat$sex)
table(dat$by_cat)
dat$sex<-factor(dat$sex,ordered = F)
dat$by_cat<-factor(dat$by_cat,ordered = T)
dat$by_cat<-as.numeric(dat$by_cat)
str(dat$sex)#
str(dat$by_cat)#

dat$edu_mf<-dat$hig_edu_mor_or_far
str(dat$edu_mf)
dat$edu_mf<-as.integer(as.logical(dat$edu_mf))
dat$edu_mf<-factor(dat$edu_mf,ordered=F)
table(dat$edu_mf)
str(dat$edu_mf)

#rename variable names for use in the models
dat<-dat%>%rename(int18m=cbcl_int_c_18m,int3yr=cbcl_int_c_3yr,int5yr=cbcl_int_c_5yr,
                  ext18m=cbcl_ext_c_18m,ext3yr=cbcl_ext_c_3yr,ext5yr=cbcl_ext_c_5yr)

variable.names(dat)


source("./scripts_github/r_specify_growth_models_ses.R")

int<-dat%>%select(sex,edu_mf,by_cat,int18m,int3yr,int5yr)%>%rename(ytime1=int18m,
                                                                   ytime2=int3yr,
                                                                   ytime3=int5yr)

ext<-dat%>%select(sex,edu_mf,by_cat,ext18m,ext3yr,ext5yr)%>%rename(ytime1=ext18m,
                                                                   ytime2=ext3yr,
                                                                   ytime3=ext5yr)
fit_int<-list()
fit_int$basic <- growth(basic, data=int,  missing="fiml.x")
fit_m_int<-list()
fit_m_int$basic<-fitMeasures(fit_int$basic, c("df","chisq","pvalue","cfi","tli","srmr","rmsea","rmsea.ci.lower","rmsea.ci.upper"))

summary(fit_int$basic,fit.measures=TRUE, standardized=TRUE)

fit_m_int2<-as.data.frame(fit_m_int)
fit_m_int2 = as.data.frame(x = t(fit_m_int2), stringsAsFactors = FALSE)
fit_m_int2<-round(fit_m_int2,digits=3)

write.table(fit_m_int2,file="N:/durable/users/norarba/FLA2_rew/output/fitmeasures_int_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(fit_m_int2)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/fitmeasures_int_ldpred_ses_x.docx",row.names = T,col.names=T)

fit_ext<-list()
fit_ext$basic <- growth(basic, data=ext,  missing="fiml.x")
fit_m_ext<-list()
fit_m_ext$basic<-fitMeasures(fit_ext$basic, c("df","chisq","pvalue","cfi","tli","srmr","rmsea","rmsea.ci.lower","rmsea.ci.upper"))

fit_m_ext2<-as.data.frame(fit_m_ext)
fit_m_ext2 = as.data.frame(x = t(fit_m_ext2), stringsAsFactors = FALSE)
fit_m_ext2<-round(fit_m_ext2,digits=3)

write.table(fit_m_ext2,file="N:/durable/users/norarba/FLA2_rew/output/fitmeasures_ext_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(fit_m_ext2)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/fitmeasures_ext_ldpred_ses_x.docx",row.names = T,col.names=T)

####check semplots for models####
library(semPlot)

#semPaths with basic options-  correcly spesified
semPaths(fit_int$basic,
         whatLabels = "std",
         edge.label.cex = 1)


####RUN LGM FOR PGS NEUR ####
library(readr)
dat<-read_csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
dat$sex<-as.character(dat$sex)
dat$by_cat<-as.character(dat$by_cat)
str(dat$adjpgs_bd)
str(dat$sex)
str(dat$by_cat)
dat$sex<-recode(dat$sex,`1`=0,`2`=1)
table(dat$sex)
table(dat$by_cat)
dat$sex<-factor(dat$sex,ordered = F)
dat$by_cat<-factor(dat$by_cat,ordered = T)
dat$by_cat<-as.numeric(dat$by_cat)
str(dat$sex)#
str(dat$by_cat)#

dat$edu_mf<-dat$hig_edu_mor_or_far
str(dat$edu_mf)
dat$edu_mf<-as.integer(as.logical(dat$edu_mf))
dat$edu_mf<-factor(dat$edu_mf,ordered=F)
table(dat$edu_mf)
str(dat$edu_mf)
#rename vars to shorter names
dat<-dat%>%rename(int18m=cbcl_int_c_18m,int3yr=cbcl_int_c_3yr,int5yr=cbcl_int_c_5yr,
                  ext18m=cbcl_ext_c_18m,ext3yr=cbcl_ext_c_3yr,ext5yr=cbcl_ext_c_5yr,
                  adj_pgs=adjpgs_neur)

variable.names(dat)
####run growth models
source("./scripts_github/r_specify_growth_models_ses.R")

int<-dat%>%select(sex,edu_mf,by_cat,adj_pgs,int18m,int3yr,int5yr)%>%rename(ytime1=int18m,
                                                                           ytime2=int3yr,
                                                                           ytime3=int5yr)

ext<-dat%>%select(sex,edu_mf,by_cat,adj_pgs,ext18m,ext3yr,ext5yr)%>%rename(ytime1=ext18m,
                                                                           ytime2=ext3yr,
                                                                           ytime3=ext5yr)



fit_int<-list()

fit_int<-list()
fit_int$basic <- growth(basic, data=int,  missing="fiml.x")
fit_int$pgs_residuals <- growth(pgs_residuals, data=int,  missing="fiml.x")
fit_int$pgs_gf <- growth(pgs_gf, data=int,  missing="fiml.x")
fit_int$pgs_intercept <- growth(pgs_intercept, data=int,  missing="fiml.x")
fit_int$pgs_slope <- growth(pgs_slope, data=int,  missing="fiml.x")
fit_int$lgm_nopgs <- growth(lgm_nopgs, data=int,  missing="fiml.x")

lav_res_gf<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_gf)#
lav_res_int<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_intercept)#
lav_res_slope<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_slope)#
lav_res_nopgs<-lavTestLRT(fit_int$pgs_residuals,fit_int$lgm_nopgs)#
lav_gf_int<-lavTestLRT(fit_int$pgs_gf,fit_int$pgs_intercept)#
lav_gf_slope<-lavTestLRT(fit_int$pgs_gf,fit_int$pgs_slope)#
lav_nopgs_gf<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_gf)#
lav_nopgs_int<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_intercept)#
lav_nopgs_slope<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_slope)#

int_neur_fit<-rbind(lav_res_gf,lav_res_int,lav_res_slope,lav_res_nopgs,lav_gf_int,lav_gf_slope,lav_nopgs_gf,lav_nopgs_int,lav_nopgs_slope)
int_neur_fit2<-int_neur_fit%>%select("Df","AIC","Chisq diff","Pr(>Chisq)")

int_neur_fit2
int_neur_fit2<-round(int_neur_fit2,digits=3)
int_neur_fit2$model<-row.names(int_neur_fit2)
write.table(int_neur_fit2,file="N:/durable/users/norarba/FLA2_rew/output/neur_fit_int_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(int_neur_fit2)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/neur_fit_int_ldpred_ses_x.docx",row.names = T)

fit_ext<-list()
fit_ext$basic <- growth(basic, data=ext,  missing="fiml.x")
fit_ext$pgs_residuals <- growth(pgs_residuals, data=ext,  missing="fiml.x")
fit_ext$pgs_gf <- growth(pgs_gf, data=ext,  missing="fiml.x")
fit_ext$pgs_intercept <- growth(pgs_intercept, data=ext,  missing="fiml.x")
fit_ext$pgs_slope <- growth(pgs_slope, data=ext,  missing="fiml.x")
fit_ext$lgm_nopgs <- growth(lgm_nopgs, data=ext,  missing="fiml.x")


lav_res_gf<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_gf)#
lav_res_int<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_intercept)#
lav_res_slope<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_slope)#
lav_res_nopgs<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$lgm_nopgs)#
lav_gf_int<-lavTestLRT(fit_ext$pgs_gf,fit_ext$pgs_intercept)#
lav_gf_slope<-lavTestLRT(fit_ext$pgs_gf,fit_ext$pgs_slope)#
lav_nopgs_gf<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_gf)#
lav_nopgs_int<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_intercept)#
lav_nopgs_slope<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_slope)#

ext_neur_fit<-rbind(lav_res_gf,lav_res_int,lav_res_slope,lav_res_nopgs,lav_gf_int,lav_gf_slope,lav_nopgs_gf,lav_nopgs_int,lav_nopgs_slope)
ext_neur_fit2<-ext_neur_fit%>%select("Df","AIC","Chisq diff","Pr(>Chisq)")

ext_neur_fit2
ext_neur_fit2<-round(ext_neur_fit2,digits=3)
ext_neur_fit2$model<-row.names(ext_neur_fit2)
write.table(ext_neur_fit2,file="N:/durable/users/norarba/FLA2_rew/output/neur_fit_ext_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(ext_neur_fit2)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/neur_fit_ext_ldpred_ses_x.docx",row.names = T)


####effect estimates for best performing latent growth model

###
source("./scripts_github/r_specify_growth_models_ses.R")
summary(growth(pgs_intercept, data=int, missing="fiml.x"),standardized = TRUE,fit.measures=TRUE)

summary(growth(pgs_intercept, data=ext, missing="fiml.x"),standardized = TRUE,fit.measures=TRUE)

####get results with 95%CI
source("./scripts_github/r_specify_growth_models_ses.R")

fit_int<-list()
fit_int$basic <- growth(basic, data=int,  missing="fiml.x")
fit_int$pgs_residuals <- growth(pgs_residuals, data=int,  missing="fiml.x")
fit_int$pgs_gf <- growth(pgs_gf, data=int,  missing="fiml.x")
fit_int$pgs_intercept <- growth(pgs_intercept, data=int,  missing="fiml.x")
fit_int$pgs_slope <- growth(pgs_slope, data=int,  missing="fiml.x")
fit_int$lgm_nopgs <- growth(lgm_nopgs, data=int,  missing="fiml.x")


fit_ext<-list()
fit_ext$basic <- growth(basic, data=ext,  missing="fiml.x")
fit_ext$pgs_residuals <- growth(pgs_residuals, data=ext,  missing="fiml.x")
fit_ext$pgs_gf <- growth(pgs_gf, data=ext,  missing="fiml.x")
fit_ext$pgs_intercept <- growth(pgs_intercept, data=ext,  missing="fiml.x")
fit_ext$pgs_slope <- growth(pgs_slope, data=ext,  missing="fiml.x")
fit_ext$lgm_nopgs <- growth(lgm_nopgs, data=ext,  missing="fiml.x")


sum_int_intercept <- standardizedSolution(fit_int$pgs_intercept, ci = TRUE, level = 0.95) %>%
  filter(rhs=="adj_pgs",
         op == "~")%>%select("lhs","est.std","ci.lower","ci.upper","pvalue")
sum_int_intercept$var<-"int_neur"

sum_ext_intercept <- standardizedSolution(fit_ext$pgs_intercept, ci = TRUE, level = 0.95) %>%
  filter(rhs=="adj_pgs",
         op == "~")%>%select("lhs","est.std","ci.lower","ci.upper","pvalue")
sum_ext_intercept$var<-"ext_neur"

est_neur<-rbind(sum_int_intercept,sum_ext_intercept)

est_neurr <- est_neur %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_neurr,file="N:/durable/users/norarba/FLA2_rew/output/neur_fit_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_neurr)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/neur_fit_summary_ldpred_ses_x.docx",row.names = T)

#save p-val for fdr correction
p_int_neur<-sum_int_intercept[1,]
p_int_neur<-p_int_neur$pvalue
p_int_neur

p_ext_neur<-sum_ext_intercept[1,]
p_ext_neur<-p_ext_neur$pvalue
p_ext_neur

###extract full model results###
sum_int_intercept <- standardizedSolution(fit_int$pgs_intercept, ci = TRUE, level = 0.95) %>%
  unite(col='Regression', c('lhs', 'op', 'rhs'), sep='')
sum_int_intercept$var<-"int_neur"

est_fullmodel_int_neur <- sum_int_intercept %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_fullmodel_int_neur,file="N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_int_neur_fimlx_ses.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_fullmodel_int_neur)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_int_neur_fimlx_ses.docx",row.names = T)

sum_ext_intercept <- standardizedSolution(fit_ext$pgs_intercept, ci = TRUE, level = 0.95) %>%
  unite(col='Regression', c('lhs', 'op', 'rhs'), sep='')
sum_ext_intercept$var<-"ext_neur"

est_fullmodel_ext_neur <- sum_ext_intercept %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_fullmodel_ext_neur,file="N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_ext_neur_fimlx_ses.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_fullmodel_ext_neur)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_ext_neur_fimlx_ses.docx",row.names = T)


####RUN LGM FOR PGS BD ####
library(readr)
dat<-read_csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
dat$sex<-as.character(dat$sex)
dat$by_cat<-as.character(dat$by_cat)
str(dat$adjpgs_bd)
str(dat$sex)
str(dat$by_cat)
dat$sex<-recode(dat$sex,`1`=0,`2`=1)
table(dat$sex)
table(dat$by_cat)
dat$sex<-factor(dat$sex,ordered = F)
dat$by_cat<-factor(dat$by_cat,ordered = T)
dat$by_cat<-as.numeric(dat$by_cat)
str(dat$sex)#
str(dat$by_cat)#

dat$edu_mf<-dat$hig_edu_mor_or_far
str(dat$edu_mf)
dat$edu_mf<-as.integer(as.logical(dat$edu_mf))
dat$edu_mf<-factor(dat$edu_mf,ordered=F)
table(dat$edu_mf)
str(dat$edu_mf)

#rename vars to shorter names
dat<-dat%>%rename(int18m=cbcl_int_c_18m,int3yr=cbcl_int_c_3yr,int5yr=cbcl_int_c_5yr,
                  ext18m=cbcl_ext_c_18m,ext3yr=cbcl_ext_c_3yr,ext5yr=cbcl_ext_c_5yr,
                  adj_pgs=adjpgs_bd)

variable.names(dat)
####run growth models
source("./scripts_github/r_specify_growth_models_ses.R")

int<-dat%>%select(sex,edu_mf,by_cat,adj_pgs,int18m,int3yr,int5yr)%>%rename(ytime1=int18m,
                                                                           ytime2=int3yr,
                                                                           ytime3=int5yr)

ext<-dat%>%select(sex,edu_mf,by_cat,adj_pgs,ext18m,ext3yr,ext5yr)%>%rename(ytime1=ext18m,
                                                                           ytime2=ext3yr,
                                                                           ytime3=ext5yr)



fit_int<-list()

fit_int<-list()
fit_int$basic <- growth(basic, data=int,  missing="fiml.x")
fit_int$pgs_residuals <- growth(pgs_residuals, data=int,  missing="fiml.x")
fit_int$pgs_gf <- growth(pgs_gf, data=int,  missing="fiml.x")
fit_int$pgs_intercept <- growth(pgs_intercept, data=int,  missing="fiml.x")
fit_int$pgs_slope <- growth(pgs_slope, data=int,  missing="fiml.x")
fit_int$lgm_nopgs <- growth(lgm_nopgs, data=int,  missing="fiml.x")


lav_res_gf<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_gf)#
lav_res_int<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_intercept)#
lav_res_slope<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_slope)#
lav_res_nopgs<-lavTestLRT(fit_int$pgs_residuals,fit_int$lgm_nopgs)#
lav_gf_int<-lavTestLRT(fit_int$pgs_gf,fit_int$pgs_intercept)#
lav_gf_slope<-lavTestLRT(fit_int$pgs_gf,fit_int$pgs_slope)#
lav_nopgs_gf<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_gf)#
lav_nopgs_int<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_intercept)#
lav_nopgs_slope<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_slope)#

int_bd_fit<-rbind(lav_res_gf,lav_res_int,lav_res_slope,lav_res_nopgs,lav_gf_int,lav_gf_slope,lav_nopgs_gf,lav_nopgs_int,lav_nopgs_slope)
int_bd_fit2<-int_bd_fit%>%select("Df","AIC","Chisq diff","Pr(>Chisq)")

int_bd_fit2
int_bd_fit2<-round(int_bd_fit2,digits=3)
int_bd_fit2$model<-row.names(int_bd_fit2)
write.table(int_bd_fit2,file="N:/durable/users/norarba/FLA2_rew/output/bd_fit_int_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(int_bd_fit2)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/bd_fit_int_ldpred_ses_x.docx",row.names = T)

fit_ext<-list()
fit_ext$basic <- growth(basic, data=ext,  missing="fiml.x")
fit_ext$pgs_residuals <- growth(pgs_residuals, data=ext,  missing="fiml.x")
fit_ext$pgs_gf <- growth(pgs_gf, data=ext,  missing="fiml.x")
fit_ext$pgs_intercept <- growth(pgs_intercept, data=ext,  missing="fiml.x")
fit_ext$pgs_slope <- growth(pgs_slope, data=ext,  missing="fiml.x")
fit_ext$lgm_nopgs <- growth(lgm_nopgs, data=ext,  missing="fiml.x")


lav_res_gf<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_gf)#
lav_res_int<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_intercept)#
lav_res_slope<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_slope)#
lav_res_nopgs<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$lgm_nopgs)#
lav_gf_int<-lavTestLRT(fit_ext$pgs_gf,fit_ext$pgs_intercept)#
lav_gf_slope<-lavTestLRT(fit_ext$pgs_gf,fit_ext$pgs_slope)#
lav_nopgs_gf<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_gf)#
lav_nopgs_int<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_intercept)#
lav_nopgs_slope<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_slope)#

ext_bd_fit<-rbind(lav_res_gf,lav_res_int,lav_res_slope,lav_res_nopgs,lav_gf_int,lav_gf_slope,lav_nopgs_gf,lav_nopgs_int,lav_nopgs_slope)
ext_bd_fit2<-ext_bd_fit%>%select("Df","AIC","Chisq diff","Pr(>Chisq)")

ext_bd_fit2
ext_bd_fit2<-round(ext_bd_fit2,digits=3)
ext_bd_fit2$model<-row.names(ext_bd_fit2)
write.table(ext_bd_fit2,file="N:/durable/users/norarba/FLA2_rew/output/bd_fit_ext_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(ext_bd_fit2)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/bd_fit_ext_ldpred_ses_x.docx",row.names = T)


####veryfy_no_ass

###
source("./scripts_github/r_specify_growth_models_ses.R")
summary(growth(pgs_gf, data=int, missing="fiml.x"),standardized = TRUE,fit.measures=TRUE)
#summary(growth(pgs_intercept, data=ext, missing="fiml.x"),standardized = TRUE,fit.measures=TRUE)

summary(growth(pgs_gf, data=ext, missing="fiml.x"),standardized = TRUE,fit.measures=TRUE)
#summary(growth(pgs_gf, data=int, missing="fiml.x"),standardized = TRUE,fit.measures=TRUE)

####get results with 95%CI
source("./scripts_github/r_specify_growth_models_ses.R")

fit_int<-list()
fit_int$basic <- growth(basic, data=int,  missing="fiml.x")
fit_int$pgs_residuals <- growth(pgs_residuals, data=int,  missing="fiml.x")
fit_int$pgs_gf <- growth(pgs_gf, data=int,  missing="fiml.x")
fit_int$pgs_intercept <- growth(pgs_intercept, data=int,  missing="fiml.x")
fit_int$pgs_slope <- growth(pgs_slope, data=int,  missing="fiml.x")
fit_int$lgm_nopgs <- growth(lgm_nopgs, data=int,  missing="fiml.x")


fit_ext<-list()
fit_ext$basic <- growth(basic, data=ext,  missing="fiml.x")
fit_ext$pgs_residuals <- growth(pgs_residuals, data=ext,  missing="fiml.x")
fit_ext$pgs_gf <- growth(pgs_gf, data=ext,  missing="fiml.x")
fit_ext$pgs_intercept <- growth(pgs_intercept, data=ext,  missing="fiml.x")
fit_ext$pgs_slope <- growth(pgs_slope, data=ext,  missing="fiml.x")
fit_ext$lgm_nopgs <- growth(lgm_nopgs, data=ext,  missing="fiml.x")


sum_int_gf <- standardizedSolution(fit_int$pgs_gf, ci = TRUE, level = 0.95) %>%
  filter(rhs=="adj_pgs",
         op == "~")%>%select("lhs","est.std","ci.lower","ci.upper","pvalue")
sum_int_gf$var<-"int_bd"

sum_ext_gf <- standardizedSolution(fit_ext$pgs_gf, ci = TRUE, level = 0.95) %>%
  filter(rhs=="adj_pgs",
         op == "~")%>%select("lhs","est.std","ci.lower","ci.upper","pvalue")
sum_ext_gf$var<-"ext_bd"

est_bd<-rbind(sum_int_gf,sum_ext_gf)

est_bdr <- est_bd %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_bdr,file="N:/durable/users/norarba/FLA2_rew/output/bd_fit_ldpred_ses_x_240220.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_bdr)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/bd_fit_summary_ldpred_ses_x_240220.docx",row.names = T)

#no sig pgs effect and null model best performing

###extract full model results for best fitting model###
sum_int_lgm_nopgs <- standardizedSolution(fit_int$lgm_nopgs, ci = TRUE, level = 0.95) %>%
  unite(col='Regression', c('lhs', 'op', 'rhs'), sep='')
sum_int_lgm_nopgs$var<-"int_bd"

est_fullmodel_int_bd <- sum_int_lgm_nopgs %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_fullmodel_int_bd,file="N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_int_bd_fimlx_ses.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_fullmodel_int_bd)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_int_bd_fimlx_ses.docx",row.names = T)

sum_ext_lgm_nopgs <- standardizedSolution(fit_ext$lgm_nopgs, ci = TRUE, level = 0.95) %>%
  unite(col='Regression', c('lhs', 'op', 'rhs'), sep='')
sum_ext_lgm_nopgs$var<-"ext_bd"

est_fullmodel_ext_bd <- sum_ext_lgm_nopgs %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_fullmodel_ext_bd,file="N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_ext_bd_fimlx_ses.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_fullmodel_ext_bd)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_ext_bd_fimlx_ses.docx",row.names = T)



####RUN LGM FOR PGS ANX ####
library(readr)
dat<-read_csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
dat$sex<-as.character(dat$sex)
dat$by_cat<-as.character(dat$by_cat)
str(dat$adjpgs_bd)
str(dat$sex)
str(dat$by_cat)
dat$sex<-recode(dat$sex,`1`=0,`2`=1)
table(dat$sex)
table(dat$by_cat)
dat$sex<-factor(dat$sex,ordered = F)
dat$by_cat<-factor(dat$by_cat,ordered = T)
dat$by_cat<-as.numeric(dat$by_cat)
str(dat$sex)#
str(dat$by_cat)#

dat$edu_mf<-dat$hig_edu_mor_or_far
str(dat$edu_mf)
dat$edu_mf<-as.integer(as.logical(dat$edu_mf))
dat$edu_mf<-factor(dat$edu_mf,ordered=F)
table(dat$edu_mf)
str(dat$edu_mf)

#rename vars to shorter names
dat<-dat%>%rename(int18m=cbcl_int_c_18m,int3yr=cbcl_int_c_3yr,int5yr=cbcl_int_c_5yr,
                  ext18m=cbcl_ext_c_18m,ext3yr=cbcl_ext_c_3yr,ext5yr=cbcl_ext_c_5yr,
                  adj_pgs=adjpgs_anx)

variable.names(dat)
####run growth models
source("./scripts_github/r_specify_growth_models_ses.R")

int<-dat%>%select(sex,edu_mf,by_cat,adj_pgs,int18m,int3yr,int5yr)%>%rename(ytime1=int18m,
                                                                           ytime2=int3yr,
                                                                           ytime3=int5yr)

ext<-dat%>%select(sex,edu_mf,by_cat,adj_pgs,ext18m,ext3yr,ext5yr)%>%rename(ytime1=ext18m,
                                                                           ytime2=ext3yr,
                                                                           ytime3=ext5yr)

fit_int<-list()
fit_int$basic <- growth(basic, data=int,  missing="fiml.x")
fit_int$pgs_residuals <- growth(pgs_residuals, data=int,  missing="fiml.x")
fit_int$pgs_gf <- growth(pgs_gf, data=int,  missing="fiml.x")
fit_int$pgs_intercept <- growth(pgs_intercept, data=int,  missing="fiml.x")
fit_int$pgs_slope <- growth(pgs_slope, data=int,  missing="fiml.x")
fit_int$lgm_nopgs <- growth(lgm_nopgs, data=int,  missing="fiml.x")


lav_res_gf<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_gf)#
lav_res_int<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_intercept)#
lav_res_slope<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_slope)#
lav_res_nopgs<-lavTestLRT(fit_int$pgs_residuals,fit_int$lgm_nopgs)#
lav_gf_int<-lavTestLRT(fit_int$pgs_gf,fit_int$pgs_intercept)#
lav_gf_slope<-lavTestLRT(fit_int$pgs_gf,fit_int$pgs_slope)#
lav_nopgs_gf<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_gf)#
lav_nopgs_int<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_intercept)#
lav_nopgs_slope<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_slope)#

int_anx_fit<-rbind(lav_res_gf,lav_res_int,lav_res_slope,lav_res_nopgs,lav_gf_int,lav_gf_slope,lav_nopgs_gf,lav_nopgs_int,lav_nopgs_slope)
int_anx_fit2<-int_anx_fit%>%select("Df","AIC","Chisq diff","Pr(>Chisq)")

int_anx_fit2
int_anx_fit2<-round(int_anx_fit2,digits=3)
int_anx_fit2$model<-row.names(int_anx_fit2)
write.table(int_anx_fit2,file="N:/durable/users/norarba/FLA2_rew/output/anx_fit_int_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(int_anx_fit2)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/anx_fit_int_ldpred_ses_x.docx",row.names = T)

fit_ext<-list()
fit_ext$basic <- growth(basic, data=ext,  missing="fiml.x")
fit_ext$pgs_residuals <- growth(pgs_residuals, data=ext,  missing="fiml.x")
fit_ext$pgs_gf <- growth(pgs_gf, data=ext,  missing="fiml.x")
fit_ext$pgs_intercept <- growth(pgs_intercept, data=ext,  missing="fiml.x")
fit_ext$pgs_slope <- growth(pgs_slope, data=ext,  missing="fiml.x")
fit_ext$lgm_nopgs <- growth(lgm_nopgs, data=ext,  missing="fiml.x")

lav_res_gf<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_gf)#
lav_res_int<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_intercept)#
lav_res_slope<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_slope)#
lav_res_nopgs<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$lgm_nopgs)#
lav_gf_int<-lavTestLRT(fit_ext$pgs_gf,fit_ext$pgs_intercept)#
lav_gf_slope<-lavTestLRT(fit_ext$pgs_gf,fit_ext$pgs_slope)#
lav_nopgs_gf<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_gf)#
lav_nopgs_int<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_intercept)#
lav_nopgs_slope<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_slope)#

ext_anx_fit<-rbind(lav_res_gf,lav_res_int,lav_res_slope,lav_res_nopgs,lav_gf_int,lav_gf_slope,lav_nopgs_gf,lav_nopgs_int,lav_nopgs_slope)
ext_anx_fit2<-ext_anx_fit%>%select("Df","AIC","Chisq diff","Pr(>Chisq)")

ext_anx_fit2
ext_anx_fit2<-round(ext_anx_fit2,digits=3)
ext_anx_fit2$model<-row.names(ext_anx_fit2)
write.table(ext_anx_fit2,file="N:/durable/users/norarba/FLA2_rew/output/anx_fit_ext_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(ext_anx_fit2)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/anx_fit_ext_ldpred_ses_x.docx",row.names = T)


####effect estimates for best performing latent growth model

###
source("./scripts_github/r_specify_growth_models_ses.R")
summary(growth(pgs_gf, data=int, missing="fiml.x"),standardized = TRUE,fit.measures=TRUE)

summary(growth(pgs_gf, data=ext, missing="fiml.x"),standardized = TRUE,fit.measures=TRUE)

####get results with 95%CI
source("./scripts_github/r_specify_growth_models_ses.R")

fit_int<-list()
fit_int$basic <- growth(basic, data=int,  missing="fiml.x")
fit_int$pgs_residuals <- growth(pgs_residuals, data=int,  missing="fiml.x")
fit_int$pgs_gf <- growth(pgs_gf, data=int,  missing="fiml.x")
fit_int$pgs_intercept <- growth(pgs_intercept, data=int,  missing="fiml.x")
fit_int$pgs_slope <- growth(pgs_slope, data=int,  missing="fiml.x")
fit_int$lgm_nopgs <- growth(lgm_nopgs, data=int,  missing="fiml.x")


fit_ext<-list()
fit_ext$basic <- growth(basic, data=ext,  missing="fiml.x")
fit_ext$pgs_residuals <- growth(pgs_residuals, data=ext,  missing="fiml.x")
fit_ext$pgs_gf <- growth(pgs_gf, data=ext,  missing="fiml.x")
fit_ext$pgs_intercept <- growth(pgs_intercept, data=ext,  missing="fiml.x")
fit_ext$pgs_slope <- growth(pgs_slope, data=ext,  missing="fiml.x")
fit_ext$lgm_nopgs <- growth(lgm_nopgs, data=ext,  missing="fiml.x")


sum_int_slope <- standardizedSolution(fit_int$pgs_slope, ci = TRUE, level = 0.95) %>%
  filter(rhs=="adj_pgs",
         op == "~")%>%select("lhs","est.std","ci.lower","ci.upper","pvalue")
sum_int_slope$var<-"int_anx"

sum_ext_slope <- standardizedSolution(fit_ext$pgs_slope, ci = TRUE, level = 0.95) %>%
  filter(rhs=="adj_pgs",
         op == "~")%>%select("lhs","est.std","ci.lower","ci.upper","pvalue")
sum_ext_slope$var<-"ext_anx"

est_anx<-rbind(sum_int_slope,sum_ext_slope)

est_anxr <- est_anx %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_anxr,file="N:/durable/users/norarba/FLA2_rew/output/anx_fit_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_anxr)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/anx_fit_summary_ldpred_ses_x.docx",row.names = T)

#save p-val for fdr correction
p_int_anx<-sum_int_slope[2,]
p_int_anx<-p_int_anx$pvalue
p_int_anx

p_ext_anx<-sum_ext_slope[2,]
p_ext_anx<-p_ext_anx$pvalue
p_ext_anx

###extract full model results
sum_int_slope <- standardizedSolution(fit_int$pgs_slope, ci = TRUE, level = 0.95) %>%
  unite(col='Regression', c('lhs', 'op', 'rhs'), sep='')
sum_int_slope$var<-"int_anx"

est_fullmodel_int_anx <- sum_int_slope %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_fullmodel_int_anx,file="N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_int_anx_fimlx_ses.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_fullmodel_int_anx)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_int_anx_fimlx_ses.docx",row.names = T)

sum_ext_slope <- standardizedSolution(fit_ext$pgs_slope, ci = TRUE, level = 0.95) %>%
  unite(col='Regression', c('lhs', 'op', 'rhs'), sep='')
sum_ext_slope$var<-"ext_anx"

est_fullmodel_ext_anx <- sum_ext_slope %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_fullmodel_ext_anx,file="N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_ext_anx_fimlx_ses.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_fullmodel_ext_anx)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_ext_anx_fimlx_ses.docx",row.names = T)


####RUN LGM FOR PGS DEP ####

library(readr)
dat<-read_csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
dat$sex<-as.character(dat$sex)
dat$by_cat<-as.character(dat$by_cat)
str(dat$adjpgs_bd)
str(dat$sex)
str(dat$by_cat)
dat$sex<-recode(dat$sex,`1`=0,`2`=1)
table(dat$sex)
table(dat$by_cat)
dat$sex<-factor(dat$sex,ordered = F)
dat$by_cat<-factor(dat$by_cat,ordered = T)
dat$by_cat<-as.numeric(dat$by_cat)
str(dat$sex)#
str(dat$by_cat)#

dat$edu_mf<-dat$hig_edu_mor_or_far
str(dat$edu_mf)
dat$edu_mf<-as.integer(as.logical(dat$edu_mf))
dat$edu_mf<-factor(dat$edu_mf,ordered=F)
table(dat$edu_mf)
str(dat$edu_mf)

#rename vars to shorter names
dat<-dat%>%rename(int18m=cbcl_int_c_18m,int3yr=cbcl_int_c_3yr,int5yr=cbcl_int_c_5yr,
                  ext18m=cbcl_ext_c_18m,ext3yr=cbcl_ext_c_3yr,ext5yr=cbcl_ext_c_5yr,
                  adj_pgs=adjpgs_dep)

variable.names(dat)
####run growth models
source("./scripts_github/r_specify_growth_models_ses.R")

int<-dat%>%select(sex,edu_mf,by_cat,adj_pgs,int18m,int3yr,int5yr)%>%rename(ytime1=int18m,
                                                                           ytime2=int3yr,
                                                                           ytime3=int5yr)

ext<-dat%>%select(sex,edu_mf,by_cat,adj_pgs,ext18m,ext3yr,ext5yr)%>%rename(ytime1=ext18m,
                                                                           ytime2=ext3yr,
                                                                           ytime3=ext5yr)

fit_int<-list()
fit_int$basic <- growth(basic, data=int,  missing="fiml.x")
fit_int$pgs_residuals <- growth(pgs_residuals, data=int,  missing="fiml.x")
fit_int$pgs_gf <- growth(pgs_gf, data=int,  missing="fiml.x")
fit_int$pgs_intercept <- growth(pgs_intercept, data=int,  missing="fiml.x")
fit_int$pgs_slope <- growth(pgs_slope, data=int,  missing="fiml.x")
fit_int$lgm_nopgs <- growth(lgm_nopgs, data=int,  missing="fiml.x")

lav_res_gf<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_gf)#
lav_res_int<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_intercept)#
lav_res_slope<-lavTestLRT(fit_int$pgs_residuals,fit_int$pgs_slope)#
lav_res_nopgs<-lavTestLRT(fit_int$pgs_residuals,fit_int$lgm_nopgs)#
lav_gf_int<-lavTestLRT(fit_int$pgs_gf,fit_int$pgs_intercept)#
lav_gf_slope<-lavTestLRT(fit_int$pgs_gf,fit_int$pgs_slope)#
lav_nopgs_gf<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_gf)#
lav_nopgs_int<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_intercept)#
lav_nopgs_slope<-lavTestLRT(fit_int$lgm_nopgs,fit_int$pgs_slope)#

int_dep_fit<-rbind(lav_res_gf,lav_res_int,lav_res_slope,lav_res_nopgs,lav_gf_int,lav_gf_slope,lav_nopgs_gf,lav_nopgs_int,lav_nopgs_slope)
int_dep_fit2<-int_dep_fit%>%select("Df","AIC","Chisq diff","Pr(>Chisq)")

int_dep_fit2
int_dep_fit2<-round(int_dep_fit2,digits=3)
int_dep_fit2$model<-row.names(int_dep_fit2)
write.table(int_dep_fit2,file="N:/durable/users/norarba/FLA2_rew/output/dep_fit_int_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(int_dep_fit2)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/dep_fit_int_ldpred_ses_x.docx",row.names = T)

fit_ext<-list()
fit_ext$basic <- growth(basic, data=ext,  missing="fiml.x")
fit_ext$pgs_residuals <- growth(pgs_residuals, data=ext,  missing="fiml.x")
fit_ext$pgs_gf <- growth(pgs_gf, data=ext,  missing="fiml.x")
fit_ext$pgs_intercept <- growth(pgs_intercept, data=ext,  missing="fiml.x")
fit_ext$pgs_slope <- growth(pgs_slope, data=ext,  missing="fiml.x")
fit_ext$lgm_nopgs <- growth(lgm_nopgs, data=ext,  missing="fiml.x")


lav_res_gf<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_gf)#
lav_res_int<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_intercept)#
lav_res_slope<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$pgs_slope)#
lav_res_nopgs<-lavTestLRT(fit_ext$pgs_residuals,fit_ext$lgm_nopgs)#
lav_gf_int<-lavTestLRT(fit_ext$pgs_gf,fit_ext$pgs_intercept)#
lav_gf_slope<-lavTestLRT(fit_ext$pgs_gf,fit_ext$pgs_slope)#
lav_nopgs_gf<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_gf)#
lav_nopgs_int<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_intercept)#
lav_nopgs_slope<-lavTestLRT(fit_ext$lgm_nopgs,fit_ext$pgs_slope)#

ext_dep_fit<-rbind(lav_res_gf,lav_res_int,lav_res_slope,lav_res_nopgs,lav_gf_int,lav_gf_slope,lav_nopgs_gf,lav_nopgs_int,lav_nopgs_slope)
ext_dep_fit2<-ext_dep_fit%>%select("Df","AIC","Chisq diff","Pr(>Chisq)")

ext_dep_fit2
ext_dep_fit2<-round(ext_dep_fit2,digits=3)
ext_dep_fit2$model<-row.names(ext_dep_fit2)
write.table(ext_dep_fit2,file="N:/durable/users/norarba/FLA2_rew/output/dep_fit_ext_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(ext_dep_fit2)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/dep_fit_ext_ldpred_ses_x.docx",row.names = T)


####effect estimates for best performing latent growth model

###
source("./scripts_github/r_specify_growth_models_ses.R")
summary(growth(pgs_intercept, data=int, missing="fiml.x"),standardized = TRUE,fit.measures=TRUE)

summary(growth(pgs_gf, data=ext, missing="fiml.x"),standardized = TRUE,fit.measures=TRUE)

####get results with 95%CI
source("./scripts_github/r_specify_growth_models_ses.R")

fit_int<-list()
fit_int$basic <- growth(basic, data=int,  missing="fiml.x")
fit_int$pgs_residuals <- growth(pgs_residuals, data=int,  missing="fiml.x")
fit_int$pgs_gf <- growth(pgs_gf, data=int,  missing="fiml.x")
fit_int$pgs_intercept <- growth(pgs_intercept, data=int,  missing="fiml.x")
fit_int$pgs_slope <- growth(pgs_slope, data=int,  missing="fiml.x")
fit_int$lgm_nopgs <- growth(lgm_nopgs, data=int,  missing="fiml.x")


fit_ext<-list()
fit_ext$basic <- growth(basic, data=ext,  missing="fiml.x")
fit_ext$pgs_residuals <- growth(pgs_residuals, data=ext,  missing="fiml.x")
fit_ext$pgs_gf <- growth(pgs_gf, data=ext,  missing="fiml.x")
fit_ext$pgs_intercept <- growth(pgs_intercept, data=ext,  missing="fiml.x")
fit_ext$pgs_slope <- growth(pgs_slope, data=ext,  missing="fiml.x")
fit_ext$lgm_nopgs <- growth(lgm_nopgs, data=ext,  missing="fiml.x")


sum_int_intercept <- standardizedSolution(fit_int$pgs_intercept, ci = TRUE, level = 0.95) %>%
  filter(rhs=="adj_pgs",
         op == "~")%>%select("lhs","est.std","ci.lower","ci.upper","pvalue")
sum_int_intercept$var<-"int_dep"

sum_ext_gf <- standardizedSolution(fit_ext$pgs_gf, ci = TRUE, level = 0.95) %>%
  filter(rhs=="adj_pgs",
         op == "~")%>%select("lhs","est.std","ci.lower","ci.upper","pvalue")
sum_ext_gf$var<-"ext_dep"

est_dep<-rbind(sum_int_intercept,sum_ext_gf)

est_depr <- est_dep %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_depr,file="N:/durable/users/norarba/FLA2_rew/output/dep_fit_ldpred_ses_x.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_depr)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/dep_fit_summary_ldpred_ses_x.docx",row.names = T)

#save p-val for fdr correction
p_int_dep<-sum_int_intercept[1,]
p_int_dep<-p_int_dep$pvalue
p_int_dep

p_ext_dep<-sum_ext_gf
p_ext_dep<-p_ext_dep$pvalue
p_ext_dep


###extract full model results
sum_int_intercept <- standardizedSolution(fit_int$pgs_intercept, ci = TRUE, level = 0.95) %>%
  unite(col='Regression', c('lhs', 'op', 'rhs'), sep='')
sum_int_intercept$var<-"int_dep"

est_fullmodel_int_dep <- sum_int_intercept %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_fullmodel_int_dep,file="N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_int_dep_fimlx_ses.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_fullmodel_int_dep)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_int_dep_fimlx_ses.docx",row.names = T)


sum_ext_gf <- standardizedSolution(fit_ext$pgs_gf, ci = TRUE, level = 0.95) %>%
  unite(col='Regression', c('lhs', 'op', 'rhs'), sep='')
sum_ext_gf$var<-"ext_dep"

est_fullmodel_ext_dep <- sum_ext_gf %>%
  mutate_if(is.numeric,
            round,
            digits = 3)

write.table(est_fullmodel_ext_dep,file="N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_ext_dep_fimlx_ses.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(est_fullmodel_ext_dep)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/est_fullmodel_ext_dep_fimlx_ses.docx",row.names = T)



####FDR_CORRECT_P_VAL_SELECTED_MODELS ####
pvals<-c(p_int_dep,p_ext_dep,p_int_neur,p_ext_neur,p_int_anx,p_ext_anx)
frd_pval<-p.adjust(pvals,method = "BH")
frd_pval

frd_pval_r<-format(frd_pval, scientific = FALSE)

pvals<-format(pvals, scientific = FALSE)

pvals
frd_pval_r

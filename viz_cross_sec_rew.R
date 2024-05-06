#viz_cross_sec_rew
require('lavaan')
#require('semPlot')
#install.packages("semPlot")
library(semPlot)
require('psych')
require('tidyverse')
require('lme4')

library(phenotools)#this script is based on v 0.2.7
library(tidyverse)
library(readr)
library(haven)

#load df with updated npr
library(readr)
datp<-read_csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
dat$sex<-recode(dat$sex,`1`=0,`2`=1)
datp$sex<-factor(datp$sex,ordered=F)
str(datp$sex)
datp$by_cat<-factor(datp$by_cat,ordered=T)
datp$by_cat<-as.numeric(datp$by_cat)
str(datp$by_cat)

str(datp$adjpgs_bd)
str(datp$sex)#
str(datp$by_cat)#


#scaled variables
library(gtsummary)
library(psych)
datp$cbcl_ext_c_18m_sta<-scale(datp$cbcl_ext_c_18m, center = TRUE, scale =TRUE)
datp$cbcl_ext_c_3yr_sta<-scale(datp$cbcl_ext_c_3yr, center = TRUE, scale =TRUE)
datp$cbcl_ext_c_5yr_sta<-scale(datp$cbcl_ext_c_5yr, center = TRUE, scale =TRUE)

datp$cbcl_int_c_18m_sta<-scale(datp$cbcl_int_c_18m, center = TRUE, scale =TRUE)
datp$cbcl_int_c_3yr_sta<-scale(datp$cbcl_int_c_3yr, center = TRUE, scale =TRUE)
datp$cbcl_int_c_5yr_sta<-scale(datp$cbcl_int_c_5yr, center = TRUE, scale =TRUE)

datp$scared_anx_c_8yr_sta<-scale(datp$scared_anx_c_8yr, center = TRUE, scale =TRUE)
datp$smfq_dep_c_8yr_sta<-scale(datp$smfq_dep_c_8yr, center = TRUE, scale =TRUE)

datp$rsdbd_ina_c_8yr_sta<-scale(datp$rsdbd_ina_c_8yr, center = TRUE, scale =TRUE)
datp$rsdbd_hyp_c_8yr_sta<-scale(datp$rsdbd_hyp_c_8yr, center = TRUE, scale =TRUE)
datp$rsdbd_cd_c_8yr_sta<-scale(datp$rsdbd_cd_c_8yr, center = TRUE, scale =TRUE)
datp$rsdbd_odd_c_8yr_sta<-scale(datp$rsdbd_odd_c_8yr, center = TRUE, scale =TRUE)


####spesify regression models. ####
###DEP
cbcl_int_c_18m_sta_dep <- 'cbcl_int_c_18m_sta~adjpgs_dep+by_cat+sex'
cbcl_int_c_3yr_sta_dep <- 'cbcl_int_c_3yr_sta~adjpgs_dep+by_cat+sex'
cbcl_int_c_5yr_sta_dep <- 'cbcl_int_c_5yr_sta~adjpgs_dep+by_cat+sex'

cbcl_ext_c_18m_sta_dep <- 'cbcl_ext_c_18m_sta~adjpgs_dep+by_cat+sex'
cbcl_ext_c_3yr_sta_dep <- 'cbcl_ext_c_3yr_sta~adjpgs_dep+by_cat+sex'
cbcl_ext_c_5yr_sta_dep <- 'cbcl_ext_c_5yr_sta~adjpgs_dep+by_cat+sex'

smfq_dep_c_8yr_sta_dep <- 'smfq_dep_c_8yr_sta~adjpgs_dep+by_cat+sex'
scared_anx_c_8yr_sta_dep <- 'scared_anx_c_8yr_sta~adjpgs_dep+by_cat+sex'

rsdbd_ina_c_8yr_sta_dep <- 'rsdbd_ina_c_8yr_sta~adjpgs_dep+by_cat+sex'
rsdbd_odd_c_8yr_sta_dep <- 'rsdbd_odd_c_8yr_sta~adjpgs_dep+by_cat+sex'
rsdbd_hyp_c_8yr_sta_dep <- 'rsdbd_hyp_c_8yr_sta~adjpgs_dep+by_cat+sex'
rsdbd_cd_c_8yr_sta_dep <- 'rsdbd_cd_c_8yr_sta~adjpgs_dep+by_cat+sex'

###NEUR
cbcl_int_c_18m_sta_neur <- 'cbcl_int_c_18m_sta~adjpgs_neur+by_cat+sex'
cbcl_int_c_3yr_sta_neur <- 'cbcl_int_c_3yr_sta~adjpgs_neur+by_cat+sex'
cbcl_int_c_5yr_sta_neur <- 'cbcl_int_c_5yr_sta~adjpgs_neur+by_cat+sex'

cbcl_ext_c_18m_sta_neur <- 'cbcl_ext_c_18m_sta~adjpgs_neur+by_cat+sex'
cbcl_ext_c_3yr_sta_neur <- 'cbcl_ext_c_3yr_sta~adjpgs_neur+by_cat+sex'
cbcl_ext_c_5yr_sta_neur <- 'cbcl_ext_c_5yr_sta~adjpgs_neur+by_cat+sex'

smfq_dep_c_8yr_sta_neur <- 'smfq_dep_c_8yr_sta~adjpgs_neur+by_cat+sex'
scared_anx_c_8yr_sta_neur <- 'scared_anx_c_8yr_sta~adjpgs_neur+by_cat+sex'

rsdbd_ina_c_8yr_sta_neur <- 'rsdbd_ina_c_8yr_sta~adjpgs_neur+by_cat+sex'
rsdbd_odd_c_8yr_sta_neur <- 'rsdbd_odd_c_8yr_sta~adjpgs_neur+by_cat+sex'
rsdbd_hyp_c_8yr_sta_neur <- 'rsdbd_hyp_c_8yr_sta~adjpgs_neur+by_cat+sex'
rsdbd_cd_c_8yr_sta_neur <- 'rsdbd_cd_c_8yr_sta~adjpgs_neur+by_cat+sex'


###ANX
cbcl_int_c_18m_sta_anx <- 'cbcl_int_c_18m_sta~adjpgs_anx+by_cat+sex'
cbcl_int_c_3yr_sta_anx <- 'cbcl_int_c_3yr_sta~adjpgs_anx+by_cat+sex'
cbcl_int_c_5yr_sta_anx <- 'cbcl_int_c_5yr_sta~adjpgs_anx+by_cat+sex'

cbcl_ext_c_18m_sta_anx <- 'cbcl_ext_c_18m_sta~adjpgs_anx+by_cat+sex'
cbcl_ext_c_3yr_sta_anx <- 'cbcl_ext_c_3yr_sta~adjpgs_anx+by_cat+sex'
cbcl_ext_c_5yr_sta_anx <- 'cbcl_ext_c_5yr_sta~adjpgs_anx+by_cat+sex'

smfq_dep_c_8yr_sta_anx <- 'smfq_dep_c_8yr_sta~adjpgs_anx+by_cat+sex'
scared_anx_c_8yr_sta_anx <- 'scared_anx_c_8yr_sta~adjpgs_anx+by_cat+sex'

rsdbd_ina_c_8yr_sta_anx <- 'rsdbd_ina_c_8yr_sta~adjpgs_anx+by_cat+sex'
rsdbd_odd_c_8yr_sta_anx <- 'rsdbd_odd_c_8yr_sta~adjpgs_anx+by_cat+sex'
rsdbd_hyp_c_8yr_sta_anx <- 'rsdbd_hyp_c_8yr_sta~adjpgs_anx+by_cat+sex'
rsdbd_cd_c_8yr_sta_anx <- 'rsdbd_cd_c_8yr_sta~adjpgs_anx+by_cat+sex'

###BD
cbcl_int_c_18m_sta_bd <- 'cbcl_int_c_18m_sta~adjpgs_bd+by_cat+sex'
cbcl_int_c_3yr_sta_bd <- 'cbcl_int_c_3yr_sta~adjpgs_bd+by_cat+sex'
cbcl_int_c_5yr_sta_bd <- 'cbcl_int_c_5yr_sta~adjpgs_bd+by_cat+sex'

cbcl_ext_c_18m_sta_bd <- 'cbcl_ext_c_18m_sta~adjpgs_bd+by_cat+sex'
cbcl_ext_c_3yr_sta_bd <- 'cbcl_ext_c_3yr_sta~adjpgs_bd+by_cat+sex'
cbcl_ext_c_5yr_sta_bd <- 'cbcl_ext_c_5yr_sta~adjpgs_bd+by_cat+sex'

smfq_dep_c_8yr_sta_bd <- 'smfq_dep_c_8yr_sta~adjpgs_bd+by_cat+sex'
scared_anx_c_8yr_sta_bd <- 'scared_anx_c_8yr_sta~adjpgs_bd+by_cat+sex'

rsdbd_ina_c_8yr_sta_bd <- 'rsdbd_ina_c_8yr_sta~adjpgs_bd+by_cat+sex'
rsdbd_odd_c_8yr_sta_bd <- 'rsdbd_odd_c_8yr_sta~adjpgs_bd+by_cat+sex'
rsdbd_hyp_c_8yr_sta_bd <- 'rsdbd_hyp_c_8yr_sta~adjpgs_bd+by_cat+sex'
rsdbd_cd_c_8yr_sta_bd <- 'rsdbd_cd_c_8yr_sta~adjpgs_bd+by_cat+sex'


####RUN regression models####
cross_dep<-list()
cross_dep$cbcl_int_c_18m_sta_dep <- standardizedSolution(sem(model=cbcl_int_c_18m_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_dep$cbcl_int_c_3yr_sta_dep <- standardizedSolution(sem(model=cbcl_int_c_3yr_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_dep$cbcl_int_c_5yr_sta_dep <- standardizedSolution(sem(model=cbcl_int_c_5yr_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_dep$cbcl_ext_c_18m_sta_dep <- standardizedSolution(sem(model=cbcl_ext_c_18m_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_dep$cbcl_ext_c_3yr_sta_dep <- standardizedSolution(sem(model=cbcl_ext_c_3yr_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_dep$cbcl_ext_c_5yr_sta_dep <- standardizedSolution(sem(model=cbcl_ext_c_5yr_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_dep$smfq_dep_c_8yr_sta_dep <- standardizedSolution(sem(model=smfq_dep_c_8yr_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_dep$scared_anx_c_8yr_sta_dep <- standardizedSolution(sem(model=scared_anx_c_8yr_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_dep$rsdbd_ina_c_8yr_sta_dep <- standardizedSolution(sem(model=rsdbd_ina_c_8yr_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_dep$rsdbd_odd_c_8yr_sta_dep <- standardizedSolution(sem(model=rsdbd_odd_c_8yr_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_dep$rsdbd_hyp_c_8yr_sta_dep <- standardizedSolution(sem(model=rsdbd_hyp_c_8yr_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_dep$rsdbd_cd_c_8yr_sta_dep <- standardizedSolution(sem(model=rsdbd_cd_c_8yr_sta_dep, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)


cross_dep_df <-  as.data.frame(do.call(rbind, cross_dep))
cross_dep_df$PGS<-"DEP"

cross_neur<-list()
cross_neur$cbcl_int_c_18m_sta_neur <- standardizedSolution(sem(model=cbcl_int_c_18m_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_neur$cbcl_int_c_3yr_sta_neur <- standardizedSolution(sem(model=cbcl_int_c_3yr_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_neur$cbcl_int_c_5yr_sta_neur <- standardizedSolution(sem(model=cbcl_int_c_5yr_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_neur$cbcl_ext_c_18m_sta_neur <- standardizedSolution(sem(model=cbcl_ext_c_18m_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_neur$cbcl_ext_c_3yr_sta_neur <- standardizedSolution(sem(model=cbcl_ext_c_3yr_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_neur$cbcl_ext_c_5yr_sta_neur <- standardizedSolution(sem(model=cbcl_ext_c_5yr_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_neur$smfq_dep_c_8yr_sta_neur <- standardizedSolution(sem(model=smfq_dep_c_8yr_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_neur$scared_anx_c_8yr_sta_neur <- standardizedSolution(sem(model=scared_anx_c_8yr_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_neur$rsdbd_ina_c_8yr_sta_neur <- standardizedSolution(sem(model=rsdbd_ina_c_8yr_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_neur$rsdbd_odd_c_8yr_sta_neur <- standardizedSolution(sem(model=rsdbd_odd_c_8yr_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_neur$rsdbd_hyp_c_8yr_sta_neur <- standardizedSolution(sem(model=rsdbd_hyp_c_8yr_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_neur$rsdbd_cd_c_8yr_sta_neur <- standardizedSolution(sem(model=rsdbd_cd_c_8yr_sta_neur, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)


cross_neur_df <-  as.data.frame(do.call(rbind, cross_neur))
cross_neur_df$PGS<-"NEUR"

cross_anx<-list()
cross_anx$cbcl_int_c_18m_sta_anx <- standardizedSolution(sem(model=cbcl_int_c_18m_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_anx$cbcl_int_c_3yr_sta_anx <- standardizedSolution(sem(model=cbcl_int_c_3yr_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_anx$cbcl_int_c_5yr_sta_anx <- standardizedSolution(sem(model=cbcl_int_c_5yr_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_anx$cbcl_ext_c_18m_sta_anx <- standardizedSolution(sem(model=cbcl_ext_c_18m_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_anx$cbcl_ext_c_3yr_sta_anx <- standardizedSolution(sem(model=cbcl_ext_c_3yr_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_anx$cbcl_ext_c_5yr_sta_anx <- standardizedSolution(sem(model=cbcl_ext_c_5yr_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_anx$smfq_dep_c_8yr_sta_anx <- standardizedSolution(sem(model=smfq_dep_c_8yr_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_anx$scared_anx_c_8yr_sta_anx <- standardizedSolution(sem(model=scared_anx_c_8yr_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_anx$rsdbd_ina_c_8yr_sta_anx <- standardizedSolution(sem(model=rsdbd_ina_c_8yr_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_anx$rsdbd_odd_c_8yr_sta_anx <- standardizedSolution(sem(model=rsdbd_odd_c_8yr_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_anx$rsdbd_hyp_c_8yr_sta_anx <- standardizedSolution(sem(model=rsdbd_hyp_c_8yr_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_anx$rsdbd_cd_c_8yr_sta_anx <- standardizedSolution(sem(model=rsdbd_cd_c_8yr_sta_anx, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)


cross_anx_df <-  as.data.frame(do.call(rbind, cross_anx))
cross_anx_df$PGS<-"ANX"

cross_bd<-list()
cross_bd$cbcl_int_c_18m_sta_bd <- standardizedSolution(sem(model=cbcl_int_c_18m_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_bd$cbcl_int_c_3yr_sta_bd <- standardizedSolution(sem(model=cbcl_int_c_3yr_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_bd$cbcl_int_c_5yr_sta_bd <- standardizedSolution(sem(model=cbcl_int_c_5yr_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_bd$cbcl_ext_c_18m_sta_bd <- standardizedSolution(sem(model=cbcl_ext_c_18m_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_bd$cbcl_ext_c_3yr_sta_bd <- standardizedSolution(sem(model=cbcl_ext_c_3yr_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_bd$cbcl_ext_c_5yr_sta_bd <- standardizedSolution(sem(model=cbcl_ext_c_5yr_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_bd$smfq_dep_c_8yr_sta_bd <- standardizedSolution(sem(model=smfq_dep_c_8yr_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_bd$scared_anx_c_8yr_sta_bd <- standardizedSolution(sem(model=scared_anx_c_8yr_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)

cross_bd$rsdbd_ina_c_8yr_sta_bd <- standardizedSolution(sem(model=rsdbd_ina_c_8yr_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_bd$rsdbd_odd_c_8yr_sta_bd <- standardizedSolution(sem(model=rsdbd_odd_c_8yr_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_bd$rsdbd_hyp_c_8yr_sta_bd <- standardizedSolution(sem(model=rsdbd_hyp_c_8yr_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)
cross_bd$rsdbd_cd_c_8yr_sta_bd <- standardizedSolution(sem(model=rsdbd_cd_c_8yr_sta_bd, data=datp,missing='fiml', meanstructure=TRUE 
), ci = TRUE, level = 0.95)%>%
  select("lhs","est.std","ci.lower","ci.upper","pvalue")%>%slice(1)


cross_bd_df <-  as.data.frame(do.call(rbind, cross_bd))
cross_bd_df$PGS<-"BD"

####combine regression results
cross_combi_df<-rbind(cross_anx_df,cross_dep_df,cross_bd_df,cross_neur_df)

cross_combi_df$p_fdr<-p.adjust(cross_combi_df$pvalue, method="BH")

cross_combi_df
variable.names(cross_combi_df)

#rename trait names
cross_combi_df$trait<-cross_combi_df$lhs

cross_combi_df<-cross_combi_df%>%mutate(trait=recode(trait,"cbcl_int_c_18m_sta"="EMO 1.5yr","cbcl_int_c_3yr_sta"="EMO 3yr","cbcl_int_c_5yr_sta"="EMO 5yr",
                                     "cbcl_ext_c_18m_sta"="BEH 1.5yr","cbcl_ext_c_3yr_sta"="BEH 3yr","cbcl_ext_c_5yr_sta"="BEH 5yr",
                                     "smfq_dep_c_8yr_sta"="DEP 8yr","scared_anx_c_8yr_sta"="ANX 8yr",
                                     "rsdbd_ina_c_8yr_sta"="INA 8yr","rsdbd_hyp_c_8yr_sta"="HYP 8yr",
                                     "rsdbd_odd_c_8yr_sta"="ODD 8yr","rsdbd_cd_c_8yr_sta"="CD 8yr"))

#make ggplot
library(pals)
library(ggrepel)
windowsFonts(Times=windowsFont("Times New Roman"))
windowsFonts(sansserif=windowsFont("sans"))
windowsFonts(arial=windowsFont("Arial Unicode MS"))

cross_combi_df$trait <- factor(cross_combi_df$trait,      # Reordering group factor levels
                       levels = c("EMO 1.5yr","EMO 3yr","EMO 5yr",
                                  "BEH 1.5yr","BEH 3yr","BEH 5yr",
                                  "DEP 8yr","ANX 8yr",
                                  "INA 8yr","HYP 8yr","ODD 8yr","CD 8yr"))

cross_combi_df$trait<-fct_rev(cross_combi_df$trait)

cross_combi_df

cross_combi_df$PGS <- factor(cross_combi_df$PGS,      
                      levels = c("ANX","DEP","BD",
                                 "NEUR"))

#define age and type
cross_combi_df<-cross_combi_df %>%
  mutate(
    age = case_when(
      cross_combi_df$trait=="EMO 1.5yr" |  cross_combi_df$trait=="BEH 1.5yr" ~ "1.5 years",
      cross_combi_df$trait=="EMO 3yr" |  cross_combi_df$trait=="BEH 3yr"  ~ "3 years",
      cross_combi_df$trait=="EMO 5yr" |  cross_combi_df$trait=="BEH 5yr"  ~ "5 years",
      cross_combi_df$trait=="INA 8yr" |  cross_combi_df$trait=="HYP 8yr"|cross_combi_df$trait=="ODD 8yr" |  cross_combi_df$trait=="CD 8yr"|cross_combi_df$trait=="DEP 8yr" |  cross_combi_df$trait=="ANX 8yr"  ~ "8 years"
    )
  )

cross_combi_df<-cross_combi_df %>%
  mutate(
    type = case_when(
      cross_combi_df$trait=="EMO 1.5yr" | cross_combi_df$trait=="EMO 3yr"|
        cross_combi_df$trait=="EMO 5yr"|cross_combi_df$trait=="ANX 8yr"|
        cross_combi_df$trait=="DEP 8yr" ~ "Emotional difficulties",
      cross_combi_df$trait=="BEH 1.5yr" | cross_combi_df$trait=="BEH 3yr"|
        cross_combi_df$trait=="BEH 5yr"|cross_combi_df$trait=="INA 8yr"|
        cross_combi_df$trait=="HYP 8yr"|cross_combi_df$trait=="CD 8yr"|cross_combi_df$trait=="ODD 8yr" ~ "Behavioral difficulties"))


tail(cross_combi_df)

pd <- position_dodge(0.7)

plot<-ggplot(cross_combi_df, aes(x=est.std, y=trait, label=trait, shape=PGS, color=PGS)) + 
  geom_point(position=pd, size = 3.3, show.legend = TRUE) +
  geom_errorbar(aes(xmin=ci.lower, xmax=ci.upper), 
                width=0.5, position=pd, size=1.3) +
  geom_vline(xintercept = 0, linetype = "dashed")+scale_shape_manual(values = c(3, 16, 17, 15))+
  scale_color_manual(values=c("#d01c8b", "#7b3294", "#e66101", "#4dac26"))+
  ylab("Childhood emotional and behavioral difficulties")+
  xlab("Beta per standard deviation change in PGS")+
  facet_grid(type~age, scales = "free_y")+
  theme(legend.text.align=1)+ theme_bw()+scale_x_continuous(n.breaks = 5)+
  theme(strip.background = element_rect(fill="light blue"),
        legend.position = "bottom",axis.text.x = element_text(family="arial",size=14, color="black"), 
        axis.text.y = element_text(family="arial",size=14,color="black"),legend.title = element_text(family="arial",size=14,color="black"),
        legend.text = element_text(family="arial",size=14,color="black"),
        axis.title=element_text(family="arial",size=15, face="bold",color="black"),
        strip.text = element_text(family="arial",size = 15,face="bold"),
        axis.title.x = element_text(family="arial",size=15,color="black"),
        axis.title.y= element_text(family="arial",size=15,color="black"), strip.text.y = element_blank())
plot

ggsave("./output/plot_cross_sec_rew_28x24_bold_larger2.tiff",plot,dpi=900, height = 28, width = 24, units = "cm")




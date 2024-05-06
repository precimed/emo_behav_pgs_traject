#Cross_sectional_fiml 
require('lavaan')
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
datp$sex<-recode(datp$sex,`1`=0,`2`=1)
datp$sex<-factor(datp$sex,ordered=F)
str(datp$sex)
datp$by_cat<-factor(datp$by_cat,ordered=T)#Error in lav_samplestats_step1(Y = Data, wt = wt, ov.names = ov.names,  : 
                                          #lavaan ERROR: unknown ov.types:factor
datp$by_cat<-as.numeric(datp$by_cat)#
str(datp$by_cat)
str(datp$adjpgs_bd)
str(datp$sex)

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

#### combinde df for print and fdr adjustment####
cross_combi_df<-rbind(cross_dep_df,cross_neur_df,cross_anx_df,cross_bd_df)

cross_combi_df$p_fdr<-p.adjust(cross_combi_df$pvalue, method="BH")

cross_combi_df <- cross_combi_df %>%
  mutate_if(is.numeric,
            round,
            digits = 3)
cross_combi_df$model<-row.names(cross_combi_df)

write.table(cross_combi_df,file="N:/durable/users/norarba/FLA2_rew/output/cross_combi_df_rew_fiml_fn.txt",sep = ",", quote = FALSE, row.names = T)

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(cross_combi_df)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/cross_combi_df_rew_fiml_fn.docx",row.names = T)



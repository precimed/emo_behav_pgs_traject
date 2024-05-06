#fla2_descriptives

#read inn updated df
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
datp<- read.csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
#scaled variables
table(datp$anyemo_diag)

library(arsenal)
tab_general <- tableby(~sex+FAAR+age_end+ageOfFirstDiag_anyemo+hig_edu_mor_or_far+mor_alder_num+paritet_kat+FODELAND_KAT_NOR_GBD_G+
                         MORS_ALDER_G+FARS_ALDER_G+
                         anyemo_diag+dep_diag+anx_diag+bd_diag+adhd_diag+ocd_diag+cd_diag+
                         anyemo_adl+dep_adl+anx_adl+bd_adl+anyemo_child+dep_child+anx_child+bd_child+
                         ageOfFirstDiag_dep+ageOfFirstDiag_anx+ageOfFirstDiag_bd+
                         cbcl_int_c_18m+cbcl_int_c_3yr+ cbcl_int_c_5yr+
                         cbcl_ext_c_18m+cbcl_ext_c_3yr+ cbcl_ext_c_5yr+
                         smfq_dep_c_8yr+scared_anx_c_8yr+
                         rsdbd_ina_c_8yr+rsdbd_hyp_c_8yr+rsdbd_cd_c_8yr+rsdbd_odd_c_8yr+
                         nhipic_neurot_c_8yr,
                       data = datp,
                       numeric.simplify=TRUE, numeric.stats= "meansd",digits=2)
#summary(table_four)
write2word(tab_general, "N:/durable/users/norarba/FLA2_rew/descriptives_fla2.doc", title="Descriptive statistics")

library(psych)

####table by diagnosis
datps<-datp%>%select(sex,FAAR,age_end,ageOfFirstDiag_anyemo,hig_edu_mor_or_far,mor_alder_num,paritet_kat,FODELAND_KAT_NOR_GBD_G,
                     MORS_ALDER_G,FARS_ALDER_G,
                     anyemo_diag,dep_diag,anx_diag,bd_diag,adhd_diag,ocd_diag,cd_diag,
                     anyemo_adl,dep_adl,anx_adl,bd_adl,anyemo_child,dep_child,anx_child,bd_child,
                     ageOfFirstDiag_dep,ageOfFirstDiag_anx,ageOfFirstDiag_bd,
                     cbcl_int_c_18m,cbcl_int_c_3yr, cbcl_int_c_5yr,
                     cbcl_ext_c_18m,cbcl_ext_c_3yr, cbcl_ext_c_5yr,
                     smfq_dep_c_8yr,scared_anx_c_8yr,
                     rsdbd_ina_c_8yr,rsdbd_hyp_c_8yr,rsdbd_cd_c_8yr,rsdbd_odd_c_8yr,
                     nhipic_neurot_c_8yr)

datps$sex<-as.character(datps$sex)
datps$FODELAND_KAT_NOR_GBD_G<-as.character(datps$FODELAND_KAT_NOR_GBD_G)
datps$paritet_kat<-as.character(datps$paritet_kat)

table_diag <- tableby(anyemo_diag ~ ., data = datps ,numeric.test = "anova",
                      cat.test = "chisq") 

write2word(table_diag, "N:/durable/users/norarba/FLA2_rew/descriptives_fla2_desc_by_diag_cat.doc", 
           title="Descriptive statistics",pfootnote=TRUE, digits=2, digits.pct=2)

####c-alfa for scales
library(lavaan)
library(tidyverse)
library(haven)
library(readr)
install.packages("conogive")
library(conogive)
library(psych)
library(phenotools)

#import_item_level_data
q_nrb<-curate_dataset(
  variables_required=list(moba=c("cbcl_ext_c_18m", "cbcl_ext_c_3yr","cbcl_ext_c_5yr","cbcl_int_c_18m","cbcl_int_c_3yr","cbcl_int_c_5yr",
                                 "smfq_dep_c_8yr","scared_anx_c_8yr",
                                 "rsdbd_ina_c_8yr", "rsdbd_hyp_c_8yr", "rsdbd_cd_c_8yr", "rsdbd_odd_c_8yr"
  )), 
  moba_data_root_dir = "N:/durable/phenotypes/mobaQ/PDB2445_MoBa_V12/",#change to your directory here. 
  PDB = "2445",
  moba_data_version = 12, 
  completion_threshold = 0.5, #modify to your needs 
  return_items = TRUE,   #True - then includes scale itemlevel => ops can become quite heavy. 
  consistent_items = TRUE, #True for repeated measures. 
  transformations = NULL,
  log = NULL,
  out_format = "merged_df",
  npr_preprocessed= preprocd_npr,
  override_filenames = c("MBRN = PDB2445_MBRN_541_v12.sav"))#

df<-datp%>%select(preg_id,BARN_NR)
df$preg_id<-as.character(df$preg_id)
df$BARN_NR<-as.character(df$BARN_NR)
q_nrb$BARN_NR<-as.character(q_nrb$BARN_NR)

df<-left_join(df,q_nrb,by=c("preg_id","BARN_NR"))

#Calculate alphas using function: https://github.com/psychgen/scz-prs-psychopathol-dev/blob/master/scripts/00_prep_scales.R
df<-df%>%mutate(across(matches("_coded"), as.ordered))

ordinal_alpha <- function(x){
  psych::alpha(psych::polychoric(x)$rho)
}

c_alfa<-list()
c_alfa$int_18m<-ordinal_alpha(df%>%select(cbcl_int_c_18m_i1_EE908_coded,cbcl_int_c_18m_i2_EE438_coded,cbcl_int_c_18m_i3_EE439_coded,
                                          cbcl_int_c_18m_i4_EE909_coded,cbcl_int_c_18m_i5_EE963_coded))$total$std.alpha

c_alfa$int_3yr<-ordinal_alpha(df%>%select(cbcl_int_c_3yr_i1_GG321_coded,cbcl_int_c_3yr_i2_GG317_coded,cbcl_int_c_3yr_i3_GG328_coded,
                                          cbcl_int_c_3yr_i4_GG336_coded,cbcl_int_c_3yr_i5_GG323_coded))$total$std.alpha

c_alfa$int_5yr<-ordinal_alpha(df%>%select(cbcl_int_c_5yr_i1_LL309_coded,cbcl_int_c_5yr_i2_LL305_coded,cbcl_int_c_5yr_i3_LL315_coded,
                                          cbcl_int_c_5yr_i4_LL321_coded,cbcl_int_c_5yr_i5_LL310_coded))$total$std.alpha

c_alfa$ext_18m<-ordinal_alpha(df%>%select(cbcl_ext_c_18m_i1_EE435_coded,cbcl_ext_c_18m_i2_EE903_coded,cbcl_ext_c_18m_i3_EE961_coded,
                                          cbcl_ext_c_18m_i4_EE446_coded,cbcl_ext_c_18m_i5_EE447_coded,cbcl_ext_c_18m_i6_EE962_coded,
                                          cbcl_ext_c_18m_i7_EE442_coded,cbcl_ext_c_18m_i8_EE448_coded))$total$std.alpha

c_alfa$ext_3yr<-ordinal_alpha(df%>%select(cbcl_ext_c_3yr_i1_GG314_coded,cbcl_ext_c_3yr_i2_GG315_coded,cbcl_ext_c_3yr_i3_GG332_coded,
                                          cbcl_ext_c_3yr_i4_GG319_coded,cbcl_ext_c_3yr_i5_GG324_coded,cbcl_ext_c_3yr_i6_GG326_coded,
                                          cbcl_ext_c_3yr_i7_GG329_coded,cbcl_ext_c_3yr_i8_GG331_coded))$total$std.alpha

c_alfa$ext_5yr<-ordinal_alpha(df%>%select(cbcl_ext_c_5yr_i1_LL303_coded,cbcl_ext_c_5yr_i2_LL302_coded,cbcl_ext_c_5yr_i3_LL319_coded,
                                          cbcl_ext_c_5yr_i4_LL307_coded,cbcl_ext_c_5yr_i5_LL311_coded,cbcl_ext_c_5yr_i6_LL313_coded,
                                          cbcl_ext_c_5yr_i7_LL316_coded,cbcl_ext_c_5yr_i8_LL318_coded))$total$std.alpha

c_alfa$smfq_8yr<-ordinal_alpha(df%>%select(smfq_dep_c_8yr_i1_NN68_coded,smfq_dep_c_8yr_i2_NN69_coded,smfq_dep_c_8yr_i3_NN70_coded,
                                           smfq_dep_c_8yr_i4_NN71_coded,smfq_dep_c_8yr_i5_NN72_coded,smfq_dep_c_8yr_i6_NN73_coded,
                                           smfq_dep_c_8yr_i7_NN74_coded,smfq_dep_c_8yr_i8_NN75_coded,smfq_dep_c_8yr_i9_NN76_coded,
                                           smfq_dep_c_8yr_i10_NN77_coded,smfq_dep_c_8yr_i11_NN78_coded,smfq_dep_c_8yr_i12_NN79_coded,
                                           smfq_dep_c_8yr_i13_NN80_coded))$total$std.alpha

c_alfa$scared_8yr<-ordinal_alpha(df%>%select(scared_anx_c_8yr_i1_NN145_coded,scared_anx_c_8yr_i2_NN146_coded,scared_anx_c_8yr_i3_NN147_coded,
                                             scared_anx_c_8yr_i4_NN148_coded,scared_anx_c_8yr_i5_NN149_coded))$total$std.alpha

c_alfa$hyp_8yr<-ordinal_alpha(df%>%select(rsdbd_hyp_c_8yr_i1_NN128_coded,rsdbd_hyp_c_8yr_i2_NN129_coded,rsdbd_hyp_c_8yr_i3_NN130_coded,
                                          rsdbd_hyp_c_8yr_i4_NN131_coded,rsdbd_hyp_c_8yr_i5_NN132_coded,rsdbd_hyp_c_8yr_i6_NN133_coded,
                                          rsdbd_hyp_c_8yr_i7_NN134_coded,rsdbd_hyp_c_8yr_i8_NN135_coded,rsdbd_hyp_c_8yr_i9_NN136_coded))$total$std.alpha

c_alfa$ina_8yr<-ordinal_alpha(df%>%select(rsdbd_ina_c_8yr_i1_NN119_coded,rsdbd_ina_c_8yr_i2_NN120_coded,rsdbd_ina_c_8yr_i3_NN121_coded,
                                          rsdbd_ina_c_8yr_i4_NN122_coded,rsdbd_ina_c_8yr_i5_NN123_coded,rsdbd_ina_c_8yr_i6_NN124_coded,
                                          rsdbd_ina_c_8yr_i7_NN125_coded,rsdbd_ina_c_8yr_i8_NN126_coded,rsdbd_ina_c_8yr_i9_NN127_coded))$total$std.alpha

c_alfa$odd_8yr<-ordinal_alpha(df%>%select(rsdbd_odd_c_8yr_i1_NN137_coded,rsdbd_odd_c_8yr_i2_NN138_coded,rsdbd_odd_c_8yr_i3_NN139_coded,
                                          rsdbd_odd_c_8yr_i4_NN140_coded,rsdbd_odd_c_8yr_i5_NN141_coded,rsdbd_odd_c_8yr_i6_NN142_coded,
                                          rsdbd_odd_c_8yr_i7_NN143_coded,rsdbd_odd_c_8yr_i8_NN144_coded))$total$std.alpha

c_alfa$cd_8yr<-ordinal_alpha(df%>%select(rsdbd_cd_c_8yr_i1_NN111_coded,rsdbd_cd_c_8yr_i2_NN112_coded,rsdbd_cd_c_8yr_i3_NN113_coded,
                                         rsdbd_cd_c_8yr_i4_NN114_coded,rsdbd_cd_c_8yr_i5_NN115_coded,rsdbd_cd_c_8yr_i6_NN116_coded,
                                         rsdbd_cd_c_8yr_i7_NN117_coded,rsdbd_cd_c_8yr_i8_NN118_coded))$total$std.alpha


c_alfa_df <-  as.data.frame(do.call(rbind, c_alfa))
c_alfa_df$measure<-row.names(c_alfa_df)
c_alfa_df<-c_alfa_df%>%rename(c_alfa=V1)
c_alfa_df

library(officer)
sample_doc <- read_docx()
sample_doc <- sample_doc %>% body_add_table(c_alfa_df)
print(sample_doc, target = "N:/durable/users/norarba/FLA2_rew/output/c_alfa_df.docx",row.names = T)

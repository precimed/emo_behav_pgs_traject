#fla2_df_and_pgs_post_prosess
#Rproject: FLA2_rew

library(phenotools)
library(tidyverse)
library(readr)

#For NPR variables. 

preprocd_npr <-preload_npr(npr_data_root_dir =  "N:/durable/phenotypes/npr/NPR_2020_phenotools/",
                           npr_filename = "PDB2445_full_linked_20.sav") 
####make dataframe####
####questionnaires####
# Questionnaires in scale level and relevant diagnosis.

q_nrb<-curate_dataset(
  variables_required=list(moba=c("cbcl_ext_c_18m", "cbcl_ext_c_3yr","cbcl_ext_c_5yr","cbcl_int_c_18m","cbcl_int_c_3yr","cbcl_int_c_5yr",
                                 "nhipic_neurot_c_8yr",
                                 "smfq_dep_c_8yr","scared_anx_c_8yr",
                                 "rsdbd_ina_c_8yr", "rsdbd_hyp_c_8yr", "rsdbd_cd_c_8yr", "rsdbd_odd_c_8yr",
                                 "PARITET_5","AA1124", "AA1128","AA1126", "AA1130","AA1123","AA1315","AA1316"
                                 
  )), 
  moba_data_root_dir = "N:/durable/phenotypes/mobaQ/PDB2445_MoBa_V12/",#change to your directory here. 
  PDB = "2445",
  moba_data_version = 12, 
  completion_threshold = 0.5, #modify to your needs 
  return_items = FALSE,   #True - then includes scale itemlevel => ops can become quite heavy. 
  consistent_items = TRUE, #True for repeated measures. 
  transformations = NULL,
  log = NULL,
  out_format = "merged_df",
  npr_preprocessed= preprocd_npr,
  override_filenames = c("MBRN = PDB2445_MBRN_541_v12.sav"))
q_nrb # 
q_nrb<-q_nrb%>%drop_na(preg_id,BARN_NR)
q_nrb<-q_nrb%>%mutate(ID_2445=paste(preg_id,BARN_NR,sep="_"))

##recode neurot-scale as alternative vals are lacking in phenotools_0.2.7. 
  #else phenotools_0.2.7 and phenotools_0.2.9 is equal for my variables. 
library(haven)
PDB2445_Q8yrs_v12 <- read_sav("N:/durable/phenotypes/mobaQ/PDB2445_MoBa_V12/PDB2445_Q8yrs_v12.sav")
nhipic_p<-PDB2445_Q8yrs_v12
nhipic_p[nhipic_p == 0]<-NA
####convert to phenotools scale
#"NN81","NN368","NN86","NN94","NN97","NN102","NN107","NN372"
nhipic_p$NN81<-nhipic_p$NN81-1
nhipic_p$NN368<-nhipic_p$NN368-1
nhipic_p$NN86<-nhipic_p$NN86-1
nhipic_p$NN94<-nhipic_p$NN94-1
nhipic_p$NN97<-nhipic_p$NN97-1
nhipic_p$NN102<-nhipic_p$NN102-1
nhipic_p$NN107<-nhipic_p$NN107-1
nhipic_p$NN372<-nhipic_p$NN372-1

####reverse items
#NN107 og NN372 
nhipic_p$NN107<-nhipic_p$NN107-4
nhipic_p$NN372<-nhipic_p$NN372-4
#convert to absoulte numbers
nhipic_p$NN107<-abs(nhipic_p$NN107)
nhipic_p$NN372<-abs(nhipic_p$NN372)

#make variable with combined response to the two versions of the questionnaire.
for (i in 1:nrow(nhipic_p)) {
  if (is.na(nhipic_p$NN81[i])) {
    nhipic_p$NN81_368[i] <- nhipic_p$NN368[i]
  }
}

for (i in 1:nrow(nhipic_p)) {
  if (is.na(nhipic_p$NN107[i])) {
    nhipic_p$NN107_372[i] <- nhipic_p$NN372[i]
  }
}

nhipic_p$na_counts <- rowSums(is.na(nhipic_p[, c("NN81_368",  "NN86"    ,  "NN94"     , "NN97"   ,   "NN102"   ,  "NN107_372")]))
#row_means <- ifelse(na_counts <= 2, rowMeans(df[, c("var1", "var2", "var3", "var4")], na.rm = TRUE), NA)
nhipic_p$sum_50<-ifelse(nhipic_p$na_counts <3, rowMeans(nhipic_p[, c("NN81_368",  "NN86"    ,  "NN94"     , "NN97"   ,   "NN102"   ,  "NN107_372")], na.rm = TRUE), NA)
nhipic_p$sum_50<-nhipic_p$sum_50*6

#describe(neurot_pheno_use$sum_50)
describe(nhipic_p$sum_50)

###merge new df with rest of variables
variable.names(q_nrb)
variable.names(nhipic_p)
nhipic_p<-nhipic_p%>%drop_na(PREG_ID_2445,BARN_NR)
nhipic_p<-nhipic_p%>%mutate(ID_2445=paste(PREG_ID_2445,BARN_NR,sep="_"))
nhipic_p<-nhipic_p%>%select(ID_2445,sum_50)
q_child<-left_join(q_nrb,nhipic_p,by="ID_2445")
variable.names(q_child)
q_child<-q_child%>%select(-nhipic_neurot_c_8yr)
q_child<-q_child%>%rename("nhipic_neurot_c_8yr"="sum_50")


#update to new npr data
npr_minor<-read.delim("Y:/parekh/2023-08-14_parseNPR/old/2023-08-17-MoBa-ParsedNPR_minorCodes.csv", sep = "\t")
npr_major<-read.delim("Y:/parekh/2023-08-14_parseNPR/old/2023-08-17-MoBa-ParsedNPR_majorCodes.csv", sep = "\t")

npr_child_minor<-npr_minor%>%subset(Role=="Child")
npr_child_major<-npr_major%>%subset(Role=="Child")

npr_child_minor_rel<-npr_child_minor%>%select(F341_lifetime,F340_lifetime,F930_lifetime,F931_lifetime,F932_lifetime,ID_2445)
npr_child<-inner_join(npr_child_major,npr_child_minor_rel,by="ID_2445")

rm(npr_minor)
rm(npr_major)
rm(npr_child_minor)
rm(npr_child_major)

#QC logical operators
table(npr_child$F32_lifetime|npr_child$F31_lifetime)#
table(npr_child$F32_lifetime&npr_child$F31_lifetime)#
table(npr_child$F32_lifetime|npr_child$F31_lifetime|(npr_child$F32_lifetime&npr_child$F31_lifetime))#

#make lifetime diagnoses
npr_child$dep_diag<-(npr_child$F32_lifetime==TRUE|npr_child$F33_lifetime==TRUE|npr_child$F341_lifetime==TRUE)&
  (npr_child$F30_lifetime==FALSE&npr_child$F31_lifetime==FALSE&npr_child$F340_lifetime==FALSE)
npr_child$dep_diag_3232<-(npr_child$F32_lifetime==TRUE|npr_child$F33_lifetime==TRUE)&
  (npr_child$F30_lifetime==FALSE&npr_child$F31_lifetime==FALSE&npr_child$F340_lifetime==FALSE)

npr_child$bd_diag<-(npr_child$F30_lifetime==TRUE|npr_child$F31_lifetime==TRUE|npr_child$F340_lifetime==TRUE)
npr_child$bd_diag_3031<-(npr_child$F30_lifetime==TRUE|npr_child$F31_lifetime==TRUE)

npr_child$anx_diag<-(npr_child$F40_lifetime==TRUE|npr_child$F41_lifetime==TRUE|npr_child$F930_lifetime==TRUE|npr_child$F931_lifetime==TRUE|npr_child$F932_lifetime==TRUE)
npr_child$anx_diag_4041<-(npr_child$F40_lifetime==TRUE|npr_child$F41_lifetime==TRUE|npr_child$F341_lifetime==TRUE)

npr_child$anyemo_diag<-(npr_child$F30_lifetime==TRUE|npr_child$F31_lifetime==TRUE|
  npr_child$F32_lifetime==TRUE|npr_child$F33_lifetime==TRUE|
  npr_child$F34_lifetime==TRUE|npr_child$F38_lifetime==TRUE|
  npr_child$F39_lifetime==TRUE|npr_child$F40_lifetime==TRUE|
  npr_child$F41_lifetime==TRUE|npr_child$F92_lifetime==TRUE|npr_child$F93_lifetime==TRUE)

npr_child$adhd_diag<-npr_child$F90_lifetime==TRUE
npr_child$ocd_diag<-npr_child$F42_lifetime==TRUE
npr_child$cd_diag<-npr_child$F91_lifetime==TRUE
npr_child<-npr_child%>%select(-PREG_ID_2445)

#get age of onset vars 
onset<- read.csv("N:/durable/users/norarba/FLA2_rew/230826_df_onset_pravesh_nrb")

variable.names(onset)
onset$ageOfFirstDiag_anyemo<-onset$ageOfFirstDiag
onset<-onset%>%select(-ageOfFirstDiag)
  #ops age of fist dep diag includes bd cases as now. 
npr_child<-left_join(npr_child,onset,by="ID_2445")
npr_child$dep_adl<-npr_child$dep_adl==TRUE&npr_child$bd_diag==FALSE
#table(npr_child$dep_adl)
#table(onset$dep_adl)
npr_child$dep_child<-npr_child$dep_child==TRUE&npr_child$bd_diag==FALSE

#import relevant mbrn data
FODEREG <- read_sav("N:/durable/phenotypes/mobaQ/PDB2445_MoBa_V12/PDB2445_MBRN_541_v12.sav")%>%
  select(PREG_ID_2445,BARN_NR,KJONN,FAAR,FODELAND_KAT_NOR_GBD_G,MORS_ALDER_G,FARS_ALDER_G)%>%
  drop_na(PREG_ID_2445,BARN_NR)%>%mutate(ID_2445=paste(PREG_ID_2445,BARN_NR,sep="_"))%>%
  select(-BARN_NR,-PREG_ID_2445)
length(unique(FODEREG$ID_2445))
length(unique(q_child$ID_2445))
sum(is.na(q_child$preg_id))
sum(is.na(q_child$BARN_NR))

#N:\durable\phenotypes\mbrn\PDB2445_MFR_541_v12

#merge all dataframes
q_child<-left_join(q_child, FODEREG,by="ID_2445")#
sum(is.na(npr_child$ID_2445))
sum(is.na(npr_child$ChildNumber))
q_child<-left_join(q_child,npr_child,by="ID_2445")
length(unique(q_child$preg_id))# 111879
length(unique(q_nrb$preg_id))#111879

###UPDATE TO LATEST SAMPLE
SV<- read_sav("N:/durable/phenotypes/npr/2023-08-08/PDB2445_kobling_2023_05_26/PDB2445_SV_INFO_V12_20230526.sav")%>%select(PREG_ID_2445)
SV$preg_id<-as.character(SV$PREG_ID_2445)#
length(unique(SV$preg_id))#112042
sum(is.na(SV$preg_id))

pravesh_keep<- read.csv("Y:/parekh/2023-08-14_parseNPR/old/2023-08-24-MoBa-KeepList.csv")
length(unique(pravesh_keep$PREG_ID_2445))#112042

q_child_new<-inner_join(SV,q_child,by="preg_id")#113 530


library(psych)
#fix variables
q_child_new$mor_income<-q_child_new$AA1315_raw
q_child_new$far_income<-q_child_new$AA1316_raw

q_child_new$mor_edu1<-q_child_new$AA1124_raw
q_child_new$mor_edu2<-q_child_new$AA1128_raw
q_child_new$far_edu1<-q_child_new$AA1126_raw
q_child_new$far_edu2<-q_child_new$AA1130_raw

q_child_new$paritet_kat<-ifelse(q_child_new$PARITET_5_raw > 0, 1,q_child_new$PARITET_5_raw)
q_child_new$paritet_kat<-as.character(q_child_new$paritet_kat)
table(q_child_new$MORS_ALDER_G)
  #917 og 945 = categories outside age intervall 17-45 => exclude these from calculation as we do not know their age 
str(q_child_new$MORS_ALDER_G)
q_child_new$mor_alder_num<-as.numeric(q_child_new$MORS_ALDER_G)
q_child_new$mor_alder_num <- replace(q_child_new$mor_alder_num, q_child_new$mor_alder_num == 917, NA)
q_child_new$mor_alder_num <- replace(q_child_new$mor_alder_num, q_child_new$mor_alder_num == 945, NA)
describe(q_child_new$mor_alder_num)

q_child_new$parent_edu<-(q_child_new$mor_edu1 + q_child_new$far_edu1)/2#snitt nivå
table(q_child_new$parent_edu)

q_child_new$edu_mor_kat<-ifelse(q_child_new$mor_edu1>=5,"yes","no")
q_child_new$edu_far_kat<-ifelse(q_child_new$far_edu1>=5,"yes","no")
q_child_new$hig_edu_mor_or_far<-q_child_new$edu_mor_kat=="yes"|q_child_new$edu_far_kat=="yes"
table(q_child_new$mor_edu1)
table(q_child_new$edu_mor_kat)
table(q_child_new$far_edu1)
table(q_child_new$edu_far_kat)
table(q_child_new$hig_edu_mor_or_far)

q_child_new$age_end<-2022-q_child_new$birth_yr
q_child_new$KJONN<-as.character(q_child_new$KJONN)
q_child_new$FODELAND_KAT_NOR_GBD_G<-as.character(q_child_new$FODELAND_KAT_NOR_GBD_G)

q_child_new$anyemo_diag[is.na(q_child_new$anyemo_diag)] <- FALSE
q_child_new$dep_diag[is.na(q_child_new$dep_diag)] <- FALSE
q_child_new$anx_diag[is.na(q_child_new$anx_diag)] <- FALSE
q_child_new$bd_diag[is.na(q_child_new$bd_diag)] <- FALSE

q_child_new$anyemo_adl[is.na(q_child_new$anyemo_adl)] <- FALSE
q_child_new$dep_adl[is.na(q_child_new$dep_adl)] <- FALSE
q_child_new$anx_adl[is.na(q_child_new$anx_adl)] <- FALSE
q_child_new$bd_adl[is.na(q_child_new$bd_adl)] <- FALSE

q_child_new$anyemo_child[is.na(q_child_new$anyemo_child)] <- FALSE
q_child_new$dep_child[is.na(q_child_new$dep_child)] <- FALSE
q_child_new$anx_child[is.na(q_child_new$anx_child)] <- FALSE
q_child_new$bd_child[is.na(q_child_new$bd_child)] <- FALSE

q_child_new$adhd_diag[is.na(q_child_new$adhd_diag)] <- FALSE
q_child_new$ocd_diag[is.na(q_child_new$ocd_diag)] <- FALSE
q_child_new$cd_diag[is.na(q_child_new$cd_diag)] <- FALSE


###read inn genetic data
library(haven)
covar <- read.delim("N:/durable/genotype/MoBaPsychGen_v1/MoBaPsychGen_v1-ec-eur-batch-basic-qc-cov.txt")
covar_c<-covar%>%subset(Role=="Child")#
length(unique(covar_c$ID_2445))#
nrow(covar_c)#
centrix_barn<-inner_join(covar_c,q_child_new, by="ID_2445")
length(unique(centrix_barn$ID_2445))#
centrix_barn<-distinct(centrix_barn,ID_2445,.keep_all=TRUE)
length(unique(centrix_barn$ID_2445))
nrow(centrix_barn)#lengt unique ===nrow

####read inn PGS calculations and make dataframe with all of them
#neurot
neurot_prs_ldpred <-  read.csv("N:/durable/users/espehage/norarba-proj/FLA2_PRS/src/results/ldpred2_NEURN18/nhipic_neurot_c_8yr.auto", sep="")
variable.names(neurot_prs_ldpred)
neurot_prs_ldpred<-neurot_prs_ldpred%>%select("FID","IID","score")
neurot_prs_df<-inner_join(neurot_prs_ldpred,centrix_barn,by="IID")%>%select("score","ID_2445","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10",
                                                                            "genotyping_batch")
neurot_prs_df$neurot_prs_sd<-scale(neurot_prs_df$score)
neurot_prs_df<-distinct(neurot_prs_df,ID_2445,.keep_all=TRUE)
variable.names(neurot_prs_df)
length(unique(neurot_prs_df$ID_2445))#
sum(is.na(neurot_prs_df$ID_2445))
describe(neurot_prs_df$score)
describe(neurot_prs_df$neurot_prs_sd)

neurot_prs_df[,c("adjpgs_neur")] <- apply(
  neurot_prs_df[,c("neurot_prs_sd")], 2, 
  function(x){
    rstandard(
      lm(x ~ 1+neurot_prs_df$PC1+neurot_prs_df$PC2+neurot_prs_df$PC3+
           neurot_prs_df$PC4+neurot_prs_df$PC5+neurot_prs_df$PC6+
           neurot_prs_df$PC7+neurot_prs_df$PC8+neurot_prs_df$PC9+
           neurot_prs_df$PC10+neurot_prs_df$genotyping_batch, 
         na.action=na.exclude)) 
  })
describe(neurot_prs_df$neurot_prs_sd)
describe(neurot_prs_df$adjpgs_neur)
neurot_prs_df<-neurot_prs_df%>%select("ID_2445","adjpgs_neur","neurot_prs_sd")
#dep
dep_prs_ldpred <-  read.csv("N:/durable/users/espehage/norarba-proj/FLA2_PRS/src/results/ldpred2_DEPH19_no23andMe/dep.auto", sep="")
variable.names(dep_prs_ldpred)
dep_prs_ldpred<-dep_prs_ldpred%>%select("FID","IID","score")
dep_prs_df<-inner_join(dep_prs_ldpred,centrix_barn,by="IID")%>%select("score","ID_2445","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10",
                                                                      "genotyping_batch")
dep_prs_df$dep_prs_sd<-scale(dep_prs_df$score)
dep_prs_df<-distinct(dep_prs_df,ID_2445,.keep_all=TRUE)
variable.names(dep_prs_df)
dep_prs_df[,c("adjpgs_dep")] <- apply(dep_prs_df[,c("dep_prs_sd")],
                                      2, 
                                      function(x){
                                        rstandard(
                                          lm(
                                            x ~ 1+dep_prs_df$PC1+dep_prs_df$PC2+dep_prs_df$PC3+dep_prs_df$PC4+dep_prs_df$PC5+dep_prs_df$PC6+dep_prs_df$PC7+dep_prs_df$PC8+dep_prs_df$PC9+dep_prs_df$PC10+dep_prs_df$genotyping_batch,
                                            na.action=na.exclude
                                          )
                                        )
                                      })
describe(dep_prs_df$dep_prs_sd)
describe(dep_prs_df$adjpgs_dep)
dep_prs_df<-dep_prs_df%>%select("ID_2445","adjpgs_dep","dep_prs_sd")

#anx

anx_prs_ldpred <-  read.csv("N:/durable/users/espehage/norarba-proj/FLA2_PRS/src/results/ldpred2_ANXP19/anx.auto", sep="")
variable.names(anx_prs_ldpred)
anx_prs_ldpred<-anx_prs_ldpred%>%select("FID","IID","score")
anx_prs_df<-inner_join(anx_prs_ldpred,centrix_barn,by="IID")%>%select("score","ID_2445","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10",
                                                                      "genotyping_batch")
anx_prs_df$anx_prs_sd<-scale(anx_prs_df$score)
anx_prs_df<-distinct(anx_prs_df,ID_2445,.keep_all=TRUE)
variable.names(anx_prs_df)
anx_prs_df[,c("adjpgs_anx")] <- apply(anx_prs_df[,c("anx_prs_sd")],
                                      2, 
                                      function(x){
                                        rstandard(
                                          lm(
                                            x ~ 1+anx_prs_df$PC1+anx_prs_df$PC2+anx_prs_df$PC3+anx_prs_df$PC4+anx_prs_df$PC5+anx_prs_df$PC6+anx_prs_df$PC7+anx_prs_df$PC8+anx_prs_df$PC9+anx_prs_df$PC10+anx_prs_df$genotyping_batch,
                                            na.action=na.exclude
                                          )
                                        )
                                      })
describe(anx_prs_df$anx_prs_sd)
describe(anx_prs_df$adjpgs_anx)
anx_prs_df<-anx_prs_df%>%select("ID_2445","adjpgs_anx","anx_prs_sd")

#BD
bd_prs_ldpred <-  read.csv("N:/durable/users/espehage/norarba-proj/FLA2_PRS/src/results/ldpred2_BDM20/bd.auto", sep="")
variable.names(bd_prs_ldpred)
bd_prs_ldpred<-bd_prs_ldpred%>%select("FID","IID","score")
bd_prs_df<-inner_join(bd_prs_ldpred,centrix_barn,by="IID")%>%select("score","ID_2445","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10",
                                                                    "genotyping_batch")
bd_prs_df$bd_prs_sd<-scale(bd_prs_df$score)
bd_prs_df<-distinct(bd_prs_df,ID_2445,.keep_all=TRUE)
variable.names(bd_prs_df)

bd_prs_df[,c("adjpgs_bd")] <- apply(bd_prs_df[,c("bd_prs_sd")],
                                    2, 
                                    function(x){
                                      rstandard(
                                        lm(
                                          x ~ 1+bd_prs_df$PC1+bd_prs_df$PC2+bd_prs_df$PC3+bd_prs_df$PC4+bd_prs_df$PC5+bd_prs_df$PC6+bd_prs_df$PC7+bd_prs_df$PC8+bd_prs_df$PC9+bd_prs_df$PC10+bd_prs_df$genotyping_batch,
                                          na.action=na.exclude
                                        )
                                      )
                                    })
describe(bd_prs_df$bd_prs_sd)
describe(bd_prs_df$adjpgs_bd)
bd_prs_df<-bd_prs_df%>%select("ID_2445","adjpgs_bd","bd_prs_sd")

##combine df
list_df = list(dep_prs_df,anx_prs_df,neurot_prs_df,bd_prs_df)
dat1 <- list_df %>% reduce(inner_join, by="ID_2445")#
variable.names(dat1)
dat2<-distinct(dat1,ID_2445,.keep_all=TRUE)# 

sum(is.na(anx_prs_df$ID_2445))
sum(is.na(dep_prs_df$ID_2445))
sum(is.na(bd_prs_df$ID_2445))
sum(is.na(neurot_prs_df$ID_2445))
sum(is.na(centrix_barn$ID_2445))

sum(is.na(dep_prs_ldpred$ID_2445))
sum(is.na(anx_prs_ldpred$ID_2445))
sum(is.na(neurot_prs_ldpred$ID_2445))
sum(is.na(bd_prs_ldpred$ID_2445))

sum(is.na(q_child_new$ID_2445))


length(unique(q_child_new$id))#
length(unique(anx_prs_df$ID_2445))#
length(unique(dep_prs_df$ID_2445))#
length(unique(bd_prs_df$ID_2445))#
length(unique(neurot_prs_df$ID_2445))#
length(unique(centrix_barn$ID_2445))#
length(unique(dat2$id))
length(unique(dat2$preg_id))#
length(unique(dat2$m_id))#
sum(is.na(dat2$BARN_NR))
sum(is.na(dat2$preg_id))
table(dat2$BARN_NR)

#save df
alldat<-inner_join(q_child_new,dat2,by="ID_2445")
variable.names(alldat)
describe(alldat$cbcl_ext_c_18m)

alldat<-alldat%>%rename(sex=KJONN)
alldat<-alldat%>%dplyr::mutate(by_cat = case_when(birth_yr < quantile(alldat$birth_yr, c(0.33,0.66))[[1]] ~ 0,
                                                  birth_yr > quantile(alldat$birth_yr, c(0.33,0.66))[[2]] ~ 2, TRUE ~1))
alldat$by_cat<-as.character(alldat$by_cat)
#
length(unique(alldat$preg_id))
length(unique(alldat$PREG_ID_2445))
length(unique(alldat$m_id))
length(unique(alldat$f_id))

#save short version of df
alldat_short<-alldat%>%select(sex,FAAR,birth_yr,by_cat,age_end,ageOfFirstDiag_anyemo,hig_edu_mor_or_far,mor_alder_num,paritet_kat,FODELAND_KAT_NOR_GBD_G,
                    MORS_ALDER_G,FARS_ALDER_G,
                    anyemo_diag,dep_diag,anx_diag,bd_diag,adhd_diag,ocd_diag,cd_diag,
                    anyemo_adl,dep_adl,anx_adl,bd_adl,
                    anyemo_child,dep_child,anx_child,bd_child,
                    ageOfFirstDiag_dep,ageOfFirstDiag_anx,ageOfFirstDiag_bd,
                    adhd_diag,ocd_diag,cd_diag,
                    neurot_prs_sd,dep_prs_sd,anx_prs_sd,bd_prs_sd,
                    adjpgs_dep,adjpgs_anx,adjpgs_neur,adjpgs_bd,
                    cbcl_int_c_18m,cbcl_int_c_3yr, cbcl_int_c_5yr,
                    cbcl_ext_c_18m,cbcl_ext_c_3yr, cbcl_ext_c_5yr,
                    smfq_dep_c_8yr,scared_anx_c_8yr,
                    rsdbd_ina_c_8yr,rsdbd_hyp_c_8yr,rsdbd_cd_c_8yr,rsdbd_odd_c_8yr,
                    nhipic_neurot_c_8yr,ID_2445,preg_id,m_id,f_id,BARN_NR)

write.csv(alldat_short,"N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv",row.names=F)

#check characteristics
str(alldat$sex)#char
str(alldat$by_cat)#char
str(alldat$cbcl_ext_c_18m)#num

str(alldat_short$sex)
str(alldat_short$by_cat)
str(alldat_short$cbcl_ext_c_18m)

#all correct 

####CORRECT AGE OF FIRST DIAGNOSIS
library(readr)
dat<-read_csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
dat$sex<-as.character(dat$sex)
dat$by_cat<-as.character(dat$by_cat)
str(dat$adjpgs_neur)

source("./scripts/getDiagInfo_p.R")

id<-dat$ID_2445

diag<-c("F30*","F31*","F32*","F33*","F34*","F38*","F39*","F40*","F41*","F92*","F93*")

by<-dat$FAAR

results<-getDiagInfo(listID2445 = id,listDiagCodes = diag, wildCard = T, yearOfBirth = by)

library(tidyverse)

genetic<- read.csv("N:/durable/users/norarba/FLA2_rew/df_alldat_short.csv")
total<- read.csv("N:/durable/users/norarba/FLA2_rew/df_upp_data_non_gen_tot.csv")
excluded<-anti_join(total,genetic,by="ID_2445")
genetic$KJONN<-genetic$sex

source("./scripts/getDiagInfo_p.R")

##gen

id_gen<-genetic$ID_2445

diag<-c("F30*","F31*","F32*","F33*","F34*","F38*","F39*","F40*","F41*","F92*","F93*")

by_gen<-genetic$FAAR

results_gen<-getDiagInfo(listID2445 = id_gen,listDiagCodes = diag, wildCard = T, yearOfBirth = by_gen)

#tot

id_tot<-total$ID_2445

diag<-c("F30*","F31*","F32*","F33*","F34*","F38*","F39*","F40*","F41*","F92*","F93*")

by_tot<-total$FAAR

results_tot<-getDiagInfo(listID2445 = id_tot,listDiagCodes = diag, wildCard = T, yearOfBirth = by_tot)

#excluded
id_ex<-excluded$ID_2445

diag<-c("F30*","F31*","F32*","F33*","F34*","F38*","F39*","F40*","F41*","F92*","F93*")

by_ex<-excluded$FAAR

results_ex<-getDiagInfo(listID2445 = id_ex,listDiagCodes = diag, wildCard = T, yearOfBirth = by_ex)

###
gen_age<-results_gen[["ageFirstDiag"]]%>%select(ID2445,minAge)
mean(gen_age$minAge,na.rm=T)

table(gen_age$minAge)#
gen_age<-gen_age%>%rename("minAgeEmo"="minAge","ID_2445"="ID2445")
genetic<-left_join(genetic,gen_age,by="ID_2445")
mean(genetic$minAgeEmo,na.rm=T)

write.csv(genetic,"N:/durable/users/norarba/FLA2_rew/df_sample_short.csv",row.names=F)

tot_age<-results_tot[["ageFirstDiag"]]%>%select(ID2445,minAge)
mean(tot_age$minAge,na.rm=T)

table(tot_age$minAge)#
tot_age<-tot_age%>%rename("minAgeEmo"="minAge","ID_2445"="ID2445")
total<-left_join(total,tot_age,by="ID_2445")
mean(total$minAgeEmo,na.rm=T)

write.csv(total,"N:/durable/users/norarba/FLA2_rew/df_totalMoBa.csv",row.names=F)

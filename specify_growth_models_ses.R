#adapted from: https://github.com/psychgen/scz-prs-psychopathol-dev/blob/master/scripts/01.1_specify_growth_models.R

#  For each scale set, at each threshold:
#  1. Run a basic LGM
#  2. Run LGM with PGS on residuals
#  3. Run LGM with PGS on growth factors
#  4. Run LGM with PGS on intercept only
#  5. Run LGM with PGS effects at zero (but retaining PGS in the model)
#  6. Compare all fits and extract PGS path estimates
#  All models have sex, birth year (by_cat) and parental education (edu_mf) as a covariate


# Basic  LGM

basic <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3

#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3
#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1
#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + sex + by_cat + edu_mf
ytime2 ~ 0*1 + sex + by_cat + edu_mf
ytime3 ~ 0*1 + sex + by_cat + edu_mf
#Growth parameter intercepts (freely estimated)
i1 ~ 1 
s1 ~ 1 
'

# Basic  LGM + pgs on residuals (direct effect growth curve model)

pgs_residuals <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3
#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3
#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1
#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + adj_pgs + sex + by_cat + edu_mf
ytime2 ~ 0*1 + adj_pgs + sex + by_cat + edu_mf
ytime3 ~ 0*1 + adj_pgs + sex + by_cat + edu_mf
#Growth parameter intercepts (freely estimated)
i1 ~ 1 
s1 ~ 1 
'

# Basic  LGM + pgs on growth factors

pgs_gf <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3
#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3
#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1
#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + sex + by_cat + edu_mf
ytime2 ~ 0*1 + sex + by_cat + edu_mf
ytime3 ~ 0*1 + sex + by_cat + edu_mf
#Growth parameter intercepts (freely estimated)
i1 ~ 1 + adj_pgs 
s1 ~ 1 + adj_pgs
'

# Basic  LGM + pgs on intercept only 

pgs_intercept <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3
#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3
#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1
#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + sex + by_cat + edu_mf
ytime2 ~ 0*1 + sex + by_cat + edu_mf
ytime3 ~ 0*1 + sex + by_cat + edu_mf
#Growth parameter intercepts (freely estimated)
i1 ~ 1 + adj_pgs
s1 ~ 1 + 0*adj_pgs
'

# Basic  LGM + pgs on slope only 

pgs_slope <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3
#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3
#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1
#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + sex + by_cat + edu_mf
ytime2 ~ 0*1 + sex + by_cat + edu_mf
ytime3 ~ 0*1 + sex + by_cat + edu_mf
#Growth parameter intercepts (freely estimated)
i1 ~ 1 + 0*adj_pgs
s1 ~ 1 + adj_pgs
'

# Basic  LGM + no pgs effects


lgm_nopgs <-  
  '
#Growth parameters (latent variables)
i1 =~ 1*ytime1 + 1*ytime2 + 1*ytime3 
s1 =~ 0*ytime1 + 1.5*ytime2 + 3.5*ytime3
#Obs variable variances
ytime1 ~~ ytime1
ytime2 ~~ ytime2
ytime3 ~~ ytime3
#Growth parameter (co)variances
i1 ~~ i1
s1 ~~ s1
i1 ~~ s1
#Obs variable intercepts (fixed to 0)
ytime1 ~ 0*1 + sex + by_cat + edu_mf
ytime2 ~ 0*1 + sex + by_cat + edu_mf
ytime3 ~ 0*1 + sex + by_cat + edu_mf
#Growth parameter intercepts (freely estimated)
i1 ~ 1 + 0*adj_pgs
s1 ~ 1 + 0*adj_pgs
'


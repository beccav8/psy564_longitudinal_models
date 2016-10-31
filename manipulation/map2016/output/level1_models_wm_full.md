



This report was automatically generated with the R package **knitr**
(version 1.13).

# The purpose of this script is to create a data object (dto) which will hold all data and metadata.# Run the lines below to stitch a basic html output.knitr::stitch_rmd(
  script="./manipulation/map2016/Level1_models_full_workingmem.R",
  output="./manipulation/map2016/output/level1_models_wm_full.md"
)## 
## 
## processing file: Level1_models_full_workingmem.Rmd
## Error in parse_block(g[-1], g[1], params.src): duplicate label 'load-source'
# The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.cat("\f") # clear console# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-pathlibrary(magrittr) # enables piping : %>%library(lmerTest)library(outliers)# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.source("./scripts/common-functions.R") # used in multiple reportssource("./scripts/graph-presets.R")source("./scripts/general-graphs.R")  #in scripts foldersource("./scripts/specific-graphs.R")source("./scripts/specific-graphs-pred.R")source("./scripts/graphs-pred.R")source("./scripts/graphs-predVID.R")source("./scripts/functions-for-glm-models.R")source("./scripts/multiplot-function.R")# source("./scripts/graph-presets.R") # fonts, colors, themes# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-pathrequireNamespace("ggplot2") # graphing# requireNamespace("readr") # data inputrequireNamespace("tidyr") # data manipulationrequireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).requireNamespace("testit")# For asserting conditions meet expected patterns.requireNamespace("nlme") # estimate mixed models | esp. gls()requireNamespace("lme4") # estimate mixed models | esp. lmer()requireNamespace("arm")  # process model objectsgetwd()## [1] "C:/Users/Rebecca/Documents/GitHub/psy564_longitudinal_models"

path_input0  <- "./data/unshared/derived/map2016/map_full_bio_centered.rds" 
ds0  <- readRDS(path_input0) #total raw data  names(ds0)##  [1] "id"                 "year_in_study"      "dementia"          
##  [4] "age_bl"             "age_at_visit"       "edu"               
##  [7] "msex"               "race"               "apoe"              
## [10] "episodic"           "percep_speed"       "semantic"          
## [13] "wm"                 "global"             "dig_b"             
## [16] "dig_f"              "mmse"               "nle"               
## [19] "pss"                "physical_activity"  "al_count_BL"       
## [22] "al_count_wave"      "al_catg_BL"         "al_catg_wave"      
## [25] "pss_bp_meanc"       "pss_wp"             "social_isolation"  
## [28] "phys_bp_mean"       "phys_bp_median"     "phys_wp"           
## [31] "age_at_visit_meanc" "age_at_visit65"


#yi= B0 + eirange(ds0$wm, na.rm=TRUE)  #-3.57 to 2.34## [1] -3.568466  2.694933
range(ds0$pss_bp_meanc, na.rm=TRUE)## [1] -2.024697  1.725303
hist(ds0$wm) #relatively normal distfigure/Fully unconditional Level 1 model/ UCM-1.pdfeq_0 <- as.formula("wm ~ 1 +            
                   (1  |id)")model_0<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) lmerTest::summary((model_0))## summary from lme4 is returned
## some computational error has occurred in lmerTest
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: wm ~ 1 + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##  20997.3  21019.3 -10495.7  20991.3    11251 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.3576 -0.4920  0.0290  0.5469  4.0470 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.5480   0.7403  
##  Residual             0.2534   0.5034  
## Number of obs: 11254, groups:  id, 1843
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept) -0.12210    0.01829  -6.674
fit0<-model_0

# yi= B0j + B1j(time) + eij#B0j = gamma00 +  U0j #int#B1j = gamma10 +  ---  #slope#Time variable, Fixed Effects A-------------------------------#year in studyeq_1a <- as.formula("wm ~ 1 + year_in_study +          
                    ( 1 |id)")model_1a<- lmerTest::lmer(eq_1a, data=ds0, REML= FALSE) lmerTest::summary((model_1a))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: wm ~ 1 + year_in_study + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##  19929.1  19958.4  -9960.6  19921.1    11250 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.2506 -0.5123  0.0307  0.5589  4.0701 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.5847   0.7647  
##  Residual             0.2241   0.4734  
## Number of obs: 11254, groups:  id, 1843
## 
## Fixed effects:
##                 Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)    2.245e-02  1.922e-02  1.998e+03   1.168    0.243    
## year_in_study -5.203e-02  1.537e-03  9.892e+03 -33.847   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## year_n_stdy -0.224
fit1a<-model_1a( 0.2726 - 0.2293 ) /  0.2726  #15.88 % improved from Fully UCM deviance = 4034.5## [1] 0.1588408
#Time variable, Fixed Effects B-----------------------------#age at visit, mean centered eq_1b <- as.formula("wm ~ 1 + age_at_visit_meanc +          
                    (1  |id)")model_1b<- lmerTest::lmer(eq_1b, data=ds0, REML= FALSE) lmerTest::summary((model_1b))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: wm ~ 1 + age_at_visit_meanc + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##  19996.1  20025.4  -9994.0  19988.1    11250 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.2833 -0.5067  0.0305  0.5646  4.1232 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.5892   0.7676  
##  Residual             0.2254   0.4748  
## Number of obs: 11254, groups:  id, 1843
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)        -1.453e-01  1.881e-02  1.725e+03  -7.725 1.87e-14 ***
## age_at_visit_meanc -4.342e-02  1.326e-03  1.028e+04 -32.749  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## ag_t_vst_mn 0.036
fit1b <-model_1b( 0.2726 - 0.2297 ) /  0.2726  #15.73% improved from fully UCM, deviance =4029.8## [1] 0.1573734
#ICC.498 / (.498 + .2297) #= 68% of the variance is due to between person differences ## [1] 0.6843479
#(i.e. person average differences from the grand mean)

# yi= B0j + B1j(time) + eij#B0j = gamma00  + U0j #int#B1j = gamma10  + U1j #slope#age_centeredeq_2 <- as.formula("wm ~ 1 + age_at_visit_meanc +          
                   ( 1 + age_at_visit_meanc |id)")model_2<- lmerTest::lmer(eq_2, data=ds0, REML= FALSE) lmerTest::summary((model_2))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: wm ~ 1 + age_at_visit_meanc + (1 + age_at_visit_meanc | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##  19226.7  19270.7  -9607.3  19214.7    11248 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.9305 -0.5258  0.0159  0.5581  3.9241 
## 
## Random effects:
##  Groups   Name               Variance Std.Dev. Corr
##  id       (Intercept)        0.550532 0.74198      
##           age_at_visit_meanc 0.003821 0.06182  0.16
##  Residual                    0.184578 0.42963      
## Number of obs: 11254, groups:  id, 1843
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)        -8.241e-02  1.955e-02  1.622e+03  -4.216 2.62e-05 ***
## age_at_visit_meanc -4.554e-02  2.192e-03  1.116e+03 -20.771  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## ag_t_vst_mn 0.119
fit2<-model_2( 0.2726 - 0.177382 ) /  0.2726  #= 34.9 % improved  # deviance = 3731.9  ## [1] 0.3492957
#F.E versus F.E and R.E of time4029.8 - 3731.9 #= 297.9 #df= 5-4 = 1, SIG ## [1] 297.9

#WMtest <- as.formula("wm ~ 1 +            
                   (1  |id)")model_test<- lmerTest::lmer(test, data=ds0, REML= FALSE)lmerTest::summary((model_test))## summary from lme4 is returned
## some computational error has occurred in lmerTest
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: wm ~ 1 + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##  20997.3  21019.3 -10495.7  20991.3    11251 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.3576 -0.4920  0.0290  0.5469  4.0470 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.5480   0.7403  
##  Residual             0.2534   0.5034  
## Number of obs: 11254, groups:  id, 1843
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept) -0.12210    0.01829  -6.674
0.4719 / (0.4719 + 0.2726 ) # 63 % of the varience is explained at the between person level (average differences)## [1] 0.6338482
#PAtest <- as.formula("physical_activity ~ 1 +            
                   (1  |id)")model_test<- lmerTest::lmer(test, data=ds0, REML= FALSE)lmerTest::summary((model_test))## summary from lme4 is returned
## some computational error has occurred in lmerTest
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: physical_activity ~ 1 + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##  56360.2  56382.2 -28177.1  56354.2    11230 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.7893 -0.4711 -0.1748  0.3329 11.1622 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 5.627    2.372   
##  Residual             6.741    2.596   
## Number of obs: 11233, groups:  id, 1848
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  2.92178    0.06286   46.48
4.727/ (4.727+ 6.578) # 41% of the varience is exaplined  ## [1] 0.4181336
#Stresstest <- as.formula("pss ~ 1 +            
                   (1  |id)")model_test<- lmerTest::lmer(test, data=ds0, REML= FALSE)lmerTest::summary((model_test))## summary from lme4 is returned
## some computational error has occurred in lmerTest
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: pss ~ 1 + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   4636.2   4654.5  -2315.1   4630.2     3324 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.2420 -0.4038  0.0213  0.5138  4.3520 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.07515  0.2741  
##  Residual             0.18618  0.4315  
## Number of obs: 3327, groups:  id, 1006
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   2.0213     0.0121     167


# yi= B0j + B1j(time) + B2j(PA) + eij#B0j = gamma00  + U0j #int#B1j = gamma10  + U1j #slope age#B2j = gamma10  +     #slope PA#6 parameters #person mean centered i.e. fluctuation (i.e. at times when people exercise more than usual)eq_3 <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_wp +
                   ( 1 + age_at_visit_meanc  |id)")model_3<- lmerTest::lmer(eq_3, data=ds0, REML= FALSE)lmerTest::summary((model_3))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## wm ~ 1 + age_at_visit_meanc + phys_wp + (1 + age_at_visit_meanc |      id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##  18507.4  18558.6  -9246.7  18493.4    11057 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -7.0135 -0.5240  0.0122  0.5598  3.7963 
## 
## Random effects:
##  Groups   Name               Variance Std.Dev. Corr
##  id       (Intercept)        0.537180 0.73293      
##           age_at_visit_meanc 0.003033 0.05507  0.16
##  Residual                    0.179650 0.42385      
## Number of obs: 11064, groups:  id, 1840
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)        -8.400e-02  1.918e-02  1.626e+03  -4.381 1.26e-05 ***
## age_at_visit_meanc -4.170e-02  2.060e-03  1.062e+03 -20.238  < 2e-16 ***
## phys_wp             4.767e-03  1.795e-03  9.085e+03   2.655  0.00795 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__
## ag_t_vst_mn  0.117       
## phys_wp     -0.004  0.074
fit3<-model_3#grand mean centered (more exercise than average)eq_3a <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_bp_mean +
                    ( 1 + age_at_visit_meanc  |id)")model_3a<- lmerTest::lmer(eq_3a, data=ds0, REML= FALSE)lmerTest::summary((model_3a))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## wm ~ 1 + age_at_visit_meanc + phys_bp_mean + (1 + age_at_visit_meanc |  
##     id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##  18507.3  18558.5  -9246.7  18493.3    11057 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -7.0121 -0.5243  0.0114  0.5597  3.7965 
## 
## Random effects:
##  Groups   Name               Variance Std.Dev. Corr
##  id       (Intercept)        0.536571 0.73251      
##           age_at_visit_meanc 0.003035 0.05509  0.16
##  Residual                    0.179664 0.42387      
## Number of obs: 11064, groups:  id, 1840
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)        -8.368e-02  1.917e-02  1.625e+03  -4.366 1.34e-05 ***
## age_at_visit_meanc -4.162e-02  2.063e-03  1.067e+03 -20.172  < 2e-16 ***
## phys_bp_mean        4.651e-03  1.744e-03  1.004e+04   2.667  0.00767 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__
## ag_t_vst_mn 0.116        
## phys_bp_men 0.001  0.088
fit3<-model_3a#barley changed the model
#person mean centered eq_4 <- as.formula("wm ~ 1 + age_at_visit_meanc + pss_wp +
                   ( 1 + age_at_visit_meanc  |id)")model_4<- lmerTest::lmer(eq_4, data=ds0, REML= FALSE)lmerTest::summary((model_4))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: wm ~ 1 + age_at_visit_meanc + pss_wp + (1 + age_at_visit_meanc |  
##     id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   5470.9   5513.7  -2728.5   5456.9     3295 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8258 -0.5073 -0.0004  0.5185  3.8713 
## 
## Random effects:
##  Groups   Name               Variance  Std.Dev. Corr 
##  id       (Intercept)        0.4216304 0.64933       
##           age_at_visit_meanc 0.0006666 0.02582  -0.07
##  Residual                    0.1558366 0.39476       
## Number of obs: 3302, groups:  id, 1002
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)         1.768e-02  2.297e-02  9.437e+02   0.770    0.442    
## age_at_visit_meanc -2.002e-02  2.509e-03  4.911e+02  -7.977 1.07e-14 ***
## pss_wp             -7.749e-03  1.953e-02  2.247e+03  -0.397    0.692    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__
## ag_t_vst_mn -0.140       
## pss_wp      -0.006  0.034
#grand mean centeredeq_4a <- as.formula("wm ~ 1 + age_at_visit_meanc + pss_bp_meanc +
                    ( 1 + age_at_visit_meanc  |id)")model_4a<- lmerTest::lmer(eq_4a, data=ds0, REML= FALSE)lmerTest::summary((model_4a))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## wm ~ 1 + age_at_visit_meanc + pss_bp_meanc + (1 + age_at_visit_meanc |  
##     id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   5470.9   5513.6  -2728.5   5456.9     3295 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8240 -0.5069 -0.0029  0.5188  3.8816 
## 
## Random effects:
##  Groups   Name               Variance  Std.Dev. Corr 
##  id       (Intercept)        0.4214116 0.64916       
##           age_at_visit_meanc 0.0006574 0.02564  -0.07
##  Residual                    0.1559653 0.39492       
## Number of obs: 3302, groups:  id, 1002
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)         1.756e-02  2.296e-02  9.421e+02   0.765    0.445    
## age_at_visit_meanc -1.992e-02  2.508e-03  4.914e+02  -7.943 1.35e-14 ***
## pss_bp_meanc        8.084e-03  1.842e-02  2.759e+03   0.439    0.661    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__
## ag_t_vst_mn -0.141       
## pss_bp_menc -0.004  0.050
#raweq_4b <- as.formula("wm ~ 1 + age_at_visit_meanc + pss +
                    ( 1 + age_at_visit_meanc  |id)")model_4b<- lmerTest::lmer(eq_4b, data=ds0, REML= FALSE)lmerTest::summary((model_4b))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: wm ~ 1 + age_at_visit_meanc + pss + (1 + age_at_visit_meanc |  
##     id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   5470.9   5513.6  -2728.5   5456.9     3295 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8240 -0.5069 -0.0029  0.5188  3.8816 
## 
## Random effects:
##  Groups   Name               Variance  Std.Dev. Corr 
##  id       (Intercept)        0.4214116 0.64916       
##           age_at_visit_meanc 0.0006574 0.02564  -0.07
##  Residual                    0.1559653 0.39492       
## Number of obs: 3302, groups:  id, 1002
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)         1.195e-03  4.387e-02  3.232e+03   0.027    0.978    
## age_at_visit_meanc -1.992e-02  2.508e-03  4.910e+02  -7.943 1.35e-14 ***
## pss                 8.084e-03  1.842e-02  2.759e+03   0.439    0.661    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__
## ag_t_vst_mn -0.116       
## pss         -0.852  0.050

eq_5 <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_wp + pss_wp +
                   ( 1 + age_at_visit_meanc  |id)")model_5<- lmerTest::lmer(eq_5, data=ds0, REML= FALSE)lmerTest::summary((model_5))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## wm ~ 1 + age_at_visit_meanc + phys_wp + pss_wp + (1 + age_at_visit_meanc |  
##     id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   5437.5   5486.3  -2710.8   5421.5     3283 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8416 -0.5078 -0.0025  0.5308  3.8535 
## 
## Random effects:
##  Groups   Name               Variance  Std.Dev. Corr 
##  id       (Intercept)        0.4241002 0.65123       
##           age_at_visit_meanc 0.0005859 0.02421  -0.07
##  Residual                    0.1551070 0.39384       
## Number of obs: 3291, groups:  id, 1001
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)         1.795e-02  2.297e-02  9.429e+02   0.782    0.435    
## age_at_visit_meanc -1.919e-02  2.492e-03  5.017e+02  -7.702 7.19e-14 ***
## phys_wp             4.841e-03  3.238e-03  2.485e+03   1.495    0.135    
## pss_wp             -1.113e-02  1.954e-02  2.245e+03  -0.570    0.569    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__ phys_w
## ag_t_vst_mn -0.140              
## phys_wp      0.004  0.085       
## pss_wp      -0.006  0.031 -0.013
fit4<-model_5eq_6 <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_wp + pss_wp + phys_wp*pss_wp
                   ( 1 + age_at_visit_meanc  |id)")model_6<- lmerTest::lmer(eq_6, data=ds0, REML= FALSE)## Error: No random effects terms specified in formula
lmerTest::summary((model_6))## Error in lmerTest::summary((model_6)): object 'model_6' not found
fit4<-model_6## Error in eval(expr, envir, enclos): object 'model_6' not found


The R session information (including the OS info, R version and all
packages used):

sessionInfo()## R version 3.3.1 (2016-06-21)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=English_Canada.1252  LC_CTYPE=English_Canada.1252   
## [3] LC_MONETARY=English_Canada.1252 LC_NUMERIC=C                   
## [5] LC_TIME=English_Canada.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggplot2_2.1.0   outliers_0.14   lmerTest_2.0-32 lme4_1.1-12    
## [5] Matrix_1.2-6    magrittr_1.5   
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.5         formatR_1.4         nloptr_1.0.4       
##  [4] RColorBrewer_1.1-2  plyr_1.8.4          tools_3.3.1        
##  [7] rpart_4.1-10        evaluate_0.9        tibble_1.1         
## [10] gtable_0.2.0        nlme_3.1-128        lattice_0.20-33    
## [13] DBI_0.4-1           coda_0.18-1         gridExtra_2.2.1    
## [16] dplyr_0.5.0         stringr_1.0.0       knitr_1.13         
## [19] cluster_2.0.4       grid_3.3.1          nnet_7.3-12        
## [22] data.table_1.9.6    R6_2.1.2            survival_2.39-4    
## [25] arm_1.8-6           foreign_0.8-66      latticeExtra_0.6-28
## [28] minqa_1.2.4         Formula_1.2-1       tidyr_0.6.0        
## [31] Hmisc_3.17-4        scales_0.4.0        MASS_7.3-45        
## [34] splines_3.3.1       abind_1.4-3         testit_0.5         
## [37] assertthat_0.1      dichromat_2.0-0     colorspace_1.2-6   
## [40] stringi_1.1.1       acepack_1.3-3.3     munsell_0.4.3      
## [43] markdown_0.7.7      chron_2.3-47
Sys.time()## [1] "2016-10-31 11:03:05 PDT"



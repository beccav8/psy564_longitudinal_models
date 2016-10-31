



This report was automatically generated with the R package **knitr**
(version 1.13).

# The purpose of this script is to create a data object (dto) which will hold all data and metadata.# Run the lines below to stitch a basic html output.knitr::stitch_rmd(
  script="./manipulation/map2016/Level1_models_sample_workingmem.R",
  output="./manipulation/map2016/output/level1_models_wm_sample.md"
)## 
## 
## processing file: Level1_models_sample_workingmem.Rmd
## Error in parse_block(g[-1], g[1], params.src): duplicate label 'load-source'
# The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!
# Clear memory from previous runsbase::rm(list=base::ls(all=TRUE))cat("\f")# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-pathlibrary(magrittr) # enables piping : %>%library(lmerTest)library(outliers)# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.source("./scripts/common-functions.R") # used in multiple reportssource("./scripts/graph-presets.R")source("./scripts/general-graphs.R")  #in scripts foldersource("./scripts/specific-graphs.R")source("./scripts/specific-graphs-pred.R")source("./scripts/graphs-pred.R")source("./scripts/graphs-predVID.R")source("./scripts/functions-for-glm-models.R")source("./scripts/multiplot-function.R")# source("./scripts/graph-presets.R") # fonts, colors, themes# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-pathrequireNamespace("ggplot2") # graphing# requireNamespace("readr") # data inputrequireNamespace("tidyr") # data manipulationrequireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).requireNamespace("testit")# For asserting conditions meet expected patterns.requireNamespace("nlme") # estimate mixed models | esp. gls()requireNamespace("lme4") # estimate mixed models | esp. lmer()requireNamespace("arm")  # process model objectsgetwd()## [1] "C:/Users/Rebecca/Documents/GitHub/psy564_longitudinal_models"

path_input0  <- "./data/unshared/derived/map2016/map_sample_bio_centered.rds" 
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


#yi= B0 + eirange(ds0$wm, na.rm=TRUE)  #-3.57 to 2.34## [1] -3.568466  2.348452
range(ds0$pss_bp_meanc, na.rm=TRUE)## [1] -2.024697  1.475303
hist(ds0$wm) #relatively normal distfigure/Fully unconditional Level 1 model/ UCM-1.pdfeq_0 <- as.formula("wm ~ 1 +            
                   (1  |id)")model_0<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) lmerTest::summary((model_0))## summary from lme4 is returned
## some computational error has occurred in lmerTest
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: wm ~ 1 + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   4387.2   4404.5  -2190.6   4381.2     2378 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.8554 -0.4796  0.0577  0.5596  3.2471 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.4719   0.6869  
##  Residual             0.2726   0.5221  
## Number of obs: 2381, groups:  id, 269
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept) -0.005032   0.043699  -0.115
fit0<-model_0#yi= B0 + ei#int/B0 = -0.005, residual (e) =  0.27#deviance=  4381.2#AIC= 4387.2

# yi= B0j + B1j(time) + eij#B0j = gamma00 + gamme01 + U0j #int#B1j = gamma10 + gamme11  ---  #slope#Time variable, Fixed Effects A-------------------------------#year in studyeq_1a <- as.formula("wm ~ 1 + year_in_study +          
                    ( 1 |id)")model_1a<- lmerTest::lmer(eq_1a, data=ds0, REML= FALSE) lmerTest::summary((model_1a))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: wm ~ 1 + year_in_study + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   4042.5   4065.6  -2017.3   4034.5     2377 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5499 -0.5077  0.0667  0.5758  2.9940 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.5146   0.7173  
##  Residual             0.2293   0.4789  
## Number of obs: 2381, groups:  id, 269
## 
## Fixed effects:
##                 Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)    2.271e-01  4.678e-02  3.038e+02   4.856 1.92e-06 ***
## year_in_study -5.643e-02  2.892e-03  2.176e+03 -19.511  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## year_n_stdy -0.256
fit1a<-model_1a#deviance = 4034.5#int= 2.271e-01  (i.e. mean when year=0), slope(yrs_in_study)= -5.643e-02 (unit decrease per year)# % improved from fully UCM = UCMresid_var - model_resid_var / UCMresid_var(0.2726  - 0.2293 ) / 0.2726  ## [1] 0.1588408
#= 16 %#####SIGMA IS A WITHIN PERSON RANDOM EFFECT, THUS THIS IS HOW MUCH WITHIN PERSON##### VARIENCE TIME ACCOUNTS FOR#Time variable, Fixed Effects B-----------------------------#age at visit, mean centered eq_1b <- as.formula("wm ~ 1 + age_at_visit_meanc +          
                    (1  |id)")model_1b<- lmerTest::lmer(eq_1b, data=ds0, REML= FALSE) lmerTest::summary((model_1b))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: wm ~ 1 + age_at_visit_meanc + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   4037.8   4060.9  -2014.9   4029.8     2377 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.5902 -0.5090  0.0684  0.5776  3.0121 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.4982   0.7058  
##  Residual             0.2297   0.4792  
## Number of obs: 2381, groups:  id, 269
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)        -1.454e-02  4.455e-02  2.568e+02  -0.326    0.744    
## age_at_visit_meanc -5.208e-02  2.656e-03  2.312e+03 -19.605   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## ag_t_vst_mn 0.010
fit1b <-model_1b#deviance = 4029.8#AIC= 4037.8#int= -1.454e-02  (i.e. mean when year=0), slope(yrs_in_study)= -5.208e-02 (unit decrease per increase in unit age)(0.2726 - 0.2297 ) / 0.2726 #= 15.7 % improved ## [1] 0.1573734


# yi= B0j + B1j(time) + eij#B0j = gamma00 + gamme01 + U0j #int#B1j = gamma10 + gamme11 + U1j #slope#age_centeredeq_2 <- as.formula("wm ~ 1 + age_at_visit_meanc +          
                   ( 1 + age_at_visit_meanc |id)")model_2<- lmerTest::lmer(eq_2, data=ds0, REML= FALSE) lmerTest::summary((model_2))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: wm ~ 1 + age_at_visit_meanc + (1 + age_at_visit_meanc | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   3743.9   3778.5  -1865.9   3731.9     2375 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.8324 -0.5360  0.0346  0.5714  3.8695 
## 
## Random effects:
##  Groups   Name               Variance Std.Dev. Corr
##  id       (Intercept)        0.434802 0.65940      
##           age_at_visit_meanc 0.003775 0.06144  0.12
##  Residual                    0.177382 0.42117      
## Number of obs: 2381, groups:  id, 269
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)          0.093502   0.044177 249.610000   2.117   0.0353 *  
## age_at_visit_meanc  -0.051684   0.004868 224.940000 -10.618   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr)
## ag_t_vst_mn 0.078
fit2<-model_2#deviance = 3731.9 #reduced with R.E of time#AIC= 3743.9#int=  0.093502 (i.e. mean when year=0), slope(yrs_in_study)= -0.051684 (unit decrease per year)#residual var =  0.177382# chisq= deviance (int) - deviance(int& slope)#df= dif in number of parameters = df4029.8 - 3731.9## [1] 297.9
#297..9, df= 6-5 = 1 #significant
#WMtest <- as.formula("wm ~ 1 +            
                   (1  |id)")model_test<- lmerTest::lmer(test, data=ds0, REML= FALSE)lmerTest::summary((model_test))## summary from lme4 is returned
## some computational error has occurred in lmerTest
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: wm ~ 1 + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   4387.2   4404.5  -2190.6   4381.2     2378 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.8554 -0.4796  0.0577  0.5596  3.2471 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.4719   0.6869  
##  Residual             0.2726   0.5221  
## Number of obs: 2381, groups:  id, 269
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept) -0.005032   0.043699  -0.115
0.4719/ (0.4719  +0.2726)## [1] 0.6338482
#63% of the variance in Working Memory is BP person#PAtest <- as.formula("physical_activity ~ 1 +            
                   (1  |id)")model_test<- lmerTest::lmer(test, data=ds0, REML= FALSE)lmerTest::summary((model_test))## summary from lme4 is returned
## some computational error has occurred in lmerTest
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: physical_activity ~ 1 + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##  11662.3  11679.6  -5828.1  11656.3     2358 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.4904 -0.5375 -0.1652  0.3749  7.9980 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 4.727    2.174   
##  Residual             6.578    2.565   
## Number of obs: 2361, groups:  id, 269
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   2.9788     0.1456   20.46
#ICC = 42 % between person variance4.727 / (4.727 + 6.578)## [1] 0.4181336
#Stresstest <- as.formula("pss ~ 1 +            
                   (1  |id)")model_test<- lmerTest::lmer(test, data=ds0, REML= FALSE)lmerTest::summary((model_test))## summary from lme4 is returned
## some computational error has occurred in lmerTest
## Linear mixed model fit by maximum likelihood  ['lmerMod']
## Formula: pss ~ 1 + (1 | id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   1131.2   1145.2   -562.6   1125.2      772 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.0575 -0.3649  0.0602  0.5324  2.7419 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  id       (Intercept) 0.06978  0.2642  
##  Residual             0.20536  0.4532  
## Number of obs: 775, groups:  id, 188
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  2.02501    0.02653   76.33
0.06978/ (0.06978 + 0.20536)## [1] 0.2536163
#25% of the varience in stress is BP

# yi= B0j + B1j(time) + B2j(PA) + eij#B0j = gamma00 + gamme01 + U0j #int#B1j = gamma10 + gamme11 + U1j #slope age#B2j = gamma10 + gamme11 +    #slope PA#person mean centered i.e. fluctuation (i.e. at times when people exercise more than usual)eq_3 <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_wp +
                   ( 1 + age_at_visit_meanc  |id)")model_3<- lmerTest::lmer(eq_3, data=ds0, REML= FALSE)lmerTest::summary((model_3))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## wm ~ 1 + age_at_visit_meanc + phys_wp + (1 + age_at_visit_meanc |      id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   3572.2   3612.5  -1779.1   3558.2     2327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.9912 -0.5357  0.0380  0.5793  3.9235 
## 
## Random effects:
##  Groups   Name               Variance Std.Dev. Corr
##  id       (Intercept)        0.435869 0.66020      
##           age_at_visit_meanc 0.003265 0.05714  0.14
##  Residual                    0.169630 0.41186      
## Number of obs: 2334, groups:  id, 269
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)         8.780e-02  4.403e-02  2.492e+02   1.994   0.0472 *  
## age_at_visit_meanc -4.757e-02  4.644e-03  2.215e+02 -10.245   <2e-16 ***
## phys_wp             3.924e-03  3.714e-03  2.009e+03   1.056   0.2909    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__
## ag_t_vst_mn  0.091       
## phys_wp     -0.010  0.064
fit3<-model_3# deviance = 3558.2#int= 8.780e-02#slope (age) = -4.757e-02#slope (phys_wp) = 3.924e-03#is does PA sig improve the model?3731.9 - 3558.2## [1] 173.7
173.7 #df= 8-6 = 2   #sig better fit than just F.E&R.E of time, when F.E phys_BP is added## [1] 173.7
#grand mean centered (more exercise than average)eq_3a <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_bp_mean +
                   ( 1 + age_at_visit_meanc  |id)")model_3a<- lmerTest::lmer(eq_3a, data=ds0, REML= FALSE)lmerTest::summary((model_3a))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## wm ~ 1 + age_at_visit_meanc + phys_bp_mean + (1 + age_at_visit_meanc |  
##     id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   3572.2   3612.5  -1779.1   3558.2     2327 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.9875 -0.5359  0.0385  0.5784  3.9228 
## 
## Random effects:
##  Groups   Name               Variance Std.Dev. Corr
##  id       (Intercept)        0.435417 0.65986      
##           age_at_visit_meanc 0.003264 0.05714  0.13
##  Residual                    0.169635 0.41187      
## Number of obs: 2334, groups:  id, 269
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)         8.773e-02  4.401e-02  2.493e+02   1.993   0.0473 *  
## age_at_visit_meanc -4.751e-02  4.648e-03  2.222e+02 -10.223   <2e-16 ***
## phys_bp_mean        3.957e-03  3.648e-03  2.134e+03   1.084   0.2783    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__
## ag_t_vst_mn  0.088       
## phys_bp_men -0.012  0.076
fit3<-model_3a3731.9 - 3558.2## [1] 173.7
173.7 #df= 8-6 = 2   #sig better fit than just F.E&R.E of time, when F.E phys_BP is added## [1] 173.7
#barley changed the model
#person mean centered eq_4 <- as.formula("wm ~ 1 + age_at_visit_meanc + pss_wp +
                   ( 1 + age_at_visit_meanc  |id)")model_4<- lmerTest::lmer(eq_4, data=ds0, REML= FALSE)lmerTest::summary((model_4))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: wm ~ 1 + age_at_visit_meanc + pss_wp + (1 + age_at_visit_meanc |  
##     id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   1201.0   1233.5   -593.5   1187.0      761 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5662 -0.5293  0.0425  0.5472  3.5218 
## 
## Random effects:
##  Groups   Name               Variance  Std.Dev. Corr
##  id       (Intercept)        0.3837520 0.61948      
##           age_at_visit_meanc 0.0005657 0.02378  0.25
##  Residual                    0.1533394 0.39159      
## Number of obs: 768, groups:  id, 188
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)          0.130654   0.049689 179.300000   2.629   0.0093 ** 
## age_at_visit_meanc  -0.026112   0.005154 108.500000  -5.066 1.68e-06 ***
## pss_wp              -0.012833   0.037124 575.600000  -0.346   0.7297    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__
## ag_t_vst_mn -0.040       
## pss_wp      -0.006  0.026
#deviance = 1187.0, reduces the varience, even more than PA#grand mean centeredeq_4a <- as.formula("wm ~ 1 + age_at_visit_meanc + pss_bp_meanc +
                   ( 1 + age_at_visit_meanc  |id)")model_4a<- lmerTest::lmer(eq_4a, data=ds0, REML= FALSE)lmerTest::summary((model_4a))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## wm ~ 1 + age_at_visit_meanc + pss_bp_meanc + (1 + age_at_visit_meanc |  
##     id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   1201.1   1233.6   -593.5   1187.1      761 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5691 -0.5236  0.0415  0.5443  3.5080 
## 
## Random effects:
##  Groups   Name               Variance  Std.Dev. Corr
##  id       (Intercept)        0.3832494 0.61907      
##           age_at_visit_meanc 0.0005551 0.02356  0.25
##  Residual                    0.1535229 0.39182      
## Number of obs: 768, groups:  id, 188
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)          0.130408   0.049649 178.700000   2.627  0.00937 ** 
## age_at_visit_meanc  -0.026029   0.005146 108.400000  -5.059 1.73e-06 ***
## pss_bp_meanc         0.006395   0.035647 665.200000   0.179  0.85769    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__
## ag_t_vst_mn -0.041       
## pss_bp_menc  0.001  0.016
#deviance= 1187 #raweq_4b <- as.formula("wm ~ 1 + age_at_visit_meanc + pss +
                   ( 1 + age_at_visit_meanc  |id)")model_4b<- lmerTest::lmer(eq_4b, data=ds0, REML= FALSE)lmerTest::summary((model_4b))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: wm ~ 1 + age_at_visit_meanc + pss + (1 + age_at_visit_meanc |  
##     id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   1201.1   1233.6   -593.5   1187.1      761 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5691 -0.5236  0.0415  0.5443  3.5080 
## 
## Random effects:
##  Groups   Name               Variance  Std.Dev. Corr
##  id       (Intercept)        0.3832492 0.61907      
##           age_at_visit_meanc 0.0005551 0.02356  0.25
##  Residual                    0.1535229 0.39182      
## Number of obs: 768, groups:  id, 188
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)          0.117461   0.087547 684.500000   1.342    0.180    
## age_at_visit_meanc  -0.026029   0.005146 108.400000  -5.059 1.73e-06 ***
## pss                  0.006395   0.035647 665.200000   0.179    0.858    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__
## ag_t_vst_mn -0.036       
## pss         -0.824  0.016
#deviance= 1187 3731.9- 1187 #=2544.9 # how is pss reducing deviance this much if it isnt sig ? ## [1] 2544.9

eq_5 <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_wp + pss_wp +
                   ( 1 + age_at_visit_meanc  |id)")model_5<- lmerTest::lmer(eq_5, data=ds0, REML= FALSE)lmerTest::summary((model_5))## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## wm ~ 1 + age_at_visit_meanc + phys_wp + pss_wp + (1 + age_at_visit_meanc |  
##     id)
##    Data: ds0
## 
##      AIC      BIC   logLik deviance df.resid 
##   1200.2   1237.3   -592.1   1184.2      758 
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -3.565 -0.529  0.034  0.548  3.535 
## 
## Random effects:
##  Groups   Name               Variance  Std.Dev. Corr
##  id       (Intercept)        0.3838877 0.61959      
##           age_at_visit_meanc 0.0005362 0.02316  0.25
##  Residual                    0.1535645 0.39187      
## Number of obs: 766, groups:  id, 188
## 
## Fixed effects:
##                      Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)          0.130409   0.049661 179.500000   2.626  0.00939 ** 
## age_at_visit_meanc  -0.025457   0.005154 109.000000  -4.940 2.85e-06 ***
## phys_wp              0.004767   0.005947 603.200000   0.802  0.42315    
## pss_wp              -0.013344   0.037242 575.100000  -0.358  0.72024    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ag_t__ phys_w
## ag_t_vst_mn -0.040              
## phys_wp      0.001  0.087       
## pss_wp      -0.007  0.027  0.034
fit4<-model_5# deviance = 1184.2#int= 0.130409#slope (age) = -0.025457#slope (phys_wp) = 0.004767 #slope (pss_bp)  = -0.013344  

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
## [10] nlme_3.1-128        gtable_0.2.0        lattice_0.20-33    
## [13] DBI_0.4-1           coda_0.18-1         gridExtra_2.2.1    
## [16] stringr_1.0.0       knitr_1.13          dplyr_0.5.0        
## [19] cluster_2.0.4       grid_3.3.1          nnet_7.3-12        
## [22] data.table_1.9.6    R6_2.1.2            survival_2.39-4    
## [25] arm_1.8-6           foreign_0.8-66      latticeExtra_0.6-28
## [28] minqa_1.2.4         Formula_1.2-1       tidyr_0.6.0        
## [31] Hmisc_3.17-4        scales_0.4.0        MASS_7.3-45        
## [34] splines_3.3.1       abind_1.4-3         assertthat_0.1     
## [37] dichromat_2.0-0     testit_0.5          colorspace_1.2-6   
## [40] stringi_1.1.1       acepack_1.3-3.3     munsell_0.4.3      
## [43] markdown_0.7.7      chron_2.3-47
Sys.time()## [1] "2016-10-31 11:41:16 PDT"



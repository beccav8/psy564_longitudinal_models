
# Clear memory from previous runs
base::rm(list=base::ls(all=TRUE))
cat("\f")

# @knitr load-packages --------------------

# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(lmerTest)
library(outliers)

# @knitr load-sources --------------------

# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R")
source("./scripts/general-graphs.R")  #in scripts folder
source("./scripts/specific-graphs.R")
source("./scripts/specific-graphs-pred.R")
source("./scripts/graphs-pred.R")
source("./scripts/graphs-predVID.R")
source("./scripts/functions-for-glm-models.R")
source("./scripts/multiplot-function.R")
# source("./scripts/graph-presets.R") # fonts, colors, themes

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
# requireNamespace("readr") # data input
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("testit")# For asserting conditions meet expected patterns.
requireNamespace("nlme") # estimate mixed models | esp. gls()
requireNamespace("lme4") # estimate mixed models | esp. lmer()
requireNamespace("arm")  # process model objects
getwd()

# @knitr declare-globals ---------------------------------------------------------
# @knitr declare-globals ---------------------------------------------------------

path_input0  <- "./data/unshared/derived/map2016/data_sample.rds" 

# @knitr load-data ---------------------------------------------------------------
ds0  <- readRDS(path_input0) #total raw data  

names(ds0)

#Fully unconditional Level 1 model/ UCM -------------------------
#yi= B0 + ei

eq_0 <- as.formula("mmse ~ 1 +            
                   (1  |id)")

model_0<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_0))
fit0<-model_0

#yi= B0 + ei
#int/B0 =26.75, residual (e) = 13.829

#deviance= 13618.0
#logLik = --6809.0
#AIC= 13624.0


#TIME-VARIABLES-FIXED EFFECTS ONLY ----------------------------------------------------------------------
# yi= B0j + B1j(time) + eij
#B0j = gamma00 + gamme01 + U0j #int
#B1j = gamma10 + gamme11  ---  #slope


#Time variable, Fixed Effects A-------------------------------

##CONDITIONAL ON AGE
 ##LEVEL 1 WITHIN PERSON
## CHANGE IN Y FOR A GIVEN INDIVIDUAL ON A GIVEN MEASUREMENT OCCASION AS A FUNCTION OF THAT PERSONS PERFORMANCE FOR AGE=0 (INT)
##PLUS A SLOPE, I.E RATE OF CHANGE PER ADDITIONAL YEAR OF AGE, PLUS ERROR (EPSILON(ij) -> W/I REDISUAL VARIENCE REMAINING TO BE EXPLAINED 
##EPSILON(IJ) IS IN THE RANDOM EFFECTS

 ##LEVEL 2 BETWEEN PERSON (I.E. INTERCEPTS AND SLOPES AS OUTCOMES, EITHER ALLOWING SLOPES TO VARY OR NOT)
    #I.E. MODELING INDIVIDUAL DIFFERENCES IN E.G. INTERCEPT (OR SLOPE) AS A FUNCTION OF THE POPULATION AVERAGE INTERCEPT (Y00) (OR SLOPE Y10) FOR TIME=0
    # U0i- TELLS US IF THERE ARE INDIVIDUAL DIFFERENCES IN STARTING POINTS REMANING TO BE EXPLAINED AFTER 
    #(NON ZERO AND SIG MEANS YES, THERE IS AND WE SHOULD ADD ADDITIONAL PREDICTORS)
    #U1i IF RANDOM EFFECTS

#year in study
eq_1a <- as.formula("mmse ~ 1 + year_in_study +          
                    ( 1 |id)")
model_1a<- lmerTest::lmer(eq_1a, data=ds0, REML= FALSE) 
lmerTest::summary((model_1a))
fit1a<-model_1a
#deviance = 13151.4
#logLik = -6575.7
#AIC=13159.4
#int= 28.6713  (i.e. mean when year=0), slope(yrs_in_study)= -4.5 (unit decrease per year)


# % improved from fully UCM = UCMresid_var - model_resid_var / UCMresid_var
( 13.829 - 11.05 ) / 13.829 
#= 20 %
#####SIGMA IS A WITHIN PERSON RANDOM EFFECT, THUS THIS IS HOW MUCH WITHIN PERSON
##### VARIENCE DOES TIME ACCOUNT FOR

#wald test (is the slope sig sif. than 0?) = estimate of FE/SE (and look at distribution)
# anova(fit0,fit1a) #fit1a fits sig better
#cant use this?

#Time variable, Fixed Effects B-----------------------------

#age at visit, mean centered 
eq_1b <- as.formula("mmse ~ 1 + age_at_visit_meanc +          
                    (1  |id)")
model_1b<- lmerTest::lmer(eq_1b, data=ds0, REML= FALSE) 
lmerTest::summary((model_1b))
fit1b <-model_1b
#deviance = 13137.0
#logLik = -6568.5
#AIC= 13145.0
#int= 26.72  (i.e. mean when year=0), slope(yrs_in_study)= -0.38110 (unit decrease per increase in unit age)

anova(fit1a,fit1b) #sig?? why 

#Time variable, Fixed Effects C-----------------------------

#age at visit, centered at 65 
eq_1c <- as.formula("mmse ~ 1 + age_at_visit65 +          
                    (1  |id)")
model_1c<- lmerTest::lmer(eq_1c, data=ds0, REML= FALSE) 
lmerTest::summary((model_1c))
fit1c<-model_1c

anova(fit1b, fit1c)#NS


#FIXED-AND-RANDOM-EFFECTS-----------------------------------------------------------

# yi= B0j + B1j(time) + eij
#B0j = gamma00 + gamme01 + U0j #int
#B1j = gamma10 + gamme11 + U1j #slope


#age_centered
eq_2 <- as.formula("mmse ~ 1 + age_at_visit_meanc +          
                   ( 1 + age_at_visit_meanc |id)")
model_2<- lmerTest::lmer(eq_2, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))
fit2<-model_2
#deviance = 11995.9
#logLik = -5997.9
#AIC= 12007.9
#int= 27.97665 (i.e. mean when year=0), slope(yrs_in_study)= -0.40539 (unit decrease per year)
#residual var =  5.3849

#year in study
eq_3 <- as.formula("mmse ~ 1 + year_in_study +          
                   ( 1 + year_in_study |id)")
model_3<- lmerTest::lmer(eq_3, data=ds0, REML= FALSE) 
lmerTest::summary((model_3))
fit3<-model_3

#deviance =  11839.6 
#logLik = -5919.8
#AIC= 11851.6 
#int= 28.78428 (i.e. mean when year=0), slope(yrs_in_study)= -0.52155 (unit decrease per year)
#residual var= 5.1564 

names(ds0)
# # % improved from F.E only  = UCMresid_var - model_resid_var / UCMresid_var
#  ( 11.119  - 5.1564 ) / 11.119
#  #= 53 %
# 
# #wald test (is the slope sig sif. than 0?) = estimate of FE/SE (and look at distribution)
# anova(fit1a,fit2) #fit2 fits sig better, lower AAIC and logLik
# #can compre models that only differ in the random effects this way, but a walk test is needed
# #if fixed effects differ
# 
# 
# ##--adding-PA
# 
# eq_3 <- as.formula("mmse ~ 1 + year_in_study + phys_wp +       
#                    ( 1 + year_in_study  |id)")
# model_3<- lmerTest::lmer(eq_3, data=ds0, REML= FALSE) 
# lmerTest::summary((model_3))
# fit3<-model_3
# #how do i do a walk test to comapre this to model 2?
# #ie. added a fixed effect
# 
# 
# 
# ##--adding-PA
# 
# eq_4 <- as.formula("mmse ~ 1 + year_in_study + phys_wp +       
#                    ( 1 + year_in_study + phys_wp |id)")
# model_4<- lmerTest::lmer(eq_4, data=ds0, REML= FALSE) 
# lmerTest::summary((model_4))
# fit4<-model_4
# 
# anova(fit3,fit4)
# # Df       AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)    
# #fit3  7 54718 54769 -27352    54704                             
# #fit4 10 54681 54754 -27330    54661 43.766      3  1.692e-09 ***
# 
# 
# 
# 
# #level2 assignment ?
# #including average Pss 
# eq_4 <- as.formula("mmse ~ 1 + year_in_study + phys_wp + pss +      
#                    ( 1 + year_in_study + phys_wp |id)")
# model_4<- lmerTest::lmer(eq_4, data=ds0, REML= FALSE) 
# lmerTest::summary((model_4))
# fit4<-model_4
# 

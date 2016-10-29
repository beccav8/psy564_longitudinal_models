
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
#logLik = -6809.0
#AIC= 13624.0


#TIME-VARIABLES-FIXED EFFECTS ONLY ----------------------------------------------------------------------
# yi= B0j + B1j(time) + eij
#B0j = gamma00 + gamme01 + U0j #int
#B1j = gamma10 + gamme11  ---  #slope


#Time variable, Fixed Effects A-------------------------------

#year in study
eq_1a <- as.formula("mmse ~ 1 + year_in_study +          
                    ( 1 |id)")
model_1a<- lmerTest::lmer(eq_1a, data=ds0, REML= FALSE) 
lmerTest::summary((model_1a))
fit1a<-model_1a
#deviance = 13151.4
      #should be at least twice as lage as the difference in the number of extra parameters
      # difference in deviance between NESTED models is a chi-sq distribued
      # with df being equal to the difference in # est parameters between the two models
      # if sig, the one with the smalled deviance is better
#logLik = -6575.7
      # -2LL how much WORSE this model is from the best possible (Saturated) model
      # closer to 0 is best
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
( 13.829 - 11.119 ) / 13.829 #= 19.6 % improved 


#Time variable, Fixed Effects C-----------------------------
range(ds0$age_at_visit, na.rm=TRUE)
hist(ds0$age_at_visit)
age_at_visit75<- (ds0$age_at_visit) - (75)
#age at visit, centered at 65 
eq_1c <- as.formula("mmse ~ 1 + age_at_visit75 +          
                    (1  |id)")
model_1c<- lmerTest::lmer(eq_1c, data=ds0, REML= FALSE) 
lmerTest::summary((model_1c))
fit1c<-model_1c

#deviance = 13137.0
#logLik = -6568.5
#AIC= 13145.0
#int= 29.83518  (i.e. mean when year=0), slope(yrs_in_study)= -0.38110 (unit decrease per increase in unit age)
( 13.829 - 11.119 ) / 13.829 #= 19.6 % improved 


# the deviance of the model with age mean centered, and age centered at 75 was the lowest
#because these models have the same number of parameters being estimated, I will chose to use 
#age mean centered as my time variable 

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

# chisq= deviance (int) - deviance(int& slope)
#df= dif in number of parameters = df

13137.0 - 11995.9
#1141, df= 6-5 = 1 #for sure significant 



test <- as.formula("physical_activity ~ 1 +            
                   (1  |id)")
model_test<- lmerTest::lmer(test, data=ds0, REML= FALSE)
lmerTest::summary((model_test))

#ICC = 42 % between person varience 
4.727 / (4.727 + 6.578)


# ##--adding-PA

# yi= B0j + B1j(time) + B2j(PA) + eij
#B0j = gamma00 + gamme01 + U0j #int
#B1j = gamma10 + gamme11 + U1j #slope age
#B2j = gamma10 + gamme11 +    #slope PA
#person mean centered i.e. fluctuation (i.e. at times when people exercise more than usual)
eq_3 <- as.formula("mmse ~ 1 + age_at_visit_meanc + phys_wp +
                   ( 1 + age_at_visit_meanc  |id)")
model_3<- lmerTest::lmer(eq_3, data=ds0, REML= FALSE)
lmerTest::summary((model_3))
fit3<-model_3
# deviance = 11420.1
#int= 27.95
#slope (age) = -.362
#slope (phys_wp) = .047

#is does PA sig improve the model?
11995.9 - 11420.1
575.8 #df= 8-6 = 2   #sig better fit with F.E of within person PA


#grand mean centered (i.e do people who exercise more than the average person)
hist(ds0$phys_bp_mean)

eq_3a <- as.formula("mmse ~ 1 + age_at_visit_meanc + phys_bp_mean +
                   ( 1 + age_at_visit_meanc  |id)")
model_3a<- lmerTest::lmer(eq_3a, data=ds0, REML= FALSE)
lmerTest::summary((model_3a))
fit3<-model_3a
#deviance = 11419.8
#int= 27.954
#slope age = -.362
# slope PA bP = 0.046

## the same as WP centered?


hist(ds0$phys_bp_mean)
hist(ds0$phys_wp)

eq_3b <- as.formula("mmse ~ 1 + age_at_visit_meanc + phys_bp_mean + phys_wp +
                    ( 1 + age_at_visit_meanc  |id)")
model_3b<- lmerTest::lmer(eq_3b, data=ds0, REML= FALSE)
lmerTest::summary((model_3b))
fit3<-model_3b





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

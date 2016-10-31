# # The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# # Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/map2016/Level1_models_full_workingmem.R",
#   output="./manipulation/map2016/output/level1_models_wm_full.md"
# )
# # The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!
# 

# ----- load-source ------
  
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(lmerTest)
library(outliers)



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

# ----- specify-objects ------

path_input0  <- "./data/unshared/derived/map2016/map_full_bio_centered.rds" 

# ----- load-data ------
ds0  <- readRDS(path_input0) #total raw data  

names(ds0)

# ----- Fully-unconditional-model ------

##-------------------------------------Fully unconditional Level 1 model/ UCM -------------------------
#yi= B0 + ei

range(ds0$wm, na.rm=TRUE)  #-3.57 to 2.34
range(ds0$pss_bp_meanc, na.rm=TRUE)


hist(ds0$wm) #relatively normal dist

eq_0 <- as.formula("wm ~ 1 +            
                   (1  |id)")

model_0<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_0))
fit0<-model_0


# ----- Fixed-effects ------
#-----------------------TIME-VARIABLES-FIXED EFFECTS ONLY --------------------------------------------------------
# yi= B0j + B1j(time) + eij
#B0j = gamma00 +  U0j #int
#B1j = gamma10 +  ---  #slope

#Time variable, Fixed Effects A-------------------------------
#year in study
eq_1a <- as.formula("wm ~ 1 + year_in_study +          
                    ( 1 |id)")
model_1a<- lmerTest::lmer(eq_1a, data=ds0, REML= FALSE) 
lmerTest::summary((model_1a))
fit1a<-model_1a
( 0.2726 - 0.2293 ) /  0.2726  #15.88 % improved from Fully UCM deviance = 4034.5

#Time variable, Fixed Effects B-----------------------------
#age at visit, mean centered 
eq_1b <- as.formula("wm ~ 1 + age_at_visit_meanc +          
                    (1  |id)")
model_1b<- lmerTest::lmer(eq_1b, data=ds0, REML= FALSE) 
lmerTest::summary((model_1b))
fit1b <-model_1b
( 0.2726 - 0.2297 ) /  0.2726  #15.73% improved from fully UCM, deviance =4029.8
#ICC
.498 / (.498 + .2297) #= 68% of the variance is due to between person differences 
#(i.e. person average differences from the grand mean)


# ----- fixed-and-random-effects ------
##-----------------------FIXED-AND-RANDOM-EFFECTS-----------------------------------------------------------

# yi= B0j + B1j(time) + eij
#B0j = gamma00  + U0j #int
#B1j = gamma10  + U1j #slope


#age_centered
eq_2 <- as.formula("wm ~ 1 + age_at_visit_meanc +          
                   ( 1 + age_at_visit_meanc |id)")
model_2<- lmerTest::lmer(eq_2, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))
fit2<-model_2

( 0.2726 - 0.177382 ) /  0.2726  #= 34.9 % improved  # deviance = 3731.9  
#F.E versus F.E and R.E of time
4029.8 - 3731.9 #= 297.9 #df= 5-4 = 1, SIG 
# ----- ICC ------
#WM
test <- as.formula("wm ~ 1 +            
                   (1  |id)")
model_test<- lmerTest::lmer(test, data=ds0, REML= FALSE)
lmerTest::summary((model_test))
0.4719 / (0.4719 + 0.2726 ) # 63 % of the varience is explained at the between person level (average differences)

#PA
test <- as.formula("physical_activity ~ 1 +            
                   (1  |id)")
model_test<- lmerTest::lmer(test, data=ds0, REML= FALSE)
lmerTest::summary((model_test))
4.727/ (4.727+ 6.578) # 41% of the varience is exaplined  

#Stress
test <- as.formula("pss ~ 1 +            
                   (1  |id)")
model_test<- lmerTest::lmer(test, data=ds0, REML= FALSE)
lmerTest::summary((model_test))

# ---- physical-activity ----
###-------------------------adding-PA---------------------------------------------------------------------

# yi= B0j + B1j(time) + B2j(PA) + eij
#B0j = gamma00  + U0j #int
#B1j = gamma10  + U1j #slope age
#B2j = gamma10  +     #slope PA
#6 parameters 

#person mean centered i.e. fluctuation (i.e. at times when people exercise more than usual)
eq_3 <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_wp +
                   ( 1 + age_at_visit_meanc  |id)")
model_3<- lmerTest::lmer(eq_3, data=ds0, REML= FALSE)
lmerTest::summary((model_3))
fit3<-model_3


#grand mean centered (more exercise than average)
eq_3a <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_bp_mean +
                    ( 1 + age_at_visit_meanc  |id)")
model_3a<- lmerTest::lmer(eq_3a, data=ds0, REML= FALSE)
lmerTest::summary((model_3a))
fit3<-model_3a

#barley changed the model

# ---- stress ----
#person mean centered 
eq_4 <- as.formula("wm ~ 1 + age_at_visit_meanc + pss_wp +
                   ( 1 + age_at_visit_meanc  |id)")
model_4<- lmerTest::lmer(eq_4, data=ds0, REML= FALSE)
lmerTest::summary((model_4))


#grand mean centered
eq_4a <- as.formula("wm ~ 1 + age_at_visit_meanc + pss_bp_meanc +
                    ( 1 + age_at_visit_meanc  |id)")
model_4a<- lmerTest::lmer(eq_4a, data=ds0, REML= FALSE)
lmerTest::summary((model_4a))


#raw
eq_4b <- as.formula("wm ~ 1 + age_at_visit_meanc + pss +
                    ( 1 + age_at_visit_meanc  |id)")
model_4b<- lmerTest::lmer(eq_4b, data=ds0, REML= FALSE)
lmerTest::summary((model_4b))


# ---- PA-Stress ----


eq_5 <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_wp + pss_wp +
                   ( 1 + age_at_visit_meanc  |id)")
model_5<- lmerTest::lmer(eq_5, data=ds0, REML= FALSE)
lmerTest::summary((model_5))
fit4<-model_5

eq_6 <- as.formula("wm ~ 1 + age_at_visit_meanc + phys_wp + pss_wp + phys_wp*pss_wp
                   ( 1 + age_at_visit_meanc  |id)")
model_6<- lmerTest::lmer(eq_6, data=ds0, REML= FALSE)
lmerTest::summary((model_6))
fit4<-model_6





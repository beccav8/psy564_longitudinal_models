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
source("./scripts/map-specific-graphs.R")

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
# str(ds0)
#1853

# str(ds0)
#  
#  for(i in unique(ds0$id)) {
#   for (j in 1:length(ds0$dementia[ds0$id==i])) {
# 
#      if (isTRUE(ds0$year_in_study[ds0$id==i][j] == 0 ) & (isTRUE(ds0$dementia[ds0$id==i][j] == 1) ) ) {
# 
#      ds0$discard[ds0$id==i][j: length(ds0$dementia[ds0$id==i]) ] <- 2       }
#    } }
# length(ds0$id)
# summary(ds0$discard) #11298 NA's

# a <- ds0[which(ds0$discard==2),]

# str(ds0)


# ----- Fully-unconditional-model ------
#yi= B0 + ei
eq_0 <- as.formula("wm ~ 1 +            
                   (1  |id)")

model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))

#ICC
0.5480 / (0.5480 + 0.2534)
# 68% BP and 32 % WP (i.e makes sense, people are likely to differ from others more than themselves)

#SE = SD/ sqrt(n)
#int
0.5034 / sqrt(11254)
#resid
0.7403/ sqrt(11254)

1.96*sqrt(0.5480)
-0.12210 + (1.96*sqrt(0.5480))
-0.12210 - (1.96*sqrt(0.5480)) 
#CI -1.57 - 1.32

#residual chi square test or wald test to determine if there is significant variability in outcome
#HLM

set.seed(1)
ids <- sample(ds0$id,10)
d <- ds0 %>%  dplyr::filter( id %in% ids)
# names(d)

# raw_smooth_lines_map(
 
# ----- Fixed-effects-time ---------------------------------------

# yi= B0j + B1j(time) + eij
#B0j = gamma00 +  U0j #int
#B1j = gamma10 +  ---  #slope

###########year in study
eq_1a <- as.formula("wm ~ 1 + year_in_study +          
                    ( 1 |id)")
model_1<- lmerTest::lmer(eq_1a, data=ds0, REML= FALSE) 
lmerTest::summary((model_1))

# % improved from fully UCM = UCMresid_var - model_resid_var / UCMresid_var
(0.2534 - 0.2241) /  0.2534  #11.56 % improved from Fully UCM deviance = 4034.5

#chi sq
#df
11251 - 11250 
20991.3 - 19921.1

#SE = SD/ sqrt(n)
#int
0.7647   / sqrt(11254)
#resid
0.4734  / sqrt(11254)


# ----------------------------
# 
# eq_1c <- as.formula("wm ~ 1 + time_since_dx +
#                     ( 1 |id)")
# model_1c<- lmerTest::lmer(eq_1c, data=ds0, REML= FALSE)
# lmerTest::summary((model_1c))
# 
# (0.2534 - 0.3557) / .2534



########## age mean centered
eq_1b <- as.formula("wm ~ 1 + age_at_visit_meanc +          
                    (1  |id)")
model_1b<- lmerTest::lmer(eq_1b, data=ds0, REML= FALSE) 
lmerTest::summary((model_1b))
(0.2534 - 0.2254) /  0.2534  #11.04% improved from fully UCM, deviance =4029.8

#both are equivalent

#SE = SD/ sqrt(n)
#int
0.7676   / sqrt(11254)
#resid
0.4748  / sqrt(11254)




# ds0$age_wp<- ds0$age_at_visit - ds0$age_bl 
# 
# eq_1b <- as.formula("wm ~ 1 + age_bl + age_wp +          
#                     (1  |id)")
# model_1b<- lmerTest::lmer(eq_1b, data=ds0, REML= FALSE) 
# lmerTest::summary((model_1b))

#age_bl and change since baseline are sig, therfore, older individuals
#perform worse, and performance decreases over time



# ----- fixed-and-random-effects ------

# yi= B0j + B1j(time) + eij
#B0j = gamma00  + U0j #int
#B1j = gamma10  + U1j #slope

#df residual subrtaction
11248-11250 
#WM
eq_c <- as.formula("wm ~ 1 + year_in_study +          
                   ( 1 + year_in_study |id)")
model_2<- lmerTest::lmer(eq_c, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))

#ICC
(.527 + .0066) / (.527 + .0066 +  0.176456) # 75% is between person
#model 0
# lmerTest::summary((model_0)) #FUCM
# #ICC 
# 0.5480/ ( 0.5480+0.2534) #= 68.4%
# 
# #F.E versus F.E and R.E of time
19921.1 - 18865.4 #= 1055.7 
#df= 
11250 - 11248



#SE = SD/ sqrt(n)
#int
0.72637   / sqrt(11254)
#year in study
0.08104/ sqrt(11254)
#resid
0.42007  / sqrt(11254)



# 
# # SE= sd / sqrt(n), where n = # of people (not observations)
# 0.42007/ sqrt(1843)



#pss/phys _pmean = persons mean (TINVC) (BP)
#pss/phys _pmeanC = persons mean - grand mean (BP)

#pss/phys _gmc =person score at that occasion - the grand mean (WP) (Experimental)
#pss/phys _gmc = person score - their mean (WP)*

# add + pss_pmean and phys_mean for level 2 models (i.e. persons mean)
names(ds0)


# ---- physical-activity ----
###-------------------------adding-PA-FE---------------------------------------------------------------------

# yi= B0j + B1j(time) + B2j(PA) + eij
#B0j = gamma00  + U0j #int
#B1j = gamma10  + U1j #slope age
#B2j = gamma10  +     #slope PA
#6 parameters 

#person mean centered i.e. fluctuation (i.e. at times when people exercise more than usual)
eq_3 <- as.formula("wm ~ 1 + year_in_study  + phys_wp +
                   ( 1 + year_in_study  |id)")
model_3<- lmerTest::lmer(eq_3, data=ds0, REML= FALSE)
lmerTest::summary((model_3))   #df resid 11057
fit3<-model_3

11248 - 11057 #= 191

18865.4 - 18227.9  #=  637.5

#SE = SD/ sqrt(n)
#int
0.7253  / sqrt(11064)
#year in study
0.0728/ sqrt(11064)
#resid
0.4151  / sqrt(11064)



# eq_3 <- as.formula("wm ~ 1 + year_in_study  + phys_wp + year_in_study*phys_wp+
#                    ( 1 + year_in_study + phys_wp  |id)")
# model_3<- lmerTest::lmer(eq_3, data=ds0, REML= FALSE)
# lmerTest::summary((model_3))   #df resid 11057
# fit3<-model_3


eq_3a <- as.formula("wm ~ 1 + year_in_study  + phys_gmc +
                   ( 1 + year_in_study  |id)")
model_3a<- lmerTest::lmer(eq_3a, data=ds0, REML= FALSE)
lmerTest::summary((model_3a))   #df resid 11057

# eq_3b <- as.formula("wm ~ 1 + year_in_study  + age_at_visit65*phys_gmc + phys_gmc +
#                    ( 1 + year_in_study  |id)")
# model_3b<- lmerTest::lmer(eq_3b, data=ds0, REML= FALSE)
# lmerTest::summary((model_3b))   #df resid 11055
# 



# ---- stress ----

#person mean centered 
eq_4 <- as.formula("wm ~ 1 + year_in_study  + pss_wp +
                   ( 1 + year_in_study  |id)")
model_4<- lmerTest::lmer(eq_4, data=ds0, REML= FALSE)
lmerTest::summary((model_4))
# lmerTest::summary((model_2))

18865.4 - 5478.7 #=5768.3 
11248  - 3295 #=15 570

summary(ds0$pss_wp) #8346 missing values 
length(ds0$pss_wp)

8346/ 11673  #71% missing data 


#nle------------

summary(ds0$nle_gmc)
#8834 NA
range(ds0$nle, na.rm=TRUE)  

# 
# #### ----- level 2
# eq_1b <- as.formula("wm ~ 1 + age_bl + year_in_study + age_bl* year_in_study +        
#                     (1  |id)")
# model_1b<- lmerTest::lmer(eq_1b, data=ds0, REML= FALSE) 
# lmerTest::summary((model_1b))

#the interaction is sig, indicating that older indivudals decline faster


# $phys_pmeanC: the effect of mean PA on intercept 
                                        
et <- as.formula("wm ~ 1 + year_in_study*age_bl  + phys_pmeanC  + msex + phys_wp*age_bl +
                   ( 1 + year_in_study  |id)")
etmod<- lmerTest::lmer(et, data=ds0, REML= FALSE)
lmerTest::summary((etmod))   #df resid 11057






et <- as.formula("wm ~ 1 + year_in_study*age_bl  + pss_pmeanC  + msex + pss_wp*age_bl +
                 ( 1 + year_in_study  |id)")
etmod<- lmerTest::lmer(et, data=ds0, REML= FALSE)
lmerTest::summary((etmod))   #df resid 11057











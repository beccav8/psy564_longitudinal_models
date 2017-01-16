# # The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# # Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/map2016/Level1_models_full_workingmem.R",
#   output="./manipulation/map2016/output/level1_models_wm_full.md"
# )
# # The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!
# 
options(scipen=20)
# ----- load-source ------

rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(lmerTest)
library(outliers)
library(psych)



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
source("./scripts/graph_themes.R")
source("./scripts/multiplot-function.R")
source("./scripts/graph_themes.R")

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



#PS
eq <- as.formula("percep_speed ~ 1 + year_in_study +          
                 ( 1 + year_in_study |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#df= 10588  
#dev =  14211.8

#int 0.692494
.832/ (sqrt(10594))
#year 0.008501
se<- 0.0922/ (sqrt(10594))
0.0085/ (se)
#resid 0.104101
0.3226/ (sqrt(10594))

#icc:
.69 + 0.008/ (.69 + 0.008 +.104)
##---------------------------------

#AGE BL-------------
# (mean(ds0$age_bl_gmc, na.rm=TRUE))

eq2 <- as.formula("percep_speed ~ 1 + year_in_study*age_bl_gmc + 
                  ( 1 + year_in_study |id)")
model_2<- lmerTest::lmer(eq2, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))

#int = y00 -  mean WM score when age_bl = 0 *i.e. the mean
#age_bl_gmc = y01 - when time =0, for every increase in BL age, percep_speed score decreases this much 

#year_in_study = y10 - average slope controlling for X (when X = 0, the mean, sig decline over time)
#year X agebl = y11- effect of bl age on the slope. That is, for every year older at BL, people sig decline faster

#chi sq
#df
#df= 
10588 -  10586 
#dev =  
14211.8- 13871.8 


#int 0.611856
0.78221/ (sqrt(10594))
#year 0.006888
0.08299/ (sqrt(10594))
#resid 0.104434
0.3226/ (sqrt(10594))

################ + gender

eq3 <- as.formula("percep_speed ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + 
                  ( 1 + year_in_study |id)")
model_3<- lmerTest::lmer(eq3, data=ds0, REML= FALSE) 
lmerTest::summary((model_3))

#msex - y02 = y00 (is now the int for sex = 0/female), thus y02 is the effect of sex on intercept for a 1 unit 
#increase in sex (male). males have an intercept of 

#year X msex = y10 (is now the slope for females)
#y12 (is the effect of sex on female slope, that is, males decline less than females, NS)

#df= 
10588 -   10584
#dev =  
13871.8 - 13858.7


#int 0.606840
0.77900/ (sqrt(10594))
#year 0.006888
0.08299/ (sqrt(10594))
#resid 0.104434
0.3226/ (sqrt(10594))



################# + education 


eq4 <- as.formula("percep_speed ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu + 
                  ( 1 + year_in_study |id)")
model_4<- lmerTest::lmer(eq4, data=ds0, REML= FALSE) 
lmerTest::summary((model_4))

#df= 
10584 - 10582 
#dev =  
13858.7 -13699.1


#int 0.550555
0.74199 / (sqrt(10594))
#year  0.006923
0.08321/ (sqrt(10594))
#resid 0.104424 
0.32315/ (sqrt(10594))



#Physical Activity --------------

describe(ds0$sdmt)
ds0$sdmt2<-ds0$sdmt/2
eq5 <- as.formula("sdmt2 ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                  phys_pmeanC + phys_wp +
                  ( 1 + year_in_study + phys_wp |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))

#df= 
10582 - 10478
#dev =  
13699.1 -13472.9 


#int           0.54
0.73512 / (sqrt( 10492))
#year          0.0065
0.08122/ (sqrt( 10492))
#resid         0.1037
0.32204/ (sqrt( 10492))


# gender X PA
eq6 <- as.formula("percep_speed ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                  phys_pmeanC*msex + phys_wp*msex +
                  ( 1 + year_in_study + phys_wp|id)")
model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE)
lmerTest::summary((model_6))


#df= 
10478 - 10476 
#dev =  
13472.9 - 13472.8


#int           0.54
0.73512 / (sqrt( 10492))
#year          0.0065
0.08122/ (sqrt( 10492))
#resid         0.1037
0.32204/ (sqrt( 10492))


#stress------------------------------
eq5b <- as.formula("percep_speed ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   pss_pmeanC + pss_wp +
                   ( 1 + year_in_study + pss_wp |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))



eq5b <- as.formula("percep_speed ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   pss_pmeanC*msex + pss_wp*msex +
                   ( 1 + year_in_study + pss_wp |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))
################# interaction with stress 
#---- PSS and interaction

#Physical Activity --------------

eq7 <- as.formula("percep_speed ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  +  year_in_study*edu +
                  phys_pmeanC*pss_pmeanC  + pss_wp*phys_wp +
                  ( 1 + year_in_study + pss_wp + phys_wp |id)")
model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
lmerTest::summary((model_7))
#df= 
10476 -7820 
#dev =  
13472.8 - 8868.3 

13472.8 - 3200

#int           0.38644
0.62165  / (sqrt( 7836))
#year          0.00525
0.07246/ (sqrt( 7836))
#resid         0.10121
0.31813/ (sqrt( 7836))


eq7a <- as.formula("percep_speed ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  +  year_in_study*edu +
                  phys_pmeanC*pss_pmeanC +  pss_wp*phys_wp +
                  ( 1 + year_in_study + phys_wp |id)")
model_7a<- lmerTest::lmer(eq7a, data=ds0, REML= FALSE) 
lmerTest::summary((model_7a))



# #Physical Activity --------------
# eq8 <- as.formula("percep_speed ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
#                   phys_pmeanC + phys_wp + phys_wp*nle_pmeanC +
#                   ( 1 + year_in_study |id)")
# model_8<- lmerTest::lmer(eq8, data=ds0, REML= FALSE) 
# lmerTest::summary((model_8))
# #chi sq
# #df
# #df from model 5 versus 7
# 
# 

names(ds0)

#keep re-reunning this to see different people in the sample  IT WORKED
ids <- sample(unique(ds0$id),1)
ds0 %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, cholesterol, al_count_BL, al_count_wave, al_catg_BL, al_catg_wave
  )

describe(ds0$al_catg_BL)

ids <- sample(unique(ds0$id),1)
ds0 %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, physical_activity, phys_pmean, phys_wp, phys_pmeanC)





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


#models--------------------------

eq <- as.formula("dig_b ~ 1 +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#resid var= 1.974

eq <- as.formula("dig_b ~ 1 + year_in_study +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#df= 11243  dev=43476.4


eq <- as.formula("dig_b ~ 1 + year_in_study +          
                 ( 1 + year_in_study |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#df=  11241  
#dev =  43153.4

11241  - 11243
43476.4 -  43153.4 


#pseudo r^2

(2.086 - 1.7793)/2.086




eq4 <- as.formula("dig_b ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu + 
                  ( 1 + year_in_study |id)")
model_4<- lmerTest::lmer(eq4, data=ds0, REML= FALSE) 
lmerTest::summary((model_4))

11241-11235 
43476.4-42904.6

#compared to random time model





#Physical Activity --------------
eq5 <- as.formula("dig_b ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                 phys_pmeanC*year_in_study + phys_wp +
                  ( 1 + year_in_study |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))
# 42023.1    11041 

#the model with the random effects of PA is a better model than the one witout


eq5a <- as.formula("dig_b ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                  + phys_pmeanC*year_in_study + phys_wp +
                  ( 1 + year_in_study + phys_wp |id)")
model_5a<- lmerTest::lmer(eq5a, data=ds0, REML= FALSE) 
lmerTest::summary((model_5a))
# 42020.5    11038 
anova(model_5, model_5a)

42020.5 - 42023.1   
11038 - 11041

1.6 / sqrt(11056)
0.12 / sqrt(11056)
0.014/ sqrt (11056)
1.33/sqrt(11056)

#wp varience explained compred to the random effects of time only
(1.7793 -  1.7608033)/(1.7793)
#varience in the intercept explained by PA BP?
(2.9036   -  2.57920 ) / 2.9036
#varience in the slope explained by PA BP? 
(0.02033 - 0.01632) / 0.02033



#does PA moderate the effects of gender on the intercept and slope
# 
# eq5s <- as.formula("dig_b ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
#                    + phys_pmeanC*year_in_study*msex + phys_wp*msex +
#                    ( 1 + year_in_study + phys_wp |id)")
# model_5s<- lmerTest::lmer(eq5s, data=ds0, REML= FALSE) 
# lmerTest::summary((model_5s))




#stress------------------------------

eq5b <- as.formula("dig_b ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   pss_pmeanC*year_in_study + pss_wp +
                   ( 1 + year_in_study  |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))
# 12719.1     3282 

(2.57 - 2.62 )/2.57
(0.020 - 0.01) / 0.020

#the addition of pss_wp in the random effects is NS
#people aren't very differnt in their stress fluctuations 
#therefore there is nothing to explain

eq5b <- as.formula("dig_b ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   pss_pmeanC*year_in_study + pss_wp +
                   ( 1 + year_in_study + pss_wp |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))
# 12716.9     3279

12719.1-12716.9     
3282-3279

#se
#int
1.62 / sqrt(3297)
0.10/ sqrt(3297)
0.23 / sqrt(3297)
1.29/ sqrt(3297)

# eq5b <- as.formula("dig_b ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
#                    year_in_study*pss_pmeanC*msex + pss_wp*msex +
#                    ( 1 + year_in_study + pss_wp |id)")
# model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE)
# lmerTest::summary((model_5b))

#---nle---------------

eq5b <- as.formula("dig_b ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   nle_pmeanC*year_in_study + nle_wp +
                   ( 1 + year_in_study  |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))
# 12719.1     3282 

(2.57 - 2.62 )/2.57
(0.020 - 0.01) / 0.020

#the addition of pss_wp in the random effects is NS
#people aren't very differnt in their stress fluctuations 
#therefore there is nothing to explain

eq5b <- as.formula("dig_b ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   nle_pmeanC*year_in_study + nle_wp +
                   ( 1 + year_in_study + nle_wp |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))






################# interaction with stress 
#---- PSS and interaction


#interacrtion --------------

# eq7 <- as.formula("dig_b ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  +  year_in_study*edu +
#                   phys_pmeanC*year_in_study*pss_pmeanC  + phys_wp*pss_pmeanC +
#                   ( 1 + year_in_study + phys_wp  |id)")
# model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
# lmerTest::summary((model_7))
# 


print(model_7,correlation=TRUE)


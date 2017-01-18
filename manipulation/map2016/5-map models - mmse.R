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

eq <- as.formula("mmse ~ 1 +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))

eq <- as.formula("mmse ~ 1 + year_in_study +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))



eq <- as.formula("mmse ~ 1 + year_in_study +          
                 ( 1 + year_in_study |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))







eq4 <- as.formula("mmse ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu + 
                  ( 1 + year_in_study |id)")
model_4<- lmerTest::lmer(eq4, data=ds0, REML= FALSE) 
lmerTest::summary((model_4))






#Physical Activity --------------
eq5 <- as.formula("mmse ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                  phys_pmeanC*year_in_study + phys_wp +
                  ( 1 + year_in_study |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))
# 42023.1    11041 




eq5a <- as.formula("mmse ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   + phys_pmeanC*year_in_study + phys_wp +
                   ( 1 + year_in_study + phys_wp |id)")
model_5a<- lmerTest::lmer(eq5a, data=ds0, REML= FALSE) 
lmerTest::summary((model_5a))
# 42020.5    11038 
anova(model_5, model_5a)




#stress------------------------------

eq5b <- as.formula("mmse ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   pss_pmeanC*year_in_study + pss_wp +
                   ( 1 + year_in_study  |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))
# 12719.1     3282 

eq5b <- as.formula("mmse ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   pss_pmeanC*year_in_study + pss_wp +
                   ( 1 + year_in_study + pss_wp |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))
# 12716.9     3279


#---nle---------------

eq5b <- as.formula("mmse ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   nle_pmeanC*year_in_study + nle_wp +
                   ( 1 + year_in_study  |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))
# 12719.1     3282 

(2.57 - 2.62 )/2.57
(0.020 - 0.01) / 0.020



eq5b <- as.formula("mmse ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   nle_pmeanC*year_in_study + nle_wp +
                   ( 1 + year_in_study + pss_wp |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))






################# interaction with stress 
#---- PSS and interaction


#interacrtion --------------

# eq7 <- as.formula("mmse ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  +  year_in_study*edu +
#                   phys_pmeanC*year_in_study*pss_pmeanC  + phys_wp*pss_pmeanC +
#                   ( 1 + year_in_study + phys_wp  |id)")
# model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
# lmerTest::summary((model_7))
# 
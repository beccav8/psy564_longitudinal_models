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
library(psych)
library(moments)


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
path_input0  <- "./data/unshared/derived/lasa_2016/dto_3center.rds" 


options(scipen=20)
# ----- load-data ------

ds0  <- readRDS(path_input0) #total raw data  
names(ds0)
# str(ds0)
length(unique(ds0$id))  #4109 participants  1852 in MAP


table(ds0$male)
# FALSE=0=male  TRUE=1=female
# 14896         13867 

head(ds0$male)
ds0$male[ds0$male == "FALSE"] <- 0
ds0$male[ds0$male == "TRUE"] <- 1


myvars <- c ("id", "male", "edu", "edu_gmc", "wave",  "age_at_visit", "age_at_visit_meanc", "age_at_visit75",
             "age_bl", "age_bl_gmc", 
             "raven_total", "mastery5", "mmse", "se", "word_test", "coding_mean",
             "pss", "nle",  
             "phys","phys_gmc")            
                                        
                        
mpLasa<- ds0[myvars]                              
str(mpLasa)

na.zero <- function (x) {
  x[is.na(x)] <- -999
  return(x)
}

mpLasa<- na.zero(mpLasa)


# "nle_1"             
# "nle_10"             "nle_11"             "nle_12"             "nle_2"             
# "nle_3"              "nle_4"              "nle_5"              "nle_6"             
# "nle_7"              "nle_8"              "nle_9" 




# ---- save-to-disk ------------------------------------------------------------


save(mpLasa, file="./data/unshared/derived/lasa_2016/mpLasa.dat", col.names=F, row.names=F)

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/lasa_2016/dto_4analyses.rds")


save(ds0, file="./data/unshared/derived/lasa_2016/mplus_lasa.RData", ascii=TRUE)
save(ds0, file="./data/unshared/derived/lasa_2016/mplus_lasa.csv")

names(dto)
# this is a flat data.frame containing combined variable
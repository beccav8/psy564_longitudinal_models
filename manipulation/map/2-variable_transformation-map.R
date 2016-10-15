# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(lmerTest)
library(outliers)
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R")
source("./scripts/general-graphs.R")  #in scripts folder
source("./scripts/specific-graphs.R")
source("./scripts/specific-graphs-pred.R")
source("./scripts/graphs-pred.R")
source("./scripts/graphs-predVID.R")
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
# ---- declare-globals ---------------------------------------------------------
# path_input  <- "./data/unshared/derived/dto.rds" 
# path_input  <- "./data/unshared/derived/dAL.rds"  
path_input  <- "./data/unshared/derived/map/dto-AL.rds"                          
# figure_path <- 'manipulation/stitched-output/te/'
# ---- load-data ---------------------------------------------------------------
ds <- readRDS(path_input)
dim(ds)
str(ds)
# head(ds)
data<-ds


#----dupilcate-check--------------------------------------------
ds_long = ds


#check for duplicates
dsd0 <- ds_long %>% dplyr::group_by(id, full_year) %>% dplyr::filter(n()>1) %>% dplyr::summarise(n=n())
# dsd0 shoud have no duplicates. Obs should equal 0 - this new data set will produce a list of which ids are duplicated, in what full-year and how many times
#2717 duplications - must fix this 

#-------apply-transformation-----------------------------------------------------

##########################################---physical-activity-variable-------

#---phys_bl_bp---- = each persons baseline score - the grand mean (between person baseline compare)

d2<- subset(data, full_year==0)   #baseline only
# mean(d2$physical_activity, na.rm=TRUE)  #3.227

# phys_baseline <- mean(d2$physical_activity, na.rm=TRUE)

data$phys_baseline_mean <- rep(mean(d2$physical_activity, na.rm=TRUE))

for(i in unique(data$id)) {
  for (j in 1:length(data$physical_activity[data$id==i])) {
    
    data$phys_baseline[data$id==i][j]<-data$physical_activity[data$id==i][1]
  }}

data$physical_activity[data$id==21305588]
data$physical_activity[data$id==21305588][1]
data$phys_baseline[data$id==21305588]

for(i in unique(data$id)) {
  for (j in 1:length(data$physical_activity[data$id==i])) {
    
    data$phys_bl_bp[data$id==i][j]<- ( (data$phys_baseline[data$id==i][j])) - (data$phys_baseline_mean[data$id==i][j]) 
} }

data$phys_bl_bp[data$id==21305588]

##----physical-activity-between-person---MEAN------------------------------------------------------------
# phys_bp = the persons mean on PA across occasions - the grand mean of PA in the population  (mean=2.6)

for(i in unique(data$id)) {
  for (j in 1:length(data$physical_activity[data$id==i])) {
    
    data$phys_bp_mean[data$id==i][j]<- ( (mean(data$physical_activity[data$id==i], na.rm =TRUE)) - (mean(data$physical_activity, na.rm =TRUE)) ) 
    
  } }

# data$physical_activity[data$id==21305588]
# data$phys_bp[data$id==21305588]
# 
# data$physical_activity[data$id==22396591]
# mean(data$physical_activity[data$id==22396591])
# data$phys_bp[data$id==22396591]

##----physical-activity-between-person---MEDIAN------------------------------------------------------------
# phys_bp = the persons median on PA across occasions - the grand median of the population  (median = 1.75)

for(i in unique(data$id)) {
  for (j in 1:length(data$physical_activity[data$id==i])) {
    
    data$phys_bp_median[data$id==i][j]<- ( (median(data$physical_activity[data$id==i], na.rm =TRUE)) - (median(data$physical_activity, na.rm =TRUE)) ) 
    
  } }

data$physical_activity[data$id==21305588]
median(data$physical_activity[data$id==21305588], na.rm =TRUE)
data$phys_bp_median[data$id==21305588]



##----physical-activity-within-person-fluctiations-MEAN-CENTERED---------------------------------------------------
# phys_wp = that persons score at time j, minus their mean (deviations-from--own-mean)


for(i in unique(data$id)) {
  for (j in 1:length(data$physical_activity[data$id==i])) {
    
    data$phys_wp[data$id==i][j]<- (data$physical_activity[data$id==i][j]) - (mean(data$physical_activity[data$id==i], na.rm =TRUE))
  } }

mean(data$physical_activity[data$id==21305588], na.rm=TRUE)
data$physical_activity[data$id==21305588]
data$phys_wp[data$id==21305588]



########################################----cognitive-variable-------

#mmse----------------------------------------------------------------

# for(i in unique(data$id)) {
#   for (j in 1:length(data$mmse[data$id==i])) {
#     
#     data$mmse_wp[data$id==i][j]<- (data$mmse[data$id==i][j]) - (mean(data$mmse[data$id==i], na.rm =TRUE))
#   } }
# 
# mean(data$mmse[data$id==21305588], na.rm=TRUE)
# data$mmse[data$id==21305588]
# data$mmse_wp[data$id==21305588]
# 
# #digit_back-----------------------------------------------------------------
# 
# for(i in unique(data$id)) {
#   for (j in 1:length(data$dsb[data$id==i])) {
#     
#     data$dsb_wp[data$id==i][j]<- (data$dsb[data$id==i][j]) - (mean(data$dsb[data$id==i], na.rm =TRUE))
#   } }
# 
# mean(data$dsb[data$id==21305588], na.rm=TRUE)
# data$dsb[data$id==21305588]
# data$dsb_wp[data$id==21305588]
# 
# #symbol_digit--------------------------------------------------------------
# 
# for(i in unique(data$id)) {
#   for (j in 1:length(data$symbol_digit_mod[data$id==i])) {
#     
#     data$symbol_digit_mod_wp[data$id==i][j]<- (data$symbol_digit_mod[data$id==i][j]) - (mean(data$symbol_digit_mod[data$id==i], na.rm =TRUE))
#   } }
# 
# mean(data$symbol_digit_mod[data$id==21305588], na.rm=TRUE)
# data$symbol_digit_mod[data$id==21305588]
# data$symbol_digit_mod_wp[data$id==21305588]

##--a few other cog variables left ( composite )

#######################################-centering-age---------------------------

mean(data$age_at_visit, na.rm=TRUE)

center_scale<- function (x) {
  scale (x, scale=FALSE)
}

data$age_at_visit_centered <- center_scale(data$age_at_visit)
data$age_at_visit_centered <- as.vector(data$age_at_visit_centered)
mean(data$age_at_visit_centered)
# table(data$age_at_visit_centered)

# data$agec

#-------------------------- centering baseline age

mean_age_baseline <- mean(data[["age_bl"]], na.rm = TRUE)   #79
est_mean_age_bl<- rep(mean(data$age_bl, na.rm=TRUE), length(data$age_bl))
data$age_bl_centered<- data$age_bl - est_mean_age_bl



saveRDS(data, "./data/unshared/derived/map/data.rds")




















# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(ggplot2)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("plyr")
requireNamespace("testit")# For asserting conditions meet expected patterns.
requireNamespace("car") # For it's `recode()` function.

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- readRDS("./data/unshared/derived/map2016/dto-AL_subset.rds")
# ---- inspect-data -------------------------------------------------------------
# the list is composed of the following elements
names(dto)
head(dto)
data<-dto


##########################-----Center-PA-----#########################################################
##----physical-activity-between-person---MEAN------------------------------------------------------------
# phys_bp = the persons mean on PA across occasions - the grand mean of PA in the population  (mean=2.94)

for(i in unique(data$id)) {
  for (j in 1:length(data$physical_activity[data$id==i])) {
    
    data$phys_bp_mean[data$id==i][j]<- ( (data$physical_activity[data$id==i][j]) - (mean(data$physical_activity, na.rm =TRUE)) ) 
    
  } }
#mean=2.94
data$physical_activity[data$id==21305588]
data$phys_bp_mean[data$id==21305588]
ids <- sample(unique(data$id),3)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,physical_activity,phys_bp_mean)


##----physical-activity-between-person---MEDIAN------------------------------------------------------------
# phys_bp = the persons score on PA at that occasion - the grand median of the population  (median = 1.75)

for(i in unique(data$id)) {
  for (j in 1:length(data$physical_activity[data$id==i])) {

    data$phys_bp_median[data$id==i][j]<- ( (data$physical_activity[data$id==i][j]) - (median(data$physical_activity, na.rm =TRUE)) )

  } }

data$physical_activity[data$id==21305588]
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


#######################################-center-age-#################################################

#center at mean age-------------------------

mean(data$age_at_visit, na.rm=TRUE) #83.15
data$age_at_visit_meanc<- (data$age_at_visit) - (mean(data$age_at_visit, na.rm=TRUE))


data$age_at_visit[data$id==21305588]
data$age_at_visit_meanc[data$id==21305588]

#center at arbitrary meaningful number-----------------------

range(data$age_at_visit, na.rm=TRUE) #53-107
histogram(data$age_at_visit)
#choose 65

data$age_at_visit65<- (data$age_at_visit) - (65)


data$age_at_visit[data$id==21305588]
data$age_at_visit65[data$id==21305588]


names(data)


saveRDS(data, "./data/unshared/derived/map2016/data_centered.rds")


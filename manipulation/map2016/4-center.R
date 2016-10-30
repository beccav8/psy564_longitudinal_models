
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


##------- center-pss------------------------------------------------------
 # between person
for(i in unique(data$id)) {
  for (j in 1:length(data$pss[data$id==i])) {
    
    data$pss_bp_meanc[data$id==i][j]<- ( (data$pss[data$id==i][j]) - (mean(data$pss, na.rm =TRUE)) )
    
  } }

data$pss[data$id==21305588]
data$pss_bp_meanc[data$id==21305588]



  # within person

for(i in unique(data$id)) {
  for (j in 1:length(data$pss[data$id==i])) {
    
    data$pss_wp[data$id==i][j]<- (data$pss[data$id==i][j]) - (mean(data$pss[data$id==i], na.rm =TRUE))
  } }

data$pss[data$id==21305588]
data$pss_wp[data$id==21305588]

#######################################-center-age-#################################################

#center at mean age-------------------------

mean(data$age_at_visit, na.rm=TRUE) #83.15
data$age_at_visit_meanc<- (data$age_at_visit) - (mean(data$age_at_visit, na.rm=TRUE))


data$age_at_visit[data$id==21305588]
data$age_at_visit_meanc[data$id==21305588]

#center at arbitrary meaningful number-----------------------

range(data$age_at_visit, na.rm=TRUE) #53-107
histogram(data$age_at_visit)
#choose 75

data$age_at_visit65<- (data$age_at_visit) - (75)


data$age_at_visit[data$id==21305588]
data$age_at_visit65[data$id==21305588]


##-- select only the variables I want (i.e. further refine)

names(data)

myvars<- c("id","year_in_study", "dementia", "age_bl","age_at_visit","edu", "msex","race","apoe",
           "episodic","percep_speed","semantic","wm","global","dig_b","dig_f","mmse",
           "nle","pss","physical_activity", "al_count_BL","al_count_wave","al_catg_BL", "al_catg_wave", "pss_bp_meanc", "pss_wp", 
           "social_isolation", "phys_bp_mean","phys_bp_median","phys_wp", "age_at_visit_meanc","age_at_visit65")


d <- data[myvars]


d$id<-as.numeric(d$id)
d$year_in_study<-as.numeric(d$year_in_study)
d$dementia<-as.numeric(d$dementia)
d$age_bl<-as.numeric(d$age_bl)
d$age_at_visit<-as.numeric(d$age_at_visit)
d$edu<-as.numeric(d$edu)
d$msex<-as.numeric(d$msex)
d$race<-as.numeric(d$race)
d$apoe<-as.numeric(d$apoe)
d$episodic<-as.numeric(d$episodic)
d$percep_speed<-as.numeric(d$percep_speed)
d$semantic<-as.numeric(d$semantic)
d$wm<-as.numeric(d$wm)
d$global<-as.numeric(d$global)
d$dig_b<-as.numeric(d$dig_b)
d$dig_f<-as.numeric(d$dig_f)
d$mmse<-as.numeric(d$mmse)
d$nle<-as.numeric(d$mmse)
d$pss<-as.numeric(d$pss)
d$physical_activity<-as.numeric(d$physical_activity)
d$al_count_BL<-as.numeric(d$al_count_BL)
d$al_count_wave<-as.numeric(d$al_count_wave)
d$al_catg_BL<-as.numeric(d$al_catg_BL)
d$al_catg_wave<-as.numeric(d$al_catg_wave)
d$social_isolation<-as.numeric(d$social_isolation)
d$phys_bp_mean<-as.numeric(d$phys_bp_mean)
d$phys_bp_median<-as.numeric(d$phys_bp_median)
d$phys_wp<-as.numeric(d$phys_wp)
d$age_at_visit_meanc<-as.numeric(d$age_at_visit_meanc)
d$age_at_visit65<-as.numeric(d$age_at_visit65)
d$pss_wp<-as.numeric(d$pss_wp)
d$pss_bp_meanc<-as.numeric(d$pss_bp_meanc)


length(unique(d$id))  #1853 participants 


saveRDS(d, "./data/unshared/derived/map2016/map_full_bio_centered.rds")

write.table(d, file="./data/unshared/derived/map2016/map_full_bio_centered", row.names=FALSE, sep="\t", quote=FALSE)


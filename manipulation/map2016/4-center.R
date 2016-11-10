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


######--------------- center PA ------------------------################

#GRAND MEAN CENTERING 
data$phys_bp_mean <- (data$physical_activity) - (mean(data$physical_activity, na.rm=TRUE))

#MEDIAN CENTER?
data$phys_bp_median <- (data$physical_activity) - (median(data$physical_activity, na.rm=TRUE))

#PERSON MEAN CENTER
phys_pmean <- aggregate(data$physical_activity, by=list(data$id), mean, na.rm=TRUE)
names(phys_pmean) <- c("id", "phys_pmean")
data <- merge(data, phys_pmean, by="id")
data$phys_wp <- data$physical_activity-data$phys_pmean

#center person means around the mean
data$phys_pmeanC <- data$phys_pmean - (mean(data$physical_activity, na.rm=TRUE))

ids <- sample(unique(data$id),1)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,physical_activity, phys_pmean, phys_pmeanC, phys_wp, phys_bp_mean, phys_bp_median)




#######------------------center stress-----------------------#################


##------- center-pss------------------------------------------------------
#GRAND MEAN CENTERING 
data$pss_bp_meanc <- (data$pss) - (mean(data$pss, na.rm=TRUE))

#PERSON MEAN CENTER
pss_pmean <- aggregate(data$pss, by=list(data$id), mean, na.rm=TRUE)
names(pss_pmean) <- c("id", "pss_pmean")
data <- merge(data, pss_pmean, by="id")
data$pss_wp <- data$pss-data$pss_pmean

#center person means around the mean
data$pss_pmeanC <- data$pss_pmean - (mean(data$pss, na.rm=TRUE))

#####---------------center-age-----------------###############
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

names(data)
head(data$dementia)
#time until dementia 
data <- data %>% dplyr::group_by(id) %>%    # evaluate patients individually
  dplyr::mutate(age_visit = as.integer(as.character(age_at_visit)),    # factor to integer
         # if no dementia, NA else min age where dementia == 1
         age_at_dx = ifelse(dementia == 0, NA, min(age_at_visit[dementia == 1]))) %>% 
  tidyr::fill(age_at_dx) %>%    # fill in NAs after non-NA (where dx == 1, then 0 like line 9)
  dplyr::mutate(time_since_dx = age_at_visit - age_at_dx)

head(data$time_since_dx)

ids <- sample(unique(data$id),1)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,time_since_dx, dementia, age_at_visit,age_at_dx)




##-- select only the variables I want (i.e. further refine)

names(data)

myvars<- c("id","year_in_study", "dementia", "age_bl","age_at_visit", "time_since_dx", "edu", "msex","race","apoe",
           "episodic","percep_speed","semantic","wm","global","dig_b","dig_f","mmse",
           "nle","pss","physical_activity", "al_count_BL","al_count_wave","al_catg_BL", "al_catg_wave", "pss_bp_meanc", "pss_wp", 
           "social_isolation", "phys_bp_mean","phys_bp_median","phys_wp", "age_at_visit_meanc","age_at_visit65",
           "phys_pmean", "pss_pmean", "pss_pmeanC", "phys_pmeanC")


d <- data[myvars]

str(d)
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
d$pss_pmean<-as.numeric(d$pss_pmean)
d$phys_pmean <-as.numeric(d$phys_pmean)

length(unique(d$id))  #1853 participants 


saveRDS(d, "./data/unshared/derived/map2016/map_full_bio_centered.rds")

write.table(d, file="./data/unshared/derived/map2016/map_full_bio_centered.dat", row.names=FALSE, sep="\t", quote=FALSE)


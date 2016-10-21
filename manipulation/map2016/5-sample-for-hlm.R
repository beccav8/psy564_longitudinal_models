
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

path_input0  <- "./data/unshared/derived/map2016/data_centered.rds" 

# @knitr load-data ---------------------------------------------------------------
map_sample  <- readRDS(path_input0) #total raw data  

names(map_sample)


set.seed(1)
ids <- sample(map_sample$id,300)
d <- map_sample %>%  dplyr::filter( id %in% ids)
dim(d)
head(d)
length(unique(d$id))


myvars<- c("id","year_in_study", "dementia", "age_bl","age_at_visit","edu", "msex","race","apoe",
           "episodic","percep_speed","semantic","wm","global","dig_b","dig_f","mmse",
           "nle","pss","physical_activity", "al_count_BL","al_count_wave","al_catg_BL", "al_catg_wave", 
           "social_isolation", "phys_bp_mean","phys_bp_median","phys_wp", "age_at_visit_meanc","age_at_visit65")

# "cts_sdmt","cts_wli","cts_wlii","cts_wliii","neglifeevents","perceivedstress","phys5itemsum")

d <- d[myvars]


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


unique(d$id)

write.table(d, file="./data/unshared/derived/map2016/hlm_map_sample.dat", row.names=FALSE, sep="\t", quote=FALSE)


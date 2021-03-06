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
library(moments)
library(psych)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("plyr")
requireNamespace("testit")# For asserting conditions meet expected patterns.
requireNamespace("car") # For it's `recode()` function.

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
# dto <- readRDS("./data/unshared/derived/map2016/dto-AL_subset.rds")
dto <- readRDS("./data/unshared/derived/map2016/dto-AL.rds")
# ---- inspect-data -------------------------------------------------------------
# the list is composed of the following elements


ids <- sample(unique(dto$id),1)
dto %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, cholesterol, al_count_BL, al_count_wave, al_catg_BL, al_catg_wave
  )


names(dto)
head(dto)
data<-dto

summary(data$pss)
hist(data$pss)
summary(data$nle)
hist(data$nle)

######--------------- center PA ------------------------################

#GRAND MEAN CENTERING   ##mean is 2.94
data$phys_gmc <- (data$physical_activity) - (mean(data$physical_activity, na.rm=TRUE))

#MEDIAN CENTER?
data$phys_gmedc <- (data$physical_activity) - (median(data$physical_activity, na.rm=TRUE))

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
  dplyr::select(id,physical_activity, phys_pmean, phys_pmeanC, phys_wp, phys_gmc, phys_gmedc)

describe(data$physical_activity)



#-- PA at baseline 
bl <- data[ which(data$year_in_study==0), ] #baseline subset
names(bl)

bl$PA_bl <- bl$physical_activity
sub<- bl[c("id", "PA_bl")]

#grand mean center PA at baseline 

sub$PA_bl_BP <- (sub$PA_bl) - (mean(sub$PA_bl, na.rm=TRUE))
data <- merge(data, sub, by="id")


names(data)


###----------------------- center pysical activity after transformation ------------------------

# summary(data$physical_activity)
# str(data$physical_activity)
# describe(data$physical_activity)
# table(data$physical_activity)
# hist(data$physical_activity)
# 
# data$physical_activity1<-(data$physical_activity) + 0.1  
# 
# data$physLOG<- log(data$physical_activity1) 
# 
# hist(data$physLOG)
# agostino.test(data$physLOG)
# 
# describe(data$physLOG)
# str(data$physLOG)
# describe(data$physLOG)
# 
# 
# #GRAND MEAN CENTERING 
# data$physLOG_gmc <- (data$physLOG) - (mean(data$physLOG, na.rm=TRUE))
# 
# #PERSON MEAN CENTER
# physLOG_pmean <- aggregate(data$physLOG, by=list(data$id), mean, na.rm=TRUE)
# names(physLOG_pmean) <- c("id", "phys_pmean")
# data <- merge(data, physLOG_pmean, by="id")
# data$physLOG_wp <- data$physLOG-data$physLOG_pmean
# 
# #center person means around the mean
# data$phys_pmeanC <- data$phys_pmean - (mean(data$physLOG, na.rm=TRUE))
# 
# ids <- sample(unique(data$id),1)
# data %>%
#   dplyr::filter(id %in% ids ) %>%
#   dplyr::group_by(id) %>%
#   dplyr::select(id,physLOG, physLOG_pmean)




#######------------------center stress-----------------------#################


##------- center-pss------------------------------------------------------
#GRAND MEAN CENTERING mean PSS = 2.02
data$pss_gmc <- (data$pss) - (mean(data$pss, na.rm=TRUE))

#PERSON MEAN CENTER
pss_pmean <- aggregate(data$pss, by=list(data$id), mean, na.rm=TRUE)
names(pss_pmean) <- c("id", "pss_pmean")
data <- merge(data, pss_pmean, by="id")
data$pss_wp <- data$pss-data$pss_pmean

#center person means around the mean
data$pss_pmeanC <- data$pss_pmean - (mean(data$pss, na.rm=TRUE))

ids <- sample(unique(data$id),1)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,pss, pss_pmean, pss_wp, pss_gmc, pss_pmeanC)


describe(data$pss)


##------- center-nle------------------------------------------------------
summary(data$nle)

8334/11673
#GRAND MEAN CENTERING 
data$nle_gmc <- (data$nle) - (mean(data$nle, na.rm=TRUE))

#PERSON MEAN CENTER
nle_pmean <- aggregate(data$nle, by=list(data$id), mean, na.rm=TRUE)
names(nle_pmean) <- c("id", "nle_pmean")
data <- merge(data, nle_pmean, by="id")
data$nle_wp <- data$nle-data$nle_pmean


#center person means around the mean
data$nle_pmeanC <- data$nle_pmean - (mean(data$nle, na.rm=TRUE))

ids <- sample(unique(data$id),1)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,nle, nle_pmean, nle_wp, nle_gmc, nle_pmeanC)

describe(data$nle)
summary(data$nle_gmc)

#-- nle at baseline 
# bl <- data[ which(data$year_in_study==0), ] #baseline subset
# names(bl)
# describe(bl$nle)
# 
# bl$nle_bl <- bl$nle
# sub<- bl[c("id", "nle_bl")]
# 
# #grand mean center PA at baseline 
# 
# sub$nle_bl_BP <- (sub$nle_bl) - (mean(sub$nle_bl, na.rm=TRUE))
# data <- merge(data, sub, by="id")

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


#-- center age at bl

data$age_bl_gmc<- (data$age_bl) - (mean(data$age_bl, na.rm=TRUE))
describe(data$age_bl)
#-- center education 

describe(data$edu) #0-28, mean is 14.73, median is 15
data$edu_gmc<- (data$edu) - (mean(data$edu, na.rm=TRUE))
describe(data$edu_gmc)
#new mean is 0, range is -14.73 to 13.27.



#-- recode gender so that it matches LASA


##-- select only the variables I want (i.e. further refine)

names(data)

myvars<- c("id","year_in_study","age_bl","age_at_visit", "time_since_dx","age_at_visit_meanc","age_at_visit65",
           "dementia",  "edu", "edu_gmc", "msex","race","apoe", 
           "episodic","percep_speed","semantic","wm","global",
           "dig_b","dig_f","mmse", "sdmt", "wl_im","wl_del","wl_rec",
           "nle", "nle_wp", "nle_pmean", "nle_pmeanC", "nle_gmc",
           "pss", "pss_pmean", "pss_pmeanC", "pss_gmc", "pss_wp", 
           "al_count_BL","al_count_wave","al_catg_BL", "al_catg_wave", 
           "physical_activity","phys_gmc","phys_gmedc","phys_wp", "cholesterol",
           "phys_pmean",  "phys_pmeanC", "PA_bl_BP", "age_bl_gmc")


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
d$sdmt<-as.numeric(d$sdmt)
d$wl_im<-as.numeric(d$wl_im)
d$wl_del<-as.numeric(d$wl_del)
d$wl_rec<-as.numeric(d$wl_rec)
  

d$nle<-as.numeric(d$nle)
d$pss<-as.numeric(d$pss)
d$physical_activity<-as.numeric(d$physical_activity)
d$al_count_BL<-as.numeric(d$al_count_BL)
d$al_count_wave<-as.numeric(d$al_count_wave)
d$phys_gmc<-as.numeric(d$phys_gmc)
d$phys_gmedc<-as.numeric(d$phys_gmedc)
d$phys_wp<-as.numeric(d$phys_wp)
d$age_at_visit_meanc<-as.numeric(d$age_at_visit_meanc)
d$age_at_visit65<-as.numeric(d$age_at_visit65)
d$pss_wp<-as.numeric(d$pss_wp)
d$pss_gmc<-as.numeric(d$pss_gmc)
d$pss_pmean<-as.numeric(d$pss_pmean)
d$phys_pmean <-as.numeric(d$phys_pmean)
d$nle <- as.numeric(d$nle)
d$nle_gmc <- as.numeric(d$nle_gmc)
d$nle_wp <- as.numeric(d$nle_wp)
d$PA_bl_BP <- as.numeric(d$PA_bl_BP)

str(d)


length(unique(d$id))  #1853 participants 


saveRDS(d, "./data/unshared/derived/map2016/map_full_bio_centered.rds")

write.table(d, file="./data/unshared/derived/map2016/map_full_bio_centered.dat", row.names=FALSE, sep="\t", quote=FALSE)


ids <- sample(unique(d$id),1)
d %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,nle, nle_pmean, nle_wp, nle_gmc, nle_pmeanC)



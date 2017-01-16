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
dto <- readRDS("./data/unshared/derived/lasa_2016/dto_trans.rds")
# ---- inspect-data -------------------------------------------------------------
# the list is composed of the following elements


names(dto)
data<-dto


#edu-----------------------------------------

describe(data$edu)  #men is 9.15 (5 - 18)
data$edu_gmc<- (data$edu) - (mean(data$edu, na.rm=TRUE))
describe(data$edu_gmc) #mean is now 0, range = -4.15 8.85
#center age ----------------------------------------------------

#center at mean age-------------------------

mean(data$age_at_visit, na.rm=TRUE) #72.57 (younger than MAP)
data$age_at_visit_meanc<- (data$age_at_visit) - (mean(data$age_at_visit, na.rm=TRUE))

#center at arbitrary meaningful number-----------------------

range(data$age_at_visit, na.rm=TRUE) #54 - 104 (simialr to MAP)
data$age_at_visit75<- (data$age_at_visit) - (75)

#-- center age at bl

bl <- data[ which(data$wave==1), ] #baseline subset
bl$age_bl <- bl$age_at_visit

sub<- bl[c("id", "age_bl")]

data <- merge(data, sub, by="id")

names(data)

ids <- sample(unique(data$id),1)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,age_at_visit,age_bl)


data$age_bl_gmc<- (data$age_bl) - (mean(data$age_bl, na.rm=TRUE))
describe(data$age_bl) #68.13
describe(data$age_bl_gmc) #-13.35 17.51


#nle ----------------------------------------
#GRAND MEAN CENTERING 
data <- rename(data, c(NLE_total = "nle"))

names(data)

#GRAND MEAN CENTERING  #mean is 0.116921
describe(data$nle)
data$nle_gmc <- (data$nle) - (mean(data$nle, na.rm=TRUE))

#PERSON MEAN CENTER (WP) 
nle_pmean <- aggregate(data$nle, by=list(data$id), mean, na.rm=TRUE)
names(nle_pmean) <- c("id", "nle_pmean")
data <- merge(data, nle_pmean, by="id")
data$nle_wp <- data$nle-data$nle_pmean

#center person means around the mean (BP) 
data$nle_bp <- data$nle_pmean - (mean(data$nle, na.rm=TRUE))

ids <- sample(unique(data$id),1)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, 
                nle_1, nle_2, nle_3, nle_4, nle_5, nle_6, nle_7, nle_8, nle_9, nle_10, nle_11, nle_12,
                nle, nle_pmean, nle_wp, nle_gmc, nle_bp)


#pss----------------------------------

#GRAND MEAN CENTERING 
data$pss_gmc <- (data$pss) - (mean(data$pss, na.rm=TRUE))

#PERSON MEAN CENTER (WP) - this variable is useless because PA is only available at one wave
pss_pmean <- aggregate(data$pss, by=list(data$id), mean, na.rm=TRUE)
names(pss_pmean) <- c("id", "pss_pmean")
data <- merge(data, pss_pmean, by="id")
data$pss_wp <- data$pss-data$pss_pmean

#center person means around the mean (BP) - this is the variable i will use if I want PSS as a TIVC
data$pss_bp <- data$pss_pmean - (mean(data$pss, na.rm=TRUE))

describe(data$pss)
#mean pss= 11.6386   0 - 38
describe(data$pss_bp) #-11.64 26.36

ids <- sample(unique(data$id),1)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,pss, pss_pmean, pss_wp, pss_gmc, pss_bp)



#se--------------------------------------
#GRAND MEAN CENTERING 
data$se_gmc <- (data$se) - (mean(data$se, na.rm=TRUE))

#PERSON MEAN CENTER (WP) 
se_pmean <- aggregate(data$se, by=list(data$id), mean, na.rm=TRUE)
names(se_pmean) <- c("id", "se_pmean")
data <- merge(data, se_pmean, by="id")
data$se_wp <- data$se-data$se_pmean

#center person means around the mean (BP) 
data$se_bp <- data$se_pmean - (mean(data$se, na.rm=TRUE))

ids <- sample(unique(data$id),1)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,se, se_pmean, se_wp, se_gmc, se_bp)

describe(data$se)

#mastery------------------------------------

#GRAND MEAN CENTERING 
data$mastery5_gmc <- (data$mastery5) - (mean(data$mastery5, na.rm=TRUE))

#PERSON MEAN CENTER (WP) 
mastery5_pmean <- aggregate(data$mastery5, by=list(data$id), mean, na.rm=TRUE)
names(mastery5_pmean) <- c("id", "mastery5_pmean")
data <- merge(data, mastery5_pmean, by="id")
data$mastery5_wp <- data$mastery5-data$mastery5_pmean

#center person means around the mean (BP) 
data$mastery5_bp <- data$mastery5_pmean - (mean(data$mastery5, na.rm=TRUE))

ids <- sample(unique(data$id),1)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,mastery5, mastery5_pmean, mastery5_wp, mastery5_gmc, mastery5_bp)

describe(data$mastery5)

#PA-------------------------------------------
data <- rename(data, c(PA_hours_wk = "phys"))
#GRAND MEAN CENTERING 
data$phys_gmc <- (data$phys) - (mean(data$phys, na.rm=TRUE))

#PERSON MEAN CENTER (WP) 
phys_pmean <- aggregate(data$phys, by=list(data$id), mean, na.rm=TRUE)
names(phys_pmean) <- c("id", "phys_pmean")
data <- merge(data, phys_pmean, by="id")
data$phys_wp <- data$phys-data$phys_pmean

#center person means around the mean (BP) 
data$phys_bp <- data$phys_pmean - (mean(data$phys, na.rm=TRUE))

ids <- sample(unique(data$id),1)
data %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,phys, phys_pmean, phys_wp, phys_gmc, phys_bp)


describe(data$phys)

# ---- save-to-disk ------------------------------------------------------------

# Save as a compressed, binary R dataset.  
# It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(data, file="./data/unshared/derived/lasa_2016/dto_3center.rds", compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/lasa_2016/dto_3center.rds")
names(dto)




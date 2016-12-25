
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
library(reshape)
library(psych)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("plyr")
requireNamespace("testit")# For asserting conditions meet expected patterns.
requireNamespace("car") # For it's `recode()` function.


# ---- load-data --------------------------------------------------------------------
getwd()

ds0 <- readRDS("C:/Users/Rebecca/Documents/GitHub/LASA/data-unshared/derived/dto.rds")
names(ds0)

# ---- tweak-data ------------------------------------

ds0 <- rename(ds0, c(HQPSSTOT="pss", ravel_b="raven_b", raven_total_coloured = "raven_total"))


get_n <- function(d){length(unique(d[,"id"]))}
ds0%>% get_n()
#  4109 unique id's

# ---- demographics --------------------------------------------

describe(ds0$male)
#coded as 0 or 1

#baseline 
bl <- ds0[ which(ds0$wave==1), ] #4109 observations, thus, all unique id's accounted for
table(bl$male)
#FALSE  TRUE 
#2128  1981


table(ds0$bycohort)

table(ds0$edu)
#what does a value of -1 mean? LASA site doesn't indicate this

# ---- cognitive outcomes -----------------------------------------

# ---- coding ------

#take the mean of all 3 trials to get a score for each participant - must exclude -1 (this is missing data)
#note: the same cases who have -1 on coding trial 1, also have it on coding trial 2 and 3, therefore, easy to clean

#no trial = -1, no valid data=-4
#range is -1 to about 45, therefore, values of -1 exist in all of the coding, must make these NA
#each has 1533 observations

describe(ds0$coding1)
describe(ds0$coding2)
describe(ds0$coding3)

#all have 1533 observations

test<-ds0[which(ds0$coding1 < 0),] #35
test<-ds0[which(ds0$coding1 < 0),] #35
test<-ds0[which(ds0$coding1 < 0),] #35

test<-ds0[which(ds0$coding1 < 0, ds0$coding2 <0, ds0$coding3<0),] #35, 

# test<- ds0
# test$coding1[test$coding1 <0] <-NA
# test2<-test[which(test$coding1 < 0),] #35

ds0$coding1[ds0$coding1 <0] <-NA
ds0$coding2[ds0$coding2 <0] <-NA
ds0$coding3[ds0$coding3 <0] <-NA

test<-ds0[which(ds0$coding1 < 0, ds0$coding2 <0, ds0$coding3<0),] #0, therefore, eliminated the -1's

#range is now in the positives only, each have  1498 observations 
describe(ds0$coding1)
describe(ds0$coding2)
describe(ds0$coding3)

#new variable coding_mean
ds0$coding_mean<- ((ds0$coding1 + ds0$coding2 + ds0$coding3)) /3 
describe(ds0$coding_mean) #mean = 25.28, n=1485


ds0 <- ds0[, -which(names(ds0) %in% c("coding1", "coding2", "coding3"))]
names(ds0)


# ---- scaled-raven ----

#-2 mean no valid data

describe(ds0$raven_total) #range = -2 to 24  # n =4098

test<-ds0[which(ds0$raven_total < 0),] #81 have no valid data
ds0$raven_total[ds0$raven_total < 0] <-NA

describe(ds0$raven_total) # n=4017, min=4 and max = 24 

ds0 <- ds0[, -which(names(ds0) %in% c("raven_a", "raven_b"))]
names(ds0)


# ---- mmse ----

describe(ds0$mmse)
ds0$mmse[ds0$mmse < 0] <-NA
describe(ds0$mmse)

# ---- word-test -----

#there were several options when selecting which word_test varible to use.
#in LASA repository (1-compose-data-frame), I select the highest score of the three trials as my outcome
#this is because a mean score is subjected to missing data, and a recall score isn't available for all of the waves
#moreover, on the LASA website, this is the reccomended method
#the origional variable name is _mtmax

describe(ds0$word_test)
#1314 observations, range = -2, 15
ds0$word_test[ds0$word_test<0] <- NA
describe(ds0$word_test)
#1299 observations, range = 0-15
names(ds0)

# ---- physical-activity --------------------------------------

#PA variables include biking, gardening, sports (1 and 2), and walking
#light and heavy household work are excluded to make the composite more comparable to MAP

#this varible was created by    (freq over the past 2 weeks X minuites each time) /2 =minutes per week


#########hat do i do if i have the minutes, but not the frequency 
########## this variable is not created yet###################


ids <- sample(unique(ds0$id),1)
ds0 %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, 
                # walking_freq, walking_min,biking_freq, biking_min,
                # garden_freq, garden_min, 
                sport1_freq, sport1_min, sport2_freq, sport2_min)       



# a vlue of -2 seems to represent missing data e.g.
#      id sport1_freq sport1_min sport2_freq sport2_min
# 1 31568          10         30           4         10
# 2 31568          -2         -2          -2         -2
# 3 31568          -2         -2          -2         -2
# 

#but I'm not sure what -1 means...

# e.g.
#      id sport1_freq sport1_min sport2_freq sport2_min
# 5 15723          -1         30          -1         75
# 6 15723           2        120           2         30
# 7 15723          NA         NA          NA         NA
# > 


# describe(ds0$physical_activity)

# ---- stress -----------------------------------------------




# ---- pss ----
head(ds0$pss)

describeBy(ds0$pss, ds0$wave)
#only availavle at wave 7
ds0$pss[ds0$pss < 0] <-NA
describe(ds0$pss)



# ---- nle ----

ids <- sample(unique(ds0$id),1)
ds0 %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, nle_1, nle_2, nle_3)       



# ---- other ------------------------------------------------

# ---- mastery ----

ds0$mastery5[ds0$mastery5 < 0] <-NA

#wave 1 - 4 only

describeBy(ds0$mastery5, ds0$wave)

# ---- self-efficacy ----

ids <- sample(unique(ds0$id),1)
ds0 %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,coding1, coding2,coding3,coding_mean)













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

## origional data composition is in the LASA repository (manipulation folder)

# ds0 <- readRDS("C:/Users/Rebecca/Documents/GitHub/LASA/data-unshared/derived/dto.rds") #laptop
ds0 <- readRDS("C:/Users/beccav8/Documents/GitHub/LASA/data-unshared/derived/dto.rds") #campus
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
#FALSE=0=male  TRUE=1=female 
#2128          1981


table(ds0$bycohort)

table(ds0$edu)
#what does a value of -1 mean? LASA site doesn't indicate this
ds0$edu[ds0$edu <0] <-NA

# ---- cognitive outcomes -----------------------------------------

# ---- coding ------ processing speed (comparable to SDMT)

#take the mean of all 3 trials to get a score for each participant - must exclude -1 (this is missing data)
#note: the same cases who have -1 on coding trial 1, also have it on coding trial 2 and 3, therefore, easy to clean

#no trial = -1, no valid data=-4
#range is -1 to about 45, therefore, values of -1 exist in all of the coding, must make these NA
#each has 1533 observations

describe(ds0$coding1)
describe(ds0$coding2)
describe(ds0$coding3)

#all have 1533 observations

test<-ds0[which(ds0$coding1 < 0),] #35 NA's
test<-ds0[which(ds0$coding1 < 0),] #35 NA's
test<-ds0[which(ds0$coding1 < 0),] #35 NA's


test<-ds0[which(ds0$coding1 < 0, ds0$coding2 <0, ds0$coding3<0),] #35, 
#NA's are consistent across trials 

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

#new variable coding_mean -i.e. coding is the mean of all 3 trials of coding 

ds0$coding_mean<- ((ds0$coding1 + ds0$coding2 + ds0$coding3)) /3 
describe(ds0$coding_mean) #mean = 25.28, n=1485


ds0 <- ds0[, -which(names(ds0) %in% c("coding1", "coding2", "coding3"))]
names(ds0)


# ---- scaled-raven ---- fluid intelligence (no comparison in MAP)

#-2 mean no valid data

describe(ds0$raven_total) #range = -2 to 24  # n =4098
unique(ds0[,"raven_total"])

test<-ds0[which(ds0$raven_total < 0),] #81 have no valid data
ds0$raven_total[ds0$raven_total < 0] <-NA

describe(ds0$raven_total) # n=4017, min=4 and max = 24 

ds0 <- ds0[, -which(names(ds0) %in% c("raven_a", "raven_b"))]
names(ds0)
unique(ds0[,"raven_total"])

# ---- mmse ---- (comparable to mmse in MAP)

describe(ds0$mmse)
ds0$mmse[ds0$mmse < 0] <-NA
describe(ds0$mmse)

# ---- word-test ----- (episodic memory, digit backward, a working memory measure
#was only measured at one wave in LASA, therefore no comparison)

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
#mins per week was then transformed into hours per week, so that it is equivalent to MAP


######### In some instances, I had the minutes, but not the frequency (i.e. how many times that week they engaged in that
#many mins)
########## therefore, this information has to be excluded (ie. -1 in the freq. column is NA)


ids <- sample(unique(ds0$id),1)
ds0 %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, 
                # walking_freq, walking_min,biking_freq, biking_min,
                # garden_freq, garden_min, 
                sport1_freq, sport1_min, sport2_freq, sport2_min)       



# a vlue of -2 seems to represent missing data e.g.
#      id sport1_freq sport1_min    sport2_freq sport2_min
# 1 31568          10         30           4         10   =40
# 2 31568          -2         -2          -2         -2
# 3 31568          -2         -2          -2         -2
# 

#but I'm not sure what -1 means...

# e.g.
#      id sport1_freq sport1_min     sport2_freq sport2_min
# 5 15723          -1         30          -1         75  = ?? 
# 6 15723           2        120           2         30
# 7 15723          NA         NA          NA         NA
# > 


# describe(ds0$physical_activity)

testing<-ds0

testing$walking_freq[testing$walking_freq<0] <- NA
testing$walking_min[testing$walking_min<0] <- NA
testing$biking_freq[testing$biking_freq<0] <- NA
testing$biking_min[testing$biking_min<0] <- NA
testing$garden_freq[testing$garden_freq<0] <- NA
testing$garden_min[testing$garden_min<0] <- NA
testing$sport1_freq[testing$sport1_freq<0] <-NA
testing$sport1_min[testing$sport1_min<0] <- NA
testing$sport2_freq[testing$sport2_freq<0] <-NA
testing$sport2_min[testing$sport2_min<0] <- NA

names(testing) 
ids <- sample(unique(testing$id),1)
testing %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, 
                walking_freq, walking_min,biking_freq, biking_min,
                garden_freq, garden_min,
                sport1_freq, sport1_min, sport2_freq, sport2_min)   



describe(ds0$walking_freq)     #4530
describe(testing$walking_freq) #3853
hist(testing$walking_freq)

describe(ds0$biking_freq)  #4530
describe(testing$biking_freq) #2593

describe(ds0$garden_freq) #4530
describe(testing$garden_freq) #1728

describe(ds0$sport1_freq) #4530
describe(testing$sport1_freq) #2287

describe(ds0$sport2_freq) #4530
describe(testing$sport2_freq) #1122

# test<- ds0[which(ds0$walking_freq == -1, ds0$biking_freq == -1, ds0$garden_freq == -1,
#                  ds0$sport1_freq == -1, ds0$sport2_freq == -1),]
                  
# 
# tw<- ds0[which(ds0$walking_freq == -1),] # 7 people have -1 ,   667 have -2
# tb<- ds0[which(ds0$biking_freq == -1),]  # 5 people have -1,   1932 have -2
# tg<- ds0[which(ds0$garden_freq == -1),]  # 2 people have -1,   1797 
# ts1<- ds0[which(ds0$sport1_freq == -1),] #251 people have -1,  2058
# ts2<- ds0[which(ds0$sport2_freq == -1),] # 181 people have -1, 3281
# thh<- ds0[which(ds0$heavyhh_freq == -1),] #0 people have -1,   1545 
# tlh<- ds0[which(ds0$lighthh_freq == -1),] # 0 people have -1,  292 

# tw<- ds0[which(ds0$walking_min == -2),]
# tb<- ds0[which(ds0$biking_min == -2),]
# tg<- ds0[which(ds0$garden_min == -2),]
# ts1<- ds0[which(ds0$sport1_min == -2),]
# ts2<- ds0[which(ds0$sport2_min == -2),]
# thh<- ds0[which(ds0$heavyhh_min == -2),]
# tlh<- ds0[which(ds0$lighthh_min == -2),]

#calculate mins per week 

testing$walking <- (testing$walking_min * testing$walking_freq)/2

ids <- sample(unique(testing$id),1)
testing %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, 
                walking_freq, walking_min, walking)

testing$yard <- (testing$garden_min * testing$garden_freq)/2
testing$bike <- (testing$biking_min * testing$biking_freq)/2
testing$sport1 <- (testing$sport1_min * testing$sport1_freq)/2
testing$sport2 <- (testing$sport2_min * testing$sport2_freq)/2


# testing$PA_min_wk<- testing$walking + testing$yard + testing$bike + testing$sport1 + testing$sport2

testing$PA_min_wk<- rowSums ( cbind(testing$walking, testing$yard, testing$bike, testing$sport1, testing$sport2), na.rm=TRUE)


ids <- sample(unique(testing$id),1)
testing %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, 
                walking, yard, bike, sport1, sport2, PA_min_wk)

# PA_min_wk must be transformed to HOURS per week

testing$PA_hours_wk <- testing$PA_min_wk/60 

ids <- sample(unique(testing$id),1)
testing %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, 
                walking, yard, bike, sport1, sport2, PA_min_wk, PA_hours_wk)



describe(testing$PA_hours_wk) #1.19 hours_wk, 0-91 - map was about2 hours, 0-43
hist(testing$PA_hours_wk) # n= 28763

ds0$PA_hours_wk <- testing$PA_hours_wk
ds0 <- ds0[, -which(names(ds0) %in% c("biking_freq",  "biking_min",   "garden_freq",  "garden_min",   "heavyhh_freq",
                                      "heavyhh_min",  "lighthh_freq", "lighthh_min",  "sport1_freq",  "sport1_min",
                                      "sport2_freq",  "sport2_min", "walking_freq", "walking_min"))]
names(ds0)
describeBy(ds0$PA_hours_wk, ds0$wave) # n=4109 at each wave

# ---- stress -----------------------------------------------

# ---- pss ----
head(ds0$pss)

describeBy(ds0$pss, ds0$wave)

#only availavle at wave 7
ds0$pss[ds0$pss < 0] <-NA
describe(ds0$pss)


# ---- nle ----
names(ds0)
#nle 1 - 12

ids <- sample(unique(ds0$id),1)
ds0 %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, nle_1, nle_2, nle_3)       

test2<- ds0

#nle_1 father died
table(test2$nle_1)

#died, died earlier, NA:died earlier, not asked, not died, short version, short version Q, terminated
# therefore, died = 1, not died = 0, everything else =NA

test2$nle_1[test2$nle_1 == "died earlier"] <- "NA"
test2$nle_1[test2$nle_1 == "NA: died earlier"] <- "NA"
test2$nle_1[test2$nle_1 == "not asked"] <- "NA"
test2$nle_1[test2$nle_1 == "short version"] <- "NA"
test2$nle_1[test2$nle_1 == "short version Q"] <- "NA"
test2$nle_1[test2$nle_1 == "terminated"] <- "NA"

test2$nle_1[test2$nle_1 == "died"] <- 1
test2$nle_1[test2$nle_1 == "not died"] <- 0

table(test2$nle_1)

test2$nle_1<- as.numeric(test2$nle_1)


#nle_2 mother died
table(test2$nle_2) #died 93, not died 155

test2$nle_2[test2$nle_2 == "died earlier"] <- "NA"
test2$nle_2[test2$nle_2 == "NA: died earlier"] <- "NA"
test2$nle_2[test2$nle_2 == "not asked"] <- "NA"
test2$nle_2[test2$nle_2 == "short version"] <- "NA"
test2$nle_2[test2$nle_2 == "short version Q"] <- "NA"
test2$nle_2[test2$nle_2 == "terminated"] <- "NA"
test2$nle_2[test2$nle_2 == "do not know"] <- "NA"
test2$nle_2[test2$nle_2 == "died"] <- 1
test2$nle_2[test2$nle_2 == "not died"] <- 0

test2$nle_2<- as.numeric(test2$nle_2)

#nle_3 brother died

table(test2$nle_3)

test2$nle_3[test2$nle_3 == "answ:no brothers"] <-"NA"
test2$nle_3[test2$nle_3 == "na:no brothers"]   <-"NA"
test2$nle_3[test2$nle_3 == "NA:no brothers"]<-"NA"
test2$nle_3[test2$nle_3 == "no answer"]<-"NA"
test2$nle_3[test2$nle_3 == "short version"]<-"NA"
test2$nle_3[test2$nle_3 == "short version Q"]<-"NA"
test2$nle_3[test2$nle_3 == "terminated"]<-"NA"

test2$nle_3[test2$nle_3 == "yes"]<-1 
test2$nle_3[test2$nle_3 == "no"] <-0

table(test2$nle_3)
test2$nle_3<- as.numeric(test2$nle_3)

#nle4 sister died
table(test2$nle_4)
test2$nle_4[test2$nle_4 == "answ:no sisters"] <-"NA"
test2$nle_4[test2$nle_4 == "na:no sisters"]   <-"NA"
test2$nle_4[test2$nle_4 == "NA:no sisters"]<-"NA"
test2$nle_4[test2$nle_4 == "no answer"]<-"NA"
test2$nle_4[test2$nle_4 == "short version"]<-"NA"
test2$nle_4[test2$nle_4 == "short version Q"]<-"NA"
test2$nle_4[test2$nle_4 == "terminated"]<-"NA"

test2$nle_4[test2$nle_4 == "yes"]<-1 
test2$nle_4[test2$nle_4 == "no"] <-0

test2$nle_4<- as.numeric(test2$nle_4)

#nle5 son died
table(test2$nle_5)

test2$nle_5[test2$nle_5 == "answ:no sons"] <-"NA"
test2$nle_5[test2$nle_5 == "na:no sons"]   <-"NA"
test2$nle_5[test2$nle_5 == "NA:no sons"]<-"NA"
test2$nle_5[test2$nle_5 == "no answer"]<-"NA"
test2$nle_5[test2$nle_5 == "short version"]<-"NA"
test2$nle_5[test2$nle_5 == "short version Q"]<-"NA"
test2$nle_5[test2$nle_5 == "terminated"]<-"NA"

test2$nle_5[test2$nle_5 == "yes"]<-1 
test2$nle_5[test2$nle_5 == "no"] <-0

test2$nle_5<- as.numeric(test2$nle_5)

#nle 6 daughter died
table(test2$nle_6)
test2$nle_6[test2$nle_6 == "answ:no daughters"] <-"NA"
test2$nle_6[test2$nle_6 == "na:no daughters"]   <-"NA"
test2$nle_6[test2$nle_6 == "NA:no daughters"]<-"NA"
test2$nle_6[test2$nle_6 == "no answer"]<-"NA"
test2$nle_6[test2$nle_6 == "short version"]<-"NA"
test2$nle_6[test2$nle_6 == "short version Q"]<-"NA"
test2$nle_6[test2$nle_6 == "terminated"]<-"NA"

test2$nle_6[test2$nle_6 == "yes"]<-1 
test2$nle_6[test2$nle_6 == "no"] <-0

test2$nle_6<- as.numeric(test2$nle_6)

#nle 7  grandchild died
table(test2$nle_7)

test2$nle_7[test2$nle_7 == "no answer" ] <-"NA"
test2$nle_7[test2$nle_7 == "short version" ] <-"NA"
test2$nle_7[test2$nle_7 == "short version Q" ] <-"NA"
test2$nle_7[test2$nle_7 == "terminated" ] <-"NA"
test2$nle_7[test2$nle_7 == "no" ] <-0
test2$nle_7[test2$nle_7 == "yes" ] <-1

test2$nle_7<- as.numeric(test2$nle_7)

#nle8 ill partner
table(test2$nle_8)

test2$nle_8[test2$nle_8 == "technical problems"] <-"NA"
test2$nle_8[test2$nle_8 == "not asked"]   <-"NA"
test2$nle_8[test2$nle_8 == "NA:no partner"]<-"NA"
test2$nle_8[test2$nle_8 == "no answer"]<-"NA"
test2$nle_8[test2$nle_8 == "short version"]<-"NA"
test2$nle_8[test2$nle_8 == "short version Q"]<-"NA"
test2$nle_8[test2$nle_8 == "terminated"]<-"NA"

test2$nle_8[test2$nle_8 == "yes"]<-1 
test2$nle_8[test2$nle_8 == "no"] <-0

test2$nle_8<- as.numeric(test2$nle_8)


#nle9 ill relatives
table(test2$nle_9)


test2$nle_9[test2$nle_9 == "no answer"]<-"NA"
test2$nle_9[test2$nle_9 == "short version"]<-"NA"
test2$nle_9[test2$nle_9 == "short version Q"]<-"NA"
test2$nle_9[test2$nle_9 == "terminated"]<-"NA"

test2$nle_9[test2$nle_9 == "yes"]<-1 
test2$nle_9[test2$nle_9 == "no"] <-0

test2$nle_9<- as.numeric(test2$nle_9)

#nle_10
table(test2$nle_10) #victim of crime

test2$nle_10[test2$nle_10 == "no answer"]<-"NA"
test2$nle_10[test2$nle_10 == "short version"]<-"NA"
test2$nle_10[test2$nle_10 == "short version Q"]<-"NA"
test2$nle_10[test2$nle_10 == "terminated"]<-"NA"

test2$nle_10[test2$nle_10 == "yes"]<-1 
test2$nle_10[test2$nle_10 == "no"] <-0

test2$nle_10<- as.numeric(test2$nle_10)

#nle_11 conflict
table(test2$nle_11)

test2$nle_11[test2$nle_11 == "no answer"]<-"NA"
test2$nle_11[test2$nle_11 == "short version"]<-"NA"
test2$nle_11[test2$nle_11 == "short version Q"]<-"NA"
test2$nle_11[test2$nle_11 == "terminated"]<-"NA"

test2$nle_11[test2$nle_11 == "yes"]<-1 
test2$nle_11[test2$nle_11 == "no"] <-0

test2$nle_11<- as.numeric(test2$nle_11)

#nle_12 financial
table(test2$nle_12)


test2$nle_12[test2$nle_12 == "no answer"]<-"NA"
test2$nle_12[test2$nle_12 == "short version"]<-"NA"
test2$nle_12[test2$nle_12 == "short version Q"]<-"NA"
test2$nle_12[test2$nle_12 == "terminated"]<-"NA"

test2$nle_12[test2$nle_12 == "yes"]<-1 
test2$nle_12[test2$nle_12 == "no"] <-0

test2$nle_12<- as.numeric(test2$nle_12)


table(test2$widow)
#divorced, married, never marries, widowhood, registered, partnership

test2$widow = ifelse( test2$widow == "widowhood", 1, 0)

table(test2$relocate)
test2$relocate = ifelse( test2$relocate == "Yes", 1, 0)

str(test2)

test2$NLE_total<- rowSums ( cbind(test2$nle_1, test2$nle_2, test2$nle_3, test2$nle_4, test2$nle_5, test2$nle_6, test2$nle_7, test2$nle_8, test2$nle_9, test2$nle_10, test2$nle_11, test2$nle_12, test2$widow, test2$relocate), na.rm=TRUE)

describe(test2$NLE_total)


ds0$NLE_total <- test2$NLE_total


describe(ds0$NLE_total)
ds0 <- ds0[, -which(names(ds0) %in% c("relocate", "widow"))]

names(ds0)
# ---- other ------------------------------------------------ 

# ---- mastery ----

ds0$mastery5[ds0$mastery5 < 0] <-NA

#wave 1 - 4 only

describeBy(ds0$mastery5, ds0$wave)

# ---- self-efficacy ----

#wave 1-4 only

ds0$se[ds0$se < 0] <-NA

describe(ds0$se)
describeBy(ds0$se, ds0$wave)


# ---- save-to-disk ------------------------------------------------------------

# Save as a compressed, binary R dataset.  
# It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(ds0, file="./data/unshared/derived/lasa_2016/dto_trans.rds", compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/lasa_2016/dto_trans.rds")
names(dto)
# this is a flat data.frame containing combined variable


# a <- statsBy(data.subset, "ID", cors=T)
# print(a, short=F)











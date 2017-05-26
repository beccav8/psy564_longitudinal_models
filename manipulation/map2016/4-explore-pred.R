# # The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# # Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/map2016/Level1_models_full_workingmem.R",
#   output="./manipulation/map2016/output/level1_models_wm_full.md"
# )
# # The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!
# 
options(scipen=20)
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
path_input0  <- "./data/unshared/derived/map2016/map_full_bio_centered.rds" 

# ----- load-data ------
ds0  <- readRDS(path_input0) #total raw data  
names(ds0)
# str(ds0)
length(unique(ds0$id))
1367 + 485

#---variable information 

#--demographics
# Baseline measures
pathFileBL       <- file.path("./data/unshared/raw/dataset_484_basic_2016-09-09.csv")
BL_raw   <- read.csv(pathFileBL, stringsAsFactors = FALSE) %>% # baseline measure
  # dplyr::rename(id = projid)%>% 
  dplyr::select(-dplyr::ends_with(".1")) # remove duplicated variables
# longitudinal observations

#sex
table(BL_raw$msex)
#485 males
#1367 females

#education
mean(ds0$edu, na.rm=TRUE)
range(ds0$edu, na.rm=TRUE)
sd(ds0$edu, na.rm=TRUE)

describe(ds0$age_bl)
hist(ds0$age_bl)

#year in study--------------------------

range(ds0$year_in_study, na.rm=TRUE)  # 0 to 18
mean(ds0$year_in_study, na.rm=TRUE)   #4.06
describe(ds0$year_in_study)
#-----explore-physical-activity--


#pss
hist(ds0$pss) #relatively normal dist
sd(ds0$pss, na.rm=TRUE) #.83
describe(ds0$pss)
agostino.test(ds0$pss)
eq_0 <- as.formula("pss ~ 1 +            
                   (1  |id)")
0.07515 / (  0.07515 + 0.18618) #28%BP
model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))


#nle 

hist(ds0$nle) #relatively normal dist
sd(ds0$nle, na.rm=TRUE) #.83
describe(ds0$nle)
agostino.test(ds0$nle)

eq_0 <- as.formula("nle ~ 1 +            
                   (1  |id)")

model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))

#3339 obs

hist(ds0$nle_wp) #relatively normal dist

table(ds0$nle)
describeBy(ds0$nle, ds0$nle)

hist(ds0$nle_pmeanC)
hist(ds0$phys_pmeanC)
hist(ds0$phys_wp)
hist(ds0$physical_activity)

hist(ds0$sdmt)



# n = 3339 observations, 1007 unique id's
# n for total participants = 11672

describeBy(ds0$nle, ds0$year_in_study)

ids <- sample(unique(ds0$id),15)
test<- ds0 %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,nle)

describeBy(test$nle, test$id)

eq_1 <- as.formula("nle ~ year_in_study + 1 +
                   (1  |id)")

model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))
#NLE does not change over time 

#PA
describe(ds0$physical_activity)
qplot(ds0$physical_activity,
      geom="histogram",
      binwidth = 0.1,
      xlim=c(0,20),
      ylim=c(0,500))

eq_0 <- as.formula("physical_activity ~ 1 +
                   (1  |id)")

model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE)
lmerTest::summary((model_ucm))

5.627 / (5.627+6.741) #45% BP

eq_1 <- as.formula("physical_activity ~ year_in_study + 1 +
                   (1  |id)")

model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))
#PA decreases with time 


ids <- sample(unique(ds0$id),15)
test<- ds0 %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,physical_activity)

describeBy(test$physical_activity, test$id)




# ds0$physical_activity1<-(ds0$physical_activity) + 0.1  

# ds0$physLOG<- log(ds0$physical_activity1)
# hist(ds0$physLOG)
# qplot(ds0$physLOG,
#       geom="histogram",
#       binwidth = 0.1,
#       xlim=c(-3,4))
# hist(ds0$physLOG)
# 
# agostino.test(ds0$physLOG)



#sdmt  ------- perceptual speed

hist(ds0$sdmt) 
sd(ds0$sdmt, na.rm=TRUE) #13.12
describe(ds0$sdmt) # mean= 35.75, sd=13.13,  min=0 max=77
agostino.test(ds0$sdmt)

d<-ds0
d$sdmt <- d$sdmt/2 
hist(d$sdmt) 
describe(d$sdmt) # mean= 35.75, sd=13.13,  min=0 max=77

eq_0 <- as.formula("sdmt ~ 1 +            
                   (1  |id)")
model_ucm<- lmerTest::lmer(eq_0, data=d, REML= FALSE) 
lmerTest::summary((model_ucm))
137.91 / (137.91+49.81) #73%


#dig_b --- working memory
hist(ds0$dig_b) 
sd(ds0$dig_b, na.rm=TRUE) #2.181367
describe(ds0$dig_b) # mean= 5.92, sd=2.18,  min=0  max=12
agostino.test(ds0$dig_b)

eq_0 <- as.formula("dig_b ~ 1 +            
                   (1  |id)")
model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))
2.804 / (2.804+2.086) #57%


describe(ds0$wl_im)



#--- graphs

ds0$msexg<-as.character(ds0$msex)

ds0$msexg <- ifelse(ds0$msexg >=1, 
                    c("Male"), c("Female")) 
library(reshape)
ds0 <- rename(ds0, c(msexg="Sex"))

set.seed(1)
ids <- sample(ds0$id,20)
graph_sample <- ds0 %>%  dplyr::filter( id %in% ids)
length(unique(graph_sample$id))




#indiviudal growth plots-------

#percep_speed-sdmt
indwm<- ggplot(graph_sample, aes(x= year_in_study, y= sdmt)) +geom_point()
indwm + facet_wrap(~id, nrow=4) +
  stat_smooth(method=lm, se=TRUE)+
  theme1
#working memory
indwm<- ggplot(graph_sample, aes(x= year_in_study, y= dig_b)) +geom_point()
indwm + facet_wrap(~id, nrow=4) +
  stat_smooth(method=lm, se=TRUE)+
  theme1

# over time -----------

g1<- ggplot2::ggplot(ds0, aes_string(x= "year_in_study", y="sdmt", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g1 <- g1 + labs(list(
  # title= "Changes in Working Memory Over Time, by Gender",
  x="Year in Study", y="perceptual Speed"))
g1

g2<- ggplot2::ggplot(ds0, aes_string(x= "year_in_study", y="dig_b", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g2 <- g2 + labs(list(
  # title= "Changes in Working Memory Over Time, by Gender",
  x="Year in Study", y="Working Memory"))
g2

g3<- ggplot2::ggplot(ds0, aes_string(x= "year_in_study", y="pss", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g3 <- g3 + labs(list(
  # title= "Changes in Working Memory Over Time, by Gender",
  x="Year in Study", y="percevied stress"))
g3

g4<- ggplot2::ggplot(ds0, aes_string(x= "year_in_study", y=" ", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g4 <- g4 + labs(list(
  # title= "Changes in Working Memory Over Time, by Gender",
  x="Year in Study", y="physical Activity"))
g4

####-----BP effects----

#phys BP
g1<- ggplot2::ggplot(ds0, aes_string(x= "phys_bp", y="sdmt")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g1 <- g1 + labs(list(
  # title= "Changes in PS Over Time, by Gender",
  x="PA_WP", y="sdmt"))
g1

g2<- ggplot2::ggplot(ds0, aes_string(x= "phys_bp", y="dig_b")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g2 <- g2 + labs(list(
  # title= "Changes in PS Over Time, by Gender",
  x="PA_WP", y="working memory"))
g2

#pss BP

g3<- ggplot2::ggplot(ds0, aes_string(x= "pss_bp", y="sdmt")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g3 <- g3 + labs(list(
  # title= "Changes in PS Over Time, by Gender",
  x="PSS", y="sdmt"))
g3

g4<- ggplot2::ggplot(ds0, aes_string(x= "pss_bp", y="dig_b")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g4 <- g4 + labs(list(
  # title= "Changes in PS Over Time, by Gender",
  x="PSS", y="working memory"))
g4



####-----WP effects----

#phys WP

g1<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="sdmt")) +
  geom_point(shape=4, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g1 <- g1 + labs(list(
  title= "Coupled Change between Physical Activity and Symbol Digit Modality",
  x="Physical Activity (WP)", y="SDMT"))
g1

g2<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="dig_b")) +
  geom_point(shape=4, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g2 <- g2 + labs(list(
  title="Coupled Change between Physical Activity and Digit Span Backward",
  x="Physical Activity", y="Digit Span Backward"))
g2

multiplot(g1, g2)

#pss WP

g3<- ggplot2::ggplot(ds0, aes_string(x= "pss_wp", y="sdmt")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g3 <- g3 + labs(list(
  # title= "Changes in PS Over Time, by Gender",
  x="PSS", y="sdmt"))
g3

g4<- ggplot2::ggplot(ds0, aes_string(x= "pss_wp", y="dig_b")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g4 <- g4 + labs(list(
  # title= "Changes in PS Over Time, by Gender",
  x="PSS", y="working memory"))
g4


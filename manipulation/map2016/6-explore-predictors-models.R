# # The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# # Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/map2016/Level1_models_full_workingmem.R",
#   output="./manipulation/map2016/output/level1_models_wm_full.md"
# )
# # The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!
#

# ----- load-source ------

rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(lmerTest)
library(outliers)



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
str(ds0)

#demographics--------

pathFileBL       <- file.path("./data/unshared/raw/map/dataset_484_basic_2016-09-09.csv")

# check if the files exist
testit::assert("File does not exist", base::file.exists(pathFileBL))

BL_raw   <- read.csv(pathFileBL, stringsAsFactors = FALSE) %>% # baseline measure
  # dplyr::rename(id = projid)%>%
  dplyr::select(-dplyr::ends_with(".1")) # remove duplicated variables
# longitudinal observations
table(BL_raw$msex)

mean(data$edu, na.rm=TRUE)
range(data$edu, na.rm=TRUE)
sd(data$edu, na.rm=TRUE)


length(unique(ds0$id))  #1853 participants


#time metric---------

mean(ds0$age_at_visit, na.rm=TRUE)
summary(ds0$age_at_visit)
sd(ds0$age_at_visit, na.rm=TRUE)

range(ds0$year_in_study, na.rm=TRUE)  # 0 to 18
mean(ds0$year_in_study, na.rm=TRUE)
sd(ds0$year_in_study, na.rm=TRUE)


# --- outcomes -------

#WM

hist(ds0$wm) #relatively normal dist
sd(ds0$wm, na.rm=TRUE) #.83

describe(ds0$wm)
#vars     n  mean   sd median trimmed  mad   min  max  range  skew kurtosis   se
 # 1   11254 -0.07 0.87  -0.02   -0.04 0.75 -3.57 2.69  6.26 -0.47     1.06 0.01


hist(ds0$percep_speed) #relatively normal dist
sd(ds0$percep_speed, na.rm=TRUE) #.83

describe(ds0$percep_speed)
# --- predictors ---------

#Physical Activity ------
hist(ds0$physical_activity) #skewed


summary(ds0$physical_activity)
sd(ds0$physical_activity, na.rm=TRUE)

describe(ds0$physical_activity)
#vars     n mean  sd median trimmed   mad   min    max   range  skew kurtosis   se
# 1   11233 2.94  3.5      2    2.32  2.72   0    43.75  43.75  2.62    11.9   0.03

describeBy(ds0$physical_activity, group=ds0$year_in_study)
#data decreases linearly over time

eq_p <- as.formula("physical_activity ~ 1 +
                   ( 1 |id)")
model_p<- lmerTest::lmer(eq_p, data=ds0, REML= FALSE)
lmerTest::summary((model_p))
fit2<-model_p
#ICC PA
(5.627) / (5.627 + 6.741) # 45% is between person

eq_p <- as.formula("physical_activity ~ 1 + year_in_study +
                   ( 1 + year_in_study |id)")
model_p<- lmerTest::lmer(eq_p, data=ds0, REML= FALSE)
lmerTest::summary((model_p))
fit2<-model_p
#ICC PA
(7.42752 + 0.04915) / (6.10546 + 7.42752 + 0.04915) # 55% is between person
#about 45 % is due to time-varying variation in the variable (WITHIN PERSON)




#stress ----

sd(ds0$pss, na.rm=TRUE)
summary(ds0$pss_wp)

hist(ds0$pss)

describeBy(ds0$pss, group=ds0$year_in_study)
#majority of data is between wave 2 and 9

table(ds0$pss_wp, na.rm=FALSE)
eq_s <- as.formula("pss ~ 1 +
                   ( 1 |id)")
model_s<- lmerTest::lmer(eq_s, data=ds0, REML= FALSE)
lmerTest::summary((model_s))
fit2<-model_s
0.07515/ (0.07515+0.18618)

# #stress
# eq_s <- as.formula("pss ~ 1 + year_in_study +
#                    ( 1 + year_in_study |id)")
# model_s<- lmerTest::lmer(eq_s, data=ds0, REML= FALSE)
# lmerTest::summary((model_s))
# fit2<-model_s
# #ICC stress
# (0.0603227 + 0.0001316) / (0.1853645 + 0.0603227 + 0.0001316 ) # 24% is explained between person
# #76% is explained within person (i.e. deviations from their own mean trajectories)


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

length(unique(ds0$id))
#1853
str(ds0)


length(unique(ds0$id))

mean(ds0$age_at_visit, na.rm=TRUE)
summary(ds0$age_at_visit)
sd(ds0$age_at_visit, na.rm=TRUE)

range(ds0$year_in_study, na.rm=TRUE)  # 0 to 18
mean(ds0$year_in_study, na.rm=TRUE)
sd(ds0$year_in_study, na.rm=TRUE)


# --- predictors ---------


range(ds0$wm, na.rm=TRUE)  #-3.57 to 2.34
range(ds0$pss, na.rm=TRUE) # 0 - 3.75
range(ds0$physical_activity, na.rm=TRUE) #0 to 42.75


hist(ds0$wm) #relatively normal dist


hist(ds0$pss)
hist(ds0$pss_pmean) #normal dist

hist(ds0$physical_activity) #skewed
hist(ds0$phys_pmean)#positive skew, more justifiable dichotomization 
summary(ds0$phys_pmean)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.000   1.153   2.250   2.924   3.938  25.380       6 



##--- explore predictors
#PA
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

#is there significant fluctuation from individual trajectories over time?
#wald test of residual effects = coef/ se, but this is on a t distribution

# #z distribution
# 6.10/2.47 #= .9932
# 1-.9932 #=.0068 the likelihood of this being due to chance is .68%, therefore its significant, p<.05


#stress
eq_s <- as.formula("pss ~ 1 +          
                   ( 1 |id)")
model_s<- lmerTest::lmer(eq_s, data=ds0, REML= FALSE) 
lmerTest::summary((model_s))
fit2<-model_s
0.07515/ (0.07515+0.18618)

#stress
eq_s <- as.formula("pss ~ 1 + year_in_study +          
                   ( 1 + year_in_study |id)")
model_s<- lmerTest::lmer(eq_s, data=ds0, REML= FALSE) 
lmerTest::summary((model_s))
fit2<-model_s
#ICC stress
(0.0603227 + 0.0001316) / (0.1853645 + 0.0603227 + 0.0001316 ) # 24% is explained between person
#76% is explained within person (i.e. deviations from their own mean trajectories)


#SE=
.4305/ sqrt(3327)
.1853/.00746

.4305/ sqrt(1006)
.1853/.013573

# #coef/stdeviation
# 0.1853645/0.43054 #= .4305 = .6664
# 1-.6664 #= 0.336 NS fluctuation over time 

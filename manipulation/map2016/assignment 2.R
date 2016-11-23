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
library(psych)



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
# names(ds0)
# str(ds0)

ds0$msexg<-as.character(ds0$msex)

set.seed(1)
ids <- sample(ds0$id,20)
graph_sample <- ds0 %>%  dplyr::filter( id %in% ids)
length(unique(graph_sample$id))


#indiviudal growth plots-------

#wm
indwm<- ggplot(graph_sample, aes(x= year_in_study, y= wm)) +geom_point()
indwm + facet_wrap(~id, nrow=4) +
 stat_smooth(method=lm, se=TRUE)+
theme1

#PA
indphys<- ggplot(graph_sample, aes(x= year_in_study, y= physical_activity)) +geom_point()
indphys + facet_wrap(~id, nrow=4) +
stat_smooth(method=lm, se=TRUE)+
theme1

#pss
indpss<- ggplot(graph_sample, aes(x= year_in_study, y= physical_activity)) +geom_point()
indpss + facet_wrap(~id, nrow=4) +
  stat_smooth(method=lm, se=TRUE) +
  theme1

##### average over time

source("./scripts/multiplot-function.R")
source("./scripts/graph_themes.R")

str(ds0$msexg)
table(ds0$msexg)

ds0$msexg <- ifelse(ds0$msexg >=1, 
                        c("Male"), c("Female")) 
library(reshape)
ds0 <- rename(ds0, c(msexg="Sex"))

g1<- ggplot2::ggplot(ds0, aes_string(x= "year_in_study", y="wm", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g1 <- g1 + labs(list(
  # title= "Changes in Working Memory Over Time, by Gender",
  x="Year in Study", y="Working Memory"))
g1


g2<- ggplot2::ggplot(ds0, aes_string(x= "year_in_study", y="pss", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g2 <- g2 + labs(list(
  # title= "PSS Score Over Time",
  x="Year in Study", y="PSS Score"))
g2


g3<- ggplot2::ggplot(ds0, aes_string(x= "year_in_study", y="physical_activity", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g3 <- g3 + labs(list(
  # title= "Physical Activity Over Time",
  x="Year in Study", y="Physical Activity Score"))
g3



over_time<- multiplot(g1, g2, g3) 
          
g4<- ggplot2::ggplot(ds0, aes_string(x= "year_in_study", y="phys_wp", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g4 <- g4 + labs(list(
  # title= "Person centered Physical Activity Over Time",
  x="Time", y="Person mean centered Physical Activity"))
g4

g5<- ggplot2::ggplot(ds0, aes_string(x= "year_in_study", y="pss_wp", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g5 <- g5 + labs(list(
  # title= "Person centered Perceived Stress Over Time",
  x="Time", y="Person mean centered Percevied Stress"))
g5

# ds0<- subset(ds0, phys_wp < 20)
# ds0<- subset(ds0, phys_wp > -10)
# ds0<- subset(ds0, phys_wp < 2) 

g6<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="pss_wp", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g6 <- g6 + labs(list(
  # title= "Coupled Change of PA and PSS",
  x="Within Person PA", y="Within person PSS"))
g6


within_person<- multiplot(g4, g5, g6) 



g7<- ggplot2::ggplot(ds0, aes_string(x= "phys_pmeanC", y="pss_pmeanC", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g7 <- g7 + labs(list(
  # title= "Coupled Change of PA and PSS",
  x="Within Person PA", y="Within person PSS"))
g7


#--models


#WM
eq <- as.formula("wm ~ 1 + year_in_study +          
                   ( 1 + year_in_study |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#df= 11248 
#dev =  18865.4
##---------------------------------

#AGE BL-------------
# (mean(ds0$age_bl_gmc, na.rm=TRUE))

eq2 <- as.formula("wm ~ 1 + year_in_study*age_bl_gmc + 
                  ( 1 + year_in_study |id)")
model_2<- lmerTest::lmer(eq2, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))

#int = y00 -  mean WM score when age_bl = 0 *i.e. the mean
#age_bl_gmc = y01 - when time =0, for every increase in BL age, wm score decreases this much 

#year_in_study = y10 - average slope controlling for X (when X = 0, the mean, sig decline over time)
#year X agebl = y11- effect of bl age on the slope. That is, for every year older at BL, people sig decline faster

#chi sq
#df
11248 - 11246 # (2)
18865.4- 18771.6

#SE = SD/ sqrt(n)
#int
#0.519149
(0.72052   / sqrt(11254))

#year in study
#0.005981
(0.07734 / sqrt(11254))
#resid 
# 0.176600 
(0.42024  / sqrt(11254))

################ + gender

eq3 <- as.formula("wm ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + 
                   ( 1 + year_in_study |id)")
model_3<- lmerTest::lmer(eq3, data=ds0, REML= FALSE) 
lmerTest::summary((model_3))


#msex - y02 = y00 (is now the int for sex = 0/female), thus y02 is the effect of sex on intercept for a 1 unit 
                #increase in sex (male). males have an intercept of 
                .0654 - .056  #=.0094 is the mean for males at baseline 
#year X msex = y10 (is now the slope for females)
              #y12 (is the effect of sex on female slope, that is, males decline less than females, NS)
                -.066 + .0095 #= 0.057

 
  #chi sq

18771.6-18768.3
#df 
11246 - 11244 
  
  #SE = SD/ sqrt(n)
  #int
  #0.518444
0.72003    / sqrt(11254)
  #year in study
#0.005958
0.07718/ sqrt(11254)
  #resid 
#0.176624
  0.42027 / sqrt(11254)



################# + education 

  
eq4 <- as.formula("wm ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu + 
                    ( 1 + year_in_study |id)")
model_4<- lmerTest::lmer(eq4, data=ds0, REML= FALSE) 
lmerTest::summary((model_4))

#chi sq
#df
11244-11242
18768.3 - 18577.5

#SE = SD/ sqrt(n)
#int
#0.468773
0.6847   / sqrt(9694)
#year in study
# 0.005027
0.0709 / sqrt(9694)
#resid 
# 0.171970
0.4147 / sqrt(9694)


#Physical Activity --------------
eq5 <- as.formula("wm ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu + phys_pmeanC + phys_wp +
                    ( 1 + year_in_study |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))
#chi sq
#df
9682 - 9680
16182.9  - 16156.7 

#SE = SD/ sqrt(n)
#int
#0.468612
0.6846   / sqrt(9694)
#year in study
# 0.004956
0.0704 / sqrt(9694)
#resid 
#  0.171644
0.4143 / sqrt(9694)




# gender X PA 
eq6 <- as.formula("wm ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu + phys_pmeanC*msex + phys_wp*msex +
                    ( 1 + year_in_study |id)")
model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE) 
lmerTest::summary((model_6))  


#chi sq
#df
9680 - 9678  
16156.7 - 16156.1
#NS

#SE = SD/ sqrt(n)
#int
#0.468498
0.68447  / sqrt(9678)
#year in study
# 0.004954
0.07039/ sqrt(9678)
#resid 
# 0.171646
0.41430 / sqrt(9678)


#---- PSS and interaction



#Physical Activity --------------
eq7 <- as.formula("wm ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu + phys_pmeanC + pss_pmeanC + phys_wp*pss_pmeanC +
                    ( 1 + year_in_study |id)")
model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
lmerTest::summary((model_7))
#chi sq
#df
#df from model 5 versus 7
9680 - 7095  #2585
16156.7 - 10813.6  #5343.1

#SE = SD/ sqrt(n)
#int
#0.364853
0.60403   / sqrt(7111)
#year in study
# 0.003923
0.06263 / sqrt(7111)
#resid 
#0.162825
0.40352 / sqrt(7111)







#--old

#PSS--------------------------

eq0 <- as.formula("wm ~ 1 + year_in_study*pss_pmean  + 
                   ( 1 + year_in_study |id)")
model_0<- lmerTest::lmer(eq0, data=ds0, REML= FALSE) 
lmerTest::summary((model_0))


#y00=intercept-average intercept when pss_mean is 0 (i.e. the mean)
#y01=pss_pmeanC - average dif in 1 unit increase in mean pss on intercept 
     #sig, ie. for every unit increase in mean stress, wm increases by .1 at baseline


#y10=year in study - average slope when pss_mean is 0
  #sig decline over time for people who have never been exposed to stress
#y11=year_in_study:pss_pmeanC - average difference in 1 unit increase in mean pss per unit increase in time
  # for every unit increase in PSS, people decline less (.0086, NS)


#wrong below?
#y00=intercept-average intercept when pss_mean is 0 (i.e. the mean)
#y01=year_in_Study - average dif in 1 unit increase in mean pss on intercept

#y10=pss_pmeanC - average slope when pss_meanC is 0
#y11=year_in_study:pss_pmeanC - average difference in 1 unit increase in mean pss per unit increase in time




eq1 <- as.formula("wm ~ 1 + year_in_study*pss_pmeanC  + pss_wp +
                   ( 1 + year_in_study |id)")
model_1<- lmerTest::lmer(eq1, data=ds0, REML= FALSE) 
lmerTest::summary((model_1))



## physical activity -----------------------

eq_c <- as.formula("wm ~ 1 + year_in_study*phys_pmeanC  + phys_wp +    
                   ( 1 + year_in_study |id)")
model_2<- lmerTest::lmer(eq_c, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))

#timeXphys_mean -y10 - the average difference in 1 unit increase in PAmean, per unit increase in time.
                       #-  WM score increases by .006 for every unit increase in PA (-.006 intercept) per unit time




ds0$episodic

library(psych)

describeBy(ds0$pss, ds0$year_in_study)

eq_c <- as.formula("episodic ~   year_in_study*pss_pmean + pss_pmean*phys_wp +
                   (  year_in_study |id)")
model_2<- lmerTest::lmer(eq_c, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))



eq_c <- as.formula("wm ~   year_in_study*pss_pmean  +     
                   (  year_in_study |id)")
model_2<- lmerTest::lmer(eq_c, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))
























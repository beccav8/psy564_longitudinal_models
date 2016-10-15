# 
# # The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# # Run the lines below to stitch a basic html output. 
knitr::stitch_rmd(
  script="./sandbox/map/mmse-map-LH.R",
  output="./sandbox/map/stitched-output/mmse-map-LH.md"
)
## The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!

#how do i make stiched output work? 

# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(lmerTest)
library(outliers)
# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R")
source("./scripts/general-graphs.R")  #in scripts folder
source("./scripts/specific-graphs.R")
source("./scripts/specific-graphs-pred.R")
source("./scripts/graphs-pred.R")
source("./scripts/graphs-predVID.R")
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
# ---- declare-globals ---------------------------------------------------------
# path_input  <- "./data/unshared/derived/dto.rds" 
# path_input  <- "./data/unshared/derived/dAL.rds"  
path_input  <- "./data/unshared/derived/map/data.rds"                          
# figure_path <- 'manipulation/stitched-output/te/'

# ---- load-data ---------------------------------------------------------------
ds <- readRDS(path_input)
dim(ds)
str(ds)
# head(ds)
# ---- specify the models-------------------------

source("./scripts/functions-for-glm-models.R")

# ---- subset---------------------------------------
#phys_bl_bp = each persons baseline score - the baseline grand mean (between person baseline compare)
#phys_bp = the persons score at time j - their mean
#phys_wp = that persons score at time j, minus their mean (deviations-from--own-mean)
#mmse_wp = the persons mmse score at time j, minus their mean, so that a positive value indicated scores higehr then their mean 
#biomarker_high = indicates if the person's mean on that particular biomarker is above the 3rd quartile

#subset
dwn <- ds %>% 
  dplyr::select(id, full_year, age_bl, al_count, phys_bl_bp, phys_bp, phys_wp, mmse, mmse_wp, age_bl_centered, female, edu) %>% 
  na.omit()
#19809 observations


#subset for biomarker models
dbio<- ds %>% 
  dplyr::select(id, full_year, age_bl, phys_bl_bp, phys_bp, phys_wp, mmse, mmse_wp, age_bl_centered, female, edu, al_count,  
                apoe, cholesterol, hemoglobin, hdlratio, hdl,ldl, glucose, creatine,  #raw scores                  
                cholesterol_HIGH,  hemoglobin_HIGH,  hdlratio_HIGH, hdl_LOW,ldl_HIGH, glucose_HIGH,creatine_HIGH) %>% 
  na.omit()
#4082 observations


# physical activity question: hours per week that the ppt. engages in 5 different categories   


# ---- quality check variables ----------------------------------------------

#mmse
boxplot(dwn$mmse)
# bpmmse<-boxplot(dwn$mmse)
hist(dwn$mmse)
# t(table(dwn$mmse, useNA = "always"))
#not normal dist

# mmse-at-baseline
# 
# d2<- subset(dwn, full_year==0, select=c(  )) 

#age at bsline 

boxplot(dwn$age_bl_centered)
# bp<-boxplot(dwn$age_bl_centered)
# bp$out 
hist(dwn$age_bl_centered)
#looks ok
#physical activity
#phys_bp
boxplot(dwn$phys_bp)
bp<-boxplot(dwn$phys_bp)
# bp$out 
hist(dwn$phys_bp)
#skweded right

#phys_bp
boxplot(dwn$phys_wp)
hist(dwn$phys_wp)
#looks okay, people are more likely to be lower then their mean

#biomarkers
hist(dbio$apoe) #?

hist(dbio$cholesterol) #normal
hist(dbio$hemoglobin)#heavy at mean
# is a form of hemoglobin (a blood pigment that carries oxygen) that is bound to glucose.
#High HbA1c levels indicate poorer control of diabetes
#not affected by short-term fluctuations in blood glucose concentrations
#6.5% signals that diabetes is present
hist(dbio$hdlratio)#normal (high ratio is bad)
hist(dbio$hdl)#normal
hist(dbio$ldl)#normal
hist(dbio$glucose)#skewed right
hist(dbio$creatine) #skewed right, heavy at low end   
#index of kidney function, high is bad (influenced by muscle tissue, therefore gender may confound this)

plyr::count(dbio, 'cholesterol_HIGH') 
plyr::count(dbio, 'hemoglobin_HIGH') 
plyr::count(dbio, 'hdl_LOW') 
plyr::count(dbio, 'ldl_HIGH')
plyr::count(dbio, 'glucose_HIGH')
plyr::count(dbio, 'creatine_HIGH')
plyr::count(dbio, 'hdlratio_HIGH') 
#(about the same ratio for all biomarkers)


# ---- obtain-multivariate-counts ---------------------------------------
computed_variables <-  c("glucose_HIGH", "cholesterol_HIGH", "hemoglobin_HIGH", "hdlratio_HIGH", "hdl_LOW", "ldl_HIGH","creatine_HIGH" ) 
bio<- dbio %>% 
  # dplyr::group_by(glucoseHIGH,hgba1cHIGH) %>% 
  dplyr::group_by_(.dots = computed_variables) %>% 
  dplyr::summarize(count =n())

#no NA's

##### Andrey's edits begin #################


# ## Outcome
#  cognition, operationalized by mmse score
# 
# ## Temporal
# - full_year - wave
# 
# ## Confounder
# - age_bl_centered - age at baseline centered at mean (M=79)
# - female 
# - edu
# 
# ## Exposure
# - physical_activity - hours per week engaged in physical activity
# - phys_bp - person's score on physical activity relative to the grand mean (between persons)
# - phys_wp - person's score on physical activity (on particular occasion) relative to the personal mean across occasions.

# ---- model-specification -------------------------
local_stem <- "dv ~ 1 + full_year "
# pooled_stem <- paste0(local_stem, "study_name_f + ") 

predictors_A <- "
age_bl_centered + 
female + 
edu 
" 

predictors_AA <-  "
age_bl_centered + 
female + 
edu +

age_bl_centered:female +
age_bl_centered:edu +
female:edu
"

predictors_B <- "
age_bl_centered + 
female + 
edu +
physical_activity +
phys_bp +
phys_wp 
"

predictors_BB <-  "
age_bl_centered + 
female + 
edu +
physical_activity +
phys_bp +
phys_wp +

age_bl_centered:female + 
age_bl_centered:edu + 
age_bl_centered:physical_activity + 
age_bl_centered:phys_bp + 
age_bl_centered:phys_wp + 

female:edu +
female:physical_activity + 
female:phys_bp + 
female:phys_wp + 

edu:physical_activity +
edu:phys_bp +
edu:phys_wp +

physical_activity:phys_bp +
physical_activity:phys_wp +

phys_bp:phys_wp +
phys_wp
" 
# ---- define-modeling-functions -------------------------
source("./scripts/modeling-functions.R")

predictors_A <- "age_bl_centered + female + edu"
model_A <- estimate_local_model(ds, "mmse", predictors_A)
summary(model_A)

# ---- equations ------------------------------------------------------
eq_0 <- as.formula("mmse ~ 1 + full_year +             
                   (1 + full_year |id)")
#does the outcome change over time? i.e. if so, it makes sense to add predictors.

eq_1 <- as.formula("mmse ~ 1 + full_year + age_bl_centered +                
                   (1 + full_year |id)")

eq_2 <- as.formula("mmse ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered +             
                   (1 + full_year |id)")

eq_3 <- as.formula("mmse ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + phys_bp +
                   (1 + full_year |id)")
#bp effects: are people with higher values than other people (on average, over time)
#also higher in mmse (on average over time) (hoffman pdf page 72)

eq_4 <- as.formula("mmse ~ 1 + full_year + age_bl_centered  + phys_bp + phys_wp + 
                   (1 + full_year |id)")
#wp effects: if you have higher values than usual (your own mean) at this occasion, do you also have a higher outcome
#value than usual at this occasion ? (hoffman pdf page 72)
# main effect of time: slope term- yearly rate of change in mmse, controlling for exercise 

#(hoffman page 75)
#- if only bp : induvidual slopes dont differ sig
#- if only wp sig : induvidual slopes are sig increasing or decreasing 

#(note)
#when you add a time varying covariate (singer and willett page 170)
#i-the intercept parameter or FE is now the value of the outcome when all Level 1 predictors (including time) are zero
#ii-the slope is now a conditional rate of change, controlling for the effects of the TVC 
#this changes the meaning of the Level-2 varience components, so comparing them across sucessive models makes no sense
#we can now only use  the F.E and goodness of fit stats to compare these models

eq_4es <- as.formula("mmse ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + phys_bp + phys_wp + edu + female +
                     (1 + full_year |id)")
#gender effects:
#main effect of time in study: when baseline age is at the mean (i.e. 0)
#main effects of age_bl: when year in study is 0 (i.e. at baseline)
#interactioin: i don't think this interaction is meaningful, this part of the model probably is not necessary 

# 
# eq_5 <- as.formula("mmse ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + phys_bp + phys_wp + edu + female + female:phys_bp + female:phys_wp +
#                    (1 + full_year |id)") 
# no gender differences in the effects of phys activity 

# ---- models ------------------------------------------------------
model_0<- lmerTest::lmer(eq_0, data=dwn, REML=TRUE) 
model_1<- lmerTest::lmer(eq_1, data=dwn, REML=TRUE) 
model_2<- lmerTest::lmer(eq_2, data=dwn, REML=TRUE) 
model_3<- lmerTest::lmer(eq_3, data=dwn, REML=TRUE) 
model_4<- lmerTest::lmer(eq_4, data=dwn, REML=TRUE) 
model_4es<- lmerTest::lmer(eq_4es, data=dwn, REML=TRUE) 
# model_5<- lmerTest::lmer(eq_5, data=dwn, REML=TRUE) 


options(scipen = 11)

lmerTest::summary((model_0))
broom::tidy(model_0, data=dwn)
lmerTest::summary((model_1))
lmerTest::summary((model_2))
lmerTest::summary((model_3))
lmerTest::summary((model_4))
lmerTest::summary((model_4es)) 
# lmerTest::summary((model_5)) 

anova(model_3, model_4, model_4es)
# because we only can use goodness of fit measures now to compare (not varience component)
#it seems like model 4 (without gender/educ) is the best fit


#allowing the effect of the TVC to vary over tiem (interaction term) -page 171 singer and willett
eq_tv_intxn <- as.formula("mmse ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + phys_bp + phys_wp + edu + female + phys_wp:full_year +
                          (1 + full_year |id)")
model_tv_intx<- lmerTest::lmer(eq_tv_intxn, data=dwn, REML=TRUE) 
lmerTest::summary((model_tv_intx))

#should i exclude gender/edu? -lose bp effects??
eq_tv_intxn <- as.formula("mmse ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + phys_bp + phys_wp + phys_wp:full_year +
                          (1 + full_year |id)")
model_tv_intx<- lmerTest::lmer(eq_tv_intxn, data=dwn, REML=TRUE) 
lmerTest::summary((model_tv_intx))
#singer and willet page 171:
#subscript j-occasion (phys_wp_ij) is what differentiates the a TVC from a TIVC
##can interpret interactin in 2 ways 
#i- the effect of within person changes in physical activity on mmse varies over time - (it fluctuates?)
#ii- the rate of change in mmse differs as a function of within person changes in PA **
#in this model it is sig- those who walk more than their average decline less over time

# hypothetical dichotomizing graph to understand the interaction 

dtemp<-dwn
summary(dtemp$phys_wp)
hist(dtemp$phys_wp)



dtemp$pred<- predict(model_tv_intx)

for(i in unique(dtemp$id)) {
  for (j in 1:length(dtemp$phys_wp[dtemp$id==i])) {
    
    if (isTRUE(dtemp$phys_wp[dtemp$id==i][j] > 0.77 )) {
      dtemp$phys_wp_dichot[dtemp$id==i][j] <- "HIGH" }
    
    else if (isTRUE(dtemp$phys_wp[dtemp$id==i][j] < (-1.1 ))){
      dtemp$phys_wp_dichot[dtemp$id==i][j] <- "LOW"  
  }
#     else {
#       dtemp$phys_wp_dichot[dtemp$id==i][j] <- "NA" 
#     } 
    }} 

ids <- sample(unique(dtemp$id),1)
dtemp %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,phys_wp_dichot,phys_wp)

set.seed(1)
ids <- sample(dtemp$id,100)
d <- dtemp %>%  dplyr::filter( id %in% ids)


g<- ggplot2::ggplot(d, aes_string(x= "full_year", y="pred", colour="phys_wp_dichot")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)+
  main_theme
# stat_smooth(method=lm, level=0.95, fullrange=FALSE)
g <- g + labs(list(
  title="interaction",
  x="years_in_study", y="mmse_predictions"))

g

#those who walk higher than their average also have higher mmmse scores at that time, and decline less
#those who walk lower than average have lower mmse scores over time, and decline at a faster rate 

#---------------------------------------------------------------------------------------
#using mmse deviation from mean as outcome  ### thus, those who score higher than their average on physical activity, 
#also score higher than their average mmse (but not higher mmse than the average population, those wp effects were not sig)
#but no between person effects, i.e. those who generally walk higher than the average person don't have higher mmse
#scores than their average (which make sense because it's relative to themselves)
#############################################################################################
eq_test <- as.formula("mmse_wp ~ 1 + full_year +  phys_wp + phys_bp +
                      (1 + full_year |id)")
model_test<- lmerTest::lmer(eq_test, data=dwn, REML=TRUE) 
lmerTest::summary((model_test))

eq_test2 <- as.formula("mmse_wp ~ 1 + full_year + age_bl_centered + phys_bp + phys_wp +
 # + edu + female + 

                        (1 + full_year |id)")
model_test2<- lmerTest::lmer(eq_test2, data=dwn, REML=TRUE) 
lmerTest::summary((model_test2))

#-graph
dtemp$pred2 <- predict(model_test2)

set.seed(1)
ids <- sample(dtemp$id,100)
d <- dtemp %>%  dplyr::filter( id %in% ids)

g<- ggplot2::ggplot(d, aes_string(x= "full_year", y="pred2", colour="phys_wp_dichot")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)+
  main_theme
# stat_smooth(method=lm, level=0.95, fullrange=FALSE)
g <- g + labs(list(
  title="interaction",
  x="years_in_study", y="mmse_predictions"))

g

#people who walk more than average score higher than their average mmse, and decline at a slower rate


#-----------------------------------------------------------------------------------------------
eq_re<- as.formula("mmse ~ 1 + full_year + age_bl_centered + phys_bp + phys_wp + edu + female + 
                   (1 + full_year + phys_wp |id)")
model_re<- lmerTest::lmer(eq_re, data=dwn, REML=TRUE) 
lmerTest::summary((model_re)) 
#only include this parameter if we except the effects of physical activity to vary systematically across people! (-singer and willet page 169)
##### when i start to include biomarkers, I would say yes, I do expect this.
#i.e. people who walk higher then normal, who are low in stress, will benifit more than those who walk higher than normal, who are high in stress
#this is my hypothesis - stress negates the effects of PA
#including the within person random effects makes phys_bp and phys_wp significant, indicating there are moderators in the relationship

#hoffman page 76

###composite biomarker variable--------------------------------------------------------------------


eq_5 <- as.formula("mmse_wp ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + phys_bp + phys_wp + edu + female + al_count + 
                   (1 + full_year + phys_wp |id)")

model_bio<- lmerTest::lmer(eq_5, data=dbio, REML=TRUE) 
lmerTest::summary((model_bio)) 


eq_5 <- as.formula("mmse_wp ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + phys_bp + phys_wp + al_count + al_count:phys_wp +
# edu + female 
                   (1 + full_year + phys_wp |id)")

model_bio<- lmerTest::lmer(eq_5, data=dbio, REML=TRUE) 
lmerTest::summary((model_bio)) 











#######--------------- old graphing stuff that would need editing below ---------------------
# 
# #--- predictions vs actual mmse
# dwn$pred7re<- predict(model_7re)
# 
# #--- graphics
# 
# set.seed(1)
# ids <- sample(ds$id,50)
# d <- dwn %>%  dplyr::filter( id %in% ids)
# dim(d)
# names(dwn)
# 
# 
# #MMSE and model predictions
# lines_predVID( 
#   d,
#   variable_name= "mmse",
#   predicted = "predre",
#   line_size=.5,
#   line_alpha=.5,
#   #   top_y = 35,
#   #   bottom_y = 0,
#   #   by_y = 5,
#   bottom_age = 0,
#   top_age = 25,
#   by_age = 5
# )
# 
# #####--- intercept versus slope of model 7
# 
# cf <- coefficients(model_7re)$id
# head(cf,20)
# 
# cf<-data.frame(cf)
# str(cf)
# names(cf)
# 
# ##intercept versus random slopes of interaction
# g<- ggplot2::ggplot(cf, aes_string(x="full_year.phys", y="X.Intercept.")) +
#   geom_point(size=5, alpha=.5, colour="blue")+
#   main_theme+
#   stat_smooth(method=lm, level=0.95, fullrange=FALSE)
# g <- g + labs(list(
#   # title="Intercept versus the Estimates Slope of the Interaction Between Time Since Baseline and phys Score",
#   x="predcited slope", y="intercept"))
# 
# g
# 
# 




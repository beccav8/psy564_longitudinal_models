
options(width=160)
rm(list=ls())
cat("\f")

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
base::source("./scripts/common-function.R")  #e.g. used to call up names_label function etc.
# base::source("./scripts/tukey_outlier.R")
# ---- load_packages ---------------------------------------------------------
#requireNamespace("dplyr")
library("ggplot2")
library("dplyr")
library("nlme") # estimate mixed models | esp. gls()
library("lme4") # estimate mixed models | esp. lmer()
library("arm")  # process model objects
# ---- preceding_scripts ------------------------------------------------------
# source("./scripts/data/0-import-raw.R")

# ---- @knitr load_data -------------------------------------------------------
getwd()
ds<-readRDS("data-unshared/derived/ds0.rds")
# head(ds) 


#################

ds <- plyr::rename(x=ds, replace = c(
  "VSTseqno" = "n_visit_id",
  "OBS.x" = "n_visits",
  "Annual.Visit.x" = "annual_visit",
  "Age.at.Onset" = "age_at_onset",
  "Age.at.Visit" = "age_at_visit",
  "Sex"="sex",
  "Rethnic" = "race",
  "Yrs.School" = "edu",
  "vstBOMC" = "blessed_test",
  "Rdiedwhy" = "reason_death",
  "RclinpathDX.x" ="Clinical_Diagnosis",
  "Thyroid.disease.UDS.A5" = "thyroid_disease",
  "LABS" = "labels_drawn",
  "EVAL_DX" = "Research_Diagnosis",
  "vstNPgds" = "GDS_geriatric_dep",
  "udsB6gds" = "USD_geriatric_dep",
  "vstCORN" = "Cornell_dep",
  "vstCESDWK"="center_epidem_dep",
  "TITLE"= "npsy_battery_version",
  "InformationWAISrAgeScaled"="information_agesc_WAIS",
  "vstNPinform"= "Information_WAIS",
  "vstNPtrails_A" = "TrailsA",
  "vstNPtrails_B" = "TrailsB",
  "VnpLMI_Awms3"= "logical_memoryI",
  "VnpLMII_Awms3" = "logical_memoryII",
  "vstNPlogmemR"="logical_mem_rec",
  "vstNPcategoryII"="sum_cat_fluency",
  "VnpMatrix" = "matrix_reasoning",
  "vstNPSymb_Digit"="symbol_digit_mod",
  "vstNPdigitspan"= "DStotal",
  "vstNPdigitspanF"= "DSF", 
  "vstNPdigitspanB"= "DSB", 
  "VnpDigitSeqWAIS_IV"="DSS",
  "vstNPboston"="boston",
  "vstNPblock" = "block",
  "FLIGHTS.STEPS.NOW" = "climb_now",
  "FLIGHTS.STEPS.BEFORE"= "climb_before",
  "BLOCKS.WALKED.NOW" = "walk_now",
  "BLOCKS.WALKED.BEFORE" = "walk_before",
  "HRS.LIGHT.EXERCISE.NOW" = "light_exercise_now",
  "HRS.LT.EXERCISE.BEFORE" = "light_exercise_before",
  "HRS.HEAVY.EXERCISE.NOW" = "heavy_exercise_now",
  "HRS.HEAVY.EXERCISE.BEFORE"= "heavy_exercise_before"
))

#---------------------------SUBSETTING
# names(ds)
#the following variables are mostly renamed
# selected_varaiables <- c("n_visit_id", "Age.at.Visit", "n_visits", "sex", "race", "edu",  "AGE.AT.ENTRY", "age_at_onset","Stroke.UDS.A5", "TIA.UDS.A5", "Hypertension.UDS.A5",
#                          "Hypercholesterolemia.UDS.A5", "Diabetes.UDS.A5", "Annual.Visit.y", "Clinical_Diagnosis",
#                          "UDS.B1.Systolic.BP", "UDS.B1.Diastolic.BP", "Glucose","HGBA1C","Cholesterol","Triglyceride","HDL","LDL",
#                          "APOE", "MMSE", "vstNPSymb_Digit", "vstNPcatAnimals", "vstNPcatVeg","sum_cat_fluency",
#                          "vstNPdigitspan","vstNPdigitspanF", "vstNPdigitspanB", "VnpDigitSeqWAIS_IV",
#                          "BlockDesignWAISrAgeScaled", "vstNPboston", "VnpMatrix", 
#                          "Logial_memIA_WMS3", "logical_memIIA_WMS3", "logical_mem_recogn", 
#                          "FLIGHTS.STEPS.NOW","FLIGHTS.STEPS.BEFORE", "BLOCKS.WALKED.NOW", "BLOCKS.WALKED.BEFORE", "HRS.LIGHT.EXERCISE.NOW", "HRS.LT.EXERCISE.BEFORE",                                         
#                          "HRS.HEAVY.EXERCISE.NOW","HRS.HEAVY.EXERCISE.BEFORE")

selected_variables <- c(
  "n_visit_id", # personal identifier
  "n_visits",  # person id by wave indicator
  "annual_visit",
  "age_at_visit",
  "sex", 
  "edu",
  "climb_now", # how many flights of stairs you climb each day? 1 flight = 10 steps
  "walk_now", # how many blocks you walk each day? 12 blocks = 1 mile
  "block",            #spatil problem solving
  "logical_memoryI",  #EPISODIC
  "matrix_reasoning", #resoning
  "symbol_digit_mod", #perceptual speed
  "DSB",             #working memory
  "sum_cat_fluency", #semantic memory
  "MMSE",
  "APOE",
  "AGE.AT.ENTRY"
)


ds <- ds[ ,selected_variables]  
str(ds)

# ds %>%  dplyr::glimpse()
ds$id <- substr(ds$n_visit_id,1,3)            #IDENTIFIES THE PERSON
ds$visit_id <- substr(ds$n_visit_id,4,6)      #IDENTIFIES THE VISIT, including indication or annual (ending in 0) /semi-annual (ending in 5)
ds[ds=="."] <- NA

t(table(ds$block, useNA = "always"))

#head(ds)

#logical vector
table(ds$sex)

ds$male <-!(ds$sex==2)  #sex=TRUE if female

head(ds) 
ds %>% dplyr::filter(id==102) %>% dplyr::slice(1:10)


d <- ds %>%
  # dplyr::filter(ds, id %in% sample(unique(id),100)) %>% 
  dplyr::select(id, visit_id, male, edu, walk_now ) 
str(d)

# recode education into numeric
d$id <- as.numeric(d$id)
d$visit_id <- as.numeric(d$visit_id)
d$edu <- as.numeric(d$edu)
d$walk_now <- as.numeric(d$walk_now)
# d$block <- as.numeric(d$block)
str(d)

d <- d %>% 
  na.omit() %>% 
  dplyr::select(id, visit_id, male, edu, walk_now )
str(d)

names(d)
d %>% dplyr::filter(id==102) %>% dplyr::slice(1:10)

# prepare data for modeling
d <- ds %>% 
  dplyr::select
  na.omit(id)



source("./scripts/functions-for-glm-models.R")
### the first basic progression of models

# specify model equations
 eq_00 <- as.formula("walk_now ~ 1")
 eq_0  <- as.formula("walk_now ~ 1 +                  (1            | id)")
 eq_1  <- as.formula("walk_now ~ 1 + visit_id +       (1            | id)")
 eq_2  <- as.formula("walk_now ~ 1 + visit_id +       (1 + visit_id | id)")
 eq_3  <- as.formula("walk_now ~ 1 + visit_id + (1 + visit_id + edu | id)")
 
# estimate model solutions
model_00 <- nlme::gls(eq_00, data=d, method="ML") 
model_0 <- lme4::lmer(eq_0 , data=d, REML=FALSE)
model_1 <- lme4::lmer(eq_1, data=d, REML=FALSE)
model_2 <- lme4::lmer(eq_2, data=d, REML=FALSE )
model_3 <- lme4::lmer(eq_3, data=d, REML=TRUE )
# Note: ICC evaluates the improvement from model_00 to model_0

# augment the original data with model prediction (modeled values of the outcome/criterion)
d$m_00 <- predict(model_00)
d$m_0 <- predict(model_0)
d$m_1 <- predict(model_1)
# d$m_2 <- predict(model_2)
head(d)

basic_model_info(model_00)
basic_model_info(model_0)
basic_model_info(model_1)
basic_model_info(model_2)
basic_model_info(model_3)
#varCorr: matrix of varience estimates and SD for the two varience compoents in the model (id and residual)
vc <- VarCorr(model_0)

model <- model_1

# class(model)
# class(summary(model))
summod <- summary(model)
# str(summod); names(summod)

summod$coefficients
vcov(model)

str(summary(model))

fixef(model)



# explore how the modeling objects are interacted here:
# https://github.com/andkov/Longitudinal_Models_of_Religiosity_NLSY97/blob/e9173ef459111e8c29d79db1a3e1ccc6a1a38086/Models/LCM/singleModel_brief.R

FE<- fixef(model) # summary of fixed effects
FEt<- summary(model)$coefficients # estimates of Fixed Effects, SE, t-value
mFE<- (summary(model)$vcov@factors$correlation) # matrix of correlations among Fixed Effects
mRE<-   data.frame(sd= (attr(summary(model)$varcor$id,"stddev"))) 
mRE$var<- mRE$sd^2
mRE<-mRE[c("var","sd")] # variances and standard deviations of random effects
mREcov<-  data.frame(     summary(model)$varcor$id   ) # covariance matrix of RE
mREcor<-  data.frame(attr(summary(model)$varcor$id,"correlation")) # corrleation matrix of RE
sigma<- sigma(model) # standard deviation of residual




model <- model_2

mInfo<-summary(model)$AICtab
mInfo["N"]<- model@devcomp$dims["N"] # number of datapoints, verify
mInfo["p"]<- model@devcomp$dims["p"] # number of estimated parameters, verify
mInfo["ids"]<- (summary(model))$ngrps # number of units on level-2, here: individuals
# mInfo<- c(mInfo, "modelName"=modelName)
mInfo<-data.frame(mInfo) # turn into a dataframe
mInfo<- plyr::rename(mInfo,replace= c("mInfo"="model_3")) # rename variables
mInfo$Coefficient <- rownames(mInfo) # save index names as a column
mi_model_3 <- mInfo # create object
print(mi_model_3)

# useful functions working with GLM model objects
summary(model) # model summary
coefficients(model) # point estimates of model parameters (aka "model solution")
vcov(model) # covariance matrix of model parameters (inspect for colliniarity)
cov2cor(vcov(model)) # view the correlation matrix of model parameters
#confint(model, level=0.95) # confidence intervals for the estimated parameters

# predict(model); fitted(model) # generate prediction of the full model (all effects)
# residuals(model) # difference b/w observed and modeled values
anova(model) # put results into a familiary ANOVA table
# influence(model) # regression diagnostics


# create a model summary object to query 
(summod <- summary(model))
str(summod)

# study example at https://github.com/andkov/psy564/blob/master/Chapters/05/R_Chapter5/ch5-ELSA.R
# download and work with the repo https://github.com/andkov/psy564


# ----- 1 ---------------------
# number of uniuqe ids
ds %>% unique() %>% length()

# ----- mmse ------------------
t <- table(ds$n_visits, ds$MMSE); t[t==0]<-".";t


ds %>% 
  dplyr::group_by(n_visits) %>% 
  dplyr::mutate(MMSE = as.integer(MMSE)) %>% 
  dplyr::summarize(mean_mmse_score = mean(MMSE),
                   sd = sd(MMSE),
                   observed = n()) %>% 
  print(n=100)

# ---- sex -----------------
ds %>% 
  dplyr::group_by(sex) %>% 
  dplyr::summarize(n=n())


# ---- race -----------------
ds %>% 
  dplyr::group_by(race) %>% 
  dplyr::summarize(n=n())


# ---- edu -----------------
ds %>% 
  dplyr::group_by(edu) %>% 
  dplyr::summarize(n=n())

table(ds$edu, useNA = "always")
ds %>% 
  dplyr::group_by(n_visits) %>% 
  dplyr::mutate(edu = as.integer(edu)) %>%
  dplyr::summarize(mean_years_edu = mean(edu),
                   sd = sd(edu),
                   observed = n()) %>% 
  print(n=50)


# ---- thyroid_disease ----------------------
table(ds$thyroid_disease)
ds %>%
  dplyr::filter(!thyroid_disease==".") %>% 
  histogram_discrete("thyroid_disease")


# ---- verbal-fluency -----------------------

table(ds$"sum_cat_fluency")
ds %>% 
  dplyr::mutate(sum_cat_fluency = as.numeric(sum_cat_fluency)) %>% 
  dplyr::filter(! sum_cat_fluency %in% c("1")) %>% 
  histogram_continuous("sum_cat_fluency")

attr(ds$sum_cat_fluency,"label") 

#---- age at entry----------------------------

ds %>% 
  dplyr::group_by(AGE.AT.ENTRY) %>% 
  dplyr::summarize(n=n())


mean(ds$AGE.AT.ENTRY)

range(ds$AGE.AT.ENTRY)
##55.4 to 100.5, how is someone entering at 100 years old!? 
#how do i locate this person? 

boxplot(ds$AGE.AT.ENTRY)
bpage<-boxplot(ds$AGE.AT.ENTRY)
bpage$out
#identifies no outliers 

#---- stroke----------------------------------

class(ds$Stroke.UDS.A5)
#factor

table(ds$Stroke.UDS.A5)
ds %>%
  dplyr::filter(!Stroke.UDS.A5==".") %>% 
  histogram_discrete("Stroke.UDS.A5")


######################## subset of FIRST VISITS ONLY
sub2<- dplyr::filter(ds, n_visits==1)
head(sub2)

#---- light exercise now

class(ds$light_exercise_now) #factor

table(sub2$light_exercise_now, useNA = "always")
sub2 %>% 
  #dplyr::group_by(n_visits) %>% 
  dplyr::mutate(light_exercise_now = as.integer(light_exercise_now)) %>%
  dplyr::summarize(mean_light_now = mean(light_exercise_now),
                   sd = sd(light_exercise_now),
                   observed = n()) %>% 
  print


sub2$light_exercise_now<-as.numeric(sub2$light_exercise_now)
ggplot(sub2, aes(x=light_exercise_now)) +geom_histogram()

boxplot(sub2$light_exercise_now)
bplight<-boxplot(sub2$light_exercise_now)
bplight$out
#no outliers

#---- heavy exercise now

class(ds$heavy_exercise_now) #factor

table(sub2$heavy_exercise_now, useNA = "always")
sub2 %>% 
  #dplyr::group_by(n_visits) %>% 
  dplyr::mutate(heavy_exercise_now = as.integer(heavy_exercise_now)) %>%
  dplyr::summarize(mean_heavy_now = mean(heavy_exercise_now),
                   sd = sd(heavy_exercise_now),
                   observed = n()) %>% 
  print

sub2$heavy_exercise_now<-as.numeric(sub2$heavy_exercise_now)
ggplot(sub2, aes(x=heavy_exercise_now)) +geom_histogram()

boxplot(sub2$heavy_exercise_now)
bplight<-boxplot(sub2$heavy_exercise_now)
bplight$out
#no outliers


#---- flights steps now

class(ds$climb_now) #factor

table(sub2$climb_now, useNA = "always")
sub2 %>% 
  #dplyr::group_by(n_visits) %>% 
  dplyr::mutate(climb_now = as.integer(climb_now)) %>%
  dplyr::summarize(mean_flight_now = mean(climb_now),
                   sd = sd(climb_now),
                   observed = n()) %>% 
  print

sub2$climb_now<-as.numeric(sub2$climb_now)
ggplot(sub2, aes(x=climb_now)) +geom_histogram()

boxplot(sub2$climb_now)
bplight<-boxplot(sub2$climb_now)
bplight$out
#no outliers

#---- blocks walked

class(ds$walk_now) #factor

table(sub2$walk_now, useNA = "always")
sub2 %>% 
  #dplyr::group_by(n_visits) %>% 
  dplyr::mutate(walk_now = as.integer(walk_now)) %>%
  dplyr::summarize(mean_blocks_now = mean(walk_now),
                   sd = sd(walk_now),
                   observed = n()) %>% 
  print

sub2$walk_now<-as.numeric(sub2$walk_now)
ggplot(sub2, aes(x=walk_now)) +geom_histogram()

boxplot(sub2$walk_now)
bplight<-boxplot(sub2$walk_now)
bplight$out
#no outliers

#### ------------ age at baseline (for mean centering) 
#so intercept and mean slope can be interpreted as excepted value for indu at baseline
class(sub2$AGE.AT.ENTRY) #numeric

table(sub2$AGE.AT.ENTRY, useNA = "always")
sub2 %>% 
  #dplyr::group_by(n_visits) %>% 
  dplyr::summarize(mean_entry_age = mean(AGE.AT.ENTRY),
                   sd = sd(AGE.AT.ENTRY),
                   observed = n()) %>% 
  print

range(sub2$AGE.AT.ENTRY)

####----------------- education at baseline
class(sub2$edu) #factor

table(sub2$edu, useNA = "always")
sub2 %>% 
  #dplyr::group_by(n_visits) %>% 
  dplyr::mutate(edu = as.integer(edu)) %>%
  dplyr::summarize(mean_entry_edu = mean(edu),
                   sd = sd(edu),
                   observed = n()) %>% 
  print

sub2$edu<-as.numeric(sub2$edu)
range(sub2$edu)


####### ------ demographics ------- #######

## APOE  # of alelles??? 

class(ds$APOE)
ds$APOE<-as.factor(ds$APOE)

ds %>% 
  dplyr::group_by(APOE) %>% 
  dplyr::summarize(n=n())

##number of visits (i.e. how many people have x amount of "waves")

class(ds$n_visits)
ds$n_visits<-as.factor(ds$n_visits)

ds %>% 
  dplyr::group_by(n_visits) %>% 
  dplyr::summarize(n=n())  #only shows me up to 10 visits? max= 49

ds$n_visits<-as.integer(ds$n_visits)
ggplot(ds, aes(x=n_visits)) +geom_histogram()






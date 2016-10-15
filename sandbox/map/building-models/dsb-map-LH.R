# Clear memory from previous runs
base::rm(list=base::ls(all=TRUE))
cat("\f")

# @knitr load-packages --------------------

# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(lmerTest)
library(outliers)

# @knitr load-sources --------------------

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

# @knitr declare-globals ---------------------------------------------------------
path_input2  <- "./data/unshared/derived/map/dwn_map_dsb.rds"
path_input1<- "./data/unshared/derived/map/dbio_map_dsb.rds"
path_input0  <- "./data/unshared/derived/map/data_tweak.rds" 

# @knitr load-data ---------------------------------------------------------------
ds0  <- readRDS(path_input0) #total raw data              
dwn  <- readRDS(path_input2) #subset for models           
dbio <- readRDS(path_input1) #subset for biomarker models  


#subset-females-and-males
dwn_female <- subset(dwn, female==TRUE) 
dwn_male <- subset(dwn, female==FALSE)  

#subset-females-and-males-
dbio_female <- subset(dbio, female==TRUE)
dbio_male <- subset(dbio, female==FALSE)

mean(ds0$age_bl, na.rm=TRUE)
length(unique(dwn_male$id))
length(unique(dwn_female$id))
# @knitr define-variables ---------------------------------------------------------------

#variables
#phys_bl_bp = each persons baseline score - the baseline grand mean (between person baseline compare)
#phys_bp_mean = the persons mean score across occasions - the grand mean 
#phys_bp_median = the persons median score across occasions - the grand median  
#phys_wp = that persons score at time j, minus their mean (deviations-from--own-mean)
#dsb_wp = the persons dsb score at time j, minus their mean, so that a positive value indicated scores higehr then their mean 
#biomarker_high = indicates if the person's mean on that particular biomarker is above the 3rd quartile

# physical activity question: hours per week that the ppt. engages in 5 different categories   


# @knitr confound-equations--------------------------------

eq_0 <- as.formula("dsb ~ 1 + full_year +             
                   (1 + full_year |id)")
###does the outcome change over time? i.e. if so, it makes sense to add predictors.

eq_1 <- as.formula("dsb ~ 1 + full_year + age_bl_centered +    
                   (1 + full_year |id)")

eq_2 <- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + 
                   (1 + full_year |id)")

####main effect of time in study: when baseline age is at the mean (i.e. 0)
####main effects of age_bl: when year in study is 0 (i.e. at baseline)
####interactioin: Those who are older decline faster over time

eq_3 <- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu +     
                   (1 + full_year |id)")


eq_3int <- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + full_year:edu +  
                   (1 + full_year |id)")

# @knitr confound-full ---------------------------------------------
model_0<- lmerTest::lmer(eq_0, data=dwn, REML=TRUE) 
model_1<- lmerTest::lmer(eq_1, data=dwn, REML=TRUE) 
model_2<- lmerTest::lmer(eq_2, data=dwn, REML=TRUE) 
model_3<- lmerTest::lmer(eq_3, data=dwn, REML=TRUE) 
model_3int<- lmerTest::lmer(eq_3int, data=dwn, REML=TRUE) 

lmerTest::summary((model_0)) 
lmerTest::summary((model_1)) 
lmerTest::summary((model_2)) 
lmerTest::summary((model_3)) 
lmerTest::summary((model_3int)) 


# @knitr confound-male ---------------------------------------------
model_0<- lmerTest::lmer(eq_0, data=dwn_male, REML=TRUE) 
model_1<- lmerTest::lmer(eq_1, data=dwn_male, REML=TRUE) 
model_2<- lmerTest::lmer(eq_2, data=dwn_male, REML=TRUE) 
model_3<- lmerTest::lmer(eq_3, data=dwn_male, REML=TRUE) 
model_3int<- lmerTest::lmer(eq_3int, data=dwn_male, REML=TRUE) 

lmerTest::summary((model_0)) 
lmerTest::summary((model_1)) 
lmerTest::summary((model_2)) 
lmerTest::summary((model_3)) 
lmerTest::summary((model_3int)) 


# @knitr confound-female ---------------------------------------------
model_0<- lmerTest::lmer(eq_0, data=dwn_female, REML=TRUE) 
model_1<- lmerTest::lmer(eq_1, data=dwn_female, REML=TRUE) 
model_2<- lmerTest::lmer(eq_2, data=dwn_female, REML=TRUE) 
model_3<- lmerTest::lmer(eq_3, data=dwn_female, REML=TRUE) 
model_3int<- lmerTest::lmer(eq_3int, data=dwn_female, REML=TRUE) 


lmerTest::summary((model_0)) ####time in study is sig
lmerTest::summary((model_1)) #### "" + age at baseline 
lmerTest::summary((model_2)) #### "" + interaction - those who are older decline faster over time
lmerTest::summary((model_3)) #### "" + edu and gender are sig
lmerTest::summary((model_3int)) #### interaction between time and education not sig

#adding exposure variables

# @knitr raw-PA-equation ---------------------------------------------
eq_3raw <- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + physical_activity +
                   (1 + full_year |id)")

# @knitr raw-full --------------------------------------------- 

model_rawm<- lmerTest::lmer(eq_3raw, data=dwn, REML=TRUE) 
lmerTest::summary((model_rawm)) #raw PA is sig for males  

# @knitr raw-male ---------------------------------------------  
model_rawm<- lmerTest::lmer(eq_3raw, data=dwn_male, REML=TRUE) 
lmerTest::summary((model_rawm)) #raw PA is sig for males

# @knitr raw-female ---------------------------------------------
model_rawf<- lmerTest::lmer(eq_3raw, data=dwn_female, REML=TRUE) 
lmerTest::summary((model_rawf)) #raw PA is sig for females 


#between person and within person effects 
# @knitr bp-wp-equation ---------------------------------------------

eq_5 <- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + phys_bp_median  +
                   (1 + full_year |id)")
####bp effects: are people with higher values than other people (on average, over time)
####also higher in dsb (on average over time) (hoffman pdf page 72)

eq_6 <- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + phys_bp_median  + phys_wp +
                   (1 + full_year |id)")

# @knitr comments0 ---------------------------------------------

##wp effects: if you have higher values than usual (your own mean) at this occasion, do you also have a higher outcome
##value than usual at this occasion ? (hoffman pdf page 72)
## main effect of time: slope term- yearly rate of change in dsb, controlling for exercise
 
##(hoffman page 75)
###- if only bp : induvidual slopes dont differ sig
#- if only wp sig : induvidual slopes are sig increasing or decreasing

# #(note)
# #when you add a time varying covariate (singer and willett page 170)
# #i-the intercept parameter or FE is now the value of the outcome when all Level 1 predictors (including time) are zero
# #ii-the slope is now a conditional rate of change, controlling for the effects of the TVC
# #this changes the meaning of the Level-2 varience components, so comparing them across sucessive models makes no sense
# #we can now only use  the F.E and goodness of fit stats to compare these models

# @knitr bp-wp-full ---------------------------------------------
model_5<- lmerTest::lmer(eq_5, data=dwn, REML=TRUE) 
model_6<- lmerTest::lmer(eq_6, data=dwn, REML=TRUE) 

lmerTest::summary((model_5))  #NS between person effects 
lmerTest::summary((model_6)) #WP effects are sig (those who walk

# @knitr bp-wp-male ---------------------------------------------
model_5<- lmerTest::lmer(eq_5, data=dwn_male, REML=TRUE) 
model_6<- lmerTest::lmer(eq_6, data=dwn_male, REML=TRUE) 

lmerTest::summary((model_5))  #NS between person effects 
lmerTest::summary((model_6)) #WP effects are sig (those who walk higher than average have higher dsb scores at that time)

# @knitr bp-wp-female ---------------------------------------------

model_5<- lmerTest::lmer(eq_5, data=dwn_female, REML=TRUE) #NS between person effects
model_6<- lmerTest::lmer(eq_6, data=dwn_female, REML=TRUE) #NS between person and within person effects for females! 

lmerTest::summary((model_5)) #bp sig at .1
lmerTest::summary((model_6)) #wp sig at .1


#allowing the effect of the TVC to vary over tiem (interaction term) -page 171 singer and willett

# @knitr comments2 ---------------------------------------------

#singer and willet page 171:
#subscript j-occasion (phys_wp_ij) is what differentiates the a TVC from a TIVC
##can interpret interactin in 2 ways 
#i- the effect of within person changes in physical activity on dsb varies over time - (it fluctuates?)
#ii- the rate of change in dsb differs as a function of within person changes in PA **
#in this model it is sig- those who walk more than their average decline less over time

# @knitr tvc-intxn-equation ---------------------------------------------
eq_tv_intxn <- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + phys_bp_median  + phys_wp + phys_wp:full_year +
                          (1 + full_year |id)")
# @knitr tvc-intxn-full ---------------------------------------------
model_tv_intx<- lmerTest::lmer(eq_tv_intxn, data=dwn, REML=TRUE) #significant interaction term
lmerTest::summary((model_tv_intx)) 

# @knitr tvc-intxn-male -------------------------------------------------------
model_tv_intx<- lmerTest::lmer(eq_tv_intxn, data=dwn_male, REML=TRUE) #significant interaction term
lmerTest::summary((model_tv_intx)) 

# @knitr tvc-intxn-female ----------------------------------------------------
model_tv_intx<- lmerTest::lmer(eq_tv_intxn, data=dwn_female, REML=TRUE) 
lmerTest::summary((model_tv_intx))


# @knitr set-up-graph -------------------------

# hypothetical dichotomizing graph to understand the interaction 

dtemp<-dwn
# summary(dtemp$phys_wp)
# hist(dtemp$phys_wp)


for (i in 1: length(dtemp$phys_wp)) {
  
  if (isTRUE(dtemp$phys_wp[i] > 0.77 )) {
    dtemp$phys_wp_dichot[i] <- "HIGH" }
  
  else if (isTRUE(dtemp$phys_wp[i] < (-1.1 ))){
    dtemp$phys_wp_dichot[i] <- "LOW"  }
  
  else {
    dtemp$phys_wp_dichot[i] <- "mid"
  }
  
}

model_tv_intx<- lmerTest::lmer(eq_tv_intxn, data=dtemp, REML=TRUE)
dtemp$pred<- predict(model_tv_intx)

dwn_femaletemp <- subset(dtemp, female==TRUE) 
dwn_maletemp <- subset(dtemp, female==FALSE)  

ids <- sample(unique(dwn_femaletemp$id),1)
dtemp %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,phys_wp, phys_wp_dichot)

 # @knitr graph0 ------------------------------------
 
 g<- ggplot2::ggplot(dwn_femaletemp, aes_string(x= "full_year", y="pred", colour="phys_wp_dichot")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)+
  main_theme
# stat_smooth(method=lm, level=0.95, fullrange=FALSE)
g <- g + labs(list(
  title="interaction between phy_wp and time in study for female",
  x="years_in_study", y="predictions"))
g

g<- ggplot2::ggplot(dwn_maletemp, aes_string(x= "full_year", y="pred", colour="phys_wp_dichot")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)+
  main_theme
# stat_smooth(method=lm, level=0.95, fullrange=FALSE)
g <- g + labs(list(
  title="interaction between phy_wp and time in study for males",
  x="years_in_study", y="predictions"))
g

table(dwn_female$full_year)
table(dwn_male$full_year)
# @knitr tvc-intxn-equation-age ---------------------------------------------
eq_tv_intxn <- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + age_at_visit_centered + edu + phys_bp_median  + phys_wp + phys_wp:age_at_visit_centered +
                          (1 + full_year |id)")
# @knitr tvc-intxn ---------------------------------------------
model_tv_intx<- lmerTest::lmer(eq_tv_intxn, data=dwn, REML=TRUE) #significant interaction term
lmerTest::summary((model_tv_intx))
dtemp$pred<- predict(model_tv_intx)

# @knitr tvc-intxn-graph ---------------------------------------------

g<- ggplot2::ggplot(dtemp, aes_string(x= "age_at_visit_centered", y="pred", colour="phys_wp_dichot")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)+
  main_theme
# stat_smooth(method=lm, level=0.95, fullrange=FALSE)
g <- g + labs(list(
  title="interaction between phy_wp and age at visit",
  x="age_at_visit", y="predictions"))

g

# @knitr comments3 ---------------------------------

#for males and females, the effects of higher PA than usual differ over time
#when you fluctuate to be higher than usual, dsb score is relatively stable
#when you are lower than usual in PA, dsb score declines at that occasion, over time, the effects of
#being lower than usual in PA result in lower dsb scores 
#- it is important to keep exercising into old age?


# @knitr dsb_wp_fluctuations ---------------------------------

###using dsb deviation from mean as outcome  
### thus, those who score higher than their average on physical activity, 
###also score higher than their average dsb 

eq_a <- as.formula("dsb_wp ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + phys_bp_median  + phys_wp + 
                     (1 + full_year |id)")
# @knitr dsb_wp_full ---------------------------------
model<- lmerTest::lmer(eq_a, data=dwn, REML=TRUE) 
lmerTest::summary((model)) 
# @knitr dsb_wp_male ---------------------------------

model<- lmerTest::lmer(eq_a, data=dwn_male, REML=TRUE) 
lmerTest::summary((model)) 

# @knitr dsb_wp_female ---------------------------------
model<- lmerTest::lmer(eq_a, data=dwn_female, REML=TRUE) 
lmerTest::summary((model)) 



#-----------------------------------------------------------------------------------------------
#including phys_wp in the random effects 

#only include this parameter if we except the effects of physical activity to vary systematically across people! (-singer and willet page 169)
##### when i start to include biomarkers, I would say yes, I do expect this.
#i.e. people who walk higher then normal, who are low in stress, will benifit more than those who walk higher than normal, who are high in stress
#this is my hypothesis - stress negates the effects of PA
#including the within person random effects makes phys_bp and phys_wp significant, indicating there are moderators in the relationship
#hoffman page 76

# @knitr Random-effects-equation ---------------------------------
eq_re<- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + phys_bp_median  + phys_wp + 
                          (1 + full_year + phys_wp |id)")
# @knitr Random-effects-full ---------------------------------
model_re<- lmerTest::lmer(eq_re, data=dwn, REML=TRUE) 
lmerTest::summary((model_re)) 

# @knitr Random-effects-male ---------------------------------
model_re<- lmerTest::lmer(eq_re, data=dwn_male, REML=TRUE) 
lmerTest::summary((model_re)) 
# @knitr Random-effects-female ---------------------------------
model_re<- lmerTest::lmer(eq_re, data=dwn_female, REML=TRUE) 
lmerTest::summary((model_re)) 

# @knitr biomarker-models -------------------------------------------------------

eq_bmk<- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + phys_bp_median  + phys_wp  + al_catg_wave +
                   (1 + full_year + phys_wp |id)")

# @knitr biomarker-full -------------------------------------------------------
model_bmk<- lmerTest::lmer(eq_bmk, data=dbio, REML=TRUE) 
lmerTest::summary((model_bmk)) #al category is sig related to dsb score
# @knitr biomarker-male -------------------------------------------------------
model_bmk<- lmerTest::lmer(eq_bmk, data=dbio_male, REML=TRUE) 
lmerTest::summary((model_bmk)) #al category is sig related to dsb score
# @knitr biomarker-female -------------------------------------------------------
model_bmk<- lmerTest::lmer(eq_bmk, data=dbio_female, REML=TRUE) 
lmerTest::summary((model_bmk)) #al category is sig related to dsb 

# @knitr graph-biomarker -------------------------------------------------------

dtemp<-dbio

model_bmk<- lmerTest::lmer(eq_bmk, data=dtemp, REML=TRUE)
dtemp$pred<- predict(model_bmk)

set.seed(1)
ids <- sample(dtemp$id,100)
d <- dtemp %>%  dplyr::filter( id %in% ids)
str(d)

ggplot(d, aes(x=al_catg_wave, y=pred)) +geom_bar(stat="identity", fill="lightblue", width=0.5)
#those high in AL have sig lower dsb scores than lose low in dsb or medium in dsb
#those with medium AL scores actually have the highest dsb scores 


# @knitr biomarker-interaction -------------------------------------------------------
eq_bmk<- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + phys_bp_median  + phys_wp + al_catg_wave + al_catg_wave:phys_wp +
                   (1 + full_year + phys_wp |id)")

model_bmk<- lmerTest::lmer(eq_bmk, data=dbio, REML=TRUE) 
lmerTest::summary((model_bmk))

model_bmk<- lmerTest::lmer(eq_bmk, data=dbio_male, REML=TRUE) 
lmerTest::summary((model_bmk)) #no interaction between walking more than usual and AL category at that time

model_bmk<- lmerTest::lmer(eq_bmk, data=dbio_female, REML=TRUE) 
lmerTest::summary((model_bmk)) #no interaction between walking more than usual and AL category at that time

# @knitr PA-predicted-by-bio -------------------------------------------------------
##does-AL-predict_fluctuations-in-PA---------------------------------------

eq_pwp<- as.formula("phys_wp ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + al_catg_wave + 
                   (1 + full_year |id)")

model_pwp<- lmerTest::lmer(eq_pwp, data=dbio, REML=TRUE) 
lmerTest::summary((model_pwp)) 


model_pwp<- lmerTest::lmer(eq_pwp, data=dbio_male, REML=TRUE) 
lmerTest::summary((model_pwp)) 

model_pwp<- lmerTest::lmer(eq_pwp, data=dbio_female, REML=TRUE) 
lmerTest::summary((model_pwp)) 

# @knitr transform -------------------------------------------------------
#do fluctuations in PA predict fluctuations in AL variables 
str(dbio_male)

for(i in unique(dbio_male$id)) {
  for (j in 1:length(dbio_male$al_count_wave[dbio_male$id==i])) {
    
    dbio_male$al_wp[dbio_male$id==i][j]<- (dbio_male$al_count_wave[dbio_male$id==i][j]) - (mean(dbio_male$al_count_wave[dbio_male$id==i], na.rm =TRUE))
  } }


# table(dbio_male$al_wp, useNA="always")
dbio_male$al_wp<-dbio_male$al_wp/10
# ggplot(dbio_male, aes(x=al_wp, y=dsb)) + geom_line()

for(i in unique(dbio_female$id)) {
  for (j in 1:length(dbio_female$al_count_wave[dbio_female$id==i])) {
    
    dbio_female$al_wp[dbio_female$id==i][j]<- (dbio_female$al_count_wave[dbio_female$id==i][j]) - (mean(dbio_female$al_count_wave[dbio_female$id==i], na.rm =TRUE))
  } }

# table(dbio_female$al_wp, useNA="always")
dbio_female$al_wp<-dbio_female$al_wp/10
# ggplot(dbio_male, aes(x=al_wp, y=dsb)) + geom_line()

#keep re-reunning this to see different people in the sample - IT WORKED
# ids <- sample(unique(dbio_male$id),1)
# dbio_male %>%
#   dplyr::filter(id %in% ids ) %>%
#   dplyr::group_by(id) %>%
#   dplyr::select(id,al_count_wave, al_wp)



# @knitr PA_Within-predicted-by-bio_within -------------------------------------------------------
##al_wp is each persons total alostatic load score at that time, minus their mean alostatic load score
##to see if fluctuation in biomarkers predict physicl activity 
# eq_pwp<- as.formula("phys_wp ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + al_wp + 
#                    (1 + full_year |id)")
# eq_pwp<- as.formula("al_wp ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + phys_bp_median + phys_wp + 
#                    (1 + full_year |id)")
# model_pwp<- lmerTest::lmer(eq_pwp, data=dbio_male, REML=TRUE) 
# lmerTest::summary((model_pwp)) 
# 
# model_pwp<- lmerTest::lmer(eq_pwp, data=dbio_female, REML=TRUE) 
# lmerTest::summary((model_pwp)) 
# 


# @knitr biomarker-interaction2 -------------------------------------------------------
eq_bmk<- as.formula("dsb ~ 1 + full_year + age_bl_centered + full_year:age_bl_centered + edu + phys_bp_median  + phys_wp + al_wp + al_wp:phys_wp +
                    (1 + full_year + phys_wp |id)")



model_bmk<- lmerTest::lmer(eq_bmk, data=dbio_female, REML=TRUE) 
lmerTest::summary((model_bmk)) 

model_bmk<- lmerTest::lmer(eq_bmk, data=dbio_male, REML=TRUE) 
lmerTest::summary((model_bmk)) 


dtemp<-dbio_male
dtemp$pred<- predict(model_bmk)
summary(dtemp$phys_wp)

for (i in 1: length(dtemp$phys_wp)) {
  
  if (isTRUE(dtemp$phys_wp[i] > 0.82 )) {
    dtemp$phys_wp_dichot[i] <- "HIGH" }
  
  else if (isTRUE(dtemp$phys_wp[i] < (-1.36 ))){
    dtemp$phys_wp_dichot[i] <- "LOW"  }
  
  else {
    dtemp$phys_wp_dichot[i] <- "mid"
  }
  
}

ids <- sample(unique(dtemp$id),1)
dtemp %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,phys_wp, phys_wp_dichot)

g<- ggplot2::ggplot(dtemp, aes_string(x= "al_wp", y="pred", colour="phys_wp_dichot")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)+
  main_theme
g <- g + labs(list(
  title="interaction between phy_wp and al_Wp in MALES",
  x="wp al fluctuation", y="mmse prediction"))

g




#######--------------- old graphing stuff that would need editing below ---------------------
# 
# #--- predictions vs actual dsb
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
# #dsb and model predictions
# lines_predVID( 
#   d,
#   variable_name= "dsb",
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




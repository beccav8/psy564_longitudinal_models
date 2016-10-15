# Clear memory from previous runs
base::rm(list=base::ls(all=TRUE))
cat("\f")

# install.packages("knitr")
# install.packages("markdown")
# install.packages("testit")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("stats")
# install.packages("ggplot2")
# install.packages("extrafont")
# install.packages("doBy")
# install.packages("psych")

# @knitr load-packages --------------------

# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(lmerTest)
library(outliers)
# Load the necessary packages.
base::require(base)
base::require(knitr)
base::require(markdown)
base::require(testit)
base::require(dplyr)
base::require(reshape2)
base::require(stringr)
base::require(stats)
base::require(ggplot2)
base::require(extrafont)
base::require(lattice)
base::require(doBy)
base::require(psych)


# @knitr load-sources --------------------

# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R")
source("./scripts/general-graphs.R")  #in scripts folder
source("./scripts/specific-graphs.R")
source("./scripts/specific-graphs-pred.R")
source("./scripts/graphs-pred.R")
source("./scripts/graphs-predVID.R")
source("./scripts/multiplot-function.R")
source("./scripts/functions-for-glm-models.R")
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

# @knitr declare-globals --------------------
path_input  <- "./data/unshared/derived/map/data.rds"     

# @knitr load-data --------------------
ds <- readRDS(path_input)
# dim(ds)
# str(ds)
# head(ds)

# @knitr define-variables --------------------

#phys_bl_bp = each persons baseline score - the baseline grand mean (between person baseline compare)
#phys_bp_mean = the persons mean score across occasions - the grand mean 
#phys_bp_median = the persons median score across occasions - the grand median  
#phys_wp = that persons score at time j, minus their mean (deviations-from--own-mean)
#dsb_wp = the persons dsb score at time j, minus their mean, so that a positive value indicated scores higehr then their mean 
#biomarker_high = indicates if the person's mean on that particular biomarker is above the 3rd quartile

# physical activity question: hours per week that the ppt. engages in 5 different categories   

# @knitr tweak-data -----------------------------------
dtest<-ds

# @knitr univariate-quality-check --------------------

# @knitr BasicGraph1 --------------------
## dsb
boxplot(dtest$dsb)
hist(dtest$dsb)
#normally distributed! 

## mmse
boxplot(dtest$mmse)
hist(dtest$mmse)

## age
boxplot(dtest$age_bl_centered)
# bp<-boxplot(dtest$age_bl_centered)
# bp$out 
hist(dtest$age_bl_centered)
#looks ok
range(dtest$age_bl, na.rm=TRUE)
## physical activity raw
boxplot(dtest$physical_activity)
# bp<-boxplot(dtest$physical_activity)
# bp$out 
dtest %>% histogram_continuous(variable_name= "physical_activity") + stat_bin(binwidth = 5)
#anything greater than 40 may be an outlier 
dtest<- subset(dtest, physical_activity < 40) 
boxplot(dtest$physical_activity)

## physical activity bp
boxplot(dtest$phys_bp_median)
dtest %>% histogram_continuous(variable_name= "phys_bp_median") + stat_bin(binwidth = 5)
#anything greater than 20 may be an outlier 
dtest<- subset(dtest, phys_bp_median < 20) 
boxplot(dtest$phys_bp_median)

## physical activity wp
boxplot(dtest$phys_wp)
dtest %>% histogram_continuous(variable_name= "phys_wp") + stat_bin(binwidth = 5)
#looks good

# @knitr BasicGraph2 --------------------

#biomarkers
hist(dtest$apoe) 
hist(dtest$cholesterol) #normal
hist(dtest$hemoglobin)#heavy at mean
# is a form of hemoglobin (a blood pigment that carries oxygen) that is bound to glucose.
#High HbA1c levels indicate poorer control of diabetes
#not affected by short-term fluctuations in blood glucose concentrations
#6.5% signals that diabetes is present
hist(dtest$hdlratio)#normal (high ratio is bad)
hist(dtest$hdl)#normal
hist(dtest$ldl)#normal
hist(dtest$glucose)#skewed right
hist(dtest$creatine) #skewed right, heavy at low end   
#index of kidney function, high is bad (influenced by muscle tissue, therefore gender may confound this)

plyr::count(dtest, 'cholesterol_HIGH_wave') 
plyr::count(dtest, 'hemoglobin_HIGH_wave') 
plyr::count(dtest, 'hdl_LOW_wave') 
plyr::count(dtest, 'ldl_HIGH_wave')
plyr::count(dtest, 'glucose_HIGH_wave')
plyr::count(dtest, 'creatine_HIGH_wave')
plyr::count(dtest, 'hdlratio_HIGH_wave') 
#(about the same ratio for all biomarkers)

#obtain multivariate counts
computed_variables <-  c("cholesterol_HIGH_wave", "hemoglobin_HIGH_wave", "hdlratio_HIGH_wave", "hdl_LOW_wave","ldl_HIGH_wave", "glucose_HIGH_wave","creatine_HIGH_wave" ) 
bio<- dtest %>% 
  # dplyr::group_by(glucoseHIGH,hgba1cHIGH) %>% 
  dplyr::group_by_(.dots = computed_variables) %>% 
  dplyr::summarize(count =n())
#no NA's

# @knitr dsb --------------------  
gdsb<- ggplot2::ggplot(dtest, aes_string(x= "full_year", y="dsb")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,15)) +
  main_theme 
gdsb <- gdsb + labs(list(
  title= "Raw dsb scores Over Time",
  x="time", y="dsb"))
gdsb
#dsb declines over time


gdsb<- ggplot2::ggplot(dtest, aes_string(x= "full_year", y="dsb", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,15)) +
  main_theme 
gdsb <- gdsb + labs(list(
  title= "Raw dsb scores for Males and Females Over Time",
  x="time", y="dsb"))
gdsb


# @knitr mmse -------------------- 
gmmse<- ggplot2::ggplot(dtest, aes_string(x= "full_year", y="mmse")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gmmse <- gmmse + labs(list(
  title= "Raw mmse scores Over Time",
  x="time", y="dsb"))
gmmse
#mmse declines over time

gmmse<- ggplot2::ggplot(dtest, aes_string(x= "full_year", y="mmse", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gmmse <- gmmse + labs(list(
  title= "Raw mmse scores Over Time",
  x="time", y="dsb"))
gmmse
#mmse declines over time

# @knitr PA --------------------  

## PA
gPA<- ggplot2::ggplot(dtest, aes_string(x= "full_year", y="physical_activity")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  # coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA <- gPA + labs(list(
  title= "Raw PA scores Over Time",
  x="time", y="Raw Physical Activity"))
gPA
#people are exercising slightly less over time 
# multiplot(gPA, gdsb, cols=1)  

gPA<- ggplot2::ggplot(dtest, aes_string(x= "full_year", y="physical_activity", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  # coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA <- gPA + labs(list(
  title= "Raw PA scores for Males and Females Over Time",
  x="time", y="Raw Physical Activity"))
gPA
#male walk more than the population average 

# @knitr PA_between --------------------

## PA_Between
gPA_bp<- ggplot2::ggplot(dtest, aes_string(x= "full_year", y="phys_bp_median")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  # coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA_bp <- gPA_bp + labs(list(
  title= "Between person effects of PA scores Over Time",
  x="time", y="Between person"))
gPA_bp

gPA_bp<- ggplot2::ggplot(dtest, aes_string(x= "full_year", y="phys_bp_median", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA_bp <- gPA_bp + labs(list(
  title= "Between person effects of PA scores",
  x="time", y="Between person"))
gPA_bp
#male walk more than the population average - but there are some males who are reporting really high PA across time,
#this could be pulling their average up 

# @knitr PA_within --------------------

## PA_Within
gPA_wp<- ggplot2::ggplot(dtest, aes_string(x= "full_year", y="phys_wp")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  # coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA_wp <- gPA_wp + labs(list(
  title= "within person effects of PA scores Over Time",
  x="time", y="Within person"))
gPA_wp
 #people walk less than their average over time.

gPA_wp<- ggplot2::ggplot(dtest, aes_string(x= "full_year", y="phys_wp", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA_wp <- gPA_wp + labs(list(
  title= "within person effects of PA scores",
  x="time", y="Within person"))
gPA_wp
#seems to be no difference between the sex's in deviations from their average exercise over time.Females 
#may walk slightly more than their average over time


# @knitr multi-variate-quality-check --------------------

# @knitr mv-raw -----------------------
## raw
gPA<- ggplot2::ggplot(dtest, aes_string(x= "physical_activity", y="dsb", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,15)) +
  main_theme 
gPA <- gPA + labs(list(
  title= "Raw PA versus dsb",
  x="Raw PA", y="Digit_span_back"))
gPA
#higher PA is associated with higher dsb scores, but this could be because it is 
#earlier in the study when PA is higher (i.e a function of age)
#females tend to have higher dsb scores then males who exercise just as much as them

gPA<- ggplot2::ggplot(dtest, aes_string(x= "physical_activity", y="mmse", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gPA <- gPA + labs(list(
  title= "Raw PA versus mmse",
  x="Raw PA", y="MMSE"))
gPA

#females tend to have higher mmse scores then males who exercise just as much as them

# @knitr mv-bp -----------------------
## between 

gPA_bp<- ggplot2::ggplot(dtest, aes_string(x= "phys_bp_median", y="dsb", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,15)) +
  main_theme 
gPA_bp <- gPA_bp + labs(list(
  title= "Between person PA Vs. DSB",
  x="Between person PA", y="Digit_span_back"))
gPA_bp
#The same amount of deviation in exercise from the population average influences feamles more, 
#where females who exercise more than the average person tend to have higher mmse scores
#given that on average males exercise more, this represnts a sample of females who are exercising
#much more than the average female.

gPA_bp<- ggplot2::ggplot(dtest, aes_string(x= "phys_bp_median", y="mmse", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gPA_bp <- gPA_bp + labs(list(
  title= "Between person PA Vs. MMSE",
  x="Between person PA", y="MMSE"))
gPA_bp
#females tend to have higher mmse scores then males who exercise just as much as them

# @knitr mv-wp -----------------------
## within

gPA_wp<- ggplot2::ggplot(dtest, aes_string(x= "phys_wp", y="dsb", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,12)) +
  main_theme 
gPA_wp <- gPA_wp + labs(list(
  title= "Within person effects of PA scores vs dsb",
  x="Within person PA", y="Digit_span_back"))
gPA_wp


gPA_wp<- ggplot2::ggplot(dtest, aes_string(x= "phys_wp", y="mmse", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gPA_wp <- gPA_wp + labs(list(
  title= "Within person effects of PA scores vs mmse ",
  x="Within person PA", y="MMSE"))
gPA_wp

saveRDS(dtest, "./data/unshared/derived/map/data_tweak.rds")


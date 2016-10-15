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


#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
# rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console



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

# path_input  <- "./data/unshared/derived/dto.rds" 
# path_input  <- "./data/unshared/derived/dAL.rds"  
path_input  <- "./data/unshared/derived/obas/data.rds"     
# figure_path <- 'manipulation/stitched-output/te/'

# @knitr load-data --------------------

ds <- readRDS(path_input)
# dim(ds)
# str(ds)
# head(ds)
str(ds$female)

# @knitr define-variables --------------------

#phys_bl_bp = each persons baseline score - the baseline grand mean (between person baseline compare)
#phys_bp_mean = the persons mean score across occasions - the grand mean 
#phys_bp_median = the persons median score across occasions - the grand median  
#phys_wp = that persons score at time j, minus their mean (deviations-from--own-mean)
#mmse_wp = the persons mmse score at time j, minus their mean, so that a positive value indicated scores higehr then their mean 
#biomarker_high = indicates if the person's mean on that particular biomarker is above the 3rd quartile

# physical activity question: hours per week that the ppt. engages in 5 different categories   

# @knitr tweak-data -----------------------------------
# ds$age_bl_centered
dwn <- ds %>% 
  dplyr::select(id, years_in_study, age_at_visit_centered,age_bl_centered, walk_now, phys_bp_mean, phys_bp_median, phys_wp, mmse, mmse_wp, female, edu) %>% 
  na.omit()
#1723 observations

#keep re-reunning this to see different people in the sample  IT WORKED
ids <- sample(unique(ds$id),1)
ds %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id, age_bl_centered, entry_age
  )

#iomarker subset
dbio<- ds %>% 
  dplyr::select(id, years_in_study, walk_now, phys_bl_bp, age_bl_centered, phys_bp_mean, phys_bp_median, phys_wp, mmse, mmse_wp, age_bl_centered, female, edu, 
                cholesterol, hemoglobin, glucose, 
                cholesterol_HIGH_wave, hemoglobin_HIGH_wave, glucose_HIGH_wave, al_count ) %>%                
  # cholesterol_HIGH,  hemoglobin_HIGH,  hdlratio_HIGH, hdl_LOW,ldl_HIGH, glucose_HIGH,creatine_HIGH) %>% 
  na.omit()
#114 observations

# #category variable for biomarkers 
# table(dbio$al_count, useNA="always")
# 
# #assigns allostatic load category for each person at every observation 
# for (i in 1:length(dbio$al_count_wave)) {
#   
#   if (isTRUE(dbio$al_count_wave[i] == 0)) {
#     dbio$al_catg_wave[i] <- "LOW" }
#   
#   else if (isTRUE(dbio$al_count_wave[i] <=2 )) {
#     dbio$al_catg_wave[i] <- "MED" }
#   
#   else 
#     # (isTRUE(dbio$phys_wp_wave[i] >=2 )){
#     dbio$al_catg_wave[i] <- "HIGH"  
# } 
# 
# #keep re-reunning this to see different people in the sample  IT WORKED
# ids <- sample(unique(dbio$id),1)
# dbio %>%
#   dplyr::filter(id %in% ids ) %>%
#   dplyr::group_by(id) %>%
#   dplyr::select(id,al_count_wave, al_catg_wave, cholesterol_HIGH_wave, hemoglobin_HIGH_wave, hdlratio_HIGH_wave, hdl_LOW_wave,ldl_HIGH_wave, glucose_HIGH_wave,creatine_HIGH_wave, al_count_wave
#   )
# 
# #frequency table of created allostatic categories 
# table(dbio$al_catg_wave, useNA="always")


# @knitr univariate-quality-check --------------------

# @knitr BasicGraph1 --------------------
#mmse
boxplot(dwn$mmse)
# bpmmse<-boxplot(dwn$mmse)
hist(dwn$mmse)
# t(table(dwn$mmse, useNA = "always"))
#not normal dist

dtest<-dwn

#physical activity raw

# table(dwn$walk_now, useNA="always")
boxplot(dwn$walk_now)
# bp<-boxplot(dwn$walk_now)
# bp$out 
dwn %>% histogram_continuous(variable_name= "walk_now") + stat_bin(binwidth = 5)

#anything greater than 50 may be an outlier 
dtest<- subset(dtest, walk_now < 50) 
boxplot(dtest$walk_now)

#physical activity bp

# table(dtest$phys_bp_median, useNA="always")
boxplot(dtest$phys_bp_median)
# bp<-boxplot(dtest$phys_bp_median)
# bp$out 
dtest %>% histogram_continuous(variable_name= "phys_bp_median") + stat_bin(binwidth = 5)

#anything greater than 18 may be an outlier 
dtest<- subset(dtest, phys_bp_median < 30) 
boxplot(dtest$phys_bp_median)


#physical activity wp

# table(dtest$phys_wp, useNA="always")
boxplot(dtest$phys_wp)
# bp<-boxplot(dtest$phys_wp)
# bp$out 
dtest %>% histogram_continuous(variable_name= "phys_wp") + stat_bin(binwidth = 5)

#looks good

# @knitr BasicGraph2 --------------------

#biomarkers

hist(dbio$cholesterol) #normal
hist(dbio$hemoglobin)#heavy at mean
# is a form of hemoglobin (a blood pigment that carries oxygen) that is bound to glucose.
#High HbA1c levels indicate poorer control of diabetes
#not affected by short-term fluctuations in blood glucose concentrations
#6.5% signals that diabetes is present
hist(dbio$glucose)#normal with the odd really high value near 160


plyr::count(dbio, 'cholesterol_HIGH_wave') 
plyr::count(dbio, 'hemoglobin_HIGH_wave') 
plyr::count(dbio, 'glucose_HIGH_wave')

#obtain multivariate counts
computed_variables <-  c("cholesterol_HIGH_wave", "hemoglobin_HIGH_wave", "glucose_HIGH_wave") 
bio<- dbio %>% 
  # dplyr::group_by(glucoseHIGH,hgba1cHIGH) %>% 
  dplyr::group_by_(.dots = computed_variables) %>% 
  dplyr::summarize(count =n())
#no NA's


# @knitr multi-variate-quality-check --------------------

#mmse and PA overtime mvf

# @knitr graph-mmse --------------------  

gmmse<- ggplot2::ggplot(dtest, aes_string(x= "years_in_study", y="mmse", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gmmse <- gmmse + labs(list(
  title= "Raw mmse scores for Males and Females Over Time",
  x="time", y="mmse"))
gmmse

# @knitr graph-PA --------------------  
gPA<- ggplot2::ggplot(dtest, aes_string(x= "years_in_study", y="walk_now", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA <- gPA + labs(list(
  title= "Raw PA scores for Males and Females Over Time",
  x="time", y="Raw Physical Activity"))
gPA
#male walk more than the population average 


# multiplot(gPA, gmmse, cols=1)  

#male walk more than the population average 
#moreover, males mmse scores remain rather stable when compared to females, who decline over time.

######time versus physical activity
# @knitr graph-PA_bp -------------------- 
gPA_bp<- ggplot2::ggplot(dtest, aes_string(x= "years_in_study", y="phys_bp_median", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  # coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA_bp <- gPA_bp + labs(list(
  title= "Between person effects of PA scores for Males and Females Over Time",
  x="time", y="Between person"))
gPA_bp
#male walk more than the population average 


# @knitr graph-PA_wp -------------------- 
gPA_wp<- ggplot2::ggplot(dtest, aes_string(x= "years_in_study", y="phys_wp", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  # coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA_wp <- gPA_wp + labs(list(
  title= "within person effects of PA scores for Males and Females Over Time",
  x="time", y="Within person"))
gPA_wp
#as time progresses, females tend to walk more then their average when compared to males 


# multiplot(gPA_bp, gPA_wp, cols=1)

#across time, male walk more than the population average but
##as time progresses, both genders tend to exercise less than usual


###############mmse versus PA
#raw
# @knitr graph-PA_mmse -------------------- 
gPA<- ggplot2::ggplot(dtest, aes_string(x= "walk_now", y="mmse", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gPA <- gPA + labs(list(
  title= "Raw PA versus mmse for Males and Females",
  x="Raw PA", y="MMSE"))
gPA

#females tend to have higher mmse scores then males who exercise just as much as them
# @knitr graph-PAbp_mmse -------------------- 
#bp
gPA_bp<- ggplot2::ggplot(dtest, aes_string(x= "phys_bp_median", y="mmse", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gPA_bp <- gPA_bp + labs(list(
  title= "mmse vs PA scores for Males and Females",
  x="Between person PA", y="MMSE"))
gPA_bp
#females tend to have higher mmse scores then males who exercise just as much as them

#wp
# @knitr graph-PAwp_mmse -------------------- 
gPA_wp<- ggplot2::ggplot(dtest, aes_string(x= "phys_wp", y="mmse", colour="female")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gPA_wp <- gPA_wp + labs(list(
  title= "within person effects of PA scores vs mmse for Males and Females",
  x="Within person PA", y="MMSE"))
gPA_wp


# multiplot(gPA_bp, gPA_wp, cols=1)
#females tend to have higher mmse scores then males who exercise just as much as them

#Males who walk less than normal have higher mmse scores than females who are exercising less
#moreover, males who walk more than normal, have lower mmse scores than females who are exercising more than usual
#it seems like variation in PA effects women more so than men, where walking less than their average results
#in lower mmse scores, but walking higher than average results in higher mmse scores

# @knitr BasicGraph6 --------------------
#
#induvidual growth curves
length(unique(dtest$id)) #2842

set.seed(2)
ids <- sample(dtest$id,50)
d <- dtest %>%  dplyr::filter( id %in% ids)

p1 <- ggplot(d, aes(x=phys_wp, y=mmse, colour=female, group=id)) +
  geom_line() +
  stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
ggtitle("Growth curve for individuals")
p1


#there are a select few males who are walking much higher tha



saveRDS(dtest, "./data/unshared/derived/obas/dwn_obas_mmse.rds")
saveRDS(dbio, "./data/unshared/derived/obas/dbio_obas_mmse.rds")









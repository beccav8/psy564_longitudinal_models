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

path_input2  <- "./data/unshared/derived/obas/dwn_obas_mmse.rds"
path_input1<- "./data/unshared/derived/obas/dbio_obas_mmse.rds"
path_input0  <- "./data/unshared/derived/obas/data.rds"     

# @knitr load-data --------------------
ds <- readRDS(path_input0)
dwn <- readRDS(path_input2)
dbio<- readRDS(path_input1)


# @knitr define-variables --------------------

#phys_bl_bp = each persons baseline score - the baseline grand mean (between person baseline compare)
#phys_bp_mean = the persons mean score across occasions - the grand mean 
#phys_bp_median = the persons median score across occasions - the grand median  
#phys_wp = that persons score at time j, minus their mean (deviations-from--own-mean)
#mmse_wp = the persons mmse score at time j, minus their mean, so that a positive value indicated scores higehr then their mean 
#biomarker_high = indicates if the person's mean on that particular biomarker is above the 3rd quartile



# @knitr tweak-data -----------------------------------
dtest<-dwn
# @knitr graph --------------------  

gmmse<- ggplot2::ggplot(dtest, aes_string(x= "years_in_study", y="mmse")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gmmse <- gmmse + labs(list(
  title= "Raw mmse scores Over Time",
  x="time", y="mmse"))
gmmse
#mmse declines over time, but there is much more variabliltiy in the middle of the study time
#low scores in mid

gPA<- ggplot2::ggplot(dtest, aes_string(x= "years_in_study", y="walk_now")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA <- gPA + labs(list(
  title= "Raw PA scores Over Time",
  x="time", y="Raw Physical Activity"))
gPA
#people are exercising much less over time 

# multiplot(gPA, gmmse, cols=1)  

######time versus physical activity

gPA_bp<- ggplot2::ggplot(dtest, aes_string(x= "years_in_study", y="phys_bp_median")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  # coord_cartesian(ylim = c(0,20)) +
  main_theme 
gPA_bp <- gPA_bp + labs(list(
  title= "Between person effects of PA scores Over Time",
  x="time", y="Between person"))
gPA_bp
#looks like people are actually walking more than average over time?



gPA_wp<- ggplot2::ggplot(dtest, aes_string(x= "years_in_study", y="phys_wp")) +
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

# multiplot(gPA_bp, gPA_wp, cols=1)


###############mmse versus PA
#raw
gPA<- ggplot2::ggplot(dtest, aes_string(x= "walk_now", y="mmse")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gPA <- gPA + labs(list(
  title= "Raw PA versus mmse ",
  x="Raw PA", y="MMSE"))
gPA
#higher PA is associated with higher mmse scores, but this could be because it is 
#earlier in the study when PA is higher (i.e a function of age)

#bp
gPA_bp<- ggplot2::ggplot(dtest, aes_string(x= "phys_bp_median", y="mmse")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gPA_bp <- gPA_bp + labs(list(
  title= "mmse vs PA scores",
  x="Between person PA", y="MMSE"))
gPA_bp
#same idea

#wp

gPA_wp<- ggplot2::ggplot(dtest, aes_string(x= "phys_wp", y="mmse")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim = c(0,30)) +
  main_theme 
gPA_wp <- gPA_wp + labs(list(
  title= "within person effects of PA scores vs mmse",
  x="Within person PA", y="MMSE"))
gPA_wp
#walking more than average is associated with higher mmse scores (also likely because they walk more when they are younger, and age brings their mean down)

# multiplot(gPA_bp, gPA_wp, cols=1)

# @knitr BasicGraph6 --------------------
#
#induvidual growth curves
length(unique(dtest$id)) #2842

set.seed(2)
ids <- sample(dtest$id,50)
d <- dtest %>%  dplyr::filter( id %in% ids)

p1 <- ggplot(d, aes(x=phys_wp, y=mmse, group=id)) +
  geom_line() +
  stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1




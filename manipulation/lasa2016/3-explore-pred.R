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
path_input0  <- "./data/unshared/derived/lasa_2016/dto_3center.rds" 


options(scipen=20)
# ----- load-data ------

ds0  <- readRDS(path_input0) #total raw data  
names(ds0)
# str(ds0)
length(unique(ds0$id))  #4109 participants  1852 in MAP


#--demographics-------------------------------

# Baseline measures
bl <- ds0[ which(ds0$wave==1), ] #baseline subset

#sex
table(bl$male)
# FALSE  TRUE 
# 2128  1981 

ds0$maleg<-as.character(ds0$male)

#sex for graphing

table(ds0$maleg)
# FALSE=0=male  TRUE=1=female
# 14896         13867 

head(ds0$maleg)
ds0$maleg[ds0$maleg == "FALSE"] <- 0
ds0$maleg[ds0$maleg == "TRUE"] <- 1


ds0$maleg <- ifelse(ds0$maleg == 1, 
                    c("Female"), c("Male")) 

library(reshape)
ds0 <- rename(ds0, c(maleg="Sex"))

table(ds0$Sex)



#education
mean(ds0$edu, na.rm=TRUE)  # 9.15     MAP: 14.73 (SD= 3.16, range= 0-28) 
range(ds0$edu, na.rm=TRUE) # 5-18
sd(ds0$edu, na.rm=TRUE)    # 3.41

describe(ds0$age_bl)
hist(ds0$age_bl)

#year in study--------------------------

range(ds0$wave, na.rm=TRUE)  #wave 1-7 in LASA,  MAP: year in study: 0 to 18
mean(ds0$wave, na.rm=TRUE)   #4.06
describe(ds0$wave)


#-----explore-predictors-------------------------
names(ds0)
#pss
hist(ds0$pss,
     main="PSS",
     xlim=c(0,40))

describe(ds0$pss)
describeBy(ds0$pss, ds0$wave) #i.e. only wave 7

agostino.test(ds0$pss) #data is skewed 



eq_0 <- as.formula("nle ~ 1 +            
                   (1  |id)")
0.07515 / (  0.07515 + 0.18618) 
model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))
0.2159 / (0.2159 +  0.1378)

hist(ds0$nle,
     main="NLE",
     xlim=c(0,10))

describe(ds0$nle)
describeBy(ds0$nle, ds0$wave) 

agostino.test(ds0$nle) #data is skewed 

#PA
describe(ds0$phys)
hist(ds0$phys,
     main="Physical Activity",
     xlim=c(0,30))


eq_0 <- as.formula("phys ~ 1 +
                   (1  |id)")

model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE)
lmerTest::summary((model_ucm))

# ICC = random intercept vari / sum of random int var + risidual var
9.022 / (9.022 + 8.903 ) #50.33 % BP

agostino.test(ds0$phys) #skewed 

names(ds0)


#----cognitive-----------------------------------------------------------------------

# CODING in LASA- MAP:sdmt  ------- perceptual speed

hist(ds0$coding_mean,
     main="Processing Speed",
     xlim=c(0,45))


describe(ds0$coding_mean) # mean= 25.28, sd=6.51,  min=5.33- 43.33

agostino.test(ds0$coding_mean) #looks normal, but has skew

eq_0 <- as.formula("coding_mean ~ 1 +            
                   (1  |id)")
model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))
28.76  / (28.76  + 13.52) #68%

names(ds0)


#in LASA - 15 word test - Episodic memory, in MAP dig_b --- working memory (only 1 unsync wave in LASA)
hist(ds0$word_test,
     main="15 Word Test",
     xlim=c(0,45))

describe(ds0$word_test) # mean= 8.87, sd=2.45,  min=0  - 15
agostino.test(ds0$dig_b)

eq_0 <- as.formula("word_test ~ 1 +            
                   (1  |id)")
model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))
2.714 / (2.714 +3.267) #45%


#--- graphs


set.seed(1)
ids <- sample(ds0$id,20)
graph_sample <- ds0 %>%  dplyr::filter( id %in% ids)
length(unique(graph_sample$id))

names(ds0)

#indiviudal growth plots-------------------------------

#percep_speed-coding_mean
indwm<- ggplot(graph_sample, aes(x= wave, y= coding_mean)) +geom_point()
indwm + facet_wrap(~id, nrow=4) +
  stat_smooth(method=lm, se=TRUE)+
  theme1
#episodic memory
indwm<- ggplot(graph_sample, aes(x= wave, y= word_test)) +geom_point()
indwm + facet_wrap(~id, nrow=4) +
  stat_smooth(method=lm, se=TRUE)+
  theme1
#mmse
indwm<- ggplot(graph_sample, aes(x= wave, y= mmse)) +geom_point()
indwm + facet_wrap(~id, nrow=4) +
  stat_smooth(method=lm, se=TRUE)+
  theme1
#mastery5
indwm<- ggplot(graph_sample, aes(x= wave, y= mastery5)) +geom_point()
indwm + facet_wrap(~id, nrow=4) +
  stat_smooth(method=lm, se=TRUE)+
  theme1

# cognition over time -------------------------------------------------------------------------------
g1<- ggplot2::ggplot(ds0, aes_string(x= "wave", y="coding_mean", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g1 <- g1 + labs(list(
  # title= "Changes in Working Memory Over Time, by Gender",
  x="Year in Study", y="perceptual Speed"))
g1

g2<- ggplot2::ggplot(ds0, aes_string(x= "wave", y="word_test", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g2 <- g2 + labs(list(
  # title= "Changes in Working Memory Over Time, by Gender",
  x="Year in Study", y="Episodic Memory"))
g2

g3<- ggplot2::ggplot(ds0, aes_string(x= "wave", y="raven_total", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g3 <- g3 + labs(list(
  # title= "Changes in Working Memory Over Time, by Gender",
  x="Year in Study", y="Fluid intelligence"))
g3

g4<- ggplot2::ggplot(ds0, aes_string(x= "wave", y="mmse", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g4 <- g4 + labs(list(
  # title= "Changes in Working Memory Over Time, by Gender",
  x="Year in Study", y="MMSE"))
g4


#Stress over time------------------------------------------------------
g5<- ggplot2::ggplot(ds0, aes_string(x= "wave", y="nle", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g5 <- g5 + labs(list(
  # title= "Changes in Working Memory Over Time, by Gender",
  x="Year in Study", y="NLE"))
g5

#very slight increase onver time, but widowhood and relocation are missing
names(ds0)

g6<- ggplot2::ggplot(ds0, aes_string(x= "wave", y="mastery5", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g6 <- g6 + labs(list(

  x="Year in Study", y="Mastery"))
g6

#nothing happening over time

g7<- ggplot2::ggplot(ds0, aes_string(x= "wave", y="se", linetype="Sex")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g7 <- g7 + labs(list(
  
  x="Year in Study", y="self-efficacy"))
g7

#nothing happening over time




####################-----BP effects---------------------------------------------------------

#phys BP-------------------
g1<- ggplot2::ggplot(ds0, aes_string(x= "phys_bp", y="coding_mean")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g1 <- g1 + labs(list(
  x="PA_BP", y="coding_mean"))
g1
#average bp PA doesnt seem to be related to coding 

g2<- ggplot2::ggplot(ds0, aes_string(x= "phys_bp", y="word_test")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g2 <- g2 + labs(list(
  x="PA_BP", y="episodic memory"))
g2

#it looks like more average PA is associated with WORSE episodic memory

names(ds0)
g3<- ggplot2::ggplot(ds0, aes_string(x= "phys_bp", y="raven_total")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g3 <- g3 + labs(list(
  x="PA_BP", y="Fluid intelligence"))
g3

#expected effects, higher average BP PA is associated with fluid intelligence 

g4<- ggplot2::ggplot(ds0, aes_string(x= "phys_bp", y="mmse")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g4 <- g4 + labs(list(
  x="PA_BP", y="MMSE"))
g4

#nothing is happening

#nle BP -------------------------------------------------------
g1<- ggplot2::ggplot(ds0, aes_string(x= "nle_bp", y="coding_mean")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g1 <- g1 + labs(list(
  x="nle_bp", y="coding_mean"))
g1
#higher stress, better processing speed 

g2<- ggplot2::ggplot(ds0, aes_string(x= "nle_bp", y="word_test")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g2 <- g2 + labs(list(
  x="nle_bp", y="episodic memory"))
g2

#higher stress, better episodic memory


names(ds0)
g3<- ggplot2::ggplot(ds0, aes_string(x= "nle_bp", y="raven_total")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g3 <- g3 + labs(list(
  x="nle_bp", y="Fluid intelligence"))
g3
#higher stress, better fluid intelligence

g4<- ggplot2::ggplot(ds0, aes_string(x= "nle_bp", y="mmse")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g4 <- g4 + labs(list(
  x="nle_bp", y="MMSE"))
g4

#higher stress, higher mmse



############## WP- effects ------------------------------------------------------------------------------------------

#phys wP-------------------
g1<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="coding_mean")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g1 <- g1 + labs(list(
  x="PA_wp", y="coding_mean"))
g1
# higher pa than usual is associated with higher coding 

g2<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="word_test")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g2 <- g2 + labs(list(
  x="PA_wp", y="episodic memory"))
g2

#same

names(ds0)
g3<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="raven_total")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g3 <- g3 + labs(list(
  x="PA_wp", y="Fluid intelligence"))
g3

#higher PA than usual, better fluid intelligence 

g4<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="mmse")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g4 <- g4 + labs(list(
  x="PA_wp", y="MMSE"))
g4

#same

#nle wp -------------------------------------------------------
g1<- ggplot2::ggplot(ds0, aes_string(x= "nle_wp", y="coding_mean")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g1 <- g1 + labs(list(
  x="nle_wp", y="coding_mean"))
g1
#higher stress than usual, worse processing speed

g2<- ggplot2::ggplot(ds0, aes_string(x= "nle_wp", y="word_test")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g2 <- g2 + labs(list(
  x="nle_wp", y="episodic memory"))
g2

#no relationship 

names(ds0)
g3<- ggplot2::ggplot(ds0, aes_string(x= "nle_wp", y="raven_total")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g3 <- g3 + labs(list(
  x="nle_wp", y="Fluid intelligence"))
g3
#higher stress than usual, worse fluid intelligence

g4<- ggplot2::ggplot(ds0, aes_string(x= "nle_wp", y="mmse")) +
  geom_point(shape=10, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g4 <- g4 + labs(list(
  x="nle_wp", y="MMSE"))
g4

#nothing is happening




# ---- save-to-disk ------------------------------------------------------------

# Save as a compressed, binary R dataset.  
# It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(ds0, file="./data/unshared/derived/lasa_2016/dto_4analyses.rds", compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/lasa_2016/dto_4analyses.rds")
names(dto)
# this is a flat data.frame containing combined variable

# # The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# # Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/map2016/Level1_models_full_workingmem.R",
#   output="./manipulation/map2016/output/level1_models_raven_total_full.md"
# )
# # The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!

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
path_input0  <- "./data/unshared/derived/lasa_2016/dto_4analyses.rds"

# ----- load-data ------
ds0  <- readRDS(path_input0) #total raw data  
names(ds0)
# str(ds0)

options(scipen=20)

str(ds0)

# ----- Fully-unconditional-model ------
#yi= B0 + ei
eq_0 <- as.formula("raven_total ~ 1 +            
                   (1  |id)")

model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))

#ICC




eq <- as.formula("raven_total ~ 1 + wave +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))


eq1 <- as.formula("raven_total ~ 1 + wave +          
                  ( 1 + wave |id)")
model1<- lmerTest::lmer(eq1, data=ds0, REML= FALSE) 
lmerTest::summary((model1))



anova(model, model1) 





eq4 <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu + 
                  ( 1 + wave |id)")
model_4<- lmerTest::lmer(eq4, data=ds0, REML= FALSE) 
lmerTest::summary((model_4))


#deviance and df compared to model 1 , sig  better fit
anova(model1, model_4)


#Physical Activity --------------
eq5 <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
                  wave*phys_bp + phys_wp +
                  ( 1 + wave |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))



eq6 <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
                  + phys_bp*wave + phys_wp +
                  ( 1 + wave + phys_wp |id)")
model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE) 
lmerTest::summary((model_6))

anova(model_5, model_6)
# NS


#wp varience explained compred to the random effects of time only
summary(model1)
(2.73639 -    2.190346  )/(2.73639)  #20%

# 
# #varience in the intercept explained by PA?
# (24.5726 - 24.1624) / 24.5726
# 
# #varience in the slope explained by PA? 
# (0.3101 - 0.2732) / 0.3101

# # gender X PA
# eq6 <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
#                   phys_bp*male + phys_wp*male +
#                   ( 1 + wave + phys_wp|id)")
# model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE)
# lmerTest::summary((model_6))



#stress------------------------------

#----------pss-

# eq5b <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
#                    pss_pmeanC*wave + pss_wp +
#                    ( 1 + wave  |id)")

#pss is only measured at one wave

names(ds0)
eq5b <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
                   pss_gmc +
                   ( 1 + wave  |id)")

#pss_bp is the score at wave 7, but treated as a time invarient covariate
#this is problematic becaue it guages pss over the past month, and shouldnt be
#generalized to all waves
#but, pss_gmc (which is not treated as TICV, but just is one point of data) has too
#few observations to run

model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))


#-------------nle-

names(ds0)
eq6a <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
                   nle_bp*wave + nle_wp +
                   ( 1 + wave  |id)")


eq6b <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
                   nle_bp*wave + nle_wp +
                   ( 1 + wave + nle_wp |id)")

model_6a<- lmerTest::lmer(eq6a, data=ds0, REML= FALSE) 
lmerTest::summary((model_6a))

model_6b<- lmerTest::lmer(eq6b, data=ds0, REML= FALSE) 
lmerTest::summary((model_6b))

anova(model_6a, model_6b)
#        Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 15 15206 15300 -7588.3    15176                             
# ..1    18 15194 15306 -7578.8    15158 18.944      3  0.0002808 ***

#only nle wp is sig (+)

#model 6b
#fix below numbers ----
# #int 18.91792
# 4.355 / (sqrt(3208))
# #year  0.12653
# 0.3557 / (sqrt(3208))
# #pss_wp 0.10
# 0.32/ (sqrt(3208))
# #resid   6.69
# 2.58/ (sqrt(3208))


# eq5b <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
#                    nle_bp*male*wave + nle_bp*male +
#                    ( 1 + wave + nle_wp |id)")
# model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE)
# lmerTest::summary((model_5b))



################# interaction with stress 
#---- PSS and interaction


#Physical Activity --------------

eq7 <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  +  wave*edu +
                  nle_bp*phys_bp + nle_bp*phys_wp +
                  ( 1 + wave + nle_wp  |id)")
model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
lmerTest::summary((model_7))

eq7a <- as.formula("raven_total ~ 1 + wave*age_bl_gmc + wave*male  +  wave*edu +
                   nle_wp*phys_bp + nle_wp*phys_wp +
                   ( 1 + wave + nle_wp  |id)")
model_7a<- lmerTest::lmer(eq7a, data=ds0, REML= FALSE) 
lmerTest::summary((model_7a))


#graphs

g1<- ggplot2::ggplot(ds0, aes_string(x= "nle_bp", y="raven_total")) +
  stat_smooth(method=lm, colour= "black", se=TRUE)+
  geom_point(size=1)

g1 <- g1 + labs(list(
  title= "Coupled Change between Physical Activity and Symbol Digit Modality",
  x="NLE (WP)", y="raven_total"))


g1<- g1 + theme(text=element_text(family='Times'),
                # legend.title=element_blank())
                panel.background = theme_rect(fill = "white", colour="black"))


g1<- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank())

g1 <- g1 +theme(panel.background = element_rect(fill = "white", colour = "black",
                                                size = 1))
g1


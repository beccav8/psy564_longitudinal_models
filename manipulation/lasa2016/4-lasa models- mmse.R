# # The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# # Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/map2016/Level1_models_full_workingmem.R",
#   output="./manipulation/map2016/output/level1_models_mmse_full.md"
# )
# # The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!

# ----- load-source ------

rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(lmerTest)
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
str(ds0)

options(scipen=20)

str(ds0)

describe(ds0$PA_bl_BP)

# ----- Fully-unconditional-model ------
#yi= B0 + ei
eq_0 <- as.formula("mmse ~ 1 +            
                   (1  |id)")

model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))

#ICC
1.708  / (1.708  + 2.891)

100-37
# 37% BP and 63 % WP (i.e makes sense, people are likely to differ from others more than themselves)


#SE = SD/ sqrt(n) fix these #'s
# #int
# 2.714 / sqrt(1299)
# #resid
# 3.267/ sqrt(1299)

# 1.96*sqrt(2.714 ) ? check how to do this again - hoffman
# -0.12210 + (1.96*sqrt(28.76))
# -0.12210 - (1.96*sqrt(28.76)) 
# #CI -11 - 11

#residual chi square test or wald test to determine if there is significant variability in outcome
#HLM



eq <- as.formula("mmse ~ 1 + wave +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))


eq1 <- as.formula("mmse ~ 1 + wave +          
                  ( 1 + wave |id)")
model1<- lmerTest::lmer(eq1, data=ds0, REML= FALSE) 
lmerTest::summary((model1))


# #----------table 1------- UCM vs time model (re)----------------
# 
# sjp.lmer(model_ucm, model1, depvar.labels= c("Model 0", "Model 1"),
#          p.numeric=FALSE, show.icc = FALSE, show.r2 = FALSE, show.ci=FALSE, show.se=TRUE,
#          show.re.var = TRUE)
# 
# #--------------------------------------------------------



#2df, dif of 80
anova(model, model1) 
#object: mmse ~ 1 + wave + (1 | id)
# ..1: mmse ~ 1 + wave + (1 + wave | id)
# Df    AIC   BIC  logLik deviance Chisq Chi Df            Pr(>Chisq)    
# object  4 15680 15706 -7836.2    15672                                   
# ..1     6 15326 15364 -7657.2    15314 358.1      2 < 0.00000000000000022 ***
#   ---
#pseudo r^2 (percent of additional residual var accounted for)

 
# 24 % of the residual varience from the first model was accounted for by the 
# inclusion of random effects of wave in model 1
(2.891 -   2.2049) / 2.891


# demographics -------------------------------------

eq2 <- as.formula("mmse ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc + 
                  ( 1 + wave |id)")
model_2<- lmerTest::lmer(eq2, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))


#deviance and df compared to model 1 , sig  better fit
anova(model1, model_2)
# #      Df   AIC   BIC  logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object  6 15326 15364 -7657.2    15314                                        
# ..1    12 15210 15285 -7592.9    15186 128.62      6 < 0.00000000000000022 ***

# higher wave is associated with poorer scores
# being a male are associated with poorer baseline scores
# higher edu_gmc at baseline is associated with higher scores

#no demographics effected rate of decline 


# #Physical Activity --------------
# eq3a <- as.formula("mmse ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
#                   wave*phys_bp + phys_wp +
#                   ( 1 + wave |id)")
# model_3a<- lmerTest::lmer(eq3a, data=ds0, REML= FALSE) 
# lmerTest::summary((model_3a))
# 
# (0.8714 - 0.86927 ) / 0.8714            
# (0.0870 - 0.08668) / 0.0870
# 
# anova(model_2, model_3a)
# 
# 
# # Number of obs: 3848, groups:  id, 550
# 
# eq3b <- as.formula("mmse ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
#                   + phys_bp*wave + phys_wp +
#                   ( 1 + wave + phys_wp |id)")
# model_3b<- lmerTest::lmer(eq3b, data=ds0, REML= FALSE) 
# lmerTest::summary((model_3b))
# 
# anova(model_3a, model_3b)
# # 
# #        Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
# # object 15 15213 15307 -7591.7    15183                            
# # ..1    18 15204 15316 -7583.8    15168 15.705      3   0.001303 **
# 
# #wp_pa sig improves model fit!   
# 
# 
# #compared to model 1B (time slope), residuals
# 
# (2.2049 - 2.19034) / 2.2049
# 
# 
# #model 6
#     #fix these values ---
# # #int
# # 2.147836
# # 1.4655  / (sqrt(1299))            
# # #wave 
# # 0.0375086
# # 0.1937 / (sqrt(1299))            
# # #phys_wp        
# # 0.0008881
# # 0.0298 /(sqrt(1299))        
# # #Residual                
# # 2.6944449 
# # 1.6415/(sqrt(1299))    
# 
# 
# 
# #wp varience explained compred to the random effects of time only
# summary(model1)
# (2.73639 -    2.190346  )/(2.73639)  #20%
# 
# # 
# # #varience in the intercept explained by PA?
# # (24.5726 - 24.1624) / 24.5726
# # 
# # #varience in the slope explained by PA? 
# # (0.3101 - 0.2732) / 0.3101
# 

eq3a <- as.formula("mmse ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                  phys_bp*wave +  phys_wp +
                   ( 1 + wave |id)")
model_3a<- lmerTest::lmer(eq3a, data=ds0, REML= FALSE) 
lmerTest::summary((model_3a))


eq3b <- as.formula("mmse ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                   + phys_bp*wave  + phys_wp +
                   ( 1 + wave + phys_wp |id)")
model_3b<- lmerTest::lmer(eq3b, data=ds0, REML= FALSE) 
lmerTest::summary((model_3b))


anova(model_3a, model_3b)


#stress------------------------------
#-------------nle-

names(ds0)
eq4a <- as.formula("mmse ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                   nle_bp*wave + nle_wp +
                   ( 1 + wave  |id)")

#compared to demographic model, intercept and slope varience 
(0.87454 - .871 ) / 0.87454        
(0.08656 - 0.087) /0.08656


eq4b <- as.formula("mmse ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                   nle_bp*wave + nle_wp +
                   ( 1 + wave + nle_wp |id)")

model_4a<- lmerTest::lmer(eq4a, data=ds0, REML= FALSE) 
lmerTest::summary((model_4a))

anova(model_2, model_4a)


model_4b<- lmerTest::lmer(eq4b, data=ds0, REML= FALSE) 
lmerTest::summary((model_4b))



anova(model_4a, model_4b)

#resid varience compared to time slope only
(2.2049 -  2.18889)/2.2049
  
  
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


# eq5b <- as.formula("mmse ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
#                    nle_bp*male*wave + nle_bp*male +
#                    ( 1 + wave + nle_wp |id)")
# model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE)
# lmerTest::summary((model_5b))


# #-----------------------table 2-------------------------------------------------------------
# sjt.lmer(model_2, model_3b, model_4b, depvar.labels= c("Model 2", "Model 3b", "Model 4b"),
#          p.numeric=FALSE, show.icc = FALSE, show.r2 = FALSE, show.ci=FALSE, show.se=TRUE,
#          pred.labels = c("Time", "Age at baseline", "MaleT", "Education", "time x Age at baseline", "Time x MaleT", 
#                          "Time x Education", "PA_BP", "PA_WP", "Time x PA_BP", "NLE_BP", "NLE_WP", "Time x NLE_BP"))  
# 
# #-------------------------------------------------------------------------------------------


################# interaction with stress 

#-------- interaction


eq5 <- as.formula("mmse ~ 1 + wave*age_bl_gmc + wave*male  +  wave*edu_gmc + 

                  nle_bp*phys_bp + nle_bp*phys_wp +
                  nle_wp*phys_bp + nle_wp*phys_wp +
                 
                  ( 1 + wave + nle_wp + phys_wp |id)")


model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))


# #table 3------------------------------------------------------------------------------
# 
# sjt.lmer(model_5, depvar.labels= c("Model 5"),
#          p.numeric=FALSE, show.icc = FALSE, show.r2 = FALSE, show.ci=FALSE, show.se=TRUE,
#          pred.labels = c("Time", "Age at baseline", "MaleT", "Education", "NLE_BP", "PA_BP", "PA_WP", "NLE_WP", "Time x Age at baseline", 
#                          "Time x MaleT", "Time x Education", "NLE_BP x PA_BP", "NLE_BP x PA_WP", "PA_BP x NLE_WP", "PA_WP x NLE_WP")) 
# 
# #-------------------------------------------------------------------------------------
# 


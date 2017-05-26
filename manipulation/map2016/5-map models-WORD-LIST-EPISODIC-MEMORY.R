# # The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# # Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/map2016/Level1_models_full_workingmem.R",
#   output="./manipulation/map2016/output/level1_models_wm_full.md"
# )
# # The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!
# 
options(scipen=20)
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
path_input0  <- "./data/unshared/derived/map2016/map_full_bio_centered.rds" 

# ----- load-data ------
ds0  <- readRDS(path_input0) #total raw data  
names(ds0)

describe(ds0$PA_bl_BP)


#models--------------------------

eq <- as.formula("wl_im ~ 1 +          
                 ( 1  |id)")
model_ucm<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))
21.48 / (21.49 + 9.53)

eq <- as.formula("wl_im ~ 1 + year_in_study +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#df= 11243  dev=43476.4


eq1 <- as.formula("wl_im ~ 1 + year_in_study +          
                  ( 1 + year_in_study |id)")
model1<- lmerTest::lmer(eq1, data=ds0, REML= FALSE) 
lmerTest::summary((model1))
#df=  11241  
#dev =  43153.4

11241  - 11243
43476.4 -  43153.4 

anova(model, model1)
anova(model_ucm, model1)

#pseudo r^2

(2.086 - 1.7793)/2.086
# 
# 
#  #----------table 1------- UCM vs time model (re)----------------
# 
#  sjt.lmer(model_ucm, model1, depvar.labels= c("Model 0", "Model 1"),
#           p.numeric=FALSE, show.icc = FALSE, show.r2 = FALSE, show.ci=FALSE, show.se=TRUE,
#           show.re.var = TRUE)
# 
#  #--------------------------------------------------------



# demographic 

eq2 <- as.formula("wl_im ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc + 
                  ( 1 + year_in_study |id)")
model_2<- lmerTest::lmer(eq2, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))


#compared to random time model
anova(model1, model_2)




#Physical Activity --------------
# eq3a<- as.formula("wl_im ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
#                   phys_pmeanC*year_in_study + phys_wp +
#                   ( 1 + year_in_study |id)")
# model_3a<- lmerTest::lmer(eq3a, data=ds0, REML= FALSE) 
# lmerTest::summary((model_3a))
# # 42023.1    11041 
# 
# 
# anova(model_2, model_3a)
# #the model with the random effects of PA is a better model than the one witout
# 
# 
# eq3b <- as.formula("wl_im ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
#                    + phys_pmeanC*year_in_study + phys_wp +
#                    ( 1 + year_in_study + phys_wp |id)")
# model_3b<- lmerTest::lmer(eq3b, data=ds0, REML= FALSE) 
# lmerTest::summary((model_3b))
# # 42020.5    11038 
# anova(model_3a, model_3b)
# 
# 
# 
# #varience in the intercept explained by PA BP?
# (14.0408   -  13.66478 ) / 14.0408
# #varience in the slope explained by PA BP? 
# (0.248 - 0.22129) / 0.248
# 
# 

#does PA moderate the effects of gender on the intercept and slope
# 
# eq5s <- as.formula("wl_im ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
#                    + phys_pmeanC*year_in_study*msex + phys_wp*msex +
#                    ( 1 + year_in_study + phys_wp |id)")
# model_5s<- lmerTest::lmer(eq5s, data=ds0, REML= FALSE) 
# lmerTest::summary((model_5s))


eq3a <- as.formula("wl_im ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
                  phys_pmeanC*year_in_study + phys_wp +
                   ( 1 + year_in_study |id)")
model_3a<- lmerTest::lmer(eq3a, data=ds0, REML= FALSE) 
lmerTest::summary((model_3a))


eq3b <- as.formula("wl_im ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
                   + phys_pmeanC*year_in_study + phys_wp +
                   ( 1 + year_in_study + phys_wp |id)")
model_3b<- lmerTest::lmer(eq3b, data=ds0, REML= FALSE) 
lmerTest::summary((model_3b))


anova(model_3a, model_3b)


#stress------------------------------


#---nle---------------

eq4a <- as.formula("wl_im ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
                   nle_pmeanC*year_in_study + nle_wp +
                   ( 1 + year_in_study  |id)")
model_4a<- lmerTest::lmer(eq4a, data=ds0, REML= FALSE) 
lmerTest::summary((model_4a))
# 12719.1     3282 


#varience in the intercept explained by PA BP?
(14.0408   -  12.5848 ) / 14.0408
#varience in the slope explained by PA BP? 
(0.248 - 0.1435) / 0.248


#the addition of pss_wp in the random effects is NS
#people aren't very differnt in their stress fluctuations 
#therefore there is nothing to explain

eq4b <- as.formula("wl_im ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
                   nle_pmeanC*year_in_study + nle_wp +
                   ( 1 + year_in_study + nle_wp |id)")
model_4b<- lmerTest::lmer(eq4b, data=ds0, REML= FALSE) 
lmerTest::summary((model_4b))

# names(ds0)
# eqt <- as.formula("wl_im ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
#                    pss_pmeanC*year_in_study + pss_wp +
#                    ( 1 + year_in_study + pss_wp |id)")
# model_t<- lmerTest::lmer(eqt, data=ds0, REML= FALSE) 
# lmerTest::summary((model_t))

anova(model_4a, model_4b)

#model 2, intercept var = 2.57, slope var = 0.02 residual = 1.78
#(Intercept)   2.57387  
#year_in_study 0.02033  
#Residual      1.77776

#int: 
(2.57 - 2.64090) / 2.57 
#slope:
(0.0203 - 0.01032) /0.0203
#resid:
(1.77776 - 1.67748) / 1.77776

#resid compared to time only model 
(1.7793 -1.664)/ 1.7793

################# interaction with stress 
#---- PSS and interaction


library(sjPlot)
library(sjmisc)
#-----------------------table 2-------------------------------------------------------------
sjt.lmer(model_2, model_3b, model_4b, depvar.labels= c("Model 2", "Model 3b", "Model 4b"),
         p.numeric=FALSE, show.icc = FALSE, show.r2 = FALSE, show.ci=FALSE, show.se=TRUE,
         pred.labels = c("Time", "Age at baseline", "MaleT", "Education", "time x Age at baseline", "Time x MaleT", 
                         "Time x Education", "PA_BP", "PA_WP", "Time x PA_BP", "NLE_BP", "NLE_WP", "Time x NLE_BP"))  

#-------------------------------------------------------------------------------------------




#interacrtion --------------
# Hypothesis 3: effects of BP stress are moderated by BP and WP physical activity 
# Hypothesis 4: effects of WP stress will be moderated by BP and WP physical activity


eq5 <- as.formula("wl_im ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  +  year_in_study*edu_gmc + 
                  
                  nle_pmeanC*phys_pmeanC + nle_pmeanC*phys_wp +
                  nle_wp*phys_pmeanC + nle_wp*phys_wp +
                  
                  ( 1 + year_in_study + nle_wp |id)")


model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))

print(model_5,correlation=TRUE)

#table 3------------------------------------------------------------------------------

sjt.lmer(model_5, depvar.labels= c("Model 5"),
         p.numeric=FALSE, show.icc = FALSE, show.r2 = FALSE, show.ci=FALSE, show.se=TRUE,
         pred.labels = c("Time", "Age at baseline", "MaleT", "Education", "NLE_BP", "PA_BP", "PA_WP", "NLE_WP", "Time x Age at baseline", 
                         "Time x MaleT", "Time x Education", "NLE_BP x PA_BP", "NLE_BP x PA_WP", "PA_BP x NLE_WP", "PA_WP x NLE_WP")) 

#-------------------------------------------------------------------------------------

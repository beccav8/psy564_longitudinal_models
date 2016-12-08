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
# str(ds0)

describe(ds0$sdmt)
ds0$sdmt_origional<-ds0$sdmt
ds0$sdmt<-ds0$sdmt/2

#models--------------------------

eq <- as.formula("sdmt ~ 1 +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#resid var= 12.45

eq <- as.formula("sdmt ~ 1 + year_in_study +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#df= 10587    
#dev =  59601.7


eq <- as.formula("sdmt ~ 1 + year_in_study +          
                 ( 1 + year_in_study |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#df= 10585   
#dev =  58014.5

10587-10585
59601.7-58014.5



#pseudo r^2

(12.45 - 7.1566 ) / 12.45



# #AGE BL-------------
# 
# eq2 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + 
#                   ( 1 + year_in_study |id)")
# model_2<- lmerTest::lmer(eq2, data=ds0, REML= FALSE) 
# lmerTest::summary((model_2))
# 
# #chi sq
# #df
# #df= 
# 10585 -  10583 
# #deviance
# 58014.5- 57653.7 
# 
# 
# #int 27.5670
# 5.2504/ (sqrt(10591))
# #year 0.3081
# 0.5551/ (sqrt(10591))
# #resid 7.1819
# 2.6799/ (sqrt(10591))
# 
# ################ + gender
# 
# eq3 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + 
#                   ( 1 + year_in_study |id)")
# model_3<- lmerTest::lmer(eq3, data=ds0, REML= FALSE) 
# lmerTest::summary((model_3))
# 
# #df= 
# 10583- 10581
# #dev =  
# 57653.7-57644.1
# 
# 
# #int 27.3997
# 5.2345/ (sqrt(10591))
# #year  0.308
# 0.5551/ (sqrt(10591))
# #resid 7.1821
# 2.6799 / (sqrt(10591))
# 


################# + education 


eq4 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu + 
                  ( 1 + year_in_study |id)")
model_4<- lmerTest::lmer(eq4, data=ds0, REML= FALSE) 
lmerTest::summary((model_4))



#Physical Activity --------------
eq5 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                  year_in_study*phys_pmeanC + phys_wp +
                  ( 1 + year_in_study |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))

#the model with the random effects of PA is a better model than the one witout
56614.6- 56600.6 
10451 - 10448 


eq5 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   + phys_pmeanC*year_in_study + phys_wp +
                  ( 1 + year_in_study + phys_wp |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))

#df= 
10579 - 10449 
#dev =  
57471.3 - 56616.9

#int 24.1643
4.9096/ (sqrt(10466))
#year  0.271076
0.5206 / (sqrt(10466))
#phys_wp 0.0048
0.0695/ (sqrt(10466))
#resid  7.073466
2.6596  / (sqrt(10466))

#wp varience explained compred to the random effects of time only
(7.1566 -  7.066991)/(7.1566)


#varience in the intercept explained by PA?
(24.5726 - 24.1624) / 24.5726

#varience in the slope explained by PA? 
(0.3101 - 0.2732) / 0.3101

# # gender X PA
# eq6 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
#                   phys_pmeanC*msex + phys_wp*msex +
#                   ( 1 + year_in_study + phys_wp|id)")
# model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE)
# lmerTest::summary((model_6))
# 


#stress------------------------------

eq5b <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   pss_pmeanC*year_in_study + pss_wp +
                   ( 1 + year_in_study  |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))
#df= 

#the addition of pss_wp in the random effects is NS
#people aren't very differnt in their stress fluctuations 
#therefore there is nothing to explain
17674.8-17672.6
3193-3190

eq5b <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   pss_pmeanC*year_in_study + pss_wp +
                   ( 1 + year_in_study + pss_wp |id)")
model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
lmerTest::summary((model_5b))
#df= 
10579 - 3191 
#dev =  
57471.3 - 17674.5

#int 18.91792
4.349 / (sqrt(3208))
#year  0.12653
0.3557 / (sqrt(3208))
#pss_wp 0.08321
0.2885/ (sqrt(3208))
#resid   6.70488
2.5894/ (sqrt(3208))


# eq5b <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
#                    pss_pmeanC*msex + pss_wp*msex +
#                    ( 1 + year_in_study + pss_wp |id)")
# model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
# lmerTest::summary((model_5b))



################# interaction with stress 
#---- PSS and interaction


#Physical Activity --------------

eq7 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  +  year_in_study*edu +
                  phys_pmeanC*year_in_study*pss_pmeanC  + phys_wp*pss_pmeanC +
                  ( 1 + year_in_study + phys_wp  |id)")
model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
lmerTest::summary((model_7))


g1<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="sdmt")) +
  geom_point(shape=4, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g1 <- g1 + labs(list(
  title= "Coupled Change between Physical Activity and Symbol Digit Modality",
  x="Physical Activity (WP)", y="SDMT"))
g1

g2<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="dig_b")) +
  geom_point(shape=4, size=1)+
  stat_smooth(method=lm, se=TRUE)+
  theme1
g2 <- g2 + labs(list(
  title="Coupled Change between Physical Activity and Digit Span Backward",
  x="Physical Activity", y="Digit Span Backward"))
g2

multiplot(g1, g2)







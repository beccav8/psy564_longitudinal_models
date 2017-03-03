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
# ds0$sdmt_origional<-ds0$sdmt
# ds0$sdmt<-ds0$sdmt/2

describe(ds0$pss)
eq <- as.formula("pss ~ 1 +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
0.07515/ (0.07515 + .186)



describe(ds0$nle)
eq <- as.formula("nle ~ 1 +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))

1.145 / (2.516 + 1.14 )
#models--------------------------

eq <- as.formula("sdmt ~ 1 +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#resid var= 12.45
137.91 / (137.91 + 49.81)

eq1 <- as.formula("sdmt ~ 1 + year_in_study +          
                 ( 1  |id)")
model1<- lmerTest::lmer(eq1, data=ds0, REML= FALSE) 
lmerTest::summary((model1))



eq2 <- as.formula("sdmt ~ 1 + year_in_study +          
                 ( 1 + year_in_study |id)")
model2<- lmerTest::lmer(eq2, data=ds0, REML= FALSE) 
lmerTest::summary((model2))

anova(model1, model2)


#pseudo r^2

(12.45 - 7.1566 ) / 12.45

(39.37 - 28.626)/ 39.37


# ################ + gender
# 
# eq3 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + 
#                   ( 1 + year_in_study |id)")
# model_3<- lmerTest::lmer(eq3, data=ds0, REML= FALSE) 
# lmerTest::summary((model_3))

################# demographic 

eq4 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc + 
                  ( 1 + year_in_study |id)")
model_4<- lmerTest::lmer(eq4, data=ds0, REML= FALSE) 
lmerTest::summary((model_4))



anova(model2, model_4)



#Physical Activity --------------

eq5 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
                  year_in_study*phys_pmeanC + phys_wp +
                  ( 1 + year_in_study |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))

# varience around intercept of demographic model
(98.29 - 96.650 ) / 98.29
# time
( 1.24 - 1.093) /1.24

( 28.71 - 28.39) /  28.71 


anova(model_4, model_5) #not fitted to same size data set- makes sense 

eq5a <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
                   + phys_pmeanC*year_in_study + phys_wp +
                  ( 1 + year_in_study + phys_wp |id)")
model_5a<- lmerTest::lmer(eq5a, data=ds0, REML= FALSE) 
lmerTest::summary((model_5a))

anova(model_5, model_5a)


#wrong values, but this is how you test sig
#int
24.112608 
4.9105  / (sqrt(10466))            
#year_in_study 
0.265695
0.5155 / (sqrt(10466))            
#phys_wp        
0.005506
0.0742/(sqrt(10466))        
#Residual                
7.066991
2.6584/(sqrt(10466))    


#wp varience explained compred to the random effects of time only
( 28.71 - 28.39) /  28.71 



#stress------------------------------

#PSS ----

# eq6 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
#                    pss_pmeanC*year_in_study + pss_wp +
#                    ( 1 + year_in_study  |id)")
# model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE) 
# lmerTest::summary((model_6))
# #df=
# #int:18.88
# #slope:0.12
# (24.57 - 18.88)/24.57 
# (0.3101 - 0.12)/0.3101
# 
# eq6a <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
#                    pss_pmeanC*year_in_study + pss_wp +
#                    ( 1 + year_in_study + pss_wp |id)")
# model_6a<- lmerTest::lmer(eq6a, data=ds0, REML= FALSE) 
# lmerTest::summary((model_6a))
# 
# #the addition of pss_wp in the random effects is NS
# #people aren't very differnt in their stress fluctuations 
# #therefore there is nothing to explain
# 
# 
# anova(model_6, model_6a)


#NLE-- 

eq6 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
                   nle_pmeanC*year_in_study + nle_wp +
                  ( 1 + year_in_study  |id)")
model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE) 
lmerTest::summary((model_6))


#reduction of BP var compared to model 2:
# varience around intercept of demographic model
(98.29 - 74.78 ) / 98.29
# time
( 1.24 - 0.4645) /1.24

#residual var

( 28.71 - 26.996) /  28.71 

eq6a <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu_gmc +
                  nle_pmeanC*year_in_study + nle_wp +
                   ( 1 + year_in_study + nle_wp |id)")
model_6a<- lmerTest::lmer(eq6a, data=ds0, REML= FALSE) 
lmerTest::summary((model_6a))

anova(model_6, model_6a)

#        Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 15 17761 17852 -8865.3    17731                           
# ..1    18 17757 17866 -8860.4    17721 9.7997      3    0.02035 *


#reduction in residual WP compared to random time slope

(7.1566 - 6.57487) / 7.1566



################# interaction with stress 


# Interaction --------------
# Hypothesis 3: effects of BP stress are moderated by BP and WP physical activity 
# Hypothesis 4: effects of WP stress will be moderated by BP and WP physical activity


eq7 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  +  year_in_study*edu_gmc + 
                  
                  nle_pmeanC*phys_pmeanC + nle_pmeanC*phys_wp +
                  nle_wp*phys_pmeanC + nle_wp*phys_wp +
                  
                  ( 1 + year_in_study + nle_wp |id)")


model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
lmerTest::summary((model_7))


# ds0$sdmt <- ds0$sdmt/2








#graphs

g1<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="sdmt")) +
  stat_smooth(method=lm, colour= "black", se=TRUE)+
  geom_point(size=1)

g1 <- g1 + labs(list(
  title= "Coupled Change between Physical Activity and Symbol Digit Modality",
  x="Physical Activity (WP)", y="SDMT"))
  

g1<- g1 + theme(text=element_text(family='Times'),
           # legend.title=element_blank())
           panel.background = theme_rect(fill = "white", colour="black"))


g1<- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank())

g1 <- g1 +theme(panel.background = element_rect(fill = "white", colour = "black",
    size = 1))
g1


g2<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="dig_b")) +
  stat_smooth(method=lm, colour= "black", se=TRUE)+
  geom_point(size=1)


g2 <- g2 + labs(list(
  title="Coupled Change between Physical Activity and Digit Span Backward",
  x="Physical Activity (WP)", y="Digit Span Backward"))

g2<- g2 + theme(text=element_text(family='Times'),
                legend.title=element_blank())

g2<- g2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))

g2 <- g2 +theme(panel.background = element_rect(fill = "white", colour = "black",
                                                size = 1))
g2


multiplot(g1, g2)





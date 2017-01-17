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

eq1 <- as.formula("sdmt ~ 1 + year_in_study +          
                 ( 1  |id)")
model1<- lmerTest::lmer(eq1, data=ds0, REML= FALSE) 
lmerTest::summary((model1))
#df= 10587    
#dev =  59601.7


eq2 <- as.formula("sdmt ~ 1 + year_in_study +          
                 ( 1 + year_in_study |id)")
model2<- lmerTest::lmer(eq2, data=ds0, REML= FALSE) 
lmerTest::summary((model2))
#df= 10585   
#dev =  58014.5

10587-10585
59601.7-58014.5

anova(model1, model2)
#         Df   AIC   BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# object  4 59610 59639 -29801    59602                                        
# ..1     6 58027 58070 -29007    58015 1587.1      2 < 0.00000000000000022 ***

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

58014.5 - 57471.3
10585 -10579 

anova(model2, model_4)
#         Df   AIC   BIC logLik deviance  Chisq Chi Df            Pr(>Chisq)    
# 2        6 58027 58070 -29007    58015                                        
# 4       12 57495 57583 -28736    57471 543.19      6 < 0.00000000000000022 ***
  ---

#Physical Activity --------------
eq5 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                  year_in_study*phys_pmeanC + phys_wp +
                  ( 1 + year_in_study |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))


eq5a <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   + phys_pmeanC*year_in_study + phys_wp +
                  ( 1 + year_in_study + phys_wp |id)")
model_5a<- lmerTest::lmer(eq5a, data=ds0, REML= FALSE) 
lmerTest::summary((model_5a))

anova(model_5, model_5a)

#        Df   AIC   BIC logLik deviance  Chisq Chi Df Pr(>Chisq)   
#  5    15 56645 56753 -28307    56615                            
# 5a    18 56637 56767 -28300    56601 14.015      3   0.002885 **


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
(7.1566 -  7.066991)/(7.1566)  #1.2%

# 
# #varience in the intercept explained by PA?
# (24.5726 - 24.1624) / 24.5726
# 
# #varience in the slope explained by PA? 
# (0.3101 - 0.2732) / 0.3101

# # gender X PA
# eq6 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
#                   phys_pmeanC*msex + phys_wp*msex +
#                   ( 1 + year_in_study + phys_wp|id)")
# model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE)
# lmerTest::summary((model_6))
# 


#stress------------------------------

#PSS ----

eq6 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   pss_pmeanC*year_in_study + pss_wp +
                   ( 1 + year_in_study  |id)")
model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE) 
lmerTest::summary((model_6))
#df=
#int:18.88
#slope:0.12
(24.57 - 18.88)/24.57 
(0.3101 - 0.12)/0.3101

eq6a <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   pss_pmeanC*year_in_study + pss_wp +
                   ( 1 + year_in_study + pss_wp |id)")
model_6a<- lmerTest::lmer(eq6a, data=ds0, REML= FALSE) 
lmerTest::summary((model_6a))

#the addition of pss_wp in the random effects is NS
#people aren't very differnt in their stress fluctuations 
#therefore there is nothing to explain


anova(model_6, model_6a)
#        Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# 6      15 17705 17796 -8837.4    17675                         
# 6A     18 17709 17818 -8836.3    17673 2.1965      3     0.5326



#int 18.91792
4.355 / (sqrt(3208))
#year  0.12653
0.3557 / (sqrt(3208))
#pss_wp 0.10
0.32/ (sqrt(3208))
#resid   6.69
2.58/ (sqrt(3208))

#NLE-- 

eq6 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                   nle_pmeanC*year_in_study + nle_wp +
                  ( 1 + year_in_study  |id)")
model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE) 
lmerTest::summary((model_6))


eq6a <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
                  nle_pmeanC*year_in_study + nle_wp +
                   ( 1 + year_in_study + nle_wp |id)")
model_6a<- lmerTest::lmer(eq6a, data=ds0, REML= FALSE) 
lmerTest::summary((model_6a))

anova(model_6, model_6a)

#        Df   AIC   BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# object 15 17761 17852 -8865.3    17731                           
# ..1    18 17757 17866 -8860.4    17721 9.7997      3    0.02035 *


# eq5b <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  + year_in_study*edu +
#                    pss_pmeanC*msex + pss_wp*msex +
#                    ( 1 + year_in_study + pss_wp |id)")
# model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE) 
# lmerTest::summary((model_5b))



################# interaction with stress 
#---- PSS and interaction


#Physical Activity --------------

eq7 <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  +  year_in_study*edu +
                  nle_pmeanC*phys_pmeanC + nle_pmeanC*phys_wp +
                  ( 1 + year_in_study + nle_wp  |id)")
model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
lmerTest::summary((model_7))

eq7a <- as.formula("sdmt ~ 1 + year_in_study*age_bl_gmc + year_in_study*msex  +  year_in_study*edu +
                   nle_wp*phys_pmeanC + nle_wp*phys_wp +
                   ( 1 + year_in_study + nle_wp  |id)")
model_7a<- lmerTest::lmer(eq7a, data=ds0, REML= FALSE) 
lmerTest::summary((model_7a))



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





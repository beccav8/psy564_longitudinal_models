# # The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# # Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/map2016/Level1_models_full_workingmem.R",
#   output="./manipulation/map2016/output/level1_models_coding_mean_full.md"
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
eq_0 <- as.formula("coding_mean ~ 1 +            
                   (1  |id)")

model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))

#ICC
28.76 / (28.76 + 13.52)
# 68% BP and 32 % WP (i.e makes sense, people are likely to differ from others more than themselves)

#SE = SD/ sqrt(n)
#int
5.362 / sqrt(1485)
#resid
13.52/ sqrt(1485)

1.96*sqrt(28.76)
-0.12210 + (1.96*sqrt(28.76))
-0.12210 - (1.96*sqrt(28.76)) 
#CI -11 - 11

#residual chi square test or wald test to determine if there is significant variability in outcome
#HLM




eq <- as.formula("coding_mean ~ 1 + wave +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))
#df= 1481    
#dev =  8050.1


eq1 <- as.formula("coding_mean ~ 1 + wave +          
                 ( 1 + wave |id)")
model1<- lmerTest::lmer(eq1, data=ds0, REML= FALSE) 
lmerTest::summary((model1))
#df= 1479 
#dev =  7970.1 


8050.1 - 7970.1 
1481- 1479 

#2df, dif of 80
anova(model, model1) #same thing  chisq=79.997 df=2 p < 0.00000000000000022 ***

#pseudo r^2 (percent of additional residual var accounted for)

(8.203  -  6.642) / 8.203
# 20% of the residual varience from the first model was accounted for by the 
# inclusion of random effects of wave in model 1



# #AGE BL-------------
# 
# eq2 <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + 
#                   ( 1 + wave |id)")
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
# eq3 <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + 
#                   ( 1 + wave |id)")
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


eq4 <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu + 
                  ( 1 + wave |id)")
model_4<- lmerTest::lmer(eq4, data=ds0, REML= FALSE) 
lmerTest::summary((model_4))


#deviance and df compared to model 1 , sig  better fit
anova(model1, model_4)
# chisq = 63.852 df=  6  p = 0.000000000007397 ***

# higher wave is associated with poorer scores
# higher age at baseline, and being a male are associated with poorer baseline scores
# higher edu at baseline is associated with higher scores

#only age at baseline is associated with a steeper decline in slope, edu and sex dont' influence rate of decline


names(ds0)

#Physical Activity --------------
eq5 <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
                  wave*phys_bp + phys_wp +
                  ( 1 + wave |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))



eq6 <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
                  + phys_bp*wave + phys_wp +
                  ( 1 + wave + phys_wp |id)")
model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE) 
lmerTest::summary((model_6))

anova(model_5, model_6)

#        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#   5    15 7931.0 8010.6 -3950.5   7901.0                         
#   6    18 7936.9 8032.3 -3950.4   7900.9 0.1857      3     0.9799

#no sig difference when we include WP effects of PA, therefore
#no varience in WP PA fluctuations to explain by stress  


#model 6

#int
26.50393433 
5.14819  / (sqrt(1485))            
#wave 
0.30829714
0.55524 / (sqrt(1485))            
#phys_wp        
0.00004607
0.006787/(sqrt(1485))        
#Residual                
6.60962
2.5709/(sqrt(1485))    




#wp varience explained compred to the random effects of time only
summary(model1)
(6.642 -   6.60962373 )/(6.642)  #0.5%

# 
# #varience in the intercept explained by PA?
# (24.5726 - 24.1624) / 24.5726
# 
# #varience in the slope explained by PA? 
# (0.3101 - 0.2732) / 0.3101

# # gender X PA
# eq6 <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
#                   phys_bp*male + phys_wp*male +
#                   ( 1 + wave + phys_wp|id)")
# model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE)
# lmerTest::summary((model_6))



#stress------------------------------

#----------pss-

# eq5b <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
#                    pss_pmeanC*wave + pss_wp +
#                    ( 1 + wave  |id)")

#pss is only measured at one wave

names(ds0)
eq5b <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
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
eq6a <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
                   nle_bp*wave + nle_wp +
                    ( 1 + wave  |id)")


eq6b <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
                   nle_bp*wave + nle_wp +
                   ( 1 + wave + nle_wp |id)")

model_6a<- lmerTest::lmer(eq6a, data=ds0, REML= FALSE) 
lmerTest::summary((model_6a))

model_6b<- lmerTest::lmer(eq6b, data=ds0, REML= FALSE) 
lmerTest::summary((model_6b))

anova(model_6a, model_6b)
#         Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)   
# 6a      15 7923.8 8003.4 -3946.9   7893.8                            
# 6b     18 7917.6 8013.1 -3940.8   7881.6   12.204      3   0.006715 **


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


# eq5b <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu +
#                    nle_bp*male*wave + nle_bp*male +
#                    ( 1 + wave + nle_wp |id)")
# model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE)
# lmerTest::summary((model_5b))



################# interaction with stress 
#---- PSS and interaction


#Physical Activity --------------

eq7 <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  +  wave*edu +
                  nle_bp*phys_bp + nle_bp*phys_wp +
                  ( 1 + wave + nle_wp  |id)")
model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
lmerTest::summary((model_7))

eq7a <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  +  wave*edu +
                  nle_wp*phys_bp + nle_wp*phys_wp +
                  ( 1 + wave + nle_wp  |id)")
model_7a<- lmerTest::lmer(eq7a, data=ds0, REML= FALSE) 
lmerTest::summary((model_7a))


#graphs

g1<- ggplot2::ggplot(ds0, aes_string(x= "phys_wp", y="coding_mean")) +
  stat_smooth(method=lm, colour= "black", se=TRUE)+
  geom_point(size=1)

g1 <- g1 + labs(list(
  title= "Coupled Change between Physical Activity and Symbol Digit Modality",
  x="Physical Activity (WP)", y="coding_mean"))


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







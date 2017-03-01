# # The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# # Run the lines below to stitch a basic html output.
# knitr::stitch_rmd(
#   script="./manipulation/map2016/Level1_models_full_workingmem.R",
#   output="./manipulation/map2016/output/level1_models_word_test_full.md"
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
eq_0 <- as.formula("word_test ~ 1 +            
                   (1  |id)")

model_ucm<- lmerTest::lmer(eq_0, data=ds0, REML= FALSE) 
lmerTest::summary((model_ucm))

#ICC
2.714 / ( 2.714 + 3.267)
# 45% BP and 55 % WP (i.e makes sense, people are likely to differ from others more than themselves)


#SE = SD/ sqrt(n)
#int
2.714 / sqrt(1299)
#resid
3.267/ sqrt(1299)

# 1.96*sqrt(2.714 ) ? check how to do this again - hoffman
# -0.12210 + (1.96*sqrt(28.76))
# -0.12210 - (1.96*sqrt(28.76)) 
# #CI -11 - 11

#residual chi square test or wald test to determine if there is significant variability in outcome
#HLM



eq <- as.formula("word_test ~ 1 + wave +          
                 ( 1  |id)")
model<- lmerTest::lmer(eq, data=ds0, REML= FALSE) 
lmerTest::summary((model))


eq1 <- as.formula("word_test ~ 1 + wave +          
                  ( 1 + wave |id)")
model1<- lmerTest::lmer(eq1, data=ds0, REML= FALSE) 
lmerTest::summary((model1))



#2df, dif of 80
anova(model, model1) 
# object: word_test ~ 1 + wave + (1 | id)
# ..1: word_test ~ 1 + wave + (1 + wave | id)
#         Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
# FE      4 5492.1 5512.7 -2742.0   5484.1                           
#  RE     6 5489.5 5520.5 -2738.8   5477.5 6.5519      2    0.03778 *

#pseudo r^2 (percent of additional residual var accounted for)

(3.267 - 2.73) / 3.267 



# #AGE BL-------------
# 
# eq2 <- as.formula("word_test ~ 1 + wave*age_bl_gmc + 
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
# eq3 <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  + 
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


eq4 <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc + 
                  ( 1 + wave |id)")
model_4<- lmerTest::lmer(eq4, data=ds0, REML= FALSE) 
lmerTest::summary((model_4))


#deviance and df compared to model 1 , sig  better fit
anova(model1, model_4)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df   Pr(>Chisq)    
# 1       6 5489.5 5520.5 -2738.8   5477.5                               
# 4      12 5458.3 5520.4 -2717.2   5434.3 43.172      6 0.0000001079 ***

# higher wave is associated with poorer scores
# being a male are associated with poorer baseline scores
# higher edu_gmc at baseline is associated with higher scores

#no demographics effected rate of decline 


#Physical Activity --------------
eq5 <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                  wave*phys_bp + phys_wp +
                  ( 1 + wave |id)")
model_5<- lmerTest::lmer(eq5, data=ds0, REML= FALSE) 
lmerTest::summary((model_5))

#compared to demographic model
(2.33998 - 2.33544)  /2.33998      
(0.04226 - 0.04021)  /0.04226

anova(model_4, model_5)

eq6 <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                  + phys_bp*wave + phys_wp +
                  ( 1 + wave + phys_wp |id)")
model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE) 
lmerTest::summary((model_6))

anova(model_5, model_6)
# 
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
# 5      15 5462.7 5540.2 -2716.3   5432.7                         
# 6      18 5464.3 5557.4 -2714.2   5428.3 4.3992      3     0.2215

#no sig difference when we include WP effects of PA, therefore
#no varience in WP PA fluctuations to explain by stress  

#compared to 1B (time slope only)
(2.73639 - 2.6944459) / 2.73639


#model 6

#int
2.147836
1.4655  / (sqrt(1299))            
#wave 
0.0375086
0.1937 / (sqrt(1299))            
#phys_wp        
0.0008881
0.0298 /(sqrt(1299))        
#Residual                
2.6944449 
1.6415/(sqrt(1299))    



#wp varience explained compred to the random effects of time only
summary(model1)
(2.73639 -    2.6944449 )/(2.73639)  #1.5%

# 
# #varience in the intercept explained by PA?
# (24.5726 - 24.1624) / 24.5726
# 
# #varience in the slope explained by PA? 
# (0.3101 - 0.2732) / 0.3101

# # gender X PA
# eq6 <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
#                   phys_bp*male + phys_wp*male +
#                   ( 1 + wave + phys_wp|id)")
# model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE)
# lmerTest::summary((model_6))



#stress------------------------------

#----------pss-

# eq5b <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
#                    pss_pmeanC*wave + pss_wp +
#                    ( 1 + wave  |id)")

#pss is only measured at one wave

names(ds0)
eq5b <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
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
eq6a <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                   nle_bp*wave + nle_wp +
                   ( 1 + wave  |id)")


eq6b <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                   nle_bp*wave + nle_wp +
                   ( 1 + wave + nle_wp |id)")

model_6a<- lmerTest::lmer(eq6a, data=ds0, REML= FALSE) 
lmerTest::summary((model_6a))

anova(model_4, model_6a)
#int and slope varience comparison 
(2.33998 - 2.41645)/2.33998       
(0.042 - 0.04312)/0.042

model_6b<- lmerTest::lmer(eq6b, data=ds0, REML= FALSE) 
lmerTest::summary((model_6b))

anova(model_6a, model_6b)

lmerTest::summary(model1)
(2.73639 -  2.60618) / 2.73639

#only nle bp is sig

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


# eq5b <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
#                    nle_bp*male*wave + nle_bp*male +
#                    ( 1 + wave + nle_wp |id)")
# model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE)
# lmerTest::summary((model_5b))



################# interaction with stress 
#---- PSS and interaction


eq7 <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  +  wave*edu_gmc + 

                  nle_bp*wave*phys_bp + nle_bp*phys_wp +
                  nle_wp*phys_bp + nle_wp*phys_wp +
                 
                  ( 1 + wave + nle_wp |id)")


model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
lmerTest::summary((model_7))




#Physical Activity --------------

eq7 <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  +  wave*edu_gmc +
                  nle_bp*phys_bp + nle_bp*phys_wp +
                  ( 1 + wave + nle_wp  |id)")
model_7<- lmerTest::lmer(eq7, data=ds0, REML= FALSE) 
lmerTest::summary((model_7))

eq7a <- as.formula("word_test ~ 1 + wave*age_bl_gmc + wave*male  +  wave*edu_gmc +
                   nle_wp*phys_bp + nle_wp*phys_wp +
                   ( 1 + wave + nle_wp  |id)")
model_7a<- lmerTest::lmer(eq7a, data=ds0, REML= FALSE) 
lmerTest::summary((model_7a))


#graphs

g1<- ggplot2::ggplot(ds0, aes_string(x= "nle_bp", y="word_test")) +
  stat_smooth(method=lm, colour= "black", se=TRUE)+
  geom_point(size=1)

g1 <- g1 + labs(list(
  title= "Coupled Change between Physical Activity and Symbol Digit Modality",
  x="NLE (WP)", y="word_test"))


g1<- g1 + theme(text=element_text(family='Times'),
                # legend.title=element_blank())
                panel.background = theme_rect(fill = "white", colour="black"))


g1<- g1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank())

g1 <- g1 +theme(panel.background = element_rect(fill = "white", colour = "black",
                                                size = 1))
g1










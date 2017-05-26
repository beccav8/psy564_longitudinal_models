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

# library(sjPlot)
# library(sjmisc)

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
describe(ds0$coding_mean)
describe(ds0$word_test)
describe(ds0$mmse)


#91 mins of

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

# 1.96*sqrt(28.76) -i think this is wrong
# -0.12210 + (1.96*sqrt(28.76))
# -0.12210 - (1.96*sqrt(28.76)) 
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

#compared to UCM
(13.52 - 6.642 ) / 13.52

8050.1 - 7970.1 
1481- 1479 

#2df, dif of 80
anova(model, model1) #same thing  chisq=79.997 df=2 p < 0.00000000000000022 ***

#pseudo r^2 (percent of additional residual var accounted for)

# 
# 
# #----------table 1------- UCM vs time model (re)----------------
# 
# sjt.lmer(model_ucm, model1, depvar.labels= c("Model 0", "Model 1"),
#          p.numeric=FALSE, show.icc = FALSE, show.r2 = FALSE, show.ci=FALSE, show.se=TRUE,
#          show.re.var = TRUE, string.dv = c("MMSE"))
# 
# #--------------------------------------------------------



################# + demographic 


eq2 <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc + 
                  ( 1 + wave |id)")
model_2<- lmerTest::lmer(eq2, data=ds0, REML= FALSE) 
lmerTest::summary((model_2))


#deviance and df compared to model 1 , sig  better fit
anova(model1, model_2)
# chisq = 63.852 df=  6  p = 0.000000000007397 ***

# higher wave is associated with poorer scores
# higher age at baseline, and being a male are associated with poorer baseline scores
# higher edu_gmc at baseline is associated with higher scores

#only age at baseline is associated with a steeper decline in slope, edu_gmc and sex dont' influence rate of decline


names(ds0)

#Physical Activity --------------
# eq3a <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
#                   wave*phys_bp + phys_wp +
#                   ( 1 + wave |id)")
# model_3a<- lmerTest::lmer(eq3a, data=ds0, REML= FALSE) 
# lmerTest::summary((model_3a))
# 
# 
# # 4 versus 5
# #int
# (26.7764 - 26.7091) / 26.77
# #wave
# (0.3206 - 0.3162) / .32
# 
# 
# 
# eq3b <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
#                   + phys_bp*wave + phys_wp +
#                   ( 1 + wave + phys_wp |id)")
# model_3b<- lmerTest::lmer(eq3b, data=ds0, REML= FALSE) 
# lmerTest::summary((model_3b))
# 
# anova(model_3a, model_3b)

#        Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#   5    15 7931.0 8010.6 -3950.5   7901.0                         
#   6    18 7936.9 8032.3 -3950.4   7900.9 0.1857      3     0.9799

#no sig difference when we include WP effects of PA, therefore
# #no varience in WP PA fluctuations to explain by stress  
# 
# # Residual compared to model 1 
# (6.642 - 6.6096233) / 6.64
# 
# 
# #model 6
# 
# #int
# 26.50393433 
# 5.14819  / (sqrt(1485))            
# #wave 
# 0.30829714
# 0.55524 / (sqrt(1485))            
# #phys_wp        
# 0.00004607
# 0.006787/(sqrt(1485))        
# #Residual                
# 6.60962
# 2.5709/(sqrt(1485))    
# 



eq3a <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                  phys_bp*wave + PA_bl_BP*wave + phys_wp +
                   ( 1 + wave |id)")
model_3a<- lmerTest::lmer(eq3a, data=ds0, REML= FALSE) 
lmerTest::summary((model_3a))


eq3b <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                   + phys_bp*wave + PA_bl_BP*wave + phys_wp +
                   ( 1 + wave + phys_wp |id)")
model_3b<- lmerTest::lmer(eq3b, data=ds0, REML= FALSE) 
lmerTest::summary((model_3b))


anova(model_3a, model_3b)

# 
# #wp varience explained compred to the random effects of time only
# summary(model1)
# (6.642 -   6.60962373 )/(6.642)  #0.5%

# 
# #varience in the intercept explained by PA?
# (24.5726 - 24.1624) / 24.5726
# 
# #varience in the slope explained by PA? 
# (0.3101 - 0.2732) / 0.3101

# # gender X PA
# eq6 <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
#                   phys_bp*male + phys_wp*male +
#                   ( 1 + wave + phys_wp|id)")
# model_6<- lmerTest::lmer(eq6, data=ds0, REML= FALSE)
# lmerTest::summary((model_6))



#stress------------------------------

#-------------nle-

names(ds0)
eq4a <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                   nle_bp*wave + nle_wp +
                    ( 1 + wave  |id)")


eq4b <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
                   nle_bp*wave + nle_wp +
                   ( 1 + wave + nle_wp |id)")



model_4a<- lmerTest::lmer(eq4a, data=ds0, REML= FALSE) 
lmerTest::summary((model_4a))

anova(model_2, model_4a) #demographic versus inclusion of stress 
lmerTest::summary((model_2))

(26.7764 -27.0459) / 26.7764          
(0.3206 - 0.3229) / 0.3206


model_4b<- lmerTest::lmer(eq4b, data=ds0, REML= FALSE) 
lmerTest::summary((model_4b))

anova(model_4a, model_4b)


lmerTest::summary(model1)
(6.642  - 6.3020) / 6.642

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


# eq5b <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  + wave*edu_gmc +
#                    nle_bp*male*wave + nle_bp*male +
#                    ( 1 + wave + nle_wp |id)")
# model_5b<- lmerTest::lmer(eq5b, data=ds0, REML= FALSE)
# lmerTest::summary((model_5b))

# library(sjPlot)
# #-----------------------table 2-------------------------------------------------------------
# sjt.lmer(model_2, model_3b, model_4b, depvar.labels= c("Model 2", "Model 3b", "Model 4b"),
#          p.numeric=FALSE, show.icc = FALSE, show.r2 = FALSE, show.ci=FALSE, show.se=TRUE,
#          pred.labels = c("Time", "Age at baseline", "MaleT", "Education", "time x Age at baseline", "Time x MaleT",
#                          "Time x Education", "PA_BP", "PA_WP", "Time x PA_BP", "NLE_BP", "NLE_WP", "Time x NLE_BP"))
# 
# #-------------------------------------------------------------------------------------------
# 
# 

################# interaction with stress 


eq5 <- as.formula("coding_mean ~ 1 + wave*age_bl_gmc + wave*male  +  wave*edu_gmc + 

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


#-----------------------------------



#hypothetical graph


describe(ds0$phys_wp)
#-32.74 - 63.08, range = 95.82
95.82/3

#low=
-32.74 +31.94 #-31 to -0.8
#medium
-0.8 + 31.94  # -0.8 - 31.14
#high
31.14 + 32.94  # 31.14 - 64.08

test<- ds0
names(test)
# test$phys_wpCAT[test$phys_wp< -0.8] <- "low"
# test$phys_wpCAT[ -0.9<= test$phys_wp & test$phys_wp <= 31] <- "med"
# test$phys_wpCAT[32<= test$phys_wp] <-"high"

test$phys_wpCAT[test$phys_wp< -0.8] <- "low"
test$phys_wpCAT[ -0.9<= test$phys_wp & test$phys_wp <= 10.99] <- "med"
test$phys_wpCAT[11<= test$phys_wp] <-"high"

# describeBy(test$phys_wpCAT, group=test$phys_wpCAT)
table(test$phys_wpCAT)


library(ggplot2)

p1 <- ggplot(data = test, aes(x = nle_wp, y = coding_mean, group = phys_wpCAT, colour= phys_wpCAT)) +       
  geom_line() + geom_point() + stat_smooth(method=lm, se=FALSE)

p1


test2<- ds0

test2$phys_wpCAT[test2$phys_wp< -0.8] <- "low"
test2$phys_wpCAT[ -0.9<= test2$phys_wp & test2$phys_wp <= 15.99] <- "med"
test2$phys_wpCAT[16<= test2$phys_wp] <-"high"


table(test2$phys_wpCAT)

p1 <- ggplot(data = test2, aes(x = nle_wp, y = coding_mean, group = phys_wpCAT, colour= phys_wpCAT)) +       
  geom_line() + geom_point() + stat_smooth(method=lm, se=FALSE)

p1


#low PA: as NLE increases, scores decline
#med/high PA, as NLE increases, scores decline, but are higher


#----

describe(test$nle_wp)

test$nle_wpCAT[test$nle_wp< -1] <- "low"
test$nle_wpCAT[ -1.1<= test$nle_wp & test$nle_wp <= 2] <- "med"
test$nle_wpCAT[2<= test$nle_wp] <-"high"



p1 <- ggplot(data = test, aes(x = phys_wp, y = coding_mean, group = nle_wpCAT, colour= nle_wpCAT)) +       
  geom_line() + geom_point() + stat_smooth(method=lm, se=FALSE)

p1

str(test)



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


g2<- ggplot2::ggplot(ds0, aes_string(x= "nle_wp", y="coding_mean")) +
  stat_smooth(method=lm, colour= "black", se=TRUE)+
  geom_point(size=1)


g2 <- g2 + labs(list(
  title="Coupled Change between NLE and Coding (processing speed)",
  x="NLE (WP)", y="coding/processing speed"))

g2<- g2 + theme(text=element_text(family='Times'),
                legend.title=element_blank())

g2<- g2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))

g2 <- g2 +theme(panel.background = element_rect(fill = "white", colour = "black",
                                                size = 1))
g2


multiplot(g1, g2)



### tables 




sjt.lmer(model_ucm, model, model_4, model_6, model_6b, model_7)














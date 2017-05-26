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
#-----------------------------------------------------------------------------------------------------

# describeBy(ds0$age_at_visit, ds0$wave)
# describeBy(ds0$edu, ds0$coding_mean)
# describeBy(ds0$edu, ds0$word_test)
# describeBy(ds0$edu, ds0$mmse)
# describeBy(ds0$edu, ds0$phys)
# describeBy(ds0$edu, ds0$nle)


#-------------------------------------------------------------------------------------------------

describe(ds0$wave) 

#-------------------coding_mean
describe(ds0$coding_mean)

eq_1 <- as.formula("coding_mean ~ wave + 1 +
                   (1  |id)")
model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))

#sig declining over time :  -1.07569    0.03763 1268.10000  -28.58 <0.0000000000000002 ***

ggplot(ds0, aes(x=wave, y=coding_mean))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("Time")+
  ylab("Coding")+
  coord_cartesian(ylim=c(5, 44), xlim=c(1,7))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.title.y=element_text(size=16,family="serif"),
        axis.title.x=element_text(size=16,family="serif"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_line(size=1),
        axis.ticks.length=unit(.1,"cm"),
        axis.text=element_text(size=12,family="serif"),
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.position="bottom",
        legend.title=element_text(size=14, family="serif"),
        legend.text=element_text(size=12,family="serif"),
        strip.background=element_blank(),
        strip.text=element_text(size=14,family="serif"))
ggsave(file="LASA-coding-time.png")


#----------------------------------- word list 
describe(ds0$word_test)

eq_1 <- as.formula("word_test ~ wave + 1 +
                   (1  |id)")
model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))

#sig declining over time :  -0.32597    0.02777 1082.50000  -11.74 <0.0000000000000002 ***

ggplot(ds0, aes(x=wave, y=word_test))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("Time")+
  ylab("15WT")+
  coord_cartesian(ylim=c(0,15), xlim=c(1,7))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.title.y=element_text(size=16,family="serif"),
        axis.title.x=element_text(size=16,family="serif"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_line(size=1),
        axis.ticks.length=unit(.1,"cm"),
        axis.text=element_text(size=12,family="serif"),
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.position="bottom",
        legend.title=element_text(size=14, family="serif"),
        legend.text=element_text(size=12,family="serif"),
        strip.background=element_blank(),
        strip.text=element_text(size=14,family="serif"))
ggsave(file="LASA-15WT-time.png")


#----------------------------------- mmse

describe(ds0$mmse)

eq_1 <- as.formula("mmse ~ wave + 1 +
                   (1  |id)")
model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))

#sig declining over time : -0.20528    0.01324 3298.00000  -15.51 <0.0000000000000002 ***

ggplot(ds0, aes(x=wave, y=mmse))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("Time")+
  ylab("MMSE")+
  coord_cartesian(ylim=c(25,30), xlim=c(1,7))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.title.y=element_text(size=16,family="serif"),
        axis.title.x=element_text(size=16,family="serif"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_line(size=1),
        axis.ticks.length=unit(.1,"cm"),
        axis.text=element_text(size=12,family="serif"),
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.position="bottom",
        legend.title=element_text(size=14, family="serif"),
        legend.text=element_text(size=12,family="serif"),
        strip.background=element_blank(),
        strip.text=element_text(size=14,family="serif"))
ggsave(file="LASA-MMSE-time.png")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


#PA over time 

describe(ds0$phys)

eq_1 <- as.formula("phys ~ wave + 1 +
                   (1  |id)")
model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))

#declining: 0.206083     0.008395 24646.000000  -24.55 <0.0000000000000002 ***


ggplot(ds0, aes(x=wave, y=phys))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("Time")+
  ylab("Physical Activity")+
  coord_cartesian(ylim=c(0,5), xlim=c(1,7))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.title.y=element_text(size=16,family="serif"),
        axis.title.x=element_text(size=16,family="serif"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_line(size=1),
        axis.ticks.length=unit(.1,"cm"),
        axis.text=element_text(size=12,family="serif"),
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.position="bottom",
        legend.title=element_text(size=14, family="serif"),
        legend.text=element_text(size=12,family="serif"),
        strip.background=element_blank(),
        strip.text=element_text(size=14,family="serif"))
ggsave(file="LASA-PA-time.png")


#----------------------------------- phys_WP
describe(ds0$phys_wp)

eq_1 <- as.formula("phys_wp ~ wave + 1 +
                   (1  |id)")
model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))

#sig declining over time :  -0.208295     0.008051 28763.000000  -25.87 <0.0000000000000002 ***
#people are exercisign less than their average over time

ggplot(ds0, aes(x=wave, y=phys_wp))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("Time")+
  ylab("PA_WP")+
  coord_cartesian(ylim=c(-2.5,2.5), xlim=c(1,7))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.title.y=element_text(size=16,family="serif"),
        axis.title.x=element_text(size=16,family="serif"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_line(size=1),
        axis.ticks.length=unit(.1,"cm"),
        axis.text=element_text(size=12,family="serif"),
        axis.line.x = element_line(size = 0.5, colour = "black"),
        axis.line.y = element_line(size = 0.5, colour = "black"),
        legend.position="bottom",
        legend.title=element_text(size=14, family="serif"),
        legend.text=element_text(size=12,family="serif"),
        strip.background=element_blank(),
        strip.text=element_text(size=14,family="serif"))
ggsave(file="LASA-PA_WP-time.png")

#-------------------------------------------------------------------------------------------------




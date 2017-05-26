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
library(moments)



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

#-----------------------------------------------------------------------------------------------------

describe(ds0$year_in_study) 

#-------------------SDMT
describe(ds0$sdmt)

eq_1 <- as.formula("sdmt ~ year_in_study + 1 +
                   (1  |id)")
model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))

#sig declining over time : -1.00853    0.02153 9096.00000  -46.84 <0.0000000000000002 ***

ggplot(ds0, aes(x=year_in_study, y=sdmt))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("Time")+
  ylab("SDMT")+
  coord_cartesian(ylim=c(30,40), xlim=c(0,18))+
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
ggsave(file="MAP-SDMT-time.png")


#----------------------------------- word list 
describe(ds0$wl_im)

eq_1 <- as.formula("wl_im ~ year_in_study + 1 +
                   (1  |id)")
model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))

#sig declining over time : -0.16992    0.01007 9653.00000  -16.88 <0.0000000000000002 ***

ggplot(ds0, aes(x=year_in_study, y=wl_im))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("Time")+
  ylab("WLI")+
  coord_cartesian(ylim=c(0,30), xlim=c(0,18))+
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
ggsave(file="MAP-WLI-time.png")


#----------------------------------- mmse

describe(ds0$mmse)

eq_1 <- as.formula("mmse ~ year_in_study + 1 +
                   (1  |id)")
model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))

#sig declining over time : -0.45095     0.00997 10168.00000  -45.23 <0.0000000000000002 ***

ggplot(ds0, aes(x=year_in_study, y=mmse))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("Time")+
  ylab("MMSE")+
  coord_cartesian(ylim=c(20,30), xlim=c(0,18))+
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
ggsave(file="MAP-MMSE-time.png")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


#PA over time 

describe(ds0$physical_activity)

eq_1 <- as.formula("physical_activity ~ year_in_study + 1 +
                   (1  |id)")
model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))


ggplot(ds0, aes(x=year_in_study, y=physical_activity))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("Time")+
  ylab("Physical Activity")+
  coord_cartesian(ylim=c(0,5), xlim=c(0,18))+
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
ggsave(file="MAP-PA-time.png")


#----------------------------------- word list 
describe(ds0$phys_wp)

eq_1 <- as.formula("phys_wp ~ year_in_study + 1 +
                   (1  |id)")
model<- lmerTest::lmer(eq_1, data=ds0, REML= FALSE)
lmerTest::summary((model))

#sig declining over time : -0.081271     0.006326 11233.000000 -12.847 <0.0000000000000002 ***
#people are exercisign less than their average over time

ggplot(ds0, aes(x=year_in_study, y=phys_wp))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("Time")+
  ylab("PA_WP")+
  coord_cartesian(ylim=c(-2.5,2.5), xlim=c(0,18))+
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
ggsave(file="MAP-PA_WP-time.png")


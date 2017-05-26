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
# str(ds0)
length(unique(ds0$id))
1367 + 485



# graphs--------------------------------


#--------------------------------------------PABP----------------------------------------
#--------------PA BP graphs -------------------------------------------------------------

describe(ds0$phys_pmeanC) #BP
describe(ds0$phys_wp)     #WP


#-------------------SDMT
describe(ds0$sdmt)

ggplot(ds0, aes(x=phys_pmeanC, y=sdmt))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_BP")+
  ylab("SDMT")+
  coord_cartesian(ylim=c(0,77), xlim=c(-3, 23))+
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
ggsave(file="PA_BP_SDMT.png")

#------------- WLI
describe(ds0$wl_im)

ggplot(ds0, aes(x=phys_pmeanC, y=wl_im))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_BP")+
  ylab("WLI")+
  coord_cartesian(ylim=c(0,30), xlim=c(-3, 23))+
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
ggsave(file="PA_BP_WLI.png")


#------------------------ MMSE

describe(ds0$mmse)

ggplot(ds0, aes(x=phys_pmeanC, y=mmse))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_BP")+
  ylab("MMSE")+
  coord_cartesian(ylim=c(0,30), xlim=c(-3, 23))+
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
ggsave(file="PA_BP_MMSE.png")



#-------------------------------------------PA WP------------------------------------
#--------------PA WP----------------------------------------------------------------

describe(ds0$phys_wp)

#-------------------SDMT
describe(ds0$sdmt)

ggplot(ds0, aes(x=phys_wp, y=sdmt))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_WP")+
  ylab("SDMT")+
  coord_cartesian(ylim=c(0,77), xlim=c(-14, 28))+
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
ggsave(file="PA_WP_SDMT.png")

#------------- WLI
describe(ds0$wl_im)

ggplot(ds0, aes(x=phys_wp, y=wl_im))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_WP")+
  ylab("WLI")+
  coord_cartesian(ylim=c(0,30), xlim=c(-14, 28))+
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
ggsave(file="PA_WP_WLI.png")


#------------------------ MMSE

describe(ds0$mmse)

ggplot(ds0, aes(x=phys_wp, y=mmse))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_WP")+
  ylab("MMSE")+
  coord_cartesian(ylim=c(0,30), xlim=c(-14, 28))+
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
ggsave(file="PA_WP_MMSE.png")




#---------------------------------STRESS BP -----------------------------------
#------------- stress BP-------------------------------------------------------

describe(ds0$nle_pmeanC)
#-------------------SDMT
describe(ds0$sdmt)

ggplot(ds0, aes(x=nle_pmeanC, y=sdmt))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_BP")+
  ylab("SDMT")+
  coord_cartesian(ylim=c(0,77), xlim=c(-3, 8))+
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
ggsave(file="NLE_BP_SDMT.png")

#------------- WLI
describe(ds0$wl_im)

ggplot(ds0, aes(x=nle_pmeanC, y=wl_im))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_BP")+
  ylab("WLI")+
  coord_cartesian(ylim=c(0,30), xlim=c(-3, 8))+
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
ggsave(file="NLE_BP_WLI.png")


#------------------------ MMSE

describe(ds0$nle_pmeanC)

ggplot(ds0, aes(x=nle_pmeanC, y=mmse))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_BP")+
  ylab("MMSE")+
  coord_cartesian(ylim=c(0,30), xlim=c(-3, 8))+
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
ggsave(file="NLE_BP_MMSE.png")


#----------------STRES WP-----------------------------------------------------
#-----------------------------------------------------------------------------


describe(ds0$nle_wp)

#-------------------SDMT
describe(ds0$sdmt)

ggplot(ds0, aes(x=nle_wp, y=sdmt))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_WP")+
  ylab("SDMT")+
  coord_cartesian(ylim=c(0,77), xlim=c(-5, 13))+
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
ggsave(file="NLE_WP_SDMT.png")

#------------- WLI
describe(ds0$wl_im)

ggplot(ds0, aes(x=nle_wp, y=wl_im))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_WP")+
  ylab("WLI")+
  coord_cartesian(ylim=c(0,30), xlim=c(-5, 13))+
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
ggsave(file="NLE_WP_WLI.png")


#------------------------ MMSE

describe(ds0$mmse)

ggplot(ds0, aes(x=nle_wp, y=mmse))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_WP")+
  ylab("MMSE")+
  coord_cartesian(ylim=c(0,30), xlim=c(-5, 13))+
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
ggsave(file="NLE_WP_MMSE.png")














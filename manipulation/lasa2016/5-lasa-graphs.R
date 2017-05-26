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
str(ds0)

options(scipen=20)

str(ds0)

# graphs--------------------------------
table(ds0$male)

#--------------------------------------------PABP----------------------------------------
#--------------PA BP graphs -------------------------------------------------------------

describe(ds0$phys_bp) #BP
describe(ds0$phys_wp)     #WP


#-------------------coding_mean
describe(ds0$coding_mean)

ggplot(ds0, aes(x=phys_bp, y=coding_mean))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_BP")+
  ylab("Coding")+
  coord_cartesian(ylim=c(5,44), xlim=c(-1.20, 32))+
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
ggsave(file="LASA-PA_BP_coding.png")

#------------- 15WT
describe(ds0$word_test)

ggplot(ds0, aes(x=phys_bp, y=word_test))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_BP")+
  ylab("15WT")+
  coord_cartesian(ylim=c(0,15),xlim=c(-1.20, 32))+
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
ggsave(file="LASA-PA_BP_15WT.png")


#------------------------ MMSE

describe(ds0$mmse)

ggplot(ds0, aes(x=phys_bp, y=mmse))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_BP")+
  ylab("MMSE")+
  coord_cartesian(ylim=c(0,30), xlim=c(-1.20, 32))+
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
ggsave(file="LASA-PA_BP_MMSE.png")



#-------------------------------------------PA WP------------------------------------
#--------------PA WP----------------------------------------------------------------

describe(ds0$phys_wp)

#-------------------coding_mean
describe(ds0$coding_mean)

ggplot(ds0, aes(x=phys_wp, y=coding_mean))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_WP")+
  ylab("Coding")+
  coord_cartesian(ylim=c(5,44), xlim=c(-33, 64))+
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
ggsave(file="LASA-PA_WP_coding.png")

#------------- 15WT
describe(ds0$word_test)

ggplot(ds0, aes(x=phys_wp, y=word_test))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_WP")+
  ylab("15WT")+
  coord_cartesian(ylim=c(0,15), xlim=c(-33, 64))+
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
ggsave(file="LASA-PA_WP_15WT.png")


#------------------------ MMSE

describe(ds0$mmse)
describe(ds0$phys_wp)

ggplot(ds0, aes(x=phys_wp, y=mmse))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("PA_WP")+
  ylab("MMSE")+
  coord_cartesian(ylim=c(0,30), xlim=c(-33,64))+
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
ggsave(file="LASA-PA_WP_MMSE.png")




#---------------------------------STRESS BP -----------------------------------
#------------- stress BP-------------------------------------------------------

describe(ds0$nle_bp)
#-------------------coding_mean
describe(ds0$coding_mean)

ggplot(ds0, aes(x=nle_bp, y=coding_mean))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_BP")+
  ylab("Coding")+
  coord_cartesian(ylim=c(5,44), xlim=c(-0.20, 4))+
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
ggsave(file="LASA-NLE_BP_coding_mean.png")

#------------- 15WT
describe(ds0$word_test)

ggplot(ds0, aes(x=nle_bp, y=word_test))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_BP")+
  ylab("15WT")+
  coord_cartesian(ylim=c(0,15), xlim=c(-0.20, 4))+
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
ggsave(file="LASA-NLE_BP_15WT.png")


#------------------------ MMSE

describe(ds0$nle_bp)
describe(ds0$nle_wp)

ggplot(ds0, aes(x=nle_bp, y=mmse))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_BP")+
  ylab("MMSE")+
  coord_cartesian(ylim=c(0,30), xlim=c(-0.20, 4))+
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
ggsave(file="LASA-NLE_BP_MMSE.png")


#----------------STRES WP-----------------------------------------------------
#-----------------------------------------------------------------------------


describe(ds0$nle_wp)

#-------------------coding_mean
describe(ds0$coding_mean)

ggplot(ds0, aes(x=nle_wp, y=coding_mean))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_WP")+
  ylab("Coding")+
  coord_cartesian(ylim=c(5,44), xlim=c(-4,4))+
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
ggsave(file="LASA-NLE_WP_coding_mean.png")

#------------- 15WT
describe(ds0$word_test)

ggplot(ds0, aes(x=nle_wp, y=word_test))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_WP")+
  ylab("15WT")+
  coord_cartesian(ylim=c(0,15), xlim=c(-4,4))+
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
ggsave(file="LASA-NLE_WP_15WT.png")


#------------------------ MMSE

describe(ds0$mmse)

ggplot(ds0, aes(x=nle_wp, y=mmse))+
  geom_smooth(method="lm",size=0.5,colour="black")+
  xlab("NLE_WP")+
  ylab("MMSE")+
  coord_cartesian(ylim=c(0,30), xlim=c(-4,4))+
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
ggsave(file="LASA-NLE_WP_MMSE.png")














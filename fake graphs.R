# The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# Run the lines below to stitch a basic html output. 
# knitr::stitch_rmd(
#   script="./manipulation/map/1-encode-biomarkers.R",
#   output="./manipulation/map/stitched-output/1-encode-biomarkers.md"
# )
# The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!


# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>% 
library(ggplot2)
# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("ggplot2") # graphing
requireNamespace("tidyr") # data manipulation
requireNamespace("dplyr") # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("plyr")
requireNamespace("testit")# For asserting conditions meet expected patterns.
requireNamespace("car") # For it's `recode()` function.

source("./scripts/graph_themes.R")
# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- read.csv("./fake_data.csv")

# ---- inspect-data -------------------------------------------------------------
head(dto)

burst1<- dto[ which(dto$burst == 1),]

sub1<- burst1[ which(burst1$day == 1),]

sub2 <- burst1[ which(burst1$day == 2),]

sub3 <- burst1[ which(burst1$day == 3),]



Day1<- ggplot2::ggplot(sub1, aes_string(x="PSS_day", y ="WM")) + geom_point() +
  stat_smooth(method=lm, se=FALSE) +
  coord_cartesian(xlim=c(0,4),ylim=c(3,17))
 Day1<- Day1 +  labs(list(
    title= " Day 1, Burst 1",
    x="PSS", y="Cognition")) +
  theme1
  Day1

  Day2<- ggplot2::ggplot(sub2, aes_string(x="PSS_day", y ="WM")) + geom_point() +
    stat_smooth(method=lm, se=FALSE) +
    coord_cartesian(xlim=c(0,4),ylim=c(3,17))
  Day2<- Day2 +  labs(list(
    title= " Day 2, Burst 1",
    x="PSS", y="Cognition")) +
    theme1
  Day2
  
  Day3<- ggplot2::ggplot(sub3, aes_string(x="PSS_day", y ="WM")) + geom_point() +
    stat_smooth(method=lm, se=FALSE) +
    coord_cartesian(xlim=c(0,4),ylim=c(3,17))
  Day3<- Day3 +  labs(list(
    title= " Day 3, Burst 1",
    x="PSS", y="Cognition")) +
    theme1
  Day3
  
  
  source("./scripts/multiplot-function.R")
Burst_one<-multiplot(Day1, Day2, Day3, cols=3)
  
  
  burst2<- dto[ which(dto$burst == 2),]
  
  sub1<- burst2[ which(burst2$day == 1),]
  
  sub2 <- burst2[ which(burst2$day == 2),]
  
  sub3 <- burst2[ which(burst2$day == 3),]
  
  Day1<- ggplot2::ggplot(sub1, aes_string(x="PSS_day", y ="WM")) + geom_point() +
    stat_smooth(method=lm, se=FALSE) +
    coord_cartesian(xlim=c(0,4),ylim=c(3,17))
  Day1<- Day1 +  labs(list(
    title= " Day 1, Burst 2",
    x="PSS", y="Cognition")) +
    theme1
  Day1
  
  Day2<- ggplot2::ggplot(sub2, aes_string(x="PSS_day", y ="WM")) + geom_point() +
    stat_smooth(method=lm, se=FALSE) +
    coord_cartesian(ylim=c(3,17)) 
  Day2<- Day2 +  labs(list(
    title= " Day 2, Burst 2",
    x="PSS", y="Cognition")) +
    theme1
  Day2
  
  Day3<- ggplot2::ggplot(sub3, aes_string(x="PSS_day", y ="WM")) + geom_point() +
    stat_smooth(method=lm, se=FALSE) +
    coord_cartesian(xlim=c(0,4),ylim=c(3,17))
  Day3<- Day3 +  labs(list(
    title= " Day 3, Burst 2",
    x="PSS", y="Cognition")) +
    theme1
  Day3
  
Burst_two<-multiplot(Day1, Day2, Day3, cols=3)

Burst_one
Burst_two

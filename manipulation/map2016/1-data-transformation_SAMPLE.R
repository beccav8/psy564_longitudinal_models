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

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- readRDS("./data/unshared/derived/map2016/dto_raw.rds")
# ---- inspect-data -------------------------------------------------------------
# the list is composed of the following elements
names(dto)
map_sample<-dto[["unitData"]]
names(map_sample)

# test <- subset(map_sample, map_sample$id %in% sample(unique(map_sample$id), size=300))


set.seed(1)
ids <- sample(map_sample$id,300)
d <- map_sample %>%  dplyr::filter( id %in% ids)
dim(d)
head(d)
unique(d$id)

names(d)
# str(d)

myvars<- c("id","year_in_study", "dementia", "age_bl","age_at_visit","edu", "msex","race","apoe",
           "episodic","percep_speed","semantic","wm","global","dig_b","dig_f","mmse",
           "nle","pss","physical_activity")

           # "cts_sdmt","cts_wli","cts_wlii","cts_wliii","neglifeevents","perceivedstress","phys5itemsum")

d <- map_sample[myvars]

d$id<-as.numeric(d$id)
d$year_in_study<-as.numeric(d$year_in_study)
d$dementia<-as.numeric(d$dementia)
d$age_bl<-as.numeric(d$age_bl)
d$age_at_visit<-as.numeric(d$age_at_visit)
d$edu<-as.numeric(d$edu)
d$msex<-as.numeric(d$msex)
d$race<-as.numeric(d$race)
d$apoe<-as.numeric(d$apoe)
d$episodic<-as.numeric(d$episodic)
d$percep_speed<-as.numeric(d$percep_speed)
d$semantic<-as.numeric(d$semantic)
d$wm<-as.numeric(d$wm)
d$global<-as.numeric(d$global)
d$dig_b<-as.numeric(d$dig_b)
d$dig_f<-as.numeric(d$dig_f)
d$mmse<-as.numeric(d$mmse)
d$nle<-as.numeric(d$mmse)
d$pss<-as.numeric(d$pss)
d$physical_activity<-as.numeric(d$physical_activity)

#continue to make numeric if it works in hlm


write.table(d, file="./data/unshared/derived/map2016/hlm_map_sample.dat", row.names=FALSE, sep="\t", quote=FALSE)




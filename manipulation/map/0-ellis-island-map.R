# The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# Run the lines below to stitch a basic html output. 
# knitr::stitch_rmd(
#   script="./manipulation/map/0-ellis-island-map.R",
#   output="./manipulation/map/stitched-output/0-ellis-island-map.md"
# )
# The above lines are executed only when the file is run in RStudio, !! NOT when an Rmd/Rnw file calls it !!

############################
##  Land on Ellis Island  ##
############################

rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  
# Ideally, no real operations are performed in these sourced scripts. 
source("./scripts/common-functions.R") # used in multiple reports

# ---- load-packages ----------------------------------------------
# Attach packages so their functions don't need to be qualified when used
# See more : http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # Pipes
library(ggplot2) # Graphs
# Functions of these packages will need to be qualified when used
# See more: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("tidyr") #  data manipulation
requireNamespace("dplyr") # f() names conflict with other packages (esp. base, stats, and plyr).
requireNamespace("testit") # for asserting conditions meet expected patterns.
# getwd()
# setwd("C:/Users/Rebecca/Documents/GitHub/cognition-stress-activity")

# ---- declare-globals ----------------------------------------------
# reach out to the curator for a dataset prepared for general consumption
data_path_input  <- "../MAP/data-unshared/derived/ds0_raw.rds"
# point to the local metadata to be used for this project (specific consumption)
metadata_path_input <- "./data/shared/meta/meta-data-map.csv" 

# ---- load-data ------------------------------------------------
# load data objects
unitData <- readRDS(data_path_input) 
metaData <- read.csv(metadata_path_input, stringsAsFactors=F, header=T)
names(unitData)
# ---- inspect-data ----------------------------------------------
# inspect loaded data objects (using basic demographic variables )
ds <- unitData # assing alias
length(unique(ds$id)) # there are this many of subjects
t <- table(ds[,"fu_year"], ds[,"died"]); t[t==0]<-".";t
t <- table(ds[,"msex"], ds[,"race"], useNA = "always"); t[t==0]<-".";t
t <- table(ds[,"educ"], ds[,"race"]); t[t==0]<-".";t

# ---- assemble-data-object-dto-1 --------------------------------------
dto <- list()
# the first element of data transfer object contains unit data
dto[["unitData"]] <- unitData
# the second element of data transfer object contains meta data
dto[["metaData"]] <-  metaData
# verify and glimpse
dto[["unitData"]] %>% dplyr::glimpse()
dto[["metaData"]] %>% dplyr::glimpse()


# ----- view-metadata-1 ---------------------------------------------
meta_data <- dto[["metaData"]] %>%
  dplyr::filter(include == TRUE) %>%
  # dplyr::select(name, name_new, type, label, construct) %>%
  dplyr::arrange(type, construct, name)
knitr::kable(meta_data)

# ----- apply-meta-data-1 -------------------------------------
# rename variables
d_rules <- dto[["metaData"]] %>%
  dplyr::filter(name %in% names(ds)) %>% 
  dplyr::select(name, name_new ) # leave only collumn, which values you wish to append
names(ds) <- d_rules[,"name_new"]

# keep only relevant variables
variables_to_keep <- dto$metaData %>% 
  dplyr::filter(include==TRUE)
variables_to_keep <- variables_to_keep$name_new
ds <- ds %>% 
  dplyr::select_(.dots = variables_to_keep)

# transfer changes to dto
dto[["unitData"]] <- ds
str(ds)

# ---- correct-value-assignmnet --------------------------------------
#the origional variable msex used the following values 
#1=male, 0=female 

table(ds$female, useNA="always") #7418 males 
ds$female <-!(ds$female==1)  #sex=TRUE if female
table(ds$female, useNA="always") #7481 FALSE (i.e. males) 


str(ds$female) 

# ---- save-to-disk ------------------------------------------------------------

# Save as a compressed, binary R dataset.  
# It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data/unshared/derived/map/dto.rds", compress="xz")
saveRDS(ds, file="./data/unshared/derived/map/dto-temp.rds")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/map/dto.rds")
names(dto)
# 1st element - unit(person) level data
names(dto[["unitData"]])
# 2nd element - meta data, info about variables
names(dto[["metaData"]])


table(ds$full_year, useNA="always") 

length(unique(ds$id))

#perceived stress


set.seed(1)
ids <- sample(ds$id,100)
d <- ds %>%  dplyr::filter( id %in% ids)

g<- ggplot2::ggplot(d, aes_string(x= "full_year", y="perceivedstress", group="id")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g <- g + labs(list(
  title= "Raw PSS scores Over Time",
  x="time", y="PSS"))
g

g1<- ggplot2::ggplot(ds, aes_string(x= "full_year", y="perceivedstress")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g1 <- g1 + labs(list(
  title= "Raw PSS scores Over Time",
  x="time", y="PSS"))
g1

g2<- ggplot2::ggplot(d, aes_string(x= "full_year", y="neglifeevents", group="id")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g2 <- g2 + labs(list(
  title= "neglifeevents scores Over Time",
  x="time", y="NLE"))
g2

g3<- ggplot2::ggplot(ds, aes_string(x= "full_year", y="neglifeevents")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g3 <- g3 + labs(list(
  title= "neglifeevents scores Over Time",
  x="time", y="NLE"))
g3

source("./scripts/multiplot-function.R")
multiplot(g, g1, g2, g3, cols=2)

set.seed(2)
ids <- sample(ds$id,40)
d <- ds %>%  dplyr::filter( id %in% ids)

g4<- ggplot2::ggplot(d, aes_string(x= "full_year", y="physical_activity", group="id")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g4 <- g4 + labs(list(
  # title= "neglifeevents scores Over Time",
  x="time", y="PA"))
g4

g5<- ggplot2::ggplot(ds, aes_string(x= "full_year", y="physical_activity")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g5 <- g5 + labs(list(
  # title= "neglifeevents scores Over Time",
  x="time", y="PA"))
g5

g6<- ggplot2::ggplot(d, aes_string(x= "full_year", y="mmse", group="id")) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g6 <- g6 + labs(list(
  # title= "neglifeevents scores Over Time",
  x="time", y="MMSE"))
g6

g7<- ggplot2::ggplot(ds, aes_string(x= "full_year", y="mmse")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g7 <- g7 + labs(list(
  # title= "neglifeevents scores Over Time",
  x="time", y="MMSE"))
g7

multiplot(g4, g5, g6, g7, cols=2)

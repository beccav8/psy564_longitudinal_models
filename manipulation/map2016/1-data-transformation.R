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
map_data<-dto[["unitData"]]
names(map_data)

head(map_data)
str(map_data)

write.table(map_data, file="./data/unshared/derived/map2016/hlm_data_prep.dat", row.names=FALSE, sep="\t", quote=FALSE)


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


# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata

path_input0  <- "./data/unshared/derived/map2016/map_full_bio_centered.rds" 

# @knitr load-data ---------------------------------------------------------------
map_sample  <- readRDS(path_input0) #total raw data  

names(map_sample)


set.seed(1)
ids <- sample(map_sample$id,300)
d <- map_sample %>%  dplyr::filter( id %in% ids)
dim(d)
head(d)
length(unique(d$id))

#270 in the sample


saveRDS(d, "./data/unshared/derived/map2016/map_sample_bio_centered.rds")
write.table(d, file="./data/unshared/derived/map2016/map_sample_bio_centered.dat", row.names=FALSE, sep="\t", quote=FALSE)


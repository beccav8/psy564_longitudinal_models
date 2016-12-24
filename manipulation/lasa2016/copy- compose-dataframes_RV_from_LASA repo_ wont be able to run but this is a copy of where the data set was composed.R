# The purpose of this script is to create a data object (dto) which will hold all data and metadata.
# Run the lines below to stitch a basic html output. 
# knitr::stitch_rmd(
#   script="./manipulation/lasa/0-ellis-island-lasa.R",
#   output="./manipulation/lasa/stitched-output/0-ellis-island-lasa.md"
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
source("./scripts/lasa-functions.R")
source("./scripts/graphs/specific-graphs.R")
source("./scripts/graphs/general-graphs.R")

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

# ---- declare-globals ----------------------------------------------
# reach out to the curator for a dataset prepared for general consumption
data_path_input  <- "./data-unshared/derived/datas.rds"
# point to the local metadata to be used for this project (specific consumption)
# metadata_path_input <- "./data/shared/meta/lasa/meta-data-lasa.csv" 

# ---- load-data ------------------------------------------------
# load data objects
datas <- readRDS(data_path_input) 
# metaData <- read.csv(metadata_path_input, stringsAsFactors=F, header=T)


# ---- examine-structure-of-input-object ---------------------
# Level 1
lapply(datas, names) # view domains, level 1 in the list
# Level 2
lapply(datas$cognitive, names) # view filenumber, level 2, many filenumbers in a domain
# Level 3
names(datas$cognitive[["035"]])

names_labels_datas("cognitive","035", "G") %>% dplyr::slice(1:10)
names_labels(datas$cognitive$`035`$G)%>% dplyr::slice(1:10)


# ---- define-renaming-function ------------------
d <- datas[["cognitive"]][["021"]][["G"]]
names(d)
correct_id_name <- function(d){
  (test1 <- any(names(d) %in% c("RespNr")))
  (test2 <- any(names(d) %in% c("Respnr")))
  
  if(test1) { 
    d <- d %>% 
      dplyr::rename(respnr = RespNr)
  } 
  if(test2){
    d <- d %>% 
      dplyr::rename(respnr = Respnr)
  }
  return(d)
}
# d <- correct_id_name(d)

# ---- correct-id-spelling ---------------------
lapply(datas$cognitive, names)

for(domain_ in names(datas)){
  # domain_ = "cognitive"
  for(number_ in names(datas[[domain_]])){
    # number_ = "021"
    for(wave_ in names(datas[[domain_]][[number_]])){
      # wave_ = "H"
      d <- datas[[domain_]][[number_]][[wave_]] 
      # names.labels2(d) %>% dplyr::slice(1:10)
      d <- correct_id_name(d)
      # names.labels2(d) %>% dplyr::slice(1:10)
      datas[[domain_]][[number_]][[wave_]] <- d
      # names.labels(domain_, number_, wave_) %>% dplyr::slice(1:10)
    }
  } 
}



names_labels_datas("cognitive","035", "G") %>% dplyr::slice(1:10)
names_labels(datas$cognitive$`035`$G)%>% dplyr::slice(1:10)

saveRDS(datas,"./data-unshared/derived/datas.rds")




# ---- combine-items --------------------------

#### DEMOGRAPHICS and CONTEXT variables 
# age
age <- merge_lasa_files(datas[["demographic"]][["008"]])
head(age)

# gender, birth year, birth cohort, education
edu_sex_co <- datas[["demographic"]][["004"]][["Z"]] %>%  
  dplyr::mutate(
    id            = respnr,
    male          = as.logical(ifelse(!is.na(sex), sex=="male", NA_integer_)),
    edu           = as.numeric(aedu)
  ) %>%
  dplyr::select(id, male, byear, bycohort, edu)
head(edu_sex_co)
names_labels(edu_sex_co)

# non-response
non_response<- merge_lasa_files(datas[["demographic"]][["002"]])
head(non_response)
names_labels(non_response)

# mortality
mortality<- merge_lasa_files(datas[["demographic"]][["990"]])
head(mortality)
names_labels(mortality)

# age_at_visit
age_at_visit<- merge_lasa_files(datas[["demographic"]][["008"]])
head(age_at_visit)
names_labels(age_at_visit)

# # gender
# gender_edu_eth<- merge_lasa_files(datas[["demographic"]][["004"]])
# head(gender_edu_eth)

##### PHYSICAL 
names(datas$physical)
lapply(datas$physical, names)
lapply(datas$cognitive, names)

#physical-activity
physical_activity<- merge_lasa_files(datas[["physical"]][["046"]])
head(physical_activity)
names_labels(physical_activity)

#-cognitive---
#15-wordtest
word_test <- merge_lasa_files(datas[["cognitive"]][["356"]])
names_labels(word_test)
#coding
coding<- merge_lasa_files(datas[["cognitive"]][["155"]])

#mmse
mmse_items <- merge_lasa_files(datas[["cognitive"]][["021"]])
mmse_score <- merge_lasa_files(datas[["cognitive"]][["221"]])

#fluid_intel
fluid_intelligence <- merge_lasa_files(datas[["cognitive"]][["222"]])

#digit_span
digit_span <- merge_lasa_files(datas[["cognitive"]][["166"]])

#-stress---
# perceived stress scale
pss<- merge_lasa_files(datas[["emotional"]][["304"]])
head(pss)
pss <- pss %>% 
  dplyr::rename(id = respnr) %>% 
  dplyr::mutate(wave = 7)

# negative life events
nle<- merge_lasa_files(datas[["emotional"]][["272"]])

# self-efficacy
self_efficacy<- merge_lasa_files(datas[["emotional"]][["228"]])

#mastery
mastery<- merge_lasa_files(datas[["emotional"]][["227"]])


#-al----
bp<-merge_lasa_files(datas[["physical"]][["151"]])

cort<-merge_lasa_files(datas[["biomaterial"]][["864"]])
inflm<-merge_lasa_files(datas[["biomaterial"]][["861"]])

chol<-merge_lasa_files(datas[["biomaterial"]][["867"]])


# ---- extract-raw-meta-data ---------------------
extract_meta_data <- function(d, name){
  meta <- names_labels(d)
  filePath <- paste0("./data-phi-free/meta/live/",name,"-live.csv")
  write.csv(meta, filePath)
}

extract_meta_data(age, "age")
# no need to export edu_sex_co metadata: it contains no longitudinal variables
extract_meta_data(non_response, "non_response")
###* has not been exported, it is longitudinal, just not sure what this variable is yet and can't find in master index
extract_meta_data(mortality, "mortality")
#no need to export " "

#Physical activity
extract_meta_data(physical_activity, "physical_activity")

#word-test
extract_meta_data(word_test, "word_test")
#coding
extract_meta_data(coding, "coding")
#mmse
extract_meta_data(mmse_items, "mmse_items")
extract_meta_data(mmse_score, "mmse_score")
#fluid_intel
extract_meta_data(fluid_intelligence, "fluid_intelligence")
#digit span
extract_meta_data(digit_span, "digit_span")

#stress
extract_meta_data(pss, "pss")
extract_meta_data(nle, "nle")

#control
extract_meta_data(self_efficacy, "self-efficacy")
extract_meta_data(mastery, "mastery")

#al
extract_meta_data(bp, "bp")
extract_meta_data(cort, "cort")
extract_meta_data(inflm, "inflm")
extract_meta_data(chol, "chol")

# ----- augment-meta-data -----------------------

# this step occurs outside of RStudio
# for each save file
# save a dead copy of the file and then add columns containing meta-data



# ----- elongate-physical-activity -------------------------------
metaData <- read.csv("./data-phi-free/meta/meta-physical_activity-dead.csv", stringsAsFactors = F)
# create clean wide data
(select_variables <- metaData$name[!is.na(metaData$include)])
# turn below once H wave for this variable is found
(omit_variables <- c(
  "HLPHYA09", "HLPHYA13","HLPHYA19", "HLPHYA24", "HLPHYA28", "HLPHYA34","HLPHYA38")
)
(select_variables <- setdiff(select_variables, omit_variables))
# turn above once H wave for this variable is found
ds_wide <- physical_activity %>%
  dplyr::select_(.dots = select_variables)
# rename variables from metadata
d_rules <- metaData %>% 
  dplyr::filter(name %in% select_variables) %>% 
  dplyr::select(name, name_new)# leave only collumn, which values you wish to append
names(ds_wide) <- d_rules[,"name_new"]
# list time-invariant(static) and time-variant(longitudinal) variables
variables_static <- as.data.frame(
  metaData %>% 
    dplyr::filter(longitudinal == FALSE) %>% 
    dplyr::select(name_new)
)$name_new
variables_longitudinal <- setdiff(colnames(ds_wide),variables_static)  # not static
# define regex rule for splitting up the names of variables
regex <- "^(\\w+)_(\\w{1})$"
# elongate with respect to wave of measurement
head(ds_wide)
ds_long <- ds_wide %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate( 
    wave     = gsub(regex,"\\2", variable),
    variable = gsub(regex, "\\1",variable) 
  )  %>% 
  tidyr::spread(variable, value) %>% 
  dplyr::mutate(
    wave = as.integer(
      plyr::mapvalues(
        x    = wave,
        from = c("b","c","d","e","f", "g") ,
        to   = c( 1,  2,  3,  4,  5,   6)
      )
    )
  )
ds_long_physical_activity <- ds_long
head(ds_long_physical_activity)



# ----- elongate-word-test -------------------------------
metaData <- read.csv("./data-phi-free/meta/meta-word_test-dead.csv", stringsAsFactors = F)
(select_variables <- metaData$name[!is.na(metaData$include)])
ds_wide <- word_test %>%
  dplyr::select_(.dots = select_variables)
# rename variables from metadata
d_rules <- metaData %>% 
  dplyr::filter(name %in% select_variables) %>% 
  dplyr::select(name, name_new)# leave only collumn, which values you wish to append
names(ds_wide) <- d_rules[,"name_new"]
# list time-invariant(static) and time-variant(longitudinal) variables
variables_static <- as.data.frame(
  metaData %>% 
    dplyr::filter(longitudinal == FALSE) %>% 
    dplyr::select(name_new)
)$name_new
variables_longitudinal <- setdiff(colnames(ds_wide),variables_static)  # not static
# define regex rule for splitting up the names of variables
regex <- "(\\w+)_(\\w{1})"
# elongate with respect to wave of measurement
head(ds_wide)
ds_long <- ds_wide %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate( 
    wave     = gsub(regex,"\\2", variable),
    variable = gsub(regex, "\\1",variable) 
  )  %>% 
  tidyr::spread(variable, value) %>% 
  dplyr::mutate(
    wave = as.integer(
      plyr::mapvalues(
        x    = wave,
        from = c("b","c","d","e","f", "g","h") ,
        to   = c( 1,  2,  3,  4,  5,   6,  7 )
      )
    )
  )
ds_long_word_test <- ds_long
head(ds_long)


# ----- elongate-coding -------------------------------

metaData <- read.csv("./data-phi-free/meta/meta-coding-dead.csv", stringsAsFactors = F)
(select_variables <- metaData$name[!is.na(metaData$include)])
ds_wide <- coding %>%
  dplyr::select_(.dots = select_variables)
# rename variables from metadata
d_rules <- metaData %>% 
  dplyr::filter(name %in% select_variables) %>% 
  dplyr::select(name, name_new)# leave only collumn, which values you wish to append
names(ds_wide) <- d_rules[,"name_new"]
# list time-invariant(static) and time-variant(longitudinal) variables
variables_static <- as.data.frame(
  metaData %>% 
    dplyr::filter(longitudinal == FALSE) %>% 
    dplyr::select(name_new)
)$name_new
variables_longitudinal <- setdiff(colnames(ds_wide),variables_static)  # not static
# define regex rule for splitting up the names of variables
regex <- "(\\w+)_(\\w{1})"
# elongate with respect to wave of measurement
head(ds_wide)
ds_long <- ds_wide %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate( 
    wave     = gsub(regex,"\\2", variable),
    variable = gsub(regex, "\\1",variable) 
  )  %>% 
  tidyr::spread(variable, value) %>% 
  dplyr::mutate(
    wave = as.integer(
      plyr::mapvalues(
        x    = wave,
        from = c("b","c","d","e","f", "g","h") ,
        to   = c( 1,  2,  3,  4,  5,   6,  7 )
      )
    )
  )
ds_long_coding<- ds_long
head(ds_long_coding)



# ----- elongate-mmse_score -------------------------------

metaData <- read.csv("./data-phi-free/meta/meta-mmse_score-dead.csv", stringsAsFactors = F)
(select_variables <- metaData$name[!is.na(metaData$include)])
ds_wide <- mmse_score %>%
  dplyr::select_(.dots = select_variables)
# rename variables from metadata
d_rules <- metaData %>% 
  dplyr::filter(name %in% select_variables) %>% 
  dplyr::select(name, name_new)# leave only collumn, which values you wish to append
names(ds_wide) <- d_rules[,"name_new"]
# list time-invariant(static) and time-variant(longitudinal) variables
variables_static <- as.data.frame(
  metaData %>% 
    dplyr::filter(longitudinal == FALSE) %>% 
    dplyr::select(name_new)
)$name_new
variables_longitudinal <- setdiff(colnames(ds_wide),variables_static)  # not static
# define regex rule for splitting up the names of variables
regex <- "(\\w+)_(\\w{1})"
# elongate with respect to wave of measurement
head(ds_wide)
ds_long <- ds_wide %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate( 
    wave     = gsub(regex,"\\2", variable),
    variable = gsub(regex, "\\1",variable) 
  )  %>% 
  tidyr::spread(variable, value) %>% 
  dplyr::mutate(
    wave = as.integer(
      plyr::mapvalues(
        x    = wave,
        from = c("b","c","d","e","f", "g","h") ,
        to   = c( 1,  2,  3,  4,  5,   6,  7 )
      )
    )
  )
ds_long_mmse<- ds_long
head(ds_long_mmse)



# ----- elongate-fluid-intelligence -------------------------------
metaData <- read.csv("./data-phi-free/meta/meta-fluid_intelligence-dead.csv", stringsAsFactors = F)
(select_variables <- metaData$name[!is.na(metaData$include)])
ds_wide <- fluid_intelligence %>%
  dplyr::select_(.dots = select_variables)
# rename variables from metadata
d_rules <- metaData %>% 
  dplyr::filter(name %in% select_variables) %>% 
  dplyr::select(name, name_new)# leave only collumn, which values you wish to append
names(ds_wide) <- d_rules[,"name_new"]
# list time-invariant(static) and time-variant(longitudinal) variables
variables_static <- as.data.frame(
  metaData %>% 
    dplyr::filter(longitudinal == FALSE) %>% 
    dplyr::select(name_new)
)$name_new
variables_longitudinal <- setdiff(colnames(ds_wide),variables_static)  # not static
# define regex rule for splitting up the names of variables
regex <- "(\\w+)_(\\w{1})"
# elongate with respect to wave of measurement
head(ds_wide)
ds_long <- ds_wide %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate( 
    wave     = gsub(regex,"\\2", variable),
    variable = gsub(regex, "\\1",variable) 
  )  %>% 
  tidyr::spread(variable, value) %>% 
  dplyr::mutate(
    wave = as.integer(
      plyr::mapvalues(
        x    = wave,
        from = c("b","c","d","e","f", "g") ,
        to   = c( 1,  2,  3,  4,  5,   6 )
      )
    )
  )
ds_long_fluid<- ds_long
head(ds_long_fluid)





# ----- elongate-age -------------------------------
#double check age worked ? 

metaData <- read.csv("./data-phi-free/meta/meta-age-dead.csv", stringsAsFactors = F)
(select_variables <- metaData$name[!is.na(metaData$include)])
ds_wide <- age_at_visit %>%
  dplyr::select_(.dots = select_variables)
# rename variables from metadata
d_rules <- metaData %>% 
  dplyr::filter(name %in% select_variables) %>% 
  dplyr::select(name, name_new)# leave only collumn, which values you wish to append
names(ds_wide) <- d_rules[,"name_new"]
# list time-invariant(static) and time-variant(longitudinal) variables
variables_static <- as.data.frame(
  metaData %>% 
    dplyr::filter(longitudinal == FALSE) %>% 
    dplyr::select(name_new)
)$name_new
variables_longitudinal <- setdiff(colnames(ds_wide),variables_static)  # not static
# define regex rule for splitting up the names of variables
regex <- "(\\w+)_(\\w{1})"
# elongate with respect to wave of measurement
head(ds_wide)
ds_long <- ds_wide %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate( 
    wave     = gsub(regex,"\\2", variable),
    variable = gsub(regex, "\\1",variable) 
  )  %>% 
  tidyr::spread(variable, value) %>% 
  dplyr::mutate(
    wave = as.integer(
      plyr::mapvalues(
        x    = wave,
        from = c("b","c","d","e","f", "g", "h") ,
        to   = c( 1,  2,  3,  4,  5,   6, 7 )
      )
    )
  )
ds_long_age<- ds_long
head(ds_long_age)



# ----- elongate-nle -------------------------------
metaData <- read.csv("./data-phi-free/meta/meta-nle-dead.csv", stringsAsFactors = F)
(select_variables <- metaData$name[!is.na(metaData$include)])
ds_wide <- nle %>%
  dplyr::select_(.dots = select_variables)
# rename variables from metadata
d_rules <- metaData %>% 
  dplyr::filter(name %in% select_variables) %>% 
  dplyr::select(name, name_new)# leave only collumn, which values you wish to append
names(ds_wide) <- d_rules[,"name_new"]
# list time-invariant(static) and time-variant(longitudinal) variables
variables_static <- as.data.frame(
  metaData %>% 
    dplyr::filter(longitudinal == FALSE) %>% 
    dplyr::select(name_new)
)$name_new
variables_longitudinal <- setdiff(colnames(ds_wide),variables_static)  # not static
# define regex rule for splitting up the names of variables
regex <- "(\\w+)_(\\w{1})"
# elongate with respect to wave of measurement
head(ds_wide)
ds_long <- ds_wide %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate( 
    wave     = gsub(regex,"\\2", variable),
    variable = gsub(regex, "\\1",variable) 
  )  %>% 
  tidyr::spread(variable, value) %>% 
  dplyr::mutate(
    wave = as.integer(
      plyr::mapvalues(
        x    = wave,
        from = c("c","d","e","f", "g", "h") ,
        to   = c( 2,  3,  4,  5,   6, 7 )
      )
    )
  )
ds_long_nle<- ds_long
head(ds_long_nle)

# ----- elongate-mastery -------------------------------
metaData <- read.csv("./data-phi-free/meta/meta-mastery-dead.csv", stringsAsFactors = F)
(select_variables <- metaData$name[!is.na(metaData$include)])
ds_wide <- mastery %>%
  dplyr::select_(.dots = select_variables)
# rename variables from metadata
d_rules <- metaData %>% 
  dplyr::filter(name %in% select_variables) %>% 
  dplyr::select(name, name_new)# leave only collumn, which values you wish to append
names(ds_wide) <- d_rules[,"name_new"]
# list time-invariant(static) and time-variant(longitudinal) variables
variables_static <- as.data.frame(
  metaData %>% 
    dplyr::filter(longitudinal == FALSE) %>% 
    dplyr::select(name_new)
)$name_new
variables_longitudinal <- setdiff(colnames(ds_wide),variables_static)  # not static
# define regex rule for splitting up the names of variables
regex <- "(\\w+)_(\\w{1})"
# elongate with respect to wave of measurement
head(ds_wide)
ds_long <- ds_wide %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate( 
    wave     = gsub(regex,"\\2", variable),
    variable = gsub(regex, "\\1",variable) 
  )  %>% 
  tidyr::spread(variable, value) %>% 
  dplyr::mutate(
    wave = as.integer(
      plyr::mapvalues(
        x    = wave,
        from = c("b","c","d","e") ,
        to   = c( 1,  2,  3,  4 )
      )
    )
  )
ds_long_mastery<- ds_long
head(ds_long_mastery)


# ----- elongate-self-efficacy -------------------------------
metaData <- read.csv("./data-phi-free/meta/meta-se-dead.csv", stringsAsFactors = F)
(select_variables <- metaData$name[!is.na(metaData$include)])
ds_wide <- self_efficacy %>%
  dplyr::select_(.dots = select_variables)
# rename variables from metadata
d_rules <- metaData %>% 
  dplyr::filter(name %in% select_variables) %>% 
  dplyr::select(name, name_new)# leave only collumn, which values you wish to append
names(ds_wide) <- d_rules[,"name_new"]
# list time-invariant(static) and time-variant(longitudinal) variables
variables_static <- as.data.frame(
  metaData %>% 
    dplyr::filter(longitudinal == FALSE) %>% 
    dplyr::select(name_new)
)$name_new
variables_longitudinal <- setdiff(colnames(ds_wide),variables_static)  # not static
# define regex rule for splitting up the names of variables
regex <- "(\\w+)_(\\w{1})"
# elongate with respect to wave of measurement
head(ds_wide)
ds_long <- ds_wide %>%
  tidyr::gather_(key="variable", value="value", variables_longitudinal) %>%
  dplyr::mutate( 
    wave     = gsub(regex,"\\2", variable),
    variable = gsub(regex, "\\1",variable) 
  )  %>% 
  tidyr::spread(variable, value) %>% 
  dplyr::mutate(
    wave = as.integer(
      plyr::mapvalues(
        x    = wave,
        from = c("b","c","d","e") ,
        to   = c( 1,  2,  3,  4 )
      )
    )
  )
ds_long_self_efficacy<- ds_long
head(ds_long_self_efficacy)



# ---- combine-data ---------------------
# store data frame in a list object for easier handling
dto_long <- list(
  "age"               = ds_long_age,
  "coding"            = ds_long_coding,
  "fluid"             = ds_long_fluid,
  "mastery"           = ds_long_mastery,
  "mmse"              = ds_long_mmse,
  "nle"               = ds_long_nle,
  "physical_activity" = ds_long_physical_activity,
  "self_efficacy"     = ds_long_self_efficacy,
  "word_test"         = ds_long_word_test,
  "pss"               = pss
)
# function to count unique ids
get_n <- function(d){length(unique(d[,"id"]))}
ds_long_age %>% get_n()
lapply(dto_long,get_n)


# left_join (merge) all longitudinal data.frames
dto_long_2 <- dto_long %>% 
  Reduce(function(dtf1,dtf2) dplyr::left_join(dtf1,dtf2,by=c("id","wave")), .)
dto_long_2 %>% get_n()
# combine longitudinal with the time invariant data
ds_combined <- edu_sex_co %>% 
  dplyr::left_join(dto_long_2 ,by="id")
# check for total unique persons    
ds_combined %>% get_n()



# ---- save-to-disk ------------------------------------------------------------

# Save as a compressed, binary R dataset.  
# It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(ds_combined, file="./data-unshared/derived/dto.rds", compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data-unshared/derived/dto.rds")
names(dto)
# this is a flat data.frame containing combined variable




###### Developmental code beyond thid point - please clean up ------



ids <- sample(unique(ds$id),1)
ds %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,birhtdate, age_at_visit, coding1, raven_a, mmse, word_test, mastery5, nle_9, walking_min)



raw_smooth_lines(ds_combined,"mmse", top_age = 100, top_time = 8)










#####################################################
# below is code inherited from 0-ellis-island-map.R
#####################################################

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
# transfer changes to dto
ds <- ds %>% dplyr::filter(study == "MAP ")
table(ds$study)
dto[["unitData"]] <- ds 

# ---- save-to-disk ------------------------------------------------------------

# Save as a compressed, binary R dataset.  
# It's no longer readable with a text editor, but it saves metadata (eg, factor information).
saveRDS(dto, file="./data/unshared/derived/dto.rds", compress="xz")

# ---- object-verification ------------------------------------------------
# the production of the dto object is now complete
# we verify its structure and content:
dto <- readRDS("./data/unshared/derived/dto.rds")
names(dto)
# 1st element - unit(person) level data
names(dto[["unitData"]])
# 2nd element - meta data, info about variables
names(dto[["metaData"]])












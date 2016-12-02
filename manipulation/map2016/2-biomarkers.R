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
dto <- readRDS("./data/unshared/derived/map2016/map_full.rds")
# ---- inspect-data -------------------------------------------------------------
# the list is composed of the following elements
names(dto)
head(dto)
ds<-dto
# 1st element - names of the studies as character vector
# names(dto[["unitData"]])

# ---- meta-table --------------------------------------------------------
# 2nd element - a dataset names and labels of raw variables + added metadata for all studies
# dto[["metaData"]] %>% dplyr::select(name_new, label, construct, type, include, label) %>% 
#   DT::datatable(
#     class   = 'cell-border stripe',
#     caption = "This is the primary metadata file. Edit at `./data/meta/meta-data-map.csv",
#     filter  = "top",
#     options = list(pageLength = 6, autoWidth = TRUE)
# )

# ds<- dto[["unitData"]]

# ---- tweak-data --------------------------------------------------------------
# 
# data<- dto[["unitData"]]
# # data$glucose
# # data$id
# # 
# length(data$glucose)
# length(data[, "glucose"])
# data[,"glucose"][data$id==20951100]
# # length(data[, toString(glucose)])
# 
# ds<- data
# 
# (ids <- sample(unique(ds$id),1))
# ds %>% 
#   dplyr::filter(id %in% ids ) %>% 
#   dplyr::group_by(id) %>% 
#   dplyr::select(id, cholesterol) %>% 
#   dplyr::mutate(
#     person_mean           = mean(cholesterol, na.rm=TRUE),
#     cholesterol_HIGH      = person_mean > 215,
#     cholesterol_HIGH_wave = cholesterol > 215
#   )


# ---- define-function-to-encode-biomarkers ------------------------------------
view_id <- function(ds,id){
  cat("View response pattern for id = ", id, "\n")
  print(ds[ds$id==id,])
  ds %>% dplyr::group_by()
  
}
# view a random person for sporadic inspections
ids <- sample(unique(ds$id),3)

#1
encode_biomarker <- function(data, measure_name, threashold, keep_wave=T){
  # measure_name <- "cholesterol"
  biomarker_name <- paste0(measure_name,"_HIGH")
  biomarker_name_wave <- paste0(biomarker_name,"_wave")
  
  a <- lazyeval::interp(~ round(mean(var, na.rm=T),2) , var = as.name(measure_name))
  b <- lazyeval::interp(~ var > threashold, var = as.name("person_mean"))
  c <- lazyeval::interp(~ var > threashold, var = as.name(measure_name))
  dots <- list (a,b,c)
  
  data <- data %>% 
    # dplyr::filter(id %in% ids) %>%
    # dplyr::select_("id", "cholesterol") %>%
    dplyr::group_by(id) %>% 
    dplyr::mutate_(.dots = setNames(dots, c("person_mean", biomarker_name, biomarker_name_wave))) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-person_mean)
  if(!keep_wave){
    data[,biomarker_name_wave] <- NULL
  }
  return(data)
}
# test function o
d <- ds %>% dplyr::select(id, cholesterol); head(d)
d <- encode_biomarker(
  data         = d, 
  measure_name = "cholesterol",
  threashold   = 215, 
  keep_wave    = T
)
head(d)

#2------------to be used with hdl, beacuse it is the reverse, where a low value indicated al biomarker
encode_biomarker_inverse <- function(data, measure_name, threashold, keep_wave=T){
  # measure_name <- "cholesterol"
  biomarker_name <- paste0(measure_name,"_LOW")
  biomarker_name_wave <- paste0(biomarker_name,"_wave")
  
  a <- lazyeval::interp(~ round(mean(var, na.rm=T),2) , var = as.name(measure_name))
  b <- lazyeval::interp(~ var < threashold, var = as.name("person_mean"))
  c <- lazyeval::interp(~ var < threashold, var = as.name(measure_name))
  dots <- list (a,b,c)
  
  data <- data %>% 
    # dplyr::filter(id %in% ids) %>%
    # dplyr::select_("id", "cholesterol") %>%
    dplyr::group_by(id) %>% 
    dplyr::mutate_(.dots = setNames(dots, c("person_mean", biomarker_name, biomarker_name_wave))) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-person_mean)
  if(!keep_wave){
    data[,biomarker_name_wave] <- NULL
  }
  return(data)
}
# test function o
d <- ds %>% dplyr::select(id, hdlchlstrl); head(d)
d <- encode_biomarker_inverse(
  data         = d, 
  measure_name = "hdlchlstrl",
  threashold   = 47, 
  keep_wave    = T
)
head(d)


# ---- apply function to all biomarkers -----------------------------------------

#cholesterol---
hist(ds$cholesterol)
summary(ds$cholesterol, useNA) #3rdq=213 

ds<- encode_biomarker(
  data         = ds, 
  measure_name = "cholesterol",
  threashold   = 213, 
  keep_wave    = T
)
# head(d)

#hba1c----

hist(ds$hba1c) 
summary(ds$hba1c, useNA) #3rdq=6.100

ds<- encode_biomarker(
  data         = ds, 
  measure_name = "hba1c",
  threashold   = 6.1, 
  keep_wave    = T
)


#hdlratio---
hist(ds$hdlratio)
summary(ds$hdlratio, useNA) #mean 3.32.66, #3rd=3.8 

ds<- encode_biomarker(
  data         = ds, 
  measure_name = "hdlratio",
  threashold   = 3.8, 
  keep_wave    = T
)



#ldl---
hist(ds$ldlchlstrl)
summary(ds$ldlchlstrl, useNA) #mean 100, #3rdq=121

ds<- encode_biomarker(
  data         = ds, 
  measure_name = "ldlchlstrl",
  threashold   = 121, 
  keep_wave    = T
)


#---hdl
hist(ds$hdlchlstrl)
summary(ds$hdlchlstrl, useNA) #1stq=47.0 ##low is bad

#encode biomarker 2
ds<- encode_biomarker_inverse(
  data         = ds, 
  measure_name = "hdlchlstrl",
  threashold   = 47, 
  keep_wave    = T
)


#--- glucose
hist(ds$glucose)
summary(ds$glucose, useNA)#3rdq=117

ds<- encode_biomarker(
  data         = ds, 
  measure_name = "glucose",
  threashold   = 117, 
  keep_wave    = T
)

#--- creatinine

hist(ds$creatinine)
names(ds)
summary(ds$creatinine, useNA) #3rdq=1.17

ds<- encode_biomarker(
  data         = ds, 
  measure_name = "creatinine",
  threashold   = 1.17, 
  keep_wave    = T
)

#keep re-reunning this to see different people in the sample - IT WORKED
ids <- sample(unique(ds$id),3)
ds %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,creatinine,creatinine_HIGH, creatinine_HIGH_wave)
ids <- sample(unique(ds$id),3)
ds %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,glucose,glucose_HIGH, glucose_HIGH_wave)

names(ds)
#--- obtain-multivariate-counts ----------------------
computed_variables <-  c("glucose_HIGH", "cholesterol_HIGH", "hba1c_HIGH", "hdlratio_HIGH", "hdlchlstrl_LOW", "ldlchlstrl_HIGH","creatinine_HIGH" ) 
bio<- ds %>% 
  # dplyr::group_by(glucoseHIGH,hgba1cHIGH) %>% 
  dplyr::group_by_(.dots = computed_variables) %>% 
  dplyr::summarize(count =n())


#--- create-composite -----------------------------------------------------------
 al_count_BL<-rep( 0, nrow(ds))
 ds$al_count_BL<-al_count_BL


 for(i in unique(ds$id)) {
   for (j in 1:length(ds$creatinine[ds$id==i])) {
     if (isTRUE(ds$creatinine_HIGH[ds$id==i][j]=='TRUE')) {
       ds$al_count_BL[ds$id==i][j] <- ds$al_count_BL[ds$id==i][j] + 1}

     if (isTRUE(ds$cholesterol_HIGH[ds$id==i][j]=='TRUE')) {
       ds$al_count_BL[ds$id==i][j] <- ds$al_count_BL[ds$id==i][j] + 1}

     if (isTRUE(ds$hba1c_HIGH[ds$id==i][j]=='TRUE')) {
       ds$al_count_BL[ds$id==i][j] <- ds$al_count_BL[ds$id==i][j] + 1}

     if (isTRUE(ds$hdlchlstrl_LOW[ds$id==i][j]=='TRUE')) {
       ds$al_count_BL[ds$id==i][j] <- ds$al_count_BL[ds$id==i][j] + 1}

     if (isTRUE(ds$hdlratio_HIGH[ds$id==i][j]=='TRUE')) {
       ds$al_count_BL[ds$id==i][j] <- ds$al_count_BL[ds$id==i][j] + 1}

     if (isTRUE(ds$ldlchlstrl_HIGH[ds$id==i][j]=='TRUE')) {
       ds$al_count_BL[ds$id==i][j] <- ds$al_count_BL[ds$id==i][j] + 1}

     if (isTRUE(ds$glucose_HIGH[ds$id==i][j]=='TRUE')) {
       ds$al_count_BL[ds$id==i][j] <- ds$al_count_BL[ds$id==i][j] + 1}

   }}
 
 test <- ds %>% 
   dplyr::select(id, cholesterol_HIGH_wave, hba1c_HIGH_wave, hdlratio_HIGH_wave, hdlchlstrl_LOW_wave,ldlchlstrl_HIGH_wave, glucose_HIGH_wave,creatinine_HIGH_wave, al_count_BL)
 head(test)
 
 ids <- sample(unique(ds$id),3)
 test <- ds %>% 
   dplyr::select(ids, al_count_BL)
 head(test)
 

#wave-----
al_count_wave<-rep( 0, nrow(ds))
ds$al_count_wave<-al_count_wave

for (i in 1:nrow(ds)) {
  
  if (isTRUE(ds$creatinine_HIGH_wave[i]=='TRUE')) {
    ds$al_count_wave[i] <- ds$al_count_wave[i] + 1}
  
  if (isTRUE(ds$cholesterol_HIGH_wave[i]=='TRUE')) {
    ds$al_count_wave[i] <- ds$al_count_wave[i] + 1}
  
  if (isTRUE(ds$hba1c_HIGH_wave[i]=='TRUE')) {
    ds$al_count_wave[i] <- ds$al_count_wave[i] + 1}
  
  if (isTRUE(ds$hdlchlstrl_LOW_wave[i]=='TRUE')) {
    ds$al_count_wave[i] <- ds$al_count_wave[i] + 1}
  
  if (isTRUE(ds$hdlratio_HIGH_wave[i]=='TRUE')) {
    ds$al_count_wave[i] <- ds$al_count_wave[i] + 1}
  
  if (isTRUE(ds$ldlchlstrl_HIGH_wave[i]=='TRUE')) {
    ds$al_count_wave[i] <- ds$al_count_wave[i] + 1}
  
  if (isTRUE(ds$glucose_HIGH_wave[i]=='TRUE')) {
    ds$al_count_wave[i] <- ds$al_count_wave[i] + 1}
  
}


test <- ds %>% 
  dplyr::select(id, cholesterol_HIGH_wave, hba1c_HIGH_wave, hdlratio_HIGH_wave, hdlchlstrl_LOW_wave,ldlchlstrl_HIGH_wave, glucose_HIGH_wave,creatinine_HIGH_wave, al_count_wave)
head(test)

test <- ds %>% 
  dplyr::select(id, al_count_wave)
head(test)

hist(ds$al_count_wave)

ds %>% histogram_discrete("al_count_wave")

# --- categorize induviduals ------------

#category variable for biomarkers 
table(ds$al_count_wave, useNA="always")

#assigns allostatic load category for each person at baseline
for (i in 1:length(ds$al_count_BL)) {
  
  if (isTRUE(ds$al_count_BL[i] == 0)) {
    ds$al_catg_BL[i] <- "LOW" }
  
  else if (isTRUE(ds$al_count_BL[i] <=2 )) {
    ds$al_catg_BL[i] <- "MED" }
  
  else 
    # (isTRUE(ds$phys_wp_wave[i] >=2 )){
    ds$al_catg_BL[i] <- "HIGH"  
} 

ids <- sample(unique(ds$id),1)
ds %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,al_count_BL, al_catg_BL
  )


#assigns allostatic load category for each person at every observation 
for (i in 1:length(ds$al_count_wave)) {
  
  if (isTRUE(ds$al_count_wave[i] == 0)) {
    ds$al_catg_wave[i] <- "LOW" }
  
  else if (isTRUE(ds$al_count_wave[i] <=2 )) {
    ds$al_catg_wave[i] <- "MED" }
  
  else 
    # (isTRUE(ds$phys_wp_wave[i] >=2 )){
    ds$al_catg_wave[i] <- "HIGH"  
} 

#keep re-reunning this to see different people in the sample  IT WORKED
ids <- sample(unique(ds$id),1)
ds %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,al_count_wave, al_catg_wave
  )

eg<-ds %>%
  dplyr::filter(id %in% ids ) %>%
  dplyr::group_by(id) %>%
  dplyr::select(id,al_count_wave, al_catg_wave, cholesterol_HIGH_wave, hba1c_HIGH_wave, hdlratio_HIGH_wave, hdlchlstrl_LOW_wave,ldlchlstrl_HIGH_wave, glucose_HIGH_wave,creatinine_HIGH_wave, al_count_wave
  )

set.seed(1)
ids <- sample(ds$id,20)
d <- ds %>%  dplyr::filter( id %in% ids)
names(ds)

p1 <- ggplot(d, aes(x=year_in_study, y=physical_activity, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1

library(lattice)
xyplot(physical_activity ~ year_in_study | id, data=d, as.table=T)



p2 <- ggplot(d, aes(x=year_in_study, y=al_count_wave, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p2

xyplot(al_count_wave ~ year_in_study | id, data=d, as.table=T)


#frequency table of created allostatic categories 
table(ds$al_catg_wave, useNA="always")
#frequency table of created allostatic categories 
table(ds$al_catg_BL, useNA="always")



#subset
names(ds)

myvars<- c("id","study","year_in_study","cogdx","dementia", "age_bl","age_at_visit","age_death","edu", "msex","race","apoe","date_at_baseline", 
           "cesdsum","episodic","percep_orient", "percep_speed","semantic","wm","global","dig_b","dig_f","mmse",
           "sdmt","wl_im","wl_del","wl_rec","nle","pss","physical_activity",
           "cholesterol", "hdlchlstrl", "hdlratio", "ldlchlstrl", "glucose", "creatinine", "hba1c", 
           "al_count_BL", "al_count_wave", "al_catg_BL", "al_catg_wave","social_isolation")

ds2<- ds[myvars]
head(ds2)
names(ds2)


# --- save-data ----------------------------------------------------------

saveRDS(ds2, "./data/unshared/derived/map2016/dto-AL_subset.rds")

saveRDS(ds, "./data/unshared/derived/map2016/dto-AL.rds")


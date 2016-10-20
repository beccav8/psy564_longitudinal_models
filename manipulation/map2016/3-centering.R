
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

# ---- load-data ---------------------------------------------------------------
# load the product of 0-ellis-island.R,  a list object containing data and metadata
dto <- readRDS("./data/unshared/derived/map2016/dto-AL_subset.rds")
# ---- inspect-data -------------------------------------------------------------
# the list is composed of the following elements
names(dto)
head(dto)
data<-dto



set.seed(1)
ids <- sample(data$id,20)
d <- data %>%  dplyr::filter( id %in% ids)
names(d)

p1 <- ggplot(d, aes(x=year_in_study, y=physical_activity, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1


library(lattice)
xyplot(physical_activity ~ year_in_study | id, data=d, as.table=T)



##########################-----Center-PA-----#########################################################
##----physical-activity-between-person---MEAN------------------------------------------------------------
# phys_bp = the persons mean on PA across occasions - the grand mean of PA in the population  (mean=2.94)

for(i in unique(data$id)) {
  for (j in 1:length(data$physical_activity[data$id==i])) {
    
    data$phys_bp_mean[data$id==i][j]<- ( (data$physical_activity[data$id==i][j]) - (mean(data$physical_activity, na.rm =TRUE)) ) 
    
  } }

data$physical_activity[data$id==21305588]
data$phys_bp_mean[data$id==21305588]


# ##----physical-activity-between-person---MEDIAN------------------------------------------------------------
# # phys_bp = the persons median on PA across occasions - the grand median of the population  (median = 1.75)
# 
# for(i in unique(data$id)) {
#   for (j in 1:length(data$physical_activity[data$id==i])) {
#     
#     data$phys_bp_median[data$id==i][j]<- ( (data$physical_activity[data$id==i][j]) - (median(data$physical_activity, na.rm =TRUE)) ) 
#     
#   } }
# 
# data$physical_activity[data$id==21305588]
# data$phys_bp_median[data$id==21305588]


##----physical-activity-within-person-fluctiations-MEAN-CENTERED---------------------------------------------------
# phys_wp = that persons score at time j, minus their mean (deviations-from--own-mean)


for(i in unique(data$id)) {
  for (j in 1:length(data$physical_activity[data$id==i])) {
    
    data$phys_wp[data$id==i][j]<- (data$physical_activity[data$id==i][j]) - (mean(data$physical_activity[data$id==i], na.rm =TRUE))
  } }

mean(data$physical_activity[data$id==21305588], na.rm=TRUE)
data$physical_activity[data$id==21305588]
data$phys_wp[data$id==21305588]


#######################################-center-age-#################################################

mean(data$age_at_visit, na.rm=TRUE) 83

data$age_at_visit_centered<- (data$age_at_visit) - (mean(data$age_at_visit, na.rm=TRUE))


data$age_at_visit[data$id==21305588]
data$age_at_visit_centered[data$id==21305588]


#-------------------------- centering baseline age

# mean_age_baseline <- mean(data[["age_bl"]], na.rm = TRUE)   #79
# est_mean_age_bl<- rep(mean(data$age_bl, na.rm=TRUE), length(data$age_bl))
# data$age_bl_centered<- data$age_bl - est_mean_age_bl

names(data)


saveRDS(data, "./data/unshared/derived/map2016/data_centered.rds")



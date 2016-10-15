# Clear memory from previous runs
base::rm(list=base::ls(all=TRUE))
cat("\f")

# install.packages("knitr")
# install.packages("markdown")
# install.packages("testit")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("stats")
# install.packages("ggplot2")
# install.packages("extrafont")
# install.packages("doBy")
# install.packages("psych")

# @knitr load-packages --------------------

# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(lmerTest)
library(outliers)
# Load the necessary packages.
base::require(base)
base::require(knitr)
base::require(markdown)
base::require(testit)
base::require(dplyr)
base::require(reshape2)
base::require(stringr)
base::require(stats)
base::require(ggplot2)
base::require(extrafont)
base::require(lattice)
base::require(doBy)
base::require(psych)


# @knitr load-sources --------------------

# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graph-presets.R")
source("./scripts/general-graphs.R")  #in scripts folder
source("./scripts/specific-graphs.R")
source("./scripts/specific-graphs-pred.R")
source("./scripts/graphs-pred.R")
source("./scripts/graphs-predVID.R")
source("./scripts/multiplot-function.R")
source("./scripts/functions-for-glm-models.R")
# source("./scripts/graph-presets.R") # fonts, colors, themes


#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
# rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console



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

# @knitr declare-globals --------------------
path_input  <- "./data/unshared/derived/map/data_tweak.rds"     

# @knitr load-data --------------------
ds <- readRDS(path_input)
# dim(ds)
# str(ds)
# head(ds)

# --- subsets for analyses
#without biomarkers 
dwn_dsb <- ds %>% 
  dplyr::select(id, full_year, age_bl, age_at_visit_centered, physical_activity, phys_bl_bp, phys_bp_mean, phys_bp_median, phys_wp, dsb, age_bl_centered, female, edu) %>% 
  na.omit()
#19998 observations

#without biomarkers 
dwn_mmse <- ds %>% 
  dplyr::select(id, full_year, age_bl, age_at_visit_centered, physical_activity, phys_bl_bp, phys_bp_mean, phys_bp_median, phys_wp, mmse, age_bl_centered, female, edu) %>% 
  na.omit()
#19591 observations

## biomarker subsets -------------

dbio_dsb <- ds %>% 
  dplyr::select(id, full_year, age_bl, physical_activity, phys_bl_bp, phys_bp_mean, phys_bp_median, phys_wp, dsb, age_bl_centered, female, edu, 
                apoe, cholesterol, hemoglobin, hdlratio, hdl,ldl, glucose, creatine, 
                cholesterol_HIGH_wave, hemoglobin_HIGH_wave, hdlratio_HIGH_wave, hdl_LOW_wave,ldl_HIGH_wave, glucose_HIGH_wave,creatine_HIGH_wave, al_count_wave, al_catg_wave ) %>%                
  # cholesterol_HIGH,  hemoglobin_HIGH,  hdlratio_HIGH, hdl_LOW,ldl_HIGH, glucose_HIGH,creatine_HIGH) %>% 
  na.omit()
#4073 observations

dbio_mmse <- ds %>% 
  dplyr::select(id, full_year, age_bl, physical_activity, phys_bl_bp, phys_bp_mean, phys_bp_median, phys_wp, mmse, age_bl_centered, female, edu, 
                apoe, cholesterol, hemoglobin, hdlratio, hdl,ldl, glucose, creatine, 
                cholesterol_HIGH_wave, hemoglobin_HIGH_wave, hdlratio_HIGH_wave, hdl_LOW_wave,ldl_HIGH_wave, glucose_HIGH_wave,creatine_HIGH_wave, al_count_wave, al_catg_wave ) %>%                
  # cholesterol_HIGH,  hemoglobin_HIGH,  hdlratio_HIGH, hdl_LOW,ldl_HIGH, glucose_HIGH,creatine_HIGH) %>% 
  na.omit()
#4046 observations

## stress variable subsets ----
#dsb and mmse together, because number of observations doesn't really change 
#these subsets include negative life events and perceived stress
stress <- ds %>% 
  dplyr::select(id, full_year, age_bl, age_at_visit_centered, neglifeevents, perceivedstress, physical_activity, phys_bl_bp, phys_bp_mean, phys_bp_median, phys_wp, dsb, mmse, age_bl_centered, female, edu) %>% 
  na.omit()
#2400 observations


# @knitr save-data ------------------

saveRDS(dwn_dsb, "./data/unshared/derived/map/dwn_map_dsb.rds")
saveRDS(dwn_mmse, "./data/unshared/derived/map/dwn_map_mmse.rds")


saveRDS(dbio_dsb, "./data/unshared/derived/map/dbio_map_dsb.rds")
saveRDS(dbio_mmse, "./data/unshared/derived/map/dbio_map_mmse.rds")

saveRDS(stress, "./data/unshared/derived/map/stress.rds")


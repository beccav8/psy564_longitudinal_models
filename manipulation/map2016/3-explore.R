
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


#year in study--------------------------

range(data$year_in_study, na.rm=TRUE)  # 0 to 18
mean(data$year_in_study, na.rm=TRUE)   #4.06
#-----explore-physical-activity---------------- 

range(data$physical_activity, na.rm =  TRUE)
#0-43 
mean(data$physical_activity, na.rm= TRUE)
#2.94

histogram(data$physical_activity)
#negatively skewed

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


g1<- ggplot2::ggplot(data, aes_string(x= "year_in_study", y="physical_activity")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g1 <- g1 + labs(list(
  title= "Raw PA scores Over Time",
  x="time", y="Physical_activity"))
g1


library(lattice)
xyplot(physical_activity ~ year_in_study | id, data=d, as.table=T)


eq_0 <- as.formula("physical_activity ~ 1 + year_in_study +             
                   (1 |id)")
#does the outcome change over time? i.e. if so, it makes sense to add predictors.

model_0<- lmerTest::lmer(eq_0, data=data, REML = FALSE) 
lmerTest::summary((model_0))
broom::tidy(model_0, data=dwn)


#----explore-stress-----------------

#PSS-----------------

range(data$pss, na.rm=TRUE) 
#0-3.75 (possible range=1-5)
mean(data$pss, na.rm=TRUE)
#2.024
histogram(data$pss)
#normal

set.seed(1)
ids <- sample(data$id,50)
d <- data %>%  dplyr::filter( id %in% ids)
names(d)

p1 <- ggplot(d, aes(x=year_in_study, y=pss, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1

#PSS doesnt seem to be measures at each occasion

g1<- ggplot2::ggplot(data, aes_string(x= "year_in_study", y="pss")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g1 <- g1 + labs(list(
  title= "Raw PSS scores Over Time",
  x="time", y="PSS"))
g1
#declines over time

library(lattice)
xyplot(pss ~ year_in_study | id, data=d, as.table=T)


eq_1 <- as.formula("pss ~ 1 + year_in_study +             
                   (1 |id)")


model_1<- lmerTest::lmer(eq_1, data=data, REML = FALSE) 
lmerTest::summary((model_1))

#NLE---------

range(data$nle, na.rm =  TRUE)
#0-17

mean(data$nle, na.rm= TRUE)
#2.45

histogram(data$nle)
#negatively skewed


p1 <- ggplot(d, aes(x=year_in_study, y=nle, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1
#also not measured well longitudinally 


g1<- ggplot2::ggplot(data, aes_string(x= "year_in_study", y="nle")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g1 <- g1 + labs(list(
  title= "Raw PSS scores Over Time",
  x="time", y="PSS"))
g1
#increases

library(lattice)
xyplot(nle ~ year_in_study | id, data=d, as.table=T)


eq_2 <- as.formula("nle ~ 1 + year_in_study +             
                   (1 |id)")


model_2<- lmerTest::lmer(eq_2, data=data, REML = FALSE) 
lmerTest::summary((model_2))


#--AL------------------------
#baseline---------

range(data$al_count_BL, na.rm =  TRUE)
#0-7
mean(data$al_count_BL, na.rm= TRUE)
#1.5
histogram(data$al_count_BL)
#pos skewed


p1 <- ggplot(d, aes(x=year_in_study, y=al_count_wave, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1
 


g1<- ggplot2::ggplot(data, aes_string(x= "year_in_study", y="al_count_wave")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
g1 <- g1 + labs(list(
  title= "al_count_wave Over Time",
  x="time", y="al_count_wave"))
g1
#increases


#-----cognitive-outcomes---------------#

#episodic---------------------------

range(data$episodic, na.rm =  TRUE)
#-3.7 to 1.9
mean(data$episodic, na.rm= TRUE)
#0.05
histogram(data$episodic)
#positively skewed 

p1 <- ggplot(d, aes(x=year_in_study, y=episodic, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  coord_cartesian(ylim=c(-4, 2))
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1



ep<- ggplot2::ggplot(data, aes_string(x= "year_in_study", y="episodic")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)+
  coord_cartesian(ylim=c(-4, 2))
ep <- ep + labs(list(
  title= "Episodic Memory Over Time",
  ep="time", y="episodic"))
ep
#increases !?!?!?!?!?!

library(lattice)
xyplot(episodic ~ year_in_study | id, data=d, as.table=T)



#percep_speed-----------------------

range(data$percep_speed, na.rm =  TRUE)
#-3.07 to 2.6
mean(data$percep_speed, na.rm= TRUE)
# -0.109
histogram(data$percep_speed)
#normal 


p1 <- ggplot(d, aes(x=year_in_study, y=percep_speed, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1



ps <- ggplot2::ggplot(data, aes_string(x= "year_in_study", y="percep_speed")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
ps <- ps + labs(list(
  title= "Perceptual Speed Over Time",
  ps="time", y="episodic"))
ps
#decreases 

library(lattice)
xyplot(percep_speed ~ year_in_study | id, data=d, as.table=T)



#wm---------------------------------

range(data$wm, na.rm =  TRUE)
#-3.57 to 2.69
mean(data$wm, na.rm= TRUE)
# -0.066
histogram(data$wm)
#normal 

p1 <- ggplot(d, aes(x=year_in_study, y=wm, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1



ps <- ggplot2::ggplot(data, aes_string(x= "year_in_study", y="wm")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
ps <- ps + labs(list(
  title= "Perceptual Speed Over Time",
  ps="time", y="perceptual Speed"))
ps
#decreases 

library(lattice)
xyplot(wm ~ year_in_study | id, data=d, as.table=T)


#sdmt--------------------------------------------------

range(data$sdmt, na.rm =  TRUE)
#0-77
mean(data$sdmt, na.rm= TRUE)
# 35.74
histogram(data$sdmt)
#normal-ish 

p1 <- ggplot(d, aes(x=year_in_study, y=sdmt, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1



ps <- ggplot2::ggplot(data, aes_string(x= "year_in_study", y="sdmt")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
ps <- ps + labs(list(
  title= "symbol digit modality Over Time",
  ps="time", y="sdmt"))
ps
#decreases 

library(lattice)
xyplot(sdmt ~ year_in_study | id, data=d, as.table=T)


#dig_b
range(data$dig_b, na.rm =  TRUE)
#0-12
mean(data$dig_b, na.rm= TRUE)
#5.92
histogram(data$dig_b)
#normal 


p1 <- ggplot(d, aes(x=year_in_study, y=dig_b, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1


ps <- ggplot2::ggplot(data, aes_string(x= "year_in_study", y="dig_b")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
ps <- ps + labs(list(
  title= "dig_b Over Time",
  ps="time", y="dig_b"))
ps
#decreases 

library(lattice)
xyplot(dig_b ~ year_in_study | id, data=d, as.table=T)


#mmse

range(data$mmse, na.rm =  TRUE)
#0-30
mean(data$mmse, na.rm= TRUE)
# 26
histogram(data$mmse)
#neg skew

p1 <- ggplot(d, aes(x=year_in_study, y=mmse, group=id)) +
  geom_line() +
  # stat_smooth(method=lm, se=FALSE)+
  scale_color_brewer(palette="Set2") +
  # geom_text(aes(label=id))+
  ggtitle("Growth curve for individuals")
p1


ps <- ggplot2::ggplot(data, aes_string(x= "year_in_study", y="mmse")) +
  # geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=FALSE)
ps <- ps + labs(list(
  title= "mmse Over Time",
  ps="time", y="mmse"))
ps
#decreases 

library(lattice)
xyplot(mmse ~ year_in_study | id, data=d, as.table=T)


###--------------------------------------------------------###########################
names(data)
g<- ggplot2::ggplot(data, aes_string(x= "phys_wp", y="percep_speed") ) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") 
  # coord_cartesian(ylim = c(0,15)) +
g

g<- ggplot2::ggplot(data, aes_string(x= "physical_activity", y="percep_speed") ) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") 
# coord_cartesian(ylim = c(0,15)) +
g


#positively correlated

g<- ggplot2::ggplot(data, aes_string(x= "phys_wp", y="mmse") ) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") 
# coord_cartesian(ylim = c(0,15)) +
g

g<- ggplot2::ggplot(data, aes_string(x= "physical_activity", y="mmse") ) +
  geom_point(shape=21, size=5)+
  stat_smooth(method=lm, se=TRUE)+
  scale_color_brewer(palette="Set2") 
# coord_cartesian(ylim = c(0,15)) +
g



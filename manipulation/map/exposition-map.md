# Exposition

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->





```
[1] "C:/Users/Rebecca/Documents/GitHub/cognition-stress-activity"
```






## Define Variables 

* phys_bl_bp = each persons baseline score - the baseline grand mean (between person baseline compare)  
* phys_bp_mean = the persons mean score across occasions - the grand mean  
* phys_bp_median = the persons median score across occasions - the grand median    
* phys_wp = that persons score at time j, minus their mean (deviations-from--own-mean)  
* mmse_wp = the persons mmse score at time j, minus their mean, so that a positive value indicated scores higehr then their mean   
* biomarker_high = indicates if the person's mean on that particular biomarker is above the 3rd quartile  

physical activity question: hours per week that the ppt. engages in 5 different categories  

## Tweak Data





```r
## dsb
boxplot(dtest$dsb)
```

<img src="figure_rmd/BasicGraph1-1.png" width="550px" />

```r
hist(dtest$dsb)
```

<img src="figure_rmd/BasicGraph1-2.png" width="550px" />

```r
#normally distributed! 

## mmse
boxplot(dtest$mmse)
```

<img src="figure_rmd/BasicGraph1-3.png" width="550px" />

```r
hist(dtest$mmse)
```

<img src="figure_rmd/BasicGraph1-4.png" width="550px" />

```r
## age
boxplot(dtest$age_bl_centered)
```

<img src="figure_rmd/BasicGraph1-5.png" width="550px" />

```r
# bp<-boxplot(dtest$age_bl_centered)
# bp$out 
hist(dtest$age_bl_centered)
```

<img src="figure_rmd/BasicGraph1-6.png" width="550px" />

```r
#looks ok

## physical activity raw
boxplot(dtest$physical_activity)
```

<img src="figure_rmd/BasicGraph1-7.png" width="550px" />

```r
# bp<-boxplot(dtest$physical_activity)
# bp$out 
dtest %>% histogram_continuous(variable_name= "physical_activity") + stat_bin(binwidth = 5)
```

<img src="figure_rmd/BasicGraph1-8.png" width="550px" />

```r
#anything greater than 40 may be an outlier 
dtest<- subset(dtest, physical_activity < 40) 
boxplot(dtest$physical_activity)
```

<img src="figure_rmd/BasicGraph1-9.png" width="550px" />

```r
## physical activity bp
boxplot(dtest$phys_bp_median)
```

<img src="figure_rmd/BasicGraph1-10.png" width="550px" />

```r
dtest %>% histogram_continuous(variable_name= "phys_bp_median") + stat_bin(binwidth = 5)
```

<img src="figure_rmd/BasicGraph1-11.png" width="550px" />

```r
#anything greater than 20 may be an outlier 
dtest<- subset(dtest, phys_bp_median < 20) 
boxplot(dtest$phys_bp_median)
```

<img src="figure_rmd/BasicGraph1-12.png" width="550px" />

```r
## physical activity wp
boxplot(dtest$phys_wp)
```

<img src="figure_rmd/BasicGraph1-13.png" width="550px" />

```r
dtest %>% histogram_continuous(variable_name= "phys_wp") + stat_bin(binwidth = 5)
```

<img src="figure_rmd/BasicGraph1-14.png" width="550px" />

```r
#looks good
```

<img src="figure_rmd/BasicGraph2-1.png" width="550px" /><img src="figure_rmd/BasicGraph2-2.png" width="550px" /><img src="figure_rmd/BasicGraph2-3.png" width="550px" /><img src="figure_rmd/BasicGraph2-4.png" width="550px" /><img src="figure_rmd/BasicGraph2-5.png" width="550px" /><img src="figure_rmd/BasicGraph2-6.png" width="550px" /><img src="figure_rmd/BasicGraph2-7.png" width="550px" /><img src="figure_rmd/BasicGraph2-8.png" width="550px" />

```
  cholesterol_HIGH_wave  freq
1                 FALSE  7202
2                  TRUE  2196
3                    NA 10959
```

```
  hemoglobin_HIGH_wave  freq
1                FALSE  4412
2                 TRUE   796
3                   NA 15149
```

```
  hdl_LOW_wave  freq
1        FALSE  6897
2         TRUE  2501
3           NA 10959
```

```
  ldl_HIGH_wave  freq
1         FALSE  7132
2          TRUE  2187
3            NA 11038
```

```
  glucose_HIGH_wave  freq
1             FALSE  7057
2              TRUE  2335
3                NA 10965
```

```
  creatine_HIGH_wave  freq
1              FALSE  7606
2               TRUE  1786
3                 NA 10965
```

```
  hdlratio_HIGH_wave  freq
1              FALSE  6968
2               TRUE  2430
3                 NA 10959
```

## Graphs  
## DSB over time
<img src="figure_rmd/dsb-1.png" width="550px" /><img src="figure_rmd/dsb-2.png" width="550px" />

## MMSE over time
<img src="figure_rmd/mmse-1.png" width="550px" /><img src="figure_rmd/mmse-2.png" width="550px" />

## PA over time
male walk more than the population average 
<img src="figure_rmd/PA-1.png" width="550px" /><img src="figure_rmd/PA-2.png" width="550px" />

## Between person effects
male walk more than the population average - but there are some males who are reporting really high PA across time,
this could be pulling their average up 
<img src="figure_rmd/PA_between-1.png" width="550px" /><img src="figure_rmd/PA_between-2.png" width="550px" />

## Within person effects
seems to be no difference between the sex's in deviations from their average exercise over time.Females 
may walk slightly more than their average over time
<img src="figure_rmd/PA_within-1.png" width="550px" /><img src="figure_rmd/PA_within-2.png" width="550px" />



<img src="figure_rmd/mv-raw-1.png" width="550px" /><img src="figure_rmd/mv-raw-2.png" width="550px" />

<img src="figure_rmd/mv-bp-1.png" width="550px" /><img src="figure_rmd/mv-bp-2.png" width="550px" />

<img src="figure_rmd/mv-wp-1.png" width="550px" /><img src="figure_rmd/mv-wp-2.png" width="550px" />


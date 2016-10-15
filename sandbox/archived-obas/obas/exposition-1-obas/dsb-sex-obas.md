# Exposition

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->






```
[1] "C:/Users/Rebecca/Documents/GitHub/cognition-stress-activity"
```

# Exposition

## Ellis-Island


The Memory and Aging Project data set was collected at Rush University Medical Clinic. [Click here to visit the Site! ](https://www.rush.edu/services-treatments/alzheimers-disease-center/radc-research/memory-and-aging-project-rush)

Loading the data and renaming the variables




```
 logi [1:5988] TRUE TRUE TRUE TRUE TRUE TRUE ...
```

## Define Variables 

* phys_bl_bp = each persons baseline score - the baseline grand mean (between person baseline compare)  
* phys_bp_mean = the persons mean score across occasions - the grand mean  
* phys_bp_median = the persons median score across occasions - the grand median    
* phys_wp = that persons score at time j, minus their mean (deviations-from--own-mean)  
* dsb_wp = the persons dsb score at time j, minus their mean, so that a positive value indicated scores higehr then their mean   
* biomarker_high = indicates if the person's mean on that particular biomarker is above the 3rd quartile  

physical activity question: hours per week that the ppt. engages in 5 different categories  

## Tweak Data


## Graphs  



```r
#dsb
boxplot(dwn$dsb)
```

<img src="figure_rmd/BasicGraph1-1.png" width="550px" />

```r
# bpdsb<-boxplot(dwn$dsb)
hist(dwn$dsb)
```

<img src="figure_rmd/BasicGraph1-2.png" width="550px" />

```r
# t(table(dwn$dsb, useNA = "always"))
#not normal dist

dtest<-dwn

#physical activity raw

# table(dwn$walk_now, useNA="always")
boxplot(dwn$walk_now)
```

<img src="figure_rmd/BasicGraph1-3.png" width="550px" />

```r
# bp<-boxplot(dwn$walk_now)
# bp$out 
dwn %>% histogram_continuous(variable_name= "walk_now") + stat_bin(binwidth = 5)
```

<img src="figure_rmd/BasicGraph1-4.png" width="550px" />

```r
#anything greater than 50 may be an outlier 
dtest<- subset(dtest, walk_now < 80) 
boxplot(dtest$walk_now)
```

<img src="figure_rmd/BasicGraph1-5.png" width="550px" />

```r
#physical activity bp

# table(dtest$phys_bp_median, useNA="always")
boxplot(dtest$phys_bp_median)
```

<img src="figure_rmd/BasicGraph1-6.png" width="550px" />

```r
# bp<-boxplot(dtest$phys_bp_median)
# bp$out 
dtest %>% histogram_continuous(variable_name= "phys_bp_median") + stat_bin(binwidth = 5)
```

<img src="figure_rmd/BasicGraph1-7.png" width="550px" />

```r
#anything greater than 18 may be an outlier 
dtest<- subset(dtest, phys_bp_median < 30) 
boxplot(dtest$phys_bp_median)
```

<img src="figure_rmd/BasicGraph1-8.png" width="550px" />

```r
#physical activity wp

# table(dtest$phys_wp, useNA="always")
boxplot(dtest$phys_wp)
```

<img src="figure_rmd/BasicGraph1-9.png" width="550px" />

```r
# bp<-boxplot(dtest$phys_wp)
# bp$out 
dtest %>% histogram_continuous(variable_name= "phys_wp") + stat_bin(binwidth = 5)
```

<img src="figure_rmd/BasicGraph1-10.png" width="550px" />

```r
#looks good
```

<img src="figure_rmd/BasicGraph2-1.png" width="550px" /><img src="figure_rmd/BasicGraph2-2.png" width="550px" /><img src="figure_rmd/BasicGraph2-3.png" width="550px" />

```
  cholesterol_HIGH_wave freq
1                 FALSE   84
2                  TRUE   28
```

```
  hemoglobin_HIGH_wave freq
1                FALSE   81
2                 TRUE   31
```

```
  glucose_HIGH_wave freq
1             FALSE   88
2              TRUE   24
```



# **dsb over time**

<img src="figure_rmd/graph-dsb-1.png" width="550px" />

# **PA over time**

<img src="figure_rmd/graph-PA-1.png" width="550px" /><img src="figure_rmd/graph-PA-2.png" width="550px" />

# **Between person effects**

<img src="figure_rmd/graph-PA_bp-1.png" width="550px" />

# **Within person effects**

<img src="figure_rmd/graph-PA_wp-1.png" width="550px" />

# **Raw PA scores and dsb**

<img src="figure_rmd/graph-PA_dsb-1.png" width="550px" />

# **Between Person PA and dsb**

<img src="figure_rmd/graph-PAbp_dsb-1.png" width="550px" />

# **Within Person PA and dsb**

<img src="figure_rmd/graph-PAwp_dsb-1.png" width="550px" />


```
[1] 278
```

<img src="figure_rmd/BasicGraph6-1.png" width="550px" />
























rm(list=ls(all=TRUE)) # clear environment
cat("\f") # clear console 


# All data land on Ellis Island
knitr::stitch_rmd(
  script="./manipulation/map/0-ellis-island-map.R", 
  output="./manipulation/map/stitched-output/0-ellis-island-map.md"
)
# look into knitr::spin() http://www.r-bloggers.com/knitrs-best-hidden-gem-spin/

# Initial reivew of variables


#MAP

rmarkdown::render(
  input = "./sandbox/map/exposition-1/exposition-0.Rmd" ,
  output_format="html_document", 
  clean=TRUE
)

rmarkdown::render(
  input = "./sandbox/map/exposition-1/exposition-1-sex-map.Rmd" ,
  output_format="html_document", 
  clean=TRUE
)




#OBAS

rmarkdown::render(
  input = "./sandbox/obas/exposition-1-obas/exposition-0.Rmd" ,
  output_format="html_document", 
  clean=TRUE
)


rmarkdown::render(
  input = "./sandbox/obas/exposition-1-obas/exposition-1-sex-obas.Rmd" ,
  output_format="html_document", 
  clean=TRUE
)

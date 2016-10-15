
lines_predVID <- function(
  ds, 
  variable_name,
     predicted,
  line_size=.5, 
  line_alpha=.5,
  
  # top_y = max(ds[,variable_name], na.rm=T),
#   bottom_y = min(ds[,variable_name],na.rm=T),
#   by_y = round(top_y/10,0),
  top_y = 35,
  bottom_y = 0,
  by_y = 5,
  
#   bottom_age = 55,
#   top_age = 110,
#   by_age = 5, 
  
  bottom_age = 0,
  top_age = 90,
  by_age = 5, 
  
  bottom_time = 10,
  top_time = 900,
  by_time = 100
){
  # d <- dto[["unitData"]]
  d <- ds

  g11 <- basic_line(d, variable_name, "years_in_study", "purple", line_alpha, line_size, T)
  g12 <- basic_line(d, predicted, "years_in_study", "purple", line_alpha, line_size, T)
  
  
  # d_observed,
  # variable_name = "cogn_global",
  # time_metric, 
  # color_name="black",
  # line_alpha=.5,
  # line_size =.5, 
  # smoothed = FALSE,
  # main_title     = variable_name,
  # x_title        = paste0("Time metric: ", time_metric),
  # y_title        = variable_name,
  # rounded_digits = 0L
  # 
  
  
  
  g11 <- g11 + labs(x="visit")
  g12 <- g12 + labs(x="visit")
  

  # bottom_age <- 60
  # top_age <- 100
  # bottom_time <- 0
  # top_time <- 20
  
  g11 <- g11 + coord_cartesian(xlim=c(bottom_age,top_age))
  g12 <- g12 + coord_cartesian(xlim=c(bottom_age,top_age))
  

  
  g11 <- g11 + scale_x_continuous(breaks=seq(bottom_age,top_age,by=by_age))
  g12 <- g12 + scale_x_continuous(breaks=seq(bottom_age,top_age,by=by_age))
  

  # top_y <- 10
  # bottom_y <- 0
  # by_y <- 1
  
  
#   g11 <- g11 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y))
#   g12 <- g12 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y))

  
  # b <- b + scale_y_continuous(limits=c(-5,5 )) 
  grid::grid.newpage()    
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=1, ncol=2,
                              widths=grid::unit(c(.5, .5) ,c("null","null")),
                              heights=grid::unit(c(.5, .5), c("null","null"))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  print(g11, vp=grid::viewport(layout.pos.row=1, layout.pos.col=1 ))
  print(g12, vp=grid::viewport(layout.pos.row=1, layout.pos.col=2 ))
  
 
  grid::popViewport(0)
  
} 

# head(ds);
# raw_smooth_lines(ds, "cogn_global")
#   
#   # ds, 
#   variable_name= "cogn_global",
#   line_size=.5,
#   line_alpha=.5,
#   top_y = max(ds[,outcome]),
#   bottom_y = min(ds[,outcome]),
#   by_y = round(top_y/10,0),
#   bottom_age = 60,
#   top_age = 100,
#   bottom_time = 0,
#   top_time = 20
# # )


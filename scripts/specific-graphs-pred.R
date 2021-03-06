
raw_smooth_lines_pred <- function(
  ds, 
  variable_name,
     pred_age,
     pred_visit,
  line_size=.5, 
  line_alpha=.5,
  
  top_y = max(ds[,variable_name], na.rm=T),
  bottom_y = min(ds[,variable_name],na.rm=T),
  by_y = round(top_y/10,0),
  
  bottom_age = 55,
  top_age = 110,
  by_age = 5, 
  
  bottom_time = 10,
  top_time = 900,
  by_time = 100
){
  # d <- dto[["unitData"]]
  d <- ds

  g11 <- basic_line(d, variable_name, "age_at_visit", "purple", line_alpha, line_size, F)
  g21 <- basic_line(d, pred_age, "age_at_visit", "purple", line_alpha, line_size, T)
  g12 <- basic_line(d, variable_name, "visit_id", "salmon", line_alpha, line_size, F)
  g22 <- basic_line(d, pred_visit, "visit_id", "salmon", line_alpha, line_size, T)
  
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
  
  
  
  g11 <- g11 + labs(x="Age at visit")
  g21 <- g21 + labs(x="Age at visit")
  
  g12 <- g12 + labs(x="Visit")
  g22 <- g22 + labs(x="Visit")
  
  # bottom_age <- 60
  # top_age <- 100
  # bottom_time <- 0
  # top_time <- 20
  
  g11 <- g11 + coord_cartesian(xlim=c(bottom_age,top_age))
  g21 <- g21 + coord_cartesian(xlim=c(bottom_age,top_age))
  
  g12 <- g12 + coord_cartesian(xlim=c(bottom_time,top_time))
  g22 <- g22 + coord_cartesian(xlim=c(bottom_time,top_time))
  
  
  
  g11 <- g11 + scale_x_continuous(breaks=seq(bottom_age,top_age,by=by_age))
  g21 <- g21 + scale_x_continuous(breaks=seq(bottom_age,top_age,by=by_age))
  
  g12 <- g12 + scale_x_continuous(breaks=seq(bottom_time,top_time,by=by_time))
  g22 <- g22 + scale_x_continuous(breaks=seq(bottom_time,top_time,by=by_time))
  
  # top_y <- 10
  # bottom_y <- 0
  # by_y <- 1
  
  
  g11 <- g11 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y))
  g21 <- g21 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y))
  g12 <- g12 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y))
  g22 <- g22 + scale_y_continuous(breaks=seq(bottom_y,top_y,by=by_y))
  
  
  
  
  # b <- b + scale_y_continuous(limits=c(-5,5 )) 
  grid::grid.newpage()    
  #Defnie the relative proportions among the panels in the mosaic.
  layout <- grid::grid.layout(nrow=2, ncol=2,
                              widths=grid::unit(c(.5, .5) ,c("null","null")),
                              heights=grid::unit(c(.5, .5), c("null","null"))
  )
  grid::pushViewport(grid::viewport(layout=layout))
  print(g11, vp=grid::viewport(layout.pos.row=1, layout.pos.col=1 ))
  print(g21, vp=grid::viewport(layout.pos.row=2, layout.pos.col=1 ))
  
  print(g12, vp=grid::viewport(layout.pos.row=1, layout.pos.col=2 ))
  print(g22, vp=grid::viewport(layout.pos.row=2, layout.pos.col=2 ))
  
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

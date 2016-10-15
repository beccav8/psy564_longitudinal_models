# model_object = model_0
basic_model_info <- function(model_object, eq=F){
  if(eq){
    cat("Model equation :")
    print(model_object$formula, showEnv = F)
    cat("\n")
  }
  (logLik<-logLik(model_object))
  (dev<-deviance(model_object))
  (AIC <- round(AIC(model_object),1))
  (BIC <- round(BIC(model_object),1))
  
  # (N <- summary(model)$dims$N)  # Number of distinct data points
  # (p <- summary(model)$dims$p)  # Number of estimated parameters
  # ids <- length(coefficients(model)$id[1]) # Number of unique units
  
  # (dfF <- round(model_object$df.residual,0))
  # (dfR <- round(model_object$df.null,0))
  # (dfD <- dfR - dfF)
  # (model_Info <- t(c("logLik"=logLik,"dev"=dev,"AIC"=AIC,"BIC"=BIC, "df_Null"=dfR, "df_Model"=dfF, "df_drop"=dfD)))
  (model_Info <- t(c("logLik"=logLik,"dev"=dev,"AIC"=AIC,"BIC"=BIC )))
  # return(model_Info)
  model_Info <- as.data.frame(model_Info)
  return(model_Info)
  # print(knitr::kable(model_Info))
  # print(model_Info)
}

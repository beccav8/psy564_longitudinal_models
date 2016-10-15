estimate_local_model <- function(data, outcome, predictors){
  
  local_stem <- paste0(outcome," ~ 1 + full_year")  
  ran_eff_spec <- "(1 + full_year |id)" 
  
  eq_formula <- as.formula(paste0(local_stem, " + ", predictors_A, " + ",ran_eff_spec))
  print(eq_formula, showEnv = FALSE)
  model_study_list <- list()
  model <- lme4::lmer(eq_formula, data=ds, REML=TRUE ) 
  return(model)
}

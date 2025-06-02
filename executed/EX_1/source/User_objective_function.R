User_objective_function <- function(obs, sim){

  missing_values <- is.na(obs)
  if(sum(missing_values) > 0){
    obs <- obs[-missing_values]
    sim <- sim[-missing_values]
  }

  Obj_func <- list()

  #############################################################################
  #############################################################################
  # DO NOT CHANGE ANYTHING ABOVE THIS
  #############################################################################
  #############################################################################

  # If user wants to calculte a new objetive function, should added in order to obj_func
  # E.g. user is calculating Obj_1 and Obj_2, should added to obj_func[[1]] & obj_func[[2]]

  #############################################################################
  # This is an example of how to calculate sqrtNSE

  s_obs <- sqrt(obs)
  s_sim <- sqrt(sim)
  s_mObs <- mean(s_obs)
  s_obs_mObs <- s_obs - s_mObs
  s_sim_obs <-  s_sim - s_obs

  sqrt_NSE <- 1 - sum(s_sim_obs**2)/sum(s_obs_mObs**2)

  Obj_func[[1]] <- sqrt_NSE

  return(Obj_func)
}


ci_single_prop_theo <- function(y, success, conf_level = 0.95){
  # calculate sample size
  n <- length(y) 
  
  # calculate p-hat
  p_hat <- sum(y == success) / n
  
  # find percentile associated with critical value
  perc_crit_value <- conf_level + ((1 - conf_level) / 2)
  
  # find critical value
  z_star <- qnorm(perc_crit_value)
  
  # calculate SE
  se <- sqrt(p_hat * (1 - p_hat) / n)
  
  # calculate ME
  me <- z_star * se
  
  # calculate CI
  ci <- p_hat + c(-1, 1) * me
  
  # return
  return(list(p_hat = round(p_hat, 4), SE = round(se, 4), ME = round(me, 4), CI = round(ci, 4)))
}
ci_single_prop_theo <- function(y, x, success, conf_level = 0.95){
  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[1])
  
  # calculate p-hat1 and p-hat2
  p_hat1 <- sum(y[x == levels(x)[1]] == success) / n1
  p_hat2 <- sum(y[x == levels(x)[2]] == success) / n2
  
  # calculate difference in p-hats
  p_hat_diff <- p_hat1 - p_hat2
  
  # find percentile associated with critical value
  perc_crit_value <- conf_level + ((1 - conf_level) / 2)
  
  # find critical value
  z_star <- qnorm(perc_crit_value)
  
  # calculate SE
  se <- sqrt((p_hat1 * (1 - p_hat1) / n1) + (p_hat2 * (1 - p_hat2) / n2))
  
  # calculate ME
  me <- z_star * se
  
  # calculate CI
  ci <- p_hat_diff + c(-1, 1) * me
  
  # return
  return(list(p_hat_diff = round(p_hat_diff, 4), SE = round(se, 4), ME = round(me, 4), CI = round(ci, 4)))
}
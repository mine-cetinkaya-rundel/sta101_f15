ci_two_mean_theo <- function(y, x, conf_level = 0.95){
  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[1])
  
  # calculate x-bar1 and x-bar2
  x_bars <- by(y, x, mean)
  x_bar1 <- as.numeric(x_bars[1])
  x_bar2 <- as.numeric(x_bars[2])
  
  # calculate difference in x-bars
  x_bar_diff <- x_bar1 - x_bar2
  
  # calculate s1 and s2
  sds <- by(y, x, sd)
  s1 <- as.numeric(sds[1])
  s2 <- as.numeric(sds[2])

  # define degrees of freedom
  df <- min(n1 - 1, n2 - 1)
  
  # find percentile associated with critical value
  perc_crit_value <- conf_level + ((1 - conf_level) / 2)
  
  # find critical value
  t_star <- qt(perc_crit_value, df)
  
  # calculate SE
  se <- sqrt((s1^2 / n1) + (s2^2 / n2))
  
  # calculate ME
  me <- t_star * se
  
  # calculate CI
  ci <- obs_diff_in_means + c(-1, 1) * me
  
  # return
  return(list(x_bar_diff = round(x_bar_diff, 4), SE = round(se, 4), ME = round(me, 4), CI = round(ci, 4)))
}
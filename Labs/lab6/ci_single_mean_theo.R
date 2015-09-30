ci_single_mean_theo <- function(y, data = NULL, conf_level = 0.95){
  
  # assign y
  y <- eval(substitute(y), data, parent.frame())

  # calculate sample size
  n <- length(y) 

  # calculate x-bar
  x_bar <- mean(y)

  # define degrees of freedom
  df <- n - 1
  
  # find percentile associated with critical value
  perc_crit_value <- conf_level + ((1 - conf_level) / 2)
  
  # find critical value
  t_star <- qt(perc_crit_value, df)
  
  # calculate SE
  se <- sd(y) / sqrt(n)
  
  # calculate ME
  me <- t_star * se
  
  # calculate CI
  ci <- x_bar + c(-1, 1)* me
  
  # plot
  p <-  ggplot(data = data, aes(x = y),
               environment = environment()) +
    geom_histogram() +
    #xlab(print(r))
  suppressWarnings(print(p))
  
  # return
  return(list(x_bar = round(x_bar, 4), SE = round(se, 4), ME = round(me, 4), CI = round(ci, 4)))
  
}
ci_single_prop_theo <- function(y, success, conf_level, 
                                y_name, show_eda_plot, show_inf_plot){
  
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
  
  # eda_plot
  d_eda <- data.frame(y = y)
  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_bar(fill = "#8FDEE1") +
    xlab(y_name) +
    ylab("") +
    ggtitle("Sample Distribution")
  
  # print plots
  if(show_eda_plot){ print(eda_plot) }
  if(show_inf_plot){ warning("No inference plot available.") }
  
  # return
  return(list(p_hat = round(p_hat, 4), SE = round(se, 4), ME = round(me, 4), CI = round(ci, 4)))
}
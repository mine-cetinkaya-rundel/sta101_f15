ci_single_mean_theo <- function(y, conf_level, 
                                y_name, show_eda_plot, show_inf_plot){

  # calculate sample size
  n <- length(y) 

  # calculate x-bar
  y_bar <- mean(y)

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
  ci <- y_bar + c(-1, 1)* me

  # eda_plot
  d_eda <- data.frame(y = y)
  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_histogram(fill = "#8FDEE1", binwidth = diff(range(y)) / 20) +
    xlab(y_name) +
    ylab("") +
    ggtitle("Sample Distribution") +
    geom_vline(xintercept = y_bar, col = "#1FBEC3", lwd = 1.5)
  
  # print plots
  if(show_eda_plot){ print(eda_plot) }
  if(show_inf_plot){ warning("No inference plot available.") }

  # return
  return(list(y_bar = round(y_bar, 4), df = df, SE = round(se, 4), 
              ME = round(me, 4), CI = round(ci, 4)))
  
}
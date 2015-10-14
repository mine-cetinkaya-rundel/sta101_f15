ci_two_mean_theo <- function(y, x, conf_level, 
                             y_name, show_eda_plot, show_inf_plot){
  
  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[2])
  
  # calculate y-bar1 and y-bar2
  y_bars <- by(y, x, mean)
  y_bar1 <- as.numeric(y_bars[1])
  y_bar2 <- as.numeric(y_bars[2])
  
  # calculate difference in y-bars
  y_bar_diff <- y_bar1 - y_bar2
  
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
  ci <- y_bar_diff + c(-1, 1) * me
  
  # eda_plot
  d_eda <- data.frame(y = y, x = x)
  d_means <- data.frame(y_bars = as.numeric(y_bars), x = levels(x))
  
  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_histogram(fill = "#8FDEE1", binwidth = diff(range(y)) / 20) +
    xlab(y_name) +
    ylab("") +
    ggtitle("Sample Distributions") +
    geom_vline(data = d_means, aes(xintercept = y_bars), col = "#1FBEC3", lwd = 1.5) +
    facet_grid(x ~ .)
    
  
  # print plots
  if(show_eda_plot){ print(eda_plot) }
  if(show_inf_plot){ warning("No inference plot available.") }
  
  # return
  return(list(y_bar_diff = round(y_bar_diff, 4), df = df, SE = round(se, 4), 
              ME = round(me, 4), CI = round(ci, 4)))
}
ht_single_mean_theo <- function(y, null, alternative,
                                y_name, show_eda_plot, show_inf_plot){

  # calculate sample size
  n <- length(y) 

  # calculate x-bar
  y_bar <- mean(y)
  
  # calculate SE
  se <- sd(y) / sqrt(n)
  
  # find critical value
  t <- (y_bar - null) / se
  
  # define degrees of freedom
  df <- n - 1

  # shading cutoffs
  if(alternative == "greater"){ x_min = y_bar; x_max = Inf }
  if(alternative == "less"){ x_min = -Inf; x_max = y_bar }
  if(alternative == "twosided"){
    if(y_bar >= null){
      x_min = c(null - (y_bar - null), y_bar)
      x_max = c(-Inf, Inf)
    }
    if(y_bar <= null){
      x_min = c(y_bar, null + (null - y_bar))
      x_max = c(-Inf, Inf)
    }    
  }
  
  # calculate p-value
  if(alternative == "greater"){ p_value <- pt(t, df, lower.tail = FALSE) }
  if(alternative == "less"){ p_value <- pt(t, df, lower.tail = TRUE) }
  if(alternative == "twosided"){
    p_value <- pt(abs(t), df, lower.tail = FALSE) * 2
  }

  # eda_plot
  d_eda <- data.frame(y = y)
  
  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_histogram(fill = "#8FDEE1", binwidth = diff(range(y)) / 20) +
    xlab(y_name) +
    ylab("") +
    ggtitle("Sample Distribution") +
    geom_vline(xintercept = y_bar, col = "#1FBEC3", lwd = 1.5)
  
  # inf_plot
  inf_plot <- ggplot(data.frame(x = c(null - 4*se, null + 4*se)), aes(x)) + 
    stat_function(fun = dnorm, args = list(mean = null, sd = se), color = "#999999") +
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    ggtitle("Sampling Distribution") +
    xlab("") +
    geom_vline(xintercept = y_bar, color = "#F57670", lwd = 1.5)
  
  # print plots
  if(show_eda_plot & !show_inf_plot){ 
    print(eda_plot)
  }
  if(!show_eda_plot & show_inf_plot){ 
    print(inf_plot)
  }
  if(show_eda_plot & show_inf_plot){
    grid.arrange(eda_plot, inf_plot, ncol = 2)
  }

  # return
  return(list(y_bar = round(y_bar, 4), SE = round(se, 4), 
              t_score = round(t, 4), df = df, p_value = round(p_value, 4)))
  
  print(p_value)
  
}
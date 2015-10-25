ht_two_mean_theo <- function(y, x, null, alternative, 
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
  
  # calculate SE
  se <- sqrt((s1^2 / n1) + (s2^2 / n2))

  # define degrees of freedom
  df <- min(n1 - 1, n2 - 1)
  
  # calculate t
  t <- (y_bar_diff - null) / se
  
  # shading cutoffs
  if(alternative == "greater"){ 
    x_min <- y_bar_diff
    x_max <- Inf 
    }
  if(alternative == "less"){ 
    x_min <- -Inf
    x_max <- y_bar_diff
    }
  if(alternative == "twosided"){
    if(y_bar_diff >= null){
      x_min <- c(null - (y_bar_diff - null), y_bar_diff)
      x_max <- c(-Inf, Inf)
    }
    if(y_bar_diff <= null){
      x_min <- c(y_bar_diff, null + (null - y_bar_diff))
      x_max <- c(-Inf, Inf)
    }    
  }
  
  # calculate p-value
  if(alternative == "greater"){ p_value <- pt(t, df, lower.tail = FALSE) }
  if(alternative == "less"){ p_value <- pt(t, df, lower.tail = TRUE) }
  if(alternative == "twosided"){
    p_value <- pt(abs(t), df, lower.tail = FALSE) * 2
  }
  
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
    

  # inf_plot
  inf_plot <- ggplot(data.frame(x = c(null - 4*se, null + 4*se)), aes(x)) + 
    stat_function(fun = dnorm, args = list(mean = null, sd = se), color = "#999999") +
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    ggtitle("Null Distribution") +
    xlab("") +
    ylab("") +
    geom_vline(xintercept = y_bar_diff, color = "#F57670", lwd = 1.5)
  
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
  return(list(y_bar_diff = round(y_bar_diff, 4), df = df, 
              SE = round(se, 4), t = round(t, 4), p_value = round(p_value, 4)))
}
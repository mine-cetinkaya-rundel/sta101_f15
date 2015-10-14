ht_two_median_sim <- function(y, x, null, alternative, 
                              y_name, x_name, show_eda_plot, show_inf_plot){
  
  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[2])
  
  # calculate y-med1 and y-med2
  y_meds <- by(y, x, median)
  y_med1 <- as.numeric(y_meds[1])
  y_med2 <- as.numeric(y_meds[2])
  
  # calculate difference in y-bars
  y_med_diff <- y_med1 - y_med2

  # create randomization distribution
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    y_sim <- sample(y, size = n, replace = FALSE)
    y_sim_meds <- by(y_sim, x, median)
    y_sim_med1 <- as.numeric(y_sim_meds[1])
    y_sim_med2 <- as.numeric(y_sim_meds[2])
    sim_dist[i] <- y_sim_med1 - y_sim_med2
  }
  
  # shading cutoffs
  if(alternative == "greater"){ 
    x_min <- y_med_diff
    x_max <- Inf 
    }
  if(alternative == "less"){ 
    x_min <- -Inf
    x_max <- y_med_diff
    }
  if(alternative == "twosided"){
    if(y_med_diff >= null){
      x_min <- c(null - (y_med_diff - null), y_med_diff)
      x_max <- c(-Inf, Inf)
    }
    if(y_med_diff <= null){
      x_min <- c(y_med_diff, null + (null - y_med_diff))
      x_max <- c(-Inf, Inf)
    }    
  }
  
  # calculate p-value
  if(alternative == "greater"){ p_value <- sum(sim_dist >= y_med_diff) / nsim }
  if(alternative == "less"){ p_value <- sum(sim_dist <= y_med_diff) / nsim }
  if(alternative == "twosided"){
    p_value <- sum(sim_dist >= y_med_diff) / nsim
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
    ggtitle("Sampling Distribution") +
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
ht_single_mean_sim <- function(y, null, alternative, nsim, seed,
                               y_name, show_eda_plot, show_inf_plot){

  # set seed
  if(!is.null(seed)){ set.seed(seed) }
  
  # calculate sample size
  n <- length(y) 
  
  # calculate y-bar
  y_bar <- mean(y)
  
  # create bootstrap distribution
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    boot_samp <- sample(y, size = n, replace = TRUE)
    sim_dist[i] <- mean(boot_samp)
  }
  
  # center bootstrap distribution at null
  sim_dist_temp <- sim_dist
  sim_dist <- sim_dist_temp - (mean(sim_dist_temp) - null)
  
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
  if(alternative == "greater"){ p_value <- sum(sim_dist >= y_bar) / nsim }
  if(alternative == "less"){ p_value <- sum(sim_dist <= y_bar) / nsim }
  if(alternative == "twosided"){
    if(y_bar >= null){
      p_value <- (sum(sim_dist >= y_bar) + sum(sim_dist <= (null - abs(y_bar - null)))) / nsim
    }
    if(y_bar <= null){
      p_value <- (sum(sim_dist <= y_bar) + sum(sim_dist >= (null + abs(y_bar - null)))) / nsim
    }    
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
  d_inf <- data.frame(sim_dist = sim_dist)
  
  inf_plot <- ggplot(data = d_inf, aes(x = sim_dist), environment = environment()) +
    geom_histogram(fill = "#CCCCCC", binwidth = diff(range(sim_dist)) / 20) +
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    xlab("randomization means") +
    ylab("") +
    ggtitle("Randomization Distribution") +
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
  return(list(y_bar = round(y_bar, 4), p_value = round(p_value, 4)))
  
}
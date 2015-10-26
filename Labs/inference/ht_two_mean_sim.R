ht_two_mean_sim <- function(y, x, null, alternative, nsim, seed,
                              y_name, x_name, show_eda_plot, show_inf_plot){

  # set seed
  if(!is.null(seed)){ set.seed(seed) }
    
  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[2])
  
  # calculate y-bar1 and y-bar2
  y_bars <- by(y, x, mean)
  y_bar1 <- as.numeric(y_meds[1])
  y_bar2 <- as.numeric(y_meds[2])
  
  # calculate difference in y-bars
  y_bar_diff <- y_bar1 - y_bar2

  # create null distribution
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    y_sim <- sample(y, size = n, replace = FALSE)
    y_sim_bars <- by(y_sim, x, mean)
    y_sim_bar1 <- as.numeric(y_sim_bars[1])
    y_sim_bar2 <- as.numeric(y_sim_bars[2])
    sim_dist[i] <- y_sim_bar1 - y_sim_bar2
  }
  
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
  if(alternative == "greater"){ p_value <- sum(sim_dist >= y_bar_diff) / nsim }
  if(alternative == "less"){ p_value <- sum(sim_dist <= y_bar_diff) / nsim }
  if(alternative == "twosided"){
    p_value <- sum(sim_dist >= y_bar_diff) / nsim
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
  d_inf <- data.frame(sim_dist = sim_dist)
  
  inf_plot <- ggplot(data = d_inf, aes(x = sim_dist), environment = environment()) +
    geom_histogram(fill = "#CCCCCC", binwidth = diff(range(sim_dist)) / 20) +
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    xlab("simulated difference in means") +
    ylab("") +
    ggtitle("Null Distribution") +
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
  return(list(y_bar_diff = round(y_bar_diff, 4), p_value = round(p_value, 4)))
}
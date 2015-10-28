ht_two_median_sim <- function(y, x, null, alternative, nsim, seed,
                              y_name, x_name, show_eda_plot, show_inf_plot){

  # set seed
  if(!is.null(seed)){ set.seed(seed) }

  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[2])
  n <- n1 + n2

  # calculate y-med1 and y-med2
  y_meds <- by(y, x, median)
  y_med1 <- as.numeric(y_meds[1])
  y_med2 <- as.numeric(y_meds[2])
  
  # calculate difference in y-meds
  y_med_diff <- y_med1 - y_med2

  # create null distribution
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
    if(p_hat_diff > null){
      p_value <- 2 * (sum(sim_dist >= y_med_diff) / nsim)
    }
    if(p_hat_diff < null){
      p_value <- 2 * (sum(sim_dist <= y_med_diff) / nsim)
    }
  }
  
  # eda_plot
  d_eda <- data.frame(y = y, x = x)

  eda_plot <- ggplot(data = d_eda, aes(x = x, y = y), environment = environment()) +
    geom_boxplot(color = "#1FBEC3", fill = "#8FDEE1", outlier.colour = "#1FBEC3") +
    xlab(x_name) +
    ylab(y_name) +
    ggtitle("Sample Distributions")
    
  # inf_plot
  d_inf <- data.frame(sim_dist = sim_dist)
  
  inf_plot <- ggplot(data = d_inf, aes(x = sim_dist), environment = environment()) +
    geom_histogram(fill = "#CCCCCC", binwidth = diff(range(sim_dist)) / 20) +
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    xlab("simulated difference in medians") +
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
  return(list(y_med_diff = round(y_med_diff, 4), p_value = round(p_value, 4)))
}
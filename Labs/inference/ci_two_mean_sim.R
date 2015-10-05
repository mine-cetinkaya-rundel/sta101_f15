ci_two_mean_sim <- function(y, x, conf_level = 0.95, 
                            boot_method = c("perc", "se"), nsim = 10000, seed = NULL){
  # set seed
  if(!is.null(seed)){set.seed(seed)}
  
  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[1])
  
  # calculate x-bar1 and x-bar2
  x_bars <- by(y, x, mean)
  x_bar1 <- as.numeric(x_bars[1])
  x_bar2 <- as.numeric(x_bars[2])
  
  # calculate difference in x-bars
  x_bar_diff <- x_bar1 - x_bar2
  
  # create bootstrap distribution
  y1 <- y[x == levels(x)[1]]
  y2 <- y[x == levels(x)[2]]
  
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    boot_samp1 <- sample(y1, size = n1, replace = TRUE)
    boot_samp2 <- sample(y2, size = n2, replace = TRUE)
    sim_dist[i] <- mean(boot_samp1) - mean(boot_samp2)
  }
  
  # for percentile method
  if(boot_method == "perc"){
    # calculate quantile cutoffs based on confidence level
    lower_quantile <- (1-conf_level) / 2
    upper_quantile <- conf_level + lower_quantile
    
    # calculate quantiles of the bootstrap distribution
    ci_lower <- as.numeric(quantile(sim_dist, lower_quantile))
    ci_upper <- as.numeric(quantile(sim_dist, upper_quantile))
    
    # put CI together
    ci <- c(ci_lower, ci_upper)
    
    # return
    return(list(x_bar_diff = round(x_bar_diff, 4), CI = round(ci, 4)))
  }
  
  # for standard error method
  if(boot_method == "se"){
    # define degrees of freedom
    df <- min(n1 - 1, n2 - 1)
    
    # find percentile associated with critical value
    perc_crit_value <- conf_level + ((1 - conf_level) / 2)
    
    # find critical value
    t_star <- qt(perc_crit_value, df)
    
    # calculate SE
    se <- sd(sim_dist)
    
    # calculate ME
    me <- t_star * se
    
    # calculate CI
    ci <- x_bar_diff + c(-1, 1) * me
    
    # return
    return(list(x_bar_diff = round(x_bar_diff, 4), SE = round(se, 4), ME = round(me, 4), CI = round(ci, 4)))
  }  
}
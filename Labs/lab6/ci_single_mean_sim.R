ci_single_mean_sim <- function(y, conf_level = 0.95, 
                               boot_method = c("perc", "se"), nsim = 10000, seed = NULL){
  # set seed
  if(!is.null(seed)){set.seed(seed)}
  
  # calculate sample size
  n <- length(y) 
  
  # calculate x-bar
  x_bar <- mean(y)
  
  # create bootstrap distribution
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    boot_samp <- sample(y, size = n, replace = TRUE)
    sim_dist[i] <- mean(boot_samp)
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
    return(list(x_bar = round(x_bar, 4), CI = round(ci, 4)))
  }
  
  # for standard error method
  if(boot_method == "se"){
    # define degrees of freedom
    df <- n - 1
    
    # find percentile associated with critical value
    perc_crit_value <- conf_level + ((1 - conf_level) / 2)
    
    # find critical value
    t_star <- qt(perc_crit_value, df)
    
    # calculate SE
    se <- sd(sim_dist)
    
    # calculate ME
    me <- t_star * se
    
    # calculate CI
    ci <- x_bar + c(-1, 1)* me
    
    # return
    return(list(x_bar = round(x_bar, 4), SE = round(se, 4), ME = round(me, 4), CI = round(ci, 4)))
  }  
}
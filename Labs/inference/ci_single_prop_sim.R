ci_single_prop_sim <- function(y, success, conf_level = 0.95,
                               boot_method = c("perc", "se"), nsim = 10000, seed = NULL){
  # set seed
  if(!is.null(seed)){set.seed(seed)}
  
  # calculate sample size
  n <- length(y) 
  
  # calculate p_hat
  p_hat <- sum(y == success) / n
  
  # create bootstrap distribution
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    boot_samp <- sample(y, size = n, replace = TRUE)
    sim_dist[i] <- sum(boot_samp == success) / n
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
    return(list(p_hat = round(p_hat, 4), CI = round(ci, 4)))
  }
  
  # for standard error method
  if(boot_method == "se"){
    
    # find percentile associated with critical value
    perc_crit_value <- conf_level + ((1 - conf_level) / 2)
    
    # find critical value
    z_star <- qnorm(perc_crit_value)
    
    # calculate SE
    se <- sd(sim_dist)
    
    # calculate ME
    me <- z_star * se
    
    # calculate CI
    ci <- p_hat + c(-1, 1) * me
    
    # return
    return(list(p_hat = round(p_hat, 4), SE = round(se, 4), ME = round(me, 4), CI = round(ci, 4)))
  }  
}
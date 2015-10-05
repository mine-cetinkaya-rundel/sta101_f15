ci_single_prop_sim <- function(y, x, success, conf_level = 0.95,
                               boot_method = c("perc", "se"), nsim = 10000, seed = NULL){
  # set seed
  if(!is.null(seed)){set.seed(seed)}
  
  # calculate n1 and n2
  ns <- by(y, x, length)
  n1 <- as.numeric(ns[1])
  n2 <- as.numeric(ns[1]) 
  
  # calculate p-hat1 and p-hat2
  p_hat1 <- sum(y[x == levels(x)[1]] == success) / n1
  p_hat2 <- sum(y[x == levels(x)[2]] == success) / n2
  
  # calculate difference in p-hats
  p_hat_diff <- p_hat1 - p_hat2
  
  # create bootstrap distribution
  y1 <- y[x == levels(x)[1]]
  y2 <- y[x == levels(x)[2]]
  
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    boot_samp1 <- sample(y1, size = n1, replace = TRUE)
    boot_samp2 <- sample(y2, size = n2, replace = TRUE)
    boot_phat1 <- sum(boot_samp1 == success) / n1
    boot_phat2 <- sum(boot_samp2 == success) / n2
    sim_dist[i] <- boot_phat1 - boot_phat2
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
    ci <- p_hat_diff + c(-1, 1) * me
    
    # return
    return(list(p_hat_diff = round(p_hat_diff, 4), SE = round(se, 4), ME = round(me, 4), CI = round(ci, 4)))
  }  
}
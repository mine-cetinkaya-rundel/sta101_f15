ht_single_prop_sim <- function(y, success, null, alternative, seed,
                                y_name, show_eda_plot, show_inf_plot, nsim){
  
  # set seed
  if(!is.null(seed)){ set.seed(seed) }

  # calculate sample size
  n <- length(y) 
  
  # calculate p-hat
  p_hat <- sum(y == success) / n
  
  # create null distribution
  sim_dist <- rep(NA, nsim)
  for(i in 1:nsim){
    sim_samp <- sample(c(TRUE, FALSE), size = n, replace = TRUE, prob = c(null, 1 - null))
    sim_dist[i] <- sum(sim_samp) / n
  }
  
  # shading cutoffs
  if(alternative == "greater"){ x_min = p_hat; x_max = Inf }
  if(alternative == "less"){ x_min = -Inf; x_max = p_hat }
  if(alternative == "twosided"){
    if(p_hat >= null){
      x_min = c(null - (p_hat - null), p_hat)
      x_max = c(-Inf, Inf)
    }
    if(p_hat <= null){
      x_min = c(p_hat, null + (null - p_hat))
      x_max = c(-Inf, Inf)
    }    
  }

  # calculate p-value
  if(alternative == "greater"){ p_value <- sum(sim_dist >= p_hat) / nsim }
  if(alternative == "less"){ p_value <- sum(sim_dist <= p_hat) / nsim }
  if(alternative == "twosided"){
    if(p_hat >= null){
      p_value <- (sum(sim_dist >= p_hat) + sum(sim_dist <= (null - abs(p_hat - null)))) / nsim
    }
    if(p_hat <= null){
      p_value <- (sum(sim_dist <= p_hat) + sum(sim_dist >= (null + abs(p_hat - null)))) / nsim
    }    
  }
  
  # eda_plot
  d_eda <- data.frame(y = y)

  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_bar(fill = "#8FDEE1") +
    xlab(y_name) +
    ylab("") +
    ggtitle("Sample Distribution")

  # inf_plot
  d_inf <- data.frame(sim_dist = sim_dist)
  
  inf_plot <- ggplot(data = d_inf, aes(x = sim_dist), environment = environment()) +
    geom_histogram(fill = "#CCCCCC", binwidth = diff(range(sim_dist)) / 20) +
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    xlab("simulated proportions") +
    ylab("") +
    ggtitle("Null Distribution") +
    geom_vline(xintercept = p_hat, color = "#F57670", lwd = 1.5)
  
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
  return(list(p_hat = round(p_hat, 4), p_value = round(p_value, 4))) 
}
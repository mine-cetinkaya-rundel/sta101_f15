inference_ci <- function(y, x = NULL, data,
                         statistic = c("mean", "median", "proportion"), 
                         success = NULL, order = NULL, 
                         method = c("theoretical","simulation"),
                         boot_method = c("perc", "se"),
                         conf_level = 0.95, 
                         nsim = 10000, seed = NULL,
                         show_sum_stats = TRUE, show_eda_plot = TRUE, show_inf_plot = TRUE, 
                         show_inf_lines = TRUE, return_sim_dist = FALSE){
  
  # single
  if(is.null(x)){
    
    # single mean
    if(statistic == "mean"){
      if(method == "theoretical"){ 
        source("ci_single_mean_theo.R")
        ci_single_mean_theo(y, conf_level)
      }
      if(method == "simulation"){ 
        source("ci_single_mean_sim.R")
        ci_single_mean_sim(y, conf_level, boot_method, nsim, seed)
      }      
    }
    
    # single median
    if(statistic == "median"){
      if(method == "theoretical"){ 
        stop("Use simulation methods for inference for the median.", call. = FALSE)
      }
      if(method == "simulation"){ 
        source("ci_single_median_sim.R")
        ci_single_mean_sim(y, conf_level, boot_method, nsim, seed)
      }
    }
    
    # single proportion
    if(statistic == "proportion"){
      if(method == "theoretical"){ 
        source("ci_single_prop_theo.R")
        ci_single_mean_theo(y, conf_level, success) 
      }
      if(method == "simulation"){ 
        source("ci_single_prop_sim.R")
        ci_single_prop_sim(y, conf_level, success, boot_method, nsim, seed)
      }
    }
  }
  
  # compare two
  if(!is.null(x)){
    
    # assign x and y
    x <- eval(substitute(x), data, parent.frame())
    y <- eval(substitute(y), data, parent.frame())
    
    # fix order, if needed
    if(order[1] != levels(x)[1]){
      x <- relevel(x, ref = levels(x)[2])
    }
    
    # compare two means
    if(statistic == "mean"){
      if(method == "theoretical"){ 
        source("ci_two_mean_theo.R")
        ci_two_mean_theo(y, x, conf_level) 
      }
      if(method == "simulation"){ 
        source("ci_two_mean_sim.R")
        ci_two_mean_sim(y, x, conf_level, boot_method, nsim, seed)
      }  
    }
    
    # compare two medians
    if(statistic == "median"){
      if(method == "theoretical"){ 
        stop("Use simulation methods for inference for the median.", call. = FALSE)
      }
      if(method == "simulation"){ 
        source("ci_two_median_sim.R")
        ci_two_median_sim(y, x, conf_level, boot_method, nsim, seed)
      }  
    }
    
    # compare two proportions
    if(statistic == "proportion"){
      if(method == "theoretical"){ 
        source("ci_two_prop_theo.R")
        ci_two_prop_theo(y, x, conf_level) 
      }
      if(method == "simulation"){ 
        source("ci_two_prop_sim.R")
        ci_two_prop_sim(y, x, conf_level, boot_method, nsim, seed)
      }  
    }
  }
  
}
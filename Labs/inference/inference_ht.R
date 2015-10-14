inference_ht <- function(y, x = NULL, data,
                         statistic = c("mean", "median", "proportion"), 
                         success = NULL, order = NULL, 
                         method = c("theoretical", "simulation"),
                         null, alternative = c("less","greater","twosided"),
                         sig_level = 0.05, 
                         nsim = 10000, seed = NULL,
                         show_sum_stats = TRUE, return_sim_dist = FALSE,
                         show_eda_plot = TRUE, show_inf_plot = TRUE){
  
  # source helper functions
  source("ht_single_mean_theo.R")
  source("ht_single_mean_sim.R")
  source("ht_single_median_sim.R")
  #source("ht_single_prop_theo.R")
  #source("ht_single_prop_sim.R")
  source("ht_two_mean_theo.R")
  #source("ht_two_mean_sim.R")
  #source("ht_two_median_sim.R")
  #source("ht_two_prop_theo.R")
  #source("ht_two_prop_sim.R")  

    # single
    if(is.null(x)){
    
    # remova NAs
    y <- y[!is.na(y)]
    
    # single mean
    if(statistic == "mean"){
      if(method == "theoretical"){ 
        res <- ht_single_mean_theo(y, null, alternative,
                                   y_name, show_eda_plot, show_inf_plot)
        return(list(y_bar = res$y_bar, SE = res$SE, t_score = res$t_score, 
                    df = res$df, p_value = res$p_value))
      }
      if(method == "simulation"){ 
        res <- ht_single_mean_sim(y, null, alternative, nsim, seed, 
                                  y_name, show_eda_plot, show_inf_plot)
        return(list(sample_statistic = res$y_bar, p_value = res$p_value))
      }      
    }
    
    # single median
    if(statistic == "median"){
      if(method == "theoretical"){ 
        stop("Use simulation methods for inference for the median.", call. = FALSE)
      }
      if(method == "simulation"){ 
        res <- ht_single_median_sim(y, null, alternative, nsim, seed,
                                    y_name, show_eda_plot, show_inf_plot)
        return(list(sample_statistic = res$y_med, p_value = res$p_value))
      }      
    }
    
    # single proportion
  #  if(statistic == "proportion"){
  #    if(method == "theoretical"){ 
  #      res <- ht_single_prop_theo(y, success, conf_level,
  #                                 y_name, show_eda_plot, show_inf_plot = FALSE) 
  #      return(list(p_hat = res$p_hat, SE = res$SE, 
  #                  ME = res$ME, CI = res$CI))
  #      
  #    }
  #    if(method == "simulation"){ 
  #      res <- ht_single_prop_sim(y, success, conf_level, boot_method, nsim, seed, 
  #                                y_name, show_eda_plot, show_inf_plot)
  #      if(boot_method == "perc"){
  #        return(list(p_hat = res$p_hat, CI = res$CI))
  #      } else {
  #        return(list(p_hat = res$p_hat, SE = res$SE, ME = res$ME, CI = res$CI))
  #      }
  #    }
  #  }
  }
  #
  #
  # compare two
  if(!is.null(x)){
  
    # remove NAs
    d <- na.omit(data.frame(y = y, x = x))
    x <- d$x
    y <- d$y

    # fix order, if needed
    if(!is.null(order)){
      if(order[1] != levels(x)[1]){
        x <- relevel(x, ref = levels(x)[2])
      }
    }
    
    # compare two means
    if(statistic == "mean"){
      
      if(method == "theoretical"){ 
        res <- ht_two_mean_theo(y, x, null, alternative, 
                                y_name, show_eda_plot, show_inf_plot)
        return(list(sample_statistic = res$y_bar_diff, df = res$df, SE = res$SE, 
                    t = res$t, p_value = res$p_value))
      }
  #    if(method == "simulation"){ 
  #      res <- ht_two_mean_sim(y, x, conf_level, boot_method, nsim, seed, 
  #                      y_name, show_eda_plot, show_inf_plot)
  #      if(boot_method == "perc"){
  #        return(list(y_bar_diff = res$y_bar_diff, CI = res$CI))
  #      } else {
  #        return(list(y_bar_diff = res$y_bar_diff, SE = res$SE, ME = res$ME, CI = res$CI))
  #      }
  #    }  
    }
  #  
  #  # compare two medians
  #  if(statistic == "median"){
  #    if(method == "theoretical"){ 
  #      stop("Use simulation methods for inference for the median.", call. = FALSE)
  #    }
  #    if(method == "simulation"){ 
  #      res <- ht_two_median_sim(y, x, conf_level, boot_method, nsim, seed, 
  #                                  y_name, show_eda_plot, show_inf_plot)
  #      if(boot_method == "perc"){
  #        return(list(y_med_diff = res$y_med_diff, CI = res$CI))
  #      } else {
  #        return(list(y_med_diff = res$y_med_diff, SE = res$SE, ME = res$ME, CI = res$CI))
  #      }
  #    }      
  #  }
  #  
  #  # compare two proportions
  #  if(statistic == "proportion"){
  #    if(method == "theoretical"){ 
  #      res <- ht_two_prop_theo(y, x, success, conf_level,
  #                              x_name, y_name, show_eda_plot, show_inf_plot = FALSE) 
  #      return(list(p_hat_diff = res$p_hat_diff, SE = res$SE, ME = res$ME, CI = res$CI))
  #    }
  #    if(method == "simulation"){ 
  #      res <- ht_two_prop_sim(y, x, success, conf_level, boot_method, nsim, seed, 
  #                             x_name, y_name, show_eda_plot, show_inf_plot)
  #      if(boot_method == "perc"){
  #        return(list(p_hat_diff = res$p_hat_diff, CI = res$CI))
  #      } else {
  #        return(list(p_hat_diff = res$p_hat_diff, SE = res$SE, ME = res$ME, CI = res$CI))
  #      }
  #    }  
  #  }
  }
}
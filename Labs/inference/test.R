inference_ci(y = weight, data = nc,
                         statistic = "mean", 
                         success = NULL, order = NULL, 
                         method = "theoretical",
                         conf_level = 0.95, 
                         show_sum_stats = TRUE, show_eda_plot = TRUE, show_inf_plot = TRUE, 
                         show_inf_lines = TRUE, return_sim_dist = FALSE)
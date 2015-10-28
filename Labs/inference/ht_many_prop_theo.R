ht_many_prop_theo <- function(y, x,
                              x_name, y_name, show_eda_plot, show_inf_plot){
  
  # chi-sq test of independence
  res <- chisq.test(x, y, correct = FALSE)
  stat <- res$statistic
  deg_fr <- res$parameter

  # print summary
  cat("Observed:\n")
  print(res$observed) 
  cat("\n")
  cat("Expected:\n")
  print(res$expected)
  cat("\n")
  
  # eda_plot
  d_eda <- data.frame(y = y, x = x)
  
  n_fill_values <- length(levels(y))
  fill_values <- colorRampPalette(c("#1FBEC3", "#C7EEF0"))( n_fill_values )

  eda_plot <- ggplot(data = d_eda, aes(x = x, fill = y), environment = environment()) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = fill_values) +
    xlab(x_name) +
    ylab("") +
    ggtitle("Sample Distribution") +
    guides(fill = guide_legend(title = y_name))
  
  # inf_plot
  x_max <- max(qchisq(0.99, df = deg_fr), stat*1.1)
  inf_plot <- ggplot(data.frame(x = c(0, x_max)), aes(x)) +
    stat_function(fun = dchisq, args = list(df = deg_fr), color = "#999999") +
    annotate("rect", xmin = stat, xmax = stat+Inf, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    ggtitle(paste0("Chi-sq Distribution\n(df = ", deg_fr, ")")) +
    xlab("") +
    ylab("") +
    geom_vline(xintercept = stat, color = "#F57670", lwd = 1.5)
  
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
  return(list(chisq = as.numeric(stat), df = as.numeric(deg_fr), p_value = res$p.value))
}
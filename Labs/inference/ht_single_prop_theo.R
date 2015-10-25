ht_single_prop_theo <- function(y, success, null, alternative, 
                                y_name, show_eda_plot, show_inf_plot){
  
  # calculate sample size
  n <- length(y) 
  
  # calculate p-hat
  p_hat <- sum(y == success) / n
  
  # calculate SE
  se <- sqrt(p_hat * (1 - p_hat) / n)
  
  # calculate test statistic
  z <- (p_hat - null) / se
  
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
  if(alternative == "greater"){ p_value <- pnorm(z, lower.tail = FALSE) }
  if(alternative == "less"){ p_value <- pnorm(z, lower.tail = TRUE) }
  if(alternative == "twosided"){
    p_value <- pnorm(abs(z), lower.tail = FALSE) * 2
  }
  
  # eda_plot
  d_eda <- data.frame(y = y)

  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_bar(fill = "#8FDEE1") +
    xlab(y_name) +
    ylab("") +
    ggtitle("Sample Distribution")

  # inf_plot
  inf_plot <- ggplot(data.frame(x = c(null - 4*se, null + 4*se)), aes(x)) + 
    stat_function(fun = dnorm, args = list(mean = null, sd = se), color = "#999999") +
    annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    ggtitle("Null Distribution") +
    xlab("") +
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
  return(list(p_hat = round(p_hat, 4), SE = round(se, 4), 
              z_score = round(z, 4), p_value = round(p_value, 4))) 
}
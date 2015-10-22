ht_many_mean_theo <- function(y, x, sig_level,
                              x_name, y_name, show_eda_plot, show_inf_plot){
  # summary stats
  ns <- by(y, x, length)
  y_bars <- by(y, x, mean)
  sds <- by(y, x, sd)
  
  # anova
  res <- anova(lm(y ~ x))
  
  # anova pieces
  terms <- c(x_name, "Residuals", "Total")
  dfs <- res$Df
  ss <- res$`Sum Sq`
  ms <- res$`Mean Sq`
  stat <- res$`F value`[1]
  pval <- res$`Pr(>F)`[1]
  
  # calculate totals
  ss_tot <- sum(ss)
  ss <- c(ss, ss_tot)
  df_tot <- sum(dfs)
  dfs <- c(dfs, df_tot)
  
  # ss format
  ss_format <- if(max(ss) < 100000){
    as.character(ss)
  } else{
    format(ss, scientific = TRUE, digits = 4)
  }
  
  # ms format
  ms_format <- if(max(ss) < 100000){
    as.character(c(ms, NA))
  } else{
    c(format(ms, scientific = TRUE, digits = 4), NA)
  }
  
  # stat format
  stat_format <- if(stat < 100000){
    as.character(c(round(stat, 4), NA, NA))
  } else{
    c(format(stat, scientific = FALSE, digits = 4), NA, NA)
  }
  
  # p-value format
  pval_format <- if(pval > 0.0001){
    as.character(c(round(pval, 4), NA, NA))
  } else{
    c(format(pval, scientific = TRUE, digits = 4), NA, NA)
  }
  
  # format output
  anova_output <- data.frame(
    df = dfs,
    Sum_Sq = ss_format,
    Mean_Sq = ms_format,
    F = stat_format,
    p_value = pval_format, 
    row.names = terms
  )

  # return
  cat("ANOVA:\n")
  print(anova_output, na.print = "")
  
  # post-hoc tests (if ANOVA is significant)
  if(pval < sig_level){
    cat("\nPairwise tests: ")
    pairwise <- pairwise.t.test(y, x, p.adj = "none", pool.sd = TRUE)
    cat(pairwise$method, "\n")
    print(tidy(pairwise), digits = 4)
  }
  
  # eda_plot
  d_eda <- data.frame(y = y, x = x)
  d_means <- data.frame(y_bars = as.numeric(y_bars), x = levels(x))
  
  eda_plot <- ggplot(data = d_eda, aes(x = y), environment = environment()) +
    geom_histogram(fill = "#8FDEE1", binwidth = diff(range(y)) / 20) +
    xlab(y_name) +
    ylab("") +
    ggtitle("Sample Distributions") +
    geom_vline(data = d_means, aes(xintercept = y_bars), col = "#1FBEC3", lwd = 1.5) +
    facet_grid(x ~ .)
  
  # inf_plot
  inf_plot <- ggplot(data.frame(x = c(0, stat * 1.2)), aes(x)) +
    stat_function(fun = df, args = list(df1 = dfs[1], df2 = dfs[2]), color = "#999999") +
    annotate("rect", xmin = stat, xmax = stat+Inf, ymin = 0, ymax = Inf, 
             alpha = 0.3, fill = "#FABAB8") +
    ggtitle(paste0("F Distribution\n(df_G = ", dfs[1], ", df_E = ", dfs[2], ")")) +
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
}
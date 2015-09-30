one_var_inference <- function(y, data,
                              y_type, 
                      statistic = c("mean", "median", "proportion"), 
                      success = NULL, order = NULL, 
                      conf_level = 0.95, sig_level = 0.05,
                      null_value = NULL, 
                      alternative = c("less","greater","twosided"), 
                      type = c("ci", "ht"), method = c("theoretical", "simulation"),
                      sim_dist = FALSE, nsim = 10000, seed = NULL,
                      sum_stats = TRUE, eda_plot = TRUE, inf_plot = TRUE, inf_lines = TRUE){
    
    # print what's going on 
    if(y_type == "numerical"){cat("Single", est, "\n")}
    if(y_type == "categorical"){cat("Single", est, "-- success:", success, "\n")}    
    
    # set statistic: mean, median, or proportion
    if (y_type == "numerical") {statistic = match.fun(est)}
    if (y_type == "categorical") {statistic = function(x) {sum(x == success)/length(x)}}
    
    actual = statistic(y)
    
    cat("Summary statistics: ")
    if(est == "mean"){
      if(eda_plot == TRUE){hist(y, main = "", cex.main = 0.75, xlab = y_name, col = COL[3,4], ylab = "")}
      cat(paste("mean =", round(actual,4), "; ","sd =", round(sd(y), 4), "; ", "n =", n), "\n")
    }
    if(est == "median"){
      if(eda_plot == TRUE){
        boxplot(y, main = "", main = "", xlab = y_name, col = COL[3,4], axes = FALSE)
        axis(2)
      }
      cat(paste("median =", round(actual,4), "; ", "n =", n), "\n")
    }
    if(est == "proportion"){
      if(eda_plot == TRUE){barplot(table(y), main = "", xlab = y_name, col = COL[3,4])}
      cat(paste("p_hat =", round(actual,4), "; ", "n =", n), "\n")
    }
    
    # simulation
    if (method == "simulation") {
      sim = matrix(NA, nrow = n, ncol = nsim)
      
      # bootstrap ci
      if (type == "ci") {
        for(i in 1:nsim) {sim[,i] = sample(y, n, replace = TRUE)}
        if (y_type == "categorical") {
          statistic = function(x) {
            which_success = which(levels(y) == success)
            sum(x == which_success)/length(x)
          }
        }				
        sim_dist = apply(sim, 2, statistic)
        
        ci = quantile(sim_dist, c( (1 - conf_level)/2 , ((1 - conf_level)/2)+conf_level ))
        
        if(inf_plot == TRUE){
          #if(y_type == "categorical"){xlim = c(min(sim_dist)-sd(sim_dist), max(sim_dist)+sd(sim_dist))}
          #if(y_type == "numerical"){xlim = c(min(sim_dist)*0.8, max(sim_dist)*1.2)}
          xlim = c(min(sim_dist)-0.8*sd(sim_dist), max(sim_dist)+0.8*sd(sim_dist))
          
          if (nsim > 500) {            
            counts = hist(sim_dist, plot = FALSE)$counts
            hist(sim_dist, xlab = "Bootstrap distribution", main = "", ylab = "", xlim = xlim, col = COL[1,2])
            if (inf_lines == TRUE) {
              for (i in 1:2) {
                segments(ci[i], 0, ci[i], 0.8 * max(counts), col=COL[4], lwd=2)
                text(round(ci[i],4), max(counts), pos=1, col=COL[4], round(ci[i],4))
              }
            }
          }
          if (nsim <= 500) {
            BHH2::dotPlot(sim_dist, xlim = xlim, pch=19, col=COL[1,2], cex = 0.8, axes = FALSE, xlab="Bootstrap distribution")
            axis(1)
            if(inf_lines == TRUE){
              for (i in 1:2) {
                segments(ci[i], 0, ci[i], 0.6, col=COL[4], lwd=2)
                text(round(ci[i],4), 0.7, pos=1, col=COL[4], round(ci[i],4))
              }
            }
          }          
        }
        
        cat(c(conf_level*100, "% Bootstrap interval = (", round(ci[1],4), ",", round(ci[2],4), ")\n"))		
      }
      
      # randomization test
      if (type == "ht") {
        if (y_type == "numerical") {
          for(i in 1:nsim) {sim[,i] = sample(y, n, replace = TRUE)}
          sim_dist_temp = apply(sim, 2, statistic)
          if (est == "mean") {
            # hypotheses
            cat(paste("H0: mu =", null, "\n"))
            cat(paste("HA: mu", sign, null, "\n"))
            sim_dist = sim_dist_temp - (mean(sim_dist_temp) - null)
          }
          
          if (est == "median") {
            cat(paste("H0: median =", null, "\n"))
            cat(paste("HA: median", sign, null, "\n"))
            sim_dist = sim_dist_temp - (median(sim_dist_temp) - null)
          }					
        }
        if (y_type == "categorical") {
          cat(paste("H0: p =", null, "\n"))
          cat(paste("HA: p", sign, null, "\n"))
          sim_dist = rbinom(nsim, n, prob = null) / n
        }
        
        smaller_tail = round(min(c(mean(sim_dist <= actual), mean(sim_dist >= actual))), 4)	
        
        if(inf_plot == TRUE){
          #if(y_type == "categorical"){xlim = c(min(sim_dist)-sd(sim_dist), max(sim_dist)+sd(sim_dist))}
          #if(y_type == "numerical"){xlim = c(min(sim_dist)*0.8, max(sim_dist)*1.2)}
          xlim = c(min(sim_dist)-0.8*sd(sim_dist), max(sim_dist)+0.8*sd(sim_dist))
          
          if (nsim > 500) {
            counts = hist(sim_dist, plot = FALSE)$counts  
            hist(sim_dist, xlab = "Randomization distribution", main = "", ylab = "", xlim = xlim, col = COL[1,2])
          }
          if (nsim <= 500) {
            BHH2::dotPlot(sim_dist, xlim = xlim, pch=19, col=COL[1,2], cex = 0.8, axes = FALSE, xlab="Randomization distribution")
            axis(1)
          }
        }          
        
        
        #alternative = match.arg(alternative)
        
        if (alternative == "less") {
          if (actual < null) {cat(paste("p-value = ", smaller_tail,"\n"))}  			
          if (actual > null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}
          if (inf_lines == TRUE) {
            if(nsim > 500) {lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col=COL[4], lwd=2)}
            if(nsim <= 500) {lines(x = c(actual,actual), y = c(0,0.8), col=COL[4], lwd=2)}
          }
        }
        if (alternative == "greater") {
          if (actual < null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}				
          if (actual > null) {cat(paste("p-value = ", smaller_tail,"\n"))}
          if (inf_lines == TRUE) {
            if(nsim > 500) {lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col=COL[4], lwd=2)}
            if(nsim <= 500) {lines(x = c(actual,actual), y = c(0,0.8), col=COL[4], lwd=2)}
          }
        }
        if (alternative == "twosided") {
          cat(paste("p-value = ", smaller_tail * 2,"\n"))
          lines(x = c(actual,actual), y = c(0,1.1*max(counts)), col=COL[4], lwd=2)
          if (actual >= null) {
            temp = actual - null
            if (inf_lines == TRUE) {
              if(nsim > 500) {lines(x = c(null - temp,null - temp), y = c(0,1.1*max(counts)), col=COL[4], lwd=2)}
              if(nsim <= 500) {lines(x = c(null - temp,null - temp), y = c(0,0.8), col=COL[4], lwd=2)}
            }						
          }
          if (actual < null) {
            temp = null - actual
            if (inf_lines == TRUE) {
              if(nsim > 500) {lines(x = c(null + temp,null + temp), y = c(0,1.1*max(counts)), col=COL[4], lwd=2)}
              if(nsim <= 500) {lines(x = c(null + temp,null + temp), y = c(0,0.8), col=COL[4], lwd=2)}
            }						
          }		
        }
        if (inf_lines == TRUE) {
          if(nsim > 500) {text(x = actual, y = 1.2*max(counts), paste("observed\n", round(actual,4)), col=COL[4], cex = 0.8)}
          if(nsim <= 500) {text(x = actual, y = 0.9, paste("observed\n", round(actual,4)), col=COL[4], cex = 0.8)}          
        }	
      }
      
    }		
    
    
    # theoretical
    if (method == "theoretical") {
      
      # confidence interval
      if (type == "ci") {
        if (y_type == "numerical") {
          if (est == "median") {stop("Use simulation methods for inference for the median.", call. = FALSE)}
          if (est == "mean") {
            # calculate me and se
            se = sd(y) / sqrt(n)
            cat(paste("Standard error =", round(se, 4), "\n"))
            if (n >= 30) {critvalue = qnorm( (1 - conf_level)/2 + conf_level )}
            if (n < 30) {critvalue = qt( (1 - conf_level)/2 + conf_level , df = n - 1)}					
          }
        }
        if (y_type == "categorical") {
          # check conditions
          suc = round(n * actual, 2)
          fail = round(n * (1 - actual), 2)
          cat(paste("Check conditions: number of successes =", round(suc), ";", "number of failures =", round(fail)), "\n")	
          if (suc < 10 | fail < 10) {
            stop("There aren't at least 10 successes and 10 failures, use simulation methods instead.", call. = FALSE)
          }
          # calculate me and se
          se = sqrt(actual * (1-actual) / n)
          cat(paste("Standard error =", round(se, 4), "\n"))
          critvalue = qnorm( (1 - conf_level)/2 + conf_level )					
        }
        me = critvalue * se
        ci = c(actual - me , actual + me)
        cat(c(conf_level*100, "% Confidence interval = (", round(ci[1],4), ",", round(ci[2],4), ")\n"))	
      }
      
      # hypothesis test
      if (type == "ht") {
        if (y_type == "numerical") {
          if (est == "median") {stop("Use simulation methods for inference for the median.", call. = FALSE)}
          if (est == "mean") {
            # hypotheses
            cat(paste("H0: mu =", null, "\n"))
            cat(paste("HA: mu", sign, null, "\n"))
            
            # calculate test statistic and p-value component
            se = sd(y) / sqrt(n)
            cat("Standard error =", round(se,4), "\n")
            teststat = (actual - null)/se
            if (n >= 30) {
              cat(paste("Test statistic: Z =", round(teststat, 3),"\n"))
              smaller_tail = round(min(pnorm(teststat), pnorm(teststat, lower.tail = FALSE)), 4)
            }
            if (n < 30) {
              cat(paste("Test statistic: T =", round(teststat, 3),"\n"))
              cat(paste("Degrees of freedom: ", n - 1, "\n"))
              smaller_tail = round(min(pt(teststat, df = n - 1), pt(teststat, df = n - 1, lower.tail = FALSE)), 4)
            }		
          }
        }
        if (y_type == "categorical") {
          if (null < 0 | null > 1) {
            stop("Null value should be a proportion between 0 and 1.", call. = FALSE)
          }
          # hypotheses
          cat(paste("H0: p =", null, "\n"))
          cat(paste("HA: p", sign, null, "\n"))
          
          # check conditions
          exp_suc = round(n * null, 2)
          exp_fail = round(n * (1 - null), 2)
          cat(paste("Check conditions: number of expected successes =", round(exp_suc), ";", "number of expected failures =", round(exp_fail)), "\n")
          if (exp_suc < 10 | exp_fail < 10) {
            stop("There aren't at least 10 expected successes and 10 expected failures, use simulation methods instead.", call. = FALSE)
          }
          # calculate test statistic and p-value
          se = sqrt(null * (1 - null) / n)
          cat("Standard error =", round(se,4), "\n")
          teststat = (actual - null)/se
          cat(paste("Test statistic: Z = ", round(teststat, 3),"\n"))
          smaller_tail = round(min(pnorm(teststat), pnorm(teststat, lower.tail = FALSE)), 4)					
        }
        
        # alternative = less
        if (alternative == "less") {
          if (actual < null) {cat(paste("p-value = ", smaller_tail,"\n"))}				
          if (actual > null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}
          normTail(L = teststat, axes = FALSE, col = COL[1,2])
          axis(1, at = c(-3, teststat, 0, 3), labels = c(NA, paste(round(actual,2)), paste(null), NA))
        }
        
        # alternative = greater
        if (alternative == "greater") {
          if (actual < null) {cat(paste("p-value = ", 1 - smaller_tail,"\n"))}				
          if (actual > null) {cat(paste("p-value = ", smaller_tail,"\n"))}
          if(inf_plot == TRUE){
            normTail(U = teststat, axes = FALSE, col = COL[1,2])
            axis(1, at = c(-3, 0, teststat, 3), labels = c(NA, paste(null), paste(round(actual,2)), NA))
          }
        }
        
        # alternative = twosided	
        if (alternative == "twosided") {
          cat(paste("p-value = ", smaller_tail * 2,"\n"))
          if (inf_lot == TRUE){
            if (actual < null) {
              normTail(L = teststat, U = -1*teststat, axes = FALSE, col = COL[1,2])
              axis(1, at = c(-3, teststat, 0, -1*teststat, 3), labels = c(NA, paste(round(actual,2)), paste(null), paste(round(null + (null - actual), 2)), NA))
            }
            if (actual > null) {
              normTail(L = -1*teststat, U = teststat, axes = FALSE, col = COL[1,2])
              axis(1, at = c(-3, -1*teststat, 0, teststat, 3), labels = c(NA, paste(round(null - (actual - null), 2)), paste(null), paste(round(actual,2)), NA))
            }
          }
        }
      }
    }	
}
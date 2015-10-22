source("inference_ci.R")

inference_ci(y = weight, data = nc, statistic = "mean", 
             method = "theoretical")

inference_ci(y = weight, data = nc, statistic = "mean", 
             method = "simulation", boot_method = "se")

inference_ci(y = weight, data = nc, statistic = "median", 
             method = "simulation", boot_method = "se")

inference_ci(y = habit, data = nc, success = "nonsmoker",
             statistic = "proportion", method = "theoretical")

inference_ci(y = habit, data = nc, success = "nonsmoker",
             statistic = "proportion", method = "simulation", boot_method = "se")

inference_ci(y = weight, x = habit, data = nc, statistic = "mean", 
             method = "theoretical", show_eda_plot = FALSE)

inference_ci(y = weight, x = habit, data = nc, statistic = "median", boot_method = "perc",
             method = "simulation")

inference_ci(y = lowbirthweight, x = habit, data = nc, success = "low",
             statistic = "proportion", method = "theoretical", order = c("smoker", "nonsmoker"))

inference_ci(y = lowbirthweight, x = habit, data = nc, success = "low",
             statistic = "proportion", method = "simulation", 
             boot_method = "perc")


source("inference_ht.R")
inference_ht(y = weeks, x = habit, data = nc, statistic = "mean", method = "theoretical", 
             null = 0, alternative = "greater", order = c("smoker", "nonsmoker"))

inference(y = weeks, x = habit, data = nc, statistic = "median", method = "theoretical", 
             null = 0, alternative = "greater", order = c("smoker", "nonsmoker"), type = "ht")

inference_ht(y = weight, x = habit, data = nc, statistic = "mean", method = "theoretical", 
             null = 0, alternative = "greater")

nc = nc[!is.na(nc$habit),]
ht_two_mean_theo(y = nc$weight, x = nc$habit, null = 0, alternative = "twosided",
                 y_name = "weight", show_eda_plot = TRUE, show_inf_plot = TRUE)





inference_ht(y = weeks, data = nc, statistic = "median", method = "simulation", 
             null = 38.5, alternative = "less")

inference_ht(y = weeks, data = nc, statistic = "mean", method = "simulation", 
             null = 38.5, alternative = "less") # some warning

inference_ht(y = weeks, data = nc, statistic = "median", method = "simulation", 
             null = 38.5, alternative = "less") # some error


source("ci_single_mean_sim.R")
source("ci_single_mean_theo.R")
source("ci_single_median_sim.R")
source("ci_single_prop_sim.R")
source("ci_single_prop_theo.R")
source("ci_two_mean_sim.R")
source("ci_two_mean_theo.R")
source("ci_two_median_sim.R")
source("ci_two_prop_sim.R")
source("ci_two_prop_theo.R")
source("ht_single_mean_sim.R")
source("ht_single_mean_theo.R")
source("ht_single_median_sim.R")
source("ht_two_mean_theo.R")
source("ht_many_mean_theo.R")
source("inference.R")
save(ci_single_mean_sim, 
     ci_single_mean_theo, 
     ci_single_median_sim, 
     ci_single_prop_sim, 
     ci_single_prop_theo, 
     ci_two_mean_sim, 
     ci_two_mean_theo, 
     ci_two_median_sim, 
     ci_two_prop_sim, 
     ci_two_prop_theo, 
     ht_single_mean_sim, 
     ht_single_mean_theo, 
     ht_single_median_sim, 
     ht_two_mean_theo, 
     ht_many_mean_theo, 
     inference, file = "inference.RData")

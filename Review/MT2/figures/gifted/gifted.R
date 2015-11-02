# load packages -----------------------------------------------------

library(ggplot2)
library(dplyr)

# load inference function -------------------------------------------

load(url("https://stat.duke.edu/~mc301/R/fun/inference.RData"))

# load data ---------------------------------------------------------

gift = read.csv("gifted.csv")

# plot mother and father iq -----------------------------------------

fatheriq_hist <- ggplot(data = gift, aes(x = fatheriq)) +
  geom_histogram(fill = "#8FDEE1", binwidth = 5) +
  xlab("Father's IQ") +
  xlim(c(100, 135))

ggsave(filename = "fatheriq_hist.pdf", plot = fatheriq_hist, height = 3, width = 5)

summary(gift$fatheriq)

motheriq_hist <- ggplot(data = gift, aes(x = motheriq)) +
  geom_histogram(fill = "#8FDEE1", binwidth = 5) +
  xlab("Mother's IQ") +
  xlim(c(100, 135))

ggsave(filename = "motheriq_hist.pdf", plot = motheriq_hist, height = 3, width = 5)

summary(gift$motheriq)

# calculate iqdiff --------------------------------------------------

gift <- gift %>%
  mutate(iqdiff = fatheriq - motheriq)

# calculate iqdiff --------------------------------------------------

iqdiff_boot <- inference(y = iqdiff, data = gift, type = "ci", method = "simulation", 
          statistic = "mean", boot_method = "se", seed = 2653)

# plot iqdiff_boot --------------------------------------------------

iqdiff_boot_hist <- ggplot(data = data.frame(iqdiff_boot), aes(x = sim_dist)) +
  geom_histogram() +
  ylab("") +
  xlab("bootstrap means") +
  ggtitle("Bootstrap Distribution")

ggsave(filename = "iqdiff_boot_hist.pdf", plot = iqdiff_boot_hist, height = 2.5, width = 7)

# bootstrap quantiles -----------------------------------------------

lower <- c(0.01, 0.025, 0.05, 0.10, 0.20)
upper <- rev(1 - lower)

round(quantile(iqdiff_boot$sim_dist, probs = c(lower, upper)), 2)

# ht for difference between motheriq and fatheriq -------------------

inference(y = iqdiff, data = gift, type = "ht", method = "theoretical", 
          statistic = "mean", null = 0, alternative = "twosided")

# histogram of counting age ----------------------------------------- 

count_hist <- ggplot(data = gift, aes(x = count)) +
  geom_histogram(fill = "#8FDEE1", binwidth = 2) +
  xlab("age when the child first counted to 10 successfully (months)")

ggsave(filename = "count_hist.pdf", plot = count_hist, height = 3, width = 5)



pdf("count_hist.pdf", height = 3, width = 5)
par(mar=c(4,3.75,0.5,0.5), mgp=c(2.7,0.7,0), las = 1)
hist(gift$count, col="#22558833", border="#225588", xlab = "age when the child first counted to 10 successfully (in months)", main = "", cex.lab = 0.8)
dev.off()

inference(gift$count, est = "mean", null = 32, method = "theoretical", alternative = "less", type = "ht")


summary(gift$count)

##

pdf("father_iq_hist.pdf", height = 3, width = 5)
par(mar=c(4,3.75,0.5,0.5), mgp=c(2.7,0.7,0), las = 1)
hist(gift$fatheriq, col="#22558833", border="#225588", xlab = "father's iq", main = "", cex.lab = 0.8)
dev.off()

summary(gift$fatheriq)

pdf("mother_iq_hist.pdf", height = 3, width = 5)
par(mar=c(4,3.75,0.5,0.5), mgp=c(2.7,0.7,0), las = 1)
hist(gift$motheriq, col="#22558833", border="#225588", xlab = "mother's iq", main = "", cex.lab = 0.8)
dev.off()

summary(gift$motheriq)


iqdff = gift$fatheriq - gift$motheriq


set.seed(5)
simdist_iqdiff = inference(iqdff, type = "ci", method = "simulation", nsim = 200, conflevel = 0.95, est = "mean", simdist = TRUE)

pdf("iqdiff_boot.pdf", width = 7.5, height = 4)
par(mar=c(4,0,0,0))
BHH2::dotPlot(simdist_iqdiff, xlab = "bootstrap statistic", axes = FALSE, xlim = c(-6.5,1))
axis(1, at = seq(-6.5,1,0.25), labels = c(-6.5, NA, -6, NA, -5.5, NA, -5, NA, -4.5, NA, -4, NA, -3.5, NA, -3, NA, -2.5, NA, -2, NA, -1.5, NA, -1, NA, -0.5, NA, 0, NA, 0.5, NA, 1))
dev.off()

pdf("iqdiff_boot_soln.pdf", width = 7.5, height = 4)
par(mar=c(4,0,0,0))
set.seed(5)
inference(iqdff, type = "ci", method = "simulation", nsim = 200, conflevel = 0.95, est = "mean")
axis(1, at = seq(-6.5,1,0.25), labels = c(-6.5, NA, -6, NA, -5.5, NA, -5, NA, -4.5, NA, -4, NA, -3.5, NA, -3, NA, -2.5, NA, -2, NA, -1.5, NA, -1, NA, -0.5, NA, 0, NA, 0.5, NA, 1))
dev.off()


inference(iqdff, type = "ht", method = "theoretical", est = "mean", null = 0, alternative = "twosided")
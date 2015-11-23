# load packages -----------------------------------------------------

library(xtable)
library(ggplot2)
library(GGally)
library(gridExtra)

# load data ---------------------------------------------------------

load(url("https://stat.duke.edu/~mc301/data/cig07.RData"))

# full model --------------------------------------------------------

m_full <- lm(CO ~ NIC + TAR + LEN + FLTR + PACK + STRENGTH + TYPE, data = cig07)
summary(m_full)$adj.r.squared # 0.7995977
xtable(summary(m_full))

# step 1 ------------------------------------------------------------

summary(update(m_full, . ~ . - NIC))$adj.r.squared
summary(update(m_full, . ~ . - TAR))$adj.r.squared
summary(update(m_full, . ~ . - LEN))$adj.r.squared
summary(update(m_full, . ~ . - FLTR))$adj.r.squared
summary(update(m_full, . ~ . - PACK))$adj.r.squared
summary(update(m_full, . ~ . - STRENGTH))$adj.r.squared
summary(update(m_full, . ~ . - TYPE))$adj.r.squared # pick, 0.7997479

m1 <- update(m_full, . ~ . - TYPE)
summary(m1)

# step 2 ------------------------------------------------------------

summary(update(m1, . ~ . - NIC))$adj.r.squared
summary(update(m1, . ~ . - TAR))$adj.r.squared
summary(update(m1, . ~ . - LEN))$adj.r.squared
summary(update(m1, . ~ . - FLTR))$adj.r.squared
summary(update(m1, . ~ . - PACK))$adj.r.squared
summary(update(m1, . ~ . - STRENGTH))$adj.r.squared

# none higher, stop in step 1

# picked model summary ----------------------------------------------

xtable(summary(m1))
summary(m1)

# pairs plot --------------------------------------------------------

pdf(file = "pairs.pdf", width = 13, height = 10)
ggpairs(cig07, columns = c(5, 3, 4, 6, 7, 8, 9)) + 
  theme_grey(base_size = 8)
dev.off()

# remove TAR --------------------------------------------------------

summary(update(m1, . ~ . - TAR))$adj.r.squared

# remove NIC --------------------------------------------------------

summary(update(m1, . ~ . - NIC))$adj.r.squared

# final model -------------------------------------------------------

m2 <- update(m1, . ~ . - NIC)

xtable(summary(m2))

# diagnostic plots --------------------------------------------------

# res vs. fit
p1 <- qplot(x = .fitted, y = .resid, data = m2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")
# res hist
p2 <- qplot(x = .resid, data = m2, geom = "histogram") +
  xlab("Residuals")
# res qq
p3 <- qplot(sample = .resid, data = m2, stat = "qq")
# order of residuals
p4 <- qplot(y = .resid, data = m2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Order of data collection") +
  ylab("Residuals")  
  
pdf(file = "diag.pdf", width = 12, height = 3)
grid.arrange(p1, p2, p3, p4, nrow = 1)
dev.off()

# prediction --------------------------------------------------------

# fit the model
m = lm(CO ~ TAR + LEN + FLTR + PACK + STRENGTH, data = cig07)
# create the new data point
smokesalot = data.frame(TAR = 12, LEN = 80, FLTR = "F", PACK = "HARD", STRENGTH = "LIGHT")
# predict
predict(m, newdata = smokesalot, interval = "prediction")

senic <- read.csv('/Users/maciewheeler/Downloads/senic_homework6.csv')

# Question 1
senic.mod <- lm(log10(length) ~ age + infection + cultur + xray + bed + patient, senic)
summary(senic.mod)
plot(senic.mod)

wts1 <- 1/fitted(lm(abs(residuals(senic.mod)) ~ age + infection + cultur + xray + bed + patient, senic))^2
senic.mod2 <- lm(log10(length) ~ age + infection + cultur + xray + bed + patient, weight = wts1, senic)
summary(senic.mod2)

newsenic <- cbind(senic$age, senic$infection, senic$cultur, senic$xray, senic$bed, senic$patient, senic$length)
newsenic
boot.wlscoef <- function(data, indices, maxit=20) {
  data <- data[indices,]
  colnames(data)<-c("x1","x2", "x3", "x4", "x5", "x6", "y")
  data <- as.data.frame(data)
  data.mod <- lm(log10(y) ~ x1 + x2 + x3 + x4 + x5 + x6, data)
  wts1 <- 1/fitted(lm(abs(residuals(data.mod)) ~ x1 + x2 + x3 + x4 + x5 + x6, data))^2
  data.mod2 <- lm(log10(y) ~ x1 + x2 + x3 + x4 + x5 + x6, weight = wts1, data)
  return(coef(data.mod2))
}

wls_boot <- boot(data=newsenic, statistic = boot.wlscoef, R=100, maxit=20)
wls_boot
boot.ci(wls_boot, type="perc", index=2)
boot.ci(wls_boot, type="perc", index=3)
boot.ci(wls_boot, type="perc", index=4)
boot.ci(wls_boot, type="perc", index=5)
boot.ci(wls_boot, type="perc", index=6)
boot.ci(wls_boot, type="perc", index=7)

# Question 2
model <- lmridge(log10(length) ~ age + infection + cultur + xray + bed + patient, data=as.data.frame(senic), K=0.02)
plot(model)
vif(model)
summary(model)

boot.ridgecoef <- function(data, indices, maxit=100) {
  data <- data[indices,]
  colnames(data)<-c("x1","x2", "x3", "x4", "x5", "x6", "y")
  data <- as.data.frame(data)
  mod <- lmridge(log10(y) ~ x1 + x2 + x3 + x4 + x5 + x6, data, K=0.02)
  return(coef(mod))
}

ridge_boot <- boot(data=data.frame(newsenic), statistic = boot.ridgecoef, R=1000, maxit=100)
ridge_boot
boot.ci(ridge_boot, type="perc", index=2)
boot.ci(ridge_boot, type="perc", index=3)
boot.ci(ridge_boot, type="perc", index=4)
boot.ci(ridge_boot, type="perc", index=5)
boot.ci(ridge_boot, type="perc", index=6)
boot.ci(ridge_boot, type="perc", index=7)

# Question 3
model1 <- rlm(log10(length) ~ age + infection + cultur + xray + bed + patient, senic, psi=psi.bisquare)
summary(model1)

boot.huber <- function(data, indices, maxit=100) {
  data <- data[indices,]
  colnames(data)<-c("x1","x2", "x3", "x4", "x5", "x6", "y")
  data <- as.data.frame(data)
  mod <- rlm(log10(y) ~ x1 + x2 + x3 + x4 + x5 + x6, data, maxit=maxit)
  return(coef(mod))
}

robust_boot <- boot(data=data.frame(newsenic), statistic = boot.huber, R=100, maxit=100)
robust_boot
boot.ci(robust_boot, type="perc", index=2)
boot.ci(robust_boot, type="perc", index=3)
boot.ci(robust_boot, type="perc", index=4)
boot.ci(robust_boot, type="perc", index=5)
boot.ci(robust_boot, type="perc", index=6)
boot.ci(robust_boot, type="perc", index=7)

# Question 4
summary(lm(log10(length) ~ factor(region), senic))

senic$region <- relevel(factor(senic$region), "2")
summary(lm(log10(length) ~ factor(region), senic))

# Question 5
summary(aov(log10(length) ~ factor(region), senic))
qt(0.95,109)

table(senic$region)

# Question 7
qt(0.95, 436)
qt(0.983,436)

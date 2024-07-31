senic <- read.csv('/Users/maciewheeler/Downloads/senic.csv')
senic <- senic[-c(114),]
dimensions <- dim(senic)

xbar <- sum(senic$cultur) / dimensions[1]
ybar <- sum(senic$length) / dimensions[1]

sum1 <- 0
for (x in 1:dimensions[1]) {
  row <- senic[x, ]
  xsub <- row$cultur - xbar
  ysub <- row$length - ybar
  sum1 <- sum1 + (xsub * ysub)
}
sum1

sum2 <- 0
for (i in 1:dimensions[1]) {
  row <- senic[i, ]
  xsub <- row$cultur - xbar
  sum2 <- sum2 + (xsub ^ 2)
}
sum2

# question 1
senic <- senic[order(senic$cultur),]

model1 <- lm(senic$length ~ senic$cultur, senic)
summary(model1)

model2 <- lm(senic$length ~ as.factor(senic$cultur), senic)
anova(model1, model2)

qf(0.95,94,17)

# question 2
senic$new_cultur <- floor(senic$cultur)
new_nbrs <- unique(floor(senic$cultur))

for (n in new_nbrs) {
  n_data <- senic[senic$new_cultur == n,]
  avg_n_data <- mean(n_data$cultur)
  
  for (i in 1:dimensions[1]) {
    if (senic$new_cultur[i] == n) {
      senic$cultur[i] <- avg_n_data
    }
  }
}

xbar <- sum(senic$cultur) / dimensions[1]
ybar <- sum(senic$length) / dimensions[1]

sum1 <- 0
for (x in 1:dimensions[1]) {
  row <- senic[x, ]
  xsub <- row$cultur - xbar
  ysub <- row$length - ybar
  sum1 <- sum1 + (xsub * ysub)
}
sum1

sum2 <- 0
for (i in 1:dimensions[1]) {
  row <- senic[i, ]
  xsub <- row$cultur - xbar
  sum2 <- sum2 + (xsub ^ 2)
}
sum2

model1 <- lm(senic$length ~ senic$cultur, senic)
summary(model1)

model2 <- lm(senic$length ~ as.factor(senic$cultur), senic)
anova(model1, model2)

qf(0.95,33,78)

# question 3
plot(senic$xray, senic$length)
model <- lm(senic$length ~ senic$xray, senic)
abline(model)

plot(model$residuals)
abline(0,0)

#plot(senic$length, model$residuals)
plot(model$fitted.values, model$residuals)
abline(0,0)

library(onewaytests)
senic$group <- cut(senic$xray, 5)
senic$residual <- model$residuals
bf.test(residual ~ group, senic)

shapiro.test(model$residuals)
qqnorm(model$residuals)
qqline(model$residuals)

# question 6
library(MASS)
bcmle <- boxcox(model, lambda=seq(-3,3,0.1))
lambda <- bcmle$x[which.max(bcmle$y)]
lambda

library(ALSM)
bcsse <- boxcox.sse(senic$xray, senic$length, l=seq(-2,2,.1))
lambda <- bcsse$lambda[which.min(bcsse$SSE)]
lambda

newmodel <- lm((senic$length ^ -1.2) ~ senic$xray, senic)
summary(newmodel)
anova(newmodel)

plot(newmodel$residuals)
abline(0,0)

qqnorm(newmodel$residuals)
qqline(newmodel$residuals)

library(SciViews)

newmodel <- lm(ln(senic$length) ~ senic$xray, senic)
summary(newmodel)
anova(newmodel)

plot(newmodel$residuals)
abline(0,0)

qqnorm(newmodel$residuals)
qqline(newmodel$residuals)

senic <- read.csv('/Users/maciewheeler/Downloads/senic.csv')
senic <- senic[-c(114),]

# question 1
qt(0.95,111)
qf(0.9,2,111)

# question 2 and 3
model <- lm(senic$length ~ senic$infection, senic)
summary(model)
anova(model)

dimensions <- dim(senic)
xbar <- sum(senic$infection) / dimensions[1]

sum2 <- 0
for (i in 1:dimensions[1]) {
  row <- senic[i, ]
  xsub <- row$infection - xbar
  sum2 <- sum2 + (xsub ^ 2)
}
sum2

qt(0.975,111)
qt(.983,111)
qf(0.9,3,110)
qf(0.9,2,111)

# question 4
Y <- as.matrix(senic$length)
colnames(Y) <- c('length')
Y

infection <- c(senic$infection)
xray <- c(senic$xray)
facility <- c(senic$facility)
intercept <- rep(1, dimensions[1])
X <- cbind(intercept, infection, xray, facility)
X

H <- X %*% solve(t(X) %*% X) %*% t(X)
H

J <- matrix(1, nrow=113, ncol=113)
J

I <- diag(113)
I

e <- (I - H) %*% Y
e

b <- solve(t(X) %*% X) %*% t(X) %*% Y
b

SSR <- t(Y) %*% (H - ((1/dimensions[1])*J)) %*% Y
SSR

SSE <- t(Y) %*% (I - H) %*% Y
SSE

SST <- t(Y) %*% (I - ((1/dimensions[1])*J)) %*% Y
SST

# question 5
model <- lm(senic$length ~ senic$infection + senic$xray + senic$facility)
anova(model)
summary(model)

# question 6
MSE <- 2.492
sigma_e <- MSE * (I - H)
sigma_e

# question 7
sigma_b <- MSE * solve(t(X) %*% X)
sigma_b

# question 8
qf(0.9,3,109)

# question 9
qt(0.95,109)
qt(0.983,109)
qf(0.9,4,109)

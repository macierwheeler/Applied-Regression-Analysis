# reading in data
senic <- read.csv('/Users/maciewheeler/Downloads/senic.csv')

# cleaning data and setting up variables
senic <- senic[-c(114),]
dimensions <- dim(senic)

# question 1.1
xbar <- sum(senic$xray) / dimensions[1]

# question 1.2
ybar <- sum(senic$length) / dimensions[1]

# question 1.3
sum1 <- 0
for (x in 1:dimensions[1]) {
  row <- senic[x, ]
  xsub <- row$xray - xbar
  ysub <- row$length - ybar
  sum1 <- sum1 + (xsub * ysub)
}
sum1

# question 1.4
sum2 <- 0
for (i in 1:dimensions[1]) {
  row <- senic[i, ]
  xsub <- row$xray - xbar
  sum2 <- sum2 + (xsub ^ 2)
}
sum2

# question 1.7
b0 <- 6.566374561
b1 <- 0.037755823
sum3 <- 0
for (n in 1:dimensions[1]) {
  row <- senic[n, ]
  yhat <- b0 + (b1 * row$xray)
  ysub <- row$length - yhat
  sum3 <- sum3 + (ysub ^ 2)
}
sum3

# question 1.9
sum4 <- 0
for (m in 1:dimensions[1]) {
  row <- senic[m, ]
  ysub <- row$length - ybar
  sum4 <- sum4 + (ysub ^ 2)
}
sum4

# question 1.10
b0 <- 6.566374561
b1 <- 0.037755823
sum5 <- 0
for (k in 1:dimensions[1]) {
  row <- senic[k, ]
  yhat <- b0 + (b1 * row$xray)
  ysub <- ybar - yhat
  sum5 <- sum5 + (ysub ^ 2)
}
sum5

# question 2
qt(0.1, 111, lower.tail=FALSE)
qt(0.05, 111, lower.tail=FALSE)

# question 3
qt(0.1, 111, lower.tail=FALSE)
2*(1-pt(4.361324289, 111))

# question 4
model <- lm(senic$length ~ senic$xray, senic)
summary(model)
anova(model)

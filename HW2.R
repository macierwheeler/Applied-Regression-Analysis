# reading in data
senic <- read.csv('/Users/maciewheeler/Downloads/senic.csv')

# cleaning data and setting up variables
senic <- senic[-c(114),]
dimensions <- dim(senic)

## question 1
# xbar and ybar
xbar <- sum(senic$infection) / dimensions[1]
ybar <- sum(senic$length) / dimensions[1]

# calculations for b0 and b1
sum1 <- 0
for (x in 1:dimensions[1]) {
  row <- senic[x, ]
  xsub <- row$infection - xbar
  ysub <- row$length - ybar
  sum1 <- sum1 + (xsub * ysub)
}
sum1

sum2 <- 0
for (i in 1:dimensions[1]) {
  row <- senic[i, ]
  xsub <- row$infection - xbar
  sum2 <- sum2 + (xsub ^ 2)
}
sum2

# t-value
qt(0.975, 111)

# model and summary
model <- lm(senic$length ~ senic$infection, senic)
summary(model)

## question 2
# nc
ncsenic <- senic[which(senic$region == 2),]
ncdim <- dim(ncsenic)

xbar <- sum(ncsenic$infection) / ncdim[1]
ybar <- sum(ncsenic$length) / ncdim[1]

sum1 <- 0
for (x in 1:ncdim[1]) {
  row <- ncsenic[x, ]
  xsub <- row$infection - xbar
  ysub <- row$length - ybar
  sum1 <- sum1 + (xsub * ysub)
}
sum1

sum2 <- 0
for (i in 1:ncdim[1]) {
  row <- ncsenic[i, ]
  xsub <- row$infection - xbar
  sum2 <- sum2 + (xsub ^ 2)
}
sum2

qt(0.975, ncdim[1] - 2)

model <- lm(ncsenic$length ~ ncsenic$infection, ncsenic)
summary(model)

# ne
nesenic <- senic[which(senic$region == 1), ]
nedim <- dim(nesenic)

xbar <- sum(nesenic$infection) / nedim[1]
ybar <- sum(nesenic$length) / nedim[1]

sum1 <- 0
for (x in 1:nedim[1]) {
  row <- nesenic[x, ]
  xsub <- row$infection - xbar
  ysub <- row$length - ybar
  sum1 <- sum1 + (xsub * ysub)
}
sum1

sum2 <- 0
for (i in 1:nedim[1]) {
  row <- nesenic[i, ]
  xsub <- row$infection - xbar
  sum2 <- sum2 + (xsub ^ 2)
}
sum2

qt(0.975, nedim[1] - 2)

model <- lm(nesenic$length ~ nesenic$infection, nesenic)
summary(model)

# s
ssenic <- senic[which(senic$region == 3), ]
sdim <- dim(ssenic)

xbar <- sum(ssenic$infection) / sdim[1]
ybar <- sum(ssenic$length) / sdim[1]

sum1 <- 0
for (x in 1:sdim[1]) {
  row <- ssenic[x, ]
  xsub <- row$infection - xbar
  ysub <- row$length - ybar
  sum1 <- sum1 + (xsub * ysub)
}
sum1

sum2 <- 0
for (i in 1:sdim[1]) {
  row <- ssenic[i, ]
  xsub <- row$infection - xbar
  sum2 <- sum2 + (xsub ^ 2)
}
sum2

qt(0.975, sdim[1] - 2)

model <- lm(ssenic$length ~ ssenic$infection, ssenic)
summary(model)

# w
wsenic <- senic[which(senic$region == 4), ]
wdim <- dim(wsenic)

xbar <- sum(wsenic$infection) / wdim[1]
ybar <- sum(wsenic$length) / wdim[1]

sum1 <- 0
for (x in 1:wdim[1]) {
  row <- wsenic[x, ]
  xsub <- row$infection - xbar
  ysub <- row$length - ybar
  sum1 <- sum1 + (xsub * ysub)
}
sum1

sum2 <- 0
for (i in 1:wdim[1]) {
  row <- wsenic[i, ]
  xsub <- row$infection - xbar
  sum2 <- sum2 + (xsub ^ 2)
}
sum2

qt(0.975, wdim[1] - 2)

model <- lm(wsenic$length ~ wsenic$infection, wsenic)
summary(model)

## question 3.1
model <- lm(senic$length ~ senic$infection, senic)
summary(model)
anova(model)

## question 3.2
cor(senic$infection, senic$length)

## question 3.3
cor.test(senic$infection, senic$length)

## question 4
qf(0.95,1,111)


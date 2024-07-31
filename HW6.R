senic <- read.csv('/Users/maciewheeler/Downloads/senic_homework6.csv')

# Question 1
senicn <- cbind(senic$length, senic$bed, senic$region)
colnames(senicn) <- c("length", "bed", "region")
senicn <- data.frame(senicn)

senicn$region[senicn$region == 1] <- 0
senicn$region[senicn$region == 2] <- 0
senicn$region[senicn$region == 3] <- 1
senicn$region[senicn$region == 4] <- 1

model <- lm(length ~ bed + region + bed:region, senicn)
anova(model)

qf(0.95,1,109)

# Question 2
senicn2 <- cbind(senic$length, senic$bed, senic$region)
colnames(senicn2) <- c("length", "bed", "region")
senicn2 <- data.frame(senicn2)
senicn2['X2'] <- 0
senicn2['X3'] <- 0
senicn2['X4'] <- 0

senicn2$X2[senicn2$region == 2] <- 1
senicn2$X3[senicn2$region == 3] <- 1
senicn2$X4[senicn2$region == 4] <- 1

model2 <- lm(length ~ bed + X2 + X3 + X4 + bed:X2 + bed:X3 + bed:X4, senicn2)
summary(model2)
anova(model2)

model3 <- lm(length ~ bed * region, senic)
summary(model3)
anova(model3)

# Question 3
meanx1 <- mean(senicn2$bed)
senicn3 <- cbind(senicn2, (senicn2$bed - meanx1))
colnames(senicn3) <- c("length", "bed", "region", "X2", "X3", "X4", "newbed")

model4 <- lm(length ~ newbed + X2 + X3 + X4 + newbed:X2 + newbed:X3 + newbed:X4, senicn3)
summary(model4)
anova(model4)

qf(0.9,1,105)
qf(0.9,3,105)

# Question 4
model5 <- lm(length ~ age + infection + cultur + xray + bed + patient + nurse + facility, senic)
avPlots(model5)

# Question 5
BestSub(senic[, c(3,4,5,6,7,10,11,12)], log10(senic[,2]), num=1)

# Question 6
model6 <- lm(length ~ age + infection + cultur + xray + bed + patient + nurse, senic)
dffits(model6)
cooks.distance(model6)

pf(0.32,8,105)
pf(0.12,8,105)

dfbetas(model6)

# Question 7
VIF(model6)

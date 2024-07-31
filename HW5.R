senic <- read.csv('/Users/maciewheeler/Downloads/senic.csv')
senic <- senic[-c(114),]

# question 1
model1 <- lm(senic$length ~ senic$age + senic$infection, senic)
model2 <- lm(senic$length ~ senic$infection)

anova(model1)
anova(model2)

qt(0.975,111)
qf(0.95,1,110)
qf(0.95,2,109)

# question 2
model3 <- lm(senic$length ~ senic$age + senic$infection + senic$cultur, senic)
anova(model3)

# question 3
model41 <- lm(senic$length ~ senic$age + senic$infection + senic$cultur, senic)
anova(model41)
model42 <- lm(senic$length ~ senic$age + senic$infection + senic$patient, senic)
anova(model42)
model43 <- lm(senic$length ~ senic$age + senic$infection + senic$nurse, senic)
anova(model43)
model44 <- lm(senic$length ~ senic$age + senic$infection + senic$facility, senic)
anova(model44)

# question 4
senic <- cbind(senic, senic$age^2)
colnames(senic) <- c("id", "length", "age", "infection", "cultur", "xray", "bed", "affiliat", "region", "patient", "nurse", "facility", "age2")

cor(senic$age, senic$age2)
model5 <- lm(length ~ age + age2, senic)
summary(model5)
anova(model5)
Anova(model5, type="II")

model6 <- lm(length ~ age2 + age, senic)
anova(model6)

xbar1 <- mean(senic$age)
sd1 <- sd(senic$age)

senicn <- cbind(senic$length, (senic$age - xbar1)/sd1, ((senic$age - xbar1)/sd1)^2)
colnames(senicn) <- c("length", "newage", "newage2")
senicn <- data.frame(senicn)

cor(senicn$newage, senicn$newage2)

model7 <- lm(length ~ newage + newage2, senicn)
summary(model7)
anova(model7)
Anova(model7, type=2)

model8 <- lm(length ~ newage2 + newage, senicn)
anova(model8)

qt(0.9875,110)

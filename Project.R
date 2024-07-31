# importing libraries
library(lmtest)
library(fmsb)
library(lmtest)
library(MASS)
library(car)
library(parameters)
library(nortest)
library(corrplot)
library(boot)
library(lmridge)
library(ALSM)
library("DAAG")

# reading in data and only keeping attributes we need
spotify <- read.csv('stat512/Spotify_attributes.csv')
spotify <- cbind(spotify$danceability, spotify$energy, spotify$instrumentalness, spotify$acousticness, spotify$valence, spotify$speechiness, spotify$explicit, spotify$year, spotify$popularity)
spotify <- data.frame(spotify)
colnames(spotify) <- c("danceability", "energy", "instrumentalness", "acousticness", "valence", "speechiness", "explicit", "year", "popularity")

# correlation matrix for all variables
corMat = cor(spotify)
corrplot(corMat, method="number")

### Question 1
model1 <- lm(popularity ~ danceability + energy, spotify)
plot(model1)
bptest(model1)
ad.test(spotify$danceability)
ad.test(spotify$energy)
VIF(model1)
summary(model1)

model1res <- residuals(model1)
qqnorm(model1res)
qqline(model1res)

# box cox for nonconstant variance
bcmle <- boxCox(model1, family="yjPower", plotit=TRUE)
lambda <- bcmle$x[which.max(bcmle$y)]
lambda

#transform with box cox
model1.bc <- lm(popularity^lambda ~ danceability + energy, spotify)

# recheck variance and normality
plot(model1.bc)

model1res2 <- residuals(model1.bc)
qqnorm(model1res2)
qqline(model1res2)

# wls for nonconstant variance
wts1 <- 1/fitted(lm(abs(residuals(model1)) ~ danceability + energy, spotify))^2
model1.wls <- lm(popularity ~ danceability + energy, weights = wts1, data = spotify)
summary(model1.wls)

# check plot to see change
plot(fitted(model1.wls), resid(model1.wls))
abline(0,0)

qqnorm(resid(model1.wls))
qqline(resid(model1.wls))

# check for/remove influential points
rs <- rstudent(model1)
n <- length(spotify$popularity)
t <- qt(1 - (0.05/(2*n)),n-1-3)
df <- dffits(model1)
d <- 2*sqrt(3/169909)
final_data <- subset(spotify,abs(rs) < abs(t)*1.5 & abs(df) < abs(d)*1.5)

# new model
new_model <- lm(popularity ~ danceability + energy, final_data)
summary(new_model)

plot(new_model)

# box cox and wls transformations
bcmle <- boxCox(new_model, family="yjPower", plotit=TRUE)
lambda <- bcmle$x[which.max(bcmle$y)]
lambda
new_model.bc <- lm(popularity^lambda ~ danceability + energy, final_data)

# recheck variance and normality
plot(new_model.bc)

# wls
wts <- 1/fitted(lm(abs(residuals(new_model)) ~ danceability + energy, data=final_data))^2
new_model.wls <- lm(popularity ~ danceability + energy, weights = wts, data = final_data)
summary(new_model.wls)

# check plots
plot(fitted(new_model.wls), resid(new_model.wls))
abline(0,0)

qqnorm(resid(new_model.wls))
qqline(resid(new_model.wls))

# CI
confint(new_model.wls,level=0.95)

# hypothesis testing
mod.compare <- lm(popularity ~ I(danceability + energy), weights = wts, data = final_data)
model_parameters(mod.compare)

anova(mod.compare, new_model.wls)

# k crossfold validation
cv.lm(data=final_data, form.lm=new_model.wls, m= 10, plotit = F)

### Question 2
model2 <- lm(popularity ~ instrumentalness + acousticness + instrumentalness:acousticness, spotify)
plot(model2)
bptest(model2)
ad.test(spotify$instrumentalness)
ad.test(spotify$acousticness)
VIF(model2)
summary(model2)

confint(model1, level = .90)

anova(lm(popularity~instrumentalness+acousticness,data=spotify),model1)

anova(model1)

rs<-rstudent(model1,data=spotify)

n<-length(spotify$popularity)

t <- qt(0.99999985286, df=169904, lower.tail=FALSE)

df <- dffits(model)

d<-2*sqrt(4/169909)

final_data <- subset(spotify,abs(rs) < abs(t)*1.5 & abs(df) < abs(d)*1.5)

out_rem_model <- lm(popularity~instrumentalness+acousticness+instrumentalness*acousticness,data=final_data)

bc <- boxCox(out_rem_model, family="yjPower", plotit = TRUE)

lambda<-bc$x[which.max(bc$y)]

bc_model<-lm(popularity^lambda~instrumentalness*acousticness,data=final_data)

wt <- 1/fitted(lm(abs(residuals(bc_model))~instrumentalness*acousticness, data = final_data))^2
wls_model <- lm(popularity^lambda~instrumentalness*acousticness,data=final_data,weights=wt)

#produce residual vs. fitted plot
plot(fitted(wls_model), resid(wls_model))

#add a horizontal line at 0 
abline(0,0)

vif(wls_model)

qqnorm(resid(wls_model))
qqline(resid(wls_model))

# Cross Validation k=10 final model
cv.lm(data=final_data, form.lm=wls_model, m= 10, plotit = F)

# Cross Validation k=10 initial model
cv.lm(data=data, form.lm=model, m= 10, plotit = F)

### Question 3
model3 <- lm(popularity ~ explicit + year, spotify)
plot(model3)
bptest(model3)
ad.test(spotify$explicit)
ad.test(spotify$year)
VIF(model3)
summary(model3)

years = strtoi(substr(spotify$release_date,1,4))
mod3 = lm(spotify$popularity~as.factor(spotify$explicit)+years)
summary(mod3)
anova(mod3)
confint(mod3, level=.90)

red = lm(spotify$popularity~years)
summary(red)
anova(red,mod3)
plot(mod3$residuals)
qqplot(mod3$residuals)
bptest(mod3)

summary(mod3)

qqnorm(resid(mod3))
qqline(resid(mod3))

vif(mod3,type = 'predictor')
cooks.distance(mod3)[cooks.distance(mod3) > qf(0.2, 3, n-3)]
rs=rstudent(mod3,data=spotify)
n=length(spotify$popularity)
t = qt(1 - (0.05)/(2*n), df=n-4, lower.tail=FALSE)
df = dffits(mod3)
d=2*sqrt(4/169909)
final_data = subset(spotify,abs(rs) < abs(t)*1.5 & abs(df) < abs(d)*1.5)

newYears = strtoi(substr(final_data$release_date,1,4))
final_data$yrs = newYears
out_rem_model = lm(popularity~as.factor(explicit)+yrs,data=final_data)
bc = boxCox(out_rem_model, family="yjPower", plotit = TRUE)


lambda<-bc$x[which.max(bc$y)]
bc_model<-lm(popularity^lambda~as.factor(explicit)+yrs,data=final_data)
bc_red_model = lm(popularity^lambda~yrs,data=final_data)
summary(bc_model)
plot(bc_model)
summary(bc_red_model)
plot(bc_red_model)
anova(bc_red_model, bc_model)
wt_red = 1/fitted(lm(abs(residuals(bc_red_model))~yrs, data = final_data))^2
wls_red_model <- lm(popularity^lambda~yrs,data=final_data,weights=wt_red)
summary(wls_red_model)


(length(spotify$acousticness) - length(final_data$acousticness)) / length(spotify$acousticness)
wt_full <- 1/fitted(lm(abs(residuals(bc_model))~as.factor(explicit)+yrs, data = final_data))^2
wls_full_model <- lm(popularity^lambda~as.factor(explicit)+yrs,data=final_data,weights=wt_full)
summary(wls_full_model)
plot(wls_full_model)

summary(wls_full_model)
anova(wls_red_model, wls_full_model)

wt_red = 1/fitted(lm(abs(residuals(bc_model))~as.factor(explicit)+yrs, data = final_data))^2
confint(wls_full_model, level=.95)
confint(wls_red_model, level=0.95)

length(final_data$release_date)
length(newYears)
# Cross Validation k=10 final model
cv.lm(data=final_data, form.lm=wls_full_model, m= 10, plotit = F)

# Cross Validation k=10 initial model
cv.lm(data=spotify, form.lm=mod3, m= 10, plotit = F)


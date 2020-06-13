library(MASS)
library(tidyverse)
data("Boston")
names(Boston)

### 3.6.2


# First linear regression on medv by using only lstat as predictor
lm_fit <- lm(medv ~ lstat, Boston)
summary(lm_fit)

# Check what is included in the model
names(lm_fit)

# use $ to access the data contained in lm_fit
lm_fit$coefficients

# get confidence interval
confint(lm_fit)

# use predict() to predict 
predict(lm_fit, data.frame(lstat=c(5,10,15,20)),
        interval = "confidence") # get confidence interval for the preidiction

predict(lm_fit, data.frame(lstat=c(5,10,15,20)),
        interval = "prediction") # get prediction interval

# plot the data
# pch="x" use x to mark the points
plot(Boston$lstat,Boston$medv, col="red",pch=1:20)

# add the least squares regression line using abline
# note: abline can draw any line, intercept a, slope b -> abline(a,b)
abline(lm_fit,lwd =3)

# use par() to dosplay multiple plot on one screen
par(mfrow=c(2,2)) #2 by 2 plots 

# compute the residuals from the model
plot(predict(lm_fit), residuals(lm_fit))

# compute the studentized from the model
plot(predict(lm_fit), rstudent(lm_fit))
# hatvalues()??

plot(hatvalues(lm_fit))

# which.max find the location of the max
which.max(hatvalues(lm_fit))





# 3.6.3 multiple regression

lm_fit <- lm(medv~lstat+age, Boston)
summary(lm_fit)

# short cut for perform regression on all variables

lm_fit <-  lm(medv~., Boston)
summary(lm_fit)
# ignore one of the variable

lm_fit_2 <- lm(medv~.-age, Boston)
summary(lm_fit_2)

# Check VIF(Variance Inflation Factors): correlation between predictors
# high VIF -> unreliable regression
library(car)
vif(lm_fit)



## 3.6.4 interaction Terms

summary(lm(medv~lstat*age, Boston))# lstat*age-> lstat+age+lstat:age;
# (lstat:age is the interation between lstat and age)

##3.6.5 Non-linear Tramsformations of the predictors
## use I(lstat^2) to create a new predictor lstat^2
lm_fit2 = lm(medv~lstat+I(lstat^2),Boston)
summary(lm_fit2)

# use anova to check if the new model is better
lm_fit = lm(medv~lstat, Boston)
anova(lm_fit, lm_fit2)

par(mfrow =c(2,2))
plot(lm_fit2)


# use poly()
lm_fit5 <- lm(medv~poly(lstat,5), Boston)
summary(lm_fit5)

# use log()
summary(lm(medv~log(rm), Boston))



# 3.6.6
library(ISLR)
data("Carseats")
names(Carseats)
lm_fit_seats <- lm(Sales~.+ Income:Advertising + Price:Age, Carseats)
summary(lm_fit_seats)
#same as
lm_fit_seats2 <- lm(Sales~.+ Income*Advertising + Price*Age, Carseats)
summary(lm_fit_seats2)

# contrasts

contrasts(Carseats$ShelveLoc)
?contrasts

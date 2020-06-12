library(MASS)
library(tidyverse)
data("Boston")
names(Boston)

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




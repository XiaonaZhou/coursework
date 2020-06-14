# 5.3.1
# The validation Set Approach
RNGkind(sample.kind = "Rounding")
library(ISLR)
set.seed(1)
train=sample(392,196)# pick 196 numbers between 1 and 392
lm_fit <- lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm_fit, Auto))[-train]^2)
lm_fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm_fit2, Auto))[-train]^2)
lm_fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm_fit3, Auto))[-train]^2)


set.seed(2)
train=sample(392,196)# pick 196 numbers between 1 and 392
lm_fit <- lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg-predict(lm_fit, Auto))[-train]^2)
lm_fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm_fit2, Auto))[-train]^2)
lm_fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm_fit3, Auto))[-train]^2)



# 5.3.2
# Leave-out-out cross validation

# glm() performs logistic regression when family="binomial"
# without this argument, it performs linear regression just like lm()


glm_fit =  glm(mpg~horsepower, data = Auto)
coef(glm_fit)

lm_fit =  lm(mpg~horsepower, data = Auto)
coef(glm_fit)


# use glm() because it can be used with cv.glm()

library(boot)
glm_fit =  glm(mpg~horsepower, data = Auto)
cv_err = cv.glm(Auto, glm_fit)
cv_err$delta # delta gives the test error


# get all the test error for coomplex polynomial fits

cv_error <- rep(0.5)
for ( i in 1:5){
  glm_fit = glm(mpg~poly(horsepower,i), data = Auto)
  cv_error[i] = cv.glm(Auto, glm_fit)$delta[1]
}
cv_error # shape drop between linear and quadratic fits


# 5.3.3 k-fold cross-validation
# the only difference in code is set K = 10 in cv.glm()

set.seed(17)
cv_error_10 <- rep(0,10)
for (i in 1:10){
  glm_fit = glm(mpg~poly(horsepower,i), data=Auto)
  cv_error_10[i] = cv.glm(Auto, glm_fit, K =10)$delta[1]
}
cv_error_10


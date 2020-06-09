pop2 <- read.csv('pop2.csv') 
head(pop2)
str(pop2)
summary(pop2)
mean_bmi <- mean(pop2$bmi)
mean_bmi
sd_bmi <- sd(pop2$bmi)
sd_bmi

X_bar <- rep(0,1e5)
for (i in 1:1e5){
  X.sample <- sample(pop2$bmi, 150)
  X_bar[i] <- mean(X.sample)
}
X_mean <- mean(X_bar)
X_mean
sd(X_bar)


# Identify, using simulations, the central region that contains 80% of the
# sampling distribution of the sample average.
quantile(X_bar, c(0.1, 0.9))



#The Normal approximation, which is the conclusion
#of the Central Limit Theorem substitutes the sampling distribution of
#the sample average by the Normal distribution with the same expectation and
#standard deviation. The percentiles are computed with the function \qnorm":

qnorm(c(0.1,0.9), mean(X_bar), sd(X_bar))

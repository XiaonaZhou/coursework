#Comparing sample average and sample medain as an estimator within normal(3,2)
X.bar <- rep(0,10^5)
X.median <- rep(0,10^5)
for (i in 1:10^5) {
  X <- rnorm(100, 3, sqrt(2))
  X.bar[i] <- mean(X)
  X.median[i] <- median(X)
}
mean(X.bar)
mean(X.median)
var(X.bar)
var(X.median)


#Comparing sample average and sample medain as an estimator within uniform(0.5,5.5)
X.bar <- rep(0,10^5)
X.median <- rep(0,10^5)
for (i in 1:10^5) {
  X <- runif(100, 0.5, 5.5)
  X.bar[i] <- mean(X)
  X.median[i] <- median(X)
}
mean(X.bar)
mean(X.median)
var(X.bar)
var(X.median)
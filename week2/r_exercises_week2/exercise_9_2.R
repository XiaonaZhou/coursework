mu <- 3.5
sigma1 <- 3
sigma2 <- 1.5
test.stat <- rep(0,10^5)
for (i in 1:10^5)
{
  s1 <- rnorm(29,mu,sigma1)
  s2 <- rnorm(21, mu, sigma2)
  s1.bar <- mean(s1)
  s2.bar <- mean(s2)
  s1.var <- var(s1)
  s2.var <- var(s2)
  test.stat[i] <- (s1.bar - s2.bar)/sqrt(s1.var/29 + s2.var/21)
}
quantile(test.stat, c(0.025,0.975))

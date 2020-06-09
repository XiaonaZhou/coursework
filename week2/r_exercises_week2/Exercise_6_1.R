#6.1.1
1 - pnorm(650,560,57)
#6.1.2
1 - pnorm(650, 630,61)
#6.1.3

#lower bound
qnorm(0.1,560,57)
#upper bound
qnorm(0.9,560,57)


#6.1.4

#lower bound
qnorm(0.1,630,61)
#upper bound
qnorm(0.9,630,61)


#6.2.1
1 - pbinom(11,27,0.32)

#6.2.2
mu <- 27*0.32
sig <- sqrt(27*0.32*(1 - 0.32))
1 - pnorm(11, mu, sig)
#6.2.3
1 - pnorm(11 + 0.5, mu, sig)
#6.2.4 
# ppois(x, mu)
1 - ppois(11,mu)

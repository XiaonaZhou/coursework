library(tidyverse)
ex2 <- read.csv('ex2.csv')
pop2 <- read.csv('pop2.csv')
ex2 %>% group_by(group) %>% 
  summarise(n_chol_leve <- n())
37/(3+37+110)
#or 
mean(ex2$group == 'HIGH')

mean(pop2$group == 'HIGH')


#3. sampling distribution

P.hat <- rep(0,10^5)

for (i in 1:10^5) {
  s <- sample(pop2$group,150)
  P.hat[i] <- mean(s == 'HIGH')
}

mean(P.hat)
var(P.hat)

p <- mean(pop2$group == 'HIGH')
p*(1-p)/150

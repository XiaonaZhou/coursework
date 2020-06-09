library(tidyverse)
magnets <- read.csv('magnets.csv')
head(magnets)
summary(magnets)
mean(magnets$change)
str(magnets)
# active <- magnets %>% 
#   filter(active == "\"1\"")
# inactive <- magnets %>% 
#   filter(active == "\"2\"")
magnets %>% 
  group_by(active) %>% 
  summarise(mean_change = mean(change),
            sd_change = sd(change))
magnets %>% 
  group_by(active) %>% 
ggplot(aes(x=active, y = change))+
  geom_boxplot()



library(tidyverse)
body_data <- read.table("body.dat.txt", header = FALSE)
weight <- body_data[,23]
height <- body_data[,24]
body_data_weight_height <- data.frame(weight,height)
body_data_weight_height

body_data_weight_height %>% 
  ggplot(aes(x=height, y = weight)) +
  geom_point()

model <- lm(body_data_weight_height$weight ~ body_data_weight_height$height)
summary(model)

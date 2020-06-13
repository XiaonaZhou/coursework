library(tidyverse)
babyweight_data <- read.table("babyweights.txt", header = T)
babyweight_data

# 6.1
model_weight_smoke <-lm(babyweight_data$bwt ~ babyweight_data$smoke)
summary(model_weight_smoke)

#6.2
model_weight_parity <-lm(babyweight_data$bwt ~ babyweight_data$parity)
summary(model_weight_parity)

#6.3
model <-lm(bwt~., babyweight_data)
summary(model)

# set up
RNGkind(sample.kind = "Rounding")
library(tidyverse)
library(scales)
library(modelr)
library(caTools)
trips_per_day <- read_tsv('trips_per_day.tsv')
head(trips_per_day)
trips_per_day %>% 
  ggplot(aes(x = tmin, y= num_trips))+
  geom_point() +
  xlab("Minimum temperature") +
  ylab("Daily trips") +
  scale_y_continuous()



# Cross-validation

# split the data into two parts, 80% train, 20% validate.
# we should have three sets of data, train, validation, test.
# in this case, think about we test the model on a new data set that we don't know yet

set.seed(42)

num_days <- nrow(trips_per_day)
frac_train <- 0.8
num_train <- floor(num_days*frac_train)
ndx <- sample(1:num_days, num_train, replace = F ) # get the index of rows that belong to train
trips_per_day_train <- trips_per_day[ndx,]
trips_per_day_validate <- trips_per_day[-ndx,]

# another method to split data
# use sample.split from caTools to split data
sample <- sample.split(trips_per_day$num_trips, SplitRatio = 0.8)
train <- subset(trips_per_day,sample == T)
validation <- subset(trips_per_day, sample == F)


# evaluste models from degrees 1 up tp 8

K <- 1:8
train_err <- c()
validate_err <- c()
for (k in K){
  # fit on the training data
  model <- lm(num_trips ~ poly(tmin, k, raw = T), trips_per_day_train)
  
  # evaluate on the training data
  # RMSE-> root mean square error. sqrt( (predict-truth)^2 /number of data )
  train_err[k] <- sqrt(mean((predict(model, trips_per_day_train)-trips_per_day_train$num_trips)^2))
  validate_err[k] <- sqrt(mean((predict(model, trips_per_day_validate ) - trips_per_day_validate$num_trips)^2))
}


plot_data <- data.frame(K, train_err, validate_err) %>% 
  gather("split","error",-K)

ggplot(plot_data, aes(x=K, y = error, color = split)) +
  geom_line() +
  scale_x_continuous(breaks = K) +
  xlab('Polynomial Degrees') +
  ylab('RMSE')

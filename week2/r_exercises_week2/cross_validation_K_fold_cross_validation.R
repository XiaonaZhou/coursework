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


# from the graph we see that validation error bottom up at for a fifth degree polynomial
# create a model at fifth degree polynomial

model <- lm(num_trips~poly(tmin, 5, raw = T), data = trips_per_day_train)

trips_per_day_train <- trips_per_day_train %>% 
  add_predictions(model) %>% 
  mutate(split = "train") # add a column that contains the string "train", like a label 

trips_per_day_validate <- trips_per_day_validate %>% 
  add_predictions(model) %>% # the predicted value are stored in a column called pred
  mutate(split = "Validate")
plot_data <- bind_rows(trips_per_day_train, trips_per_day_validate)
ggplot(plot_data, aes(x=tmin, y = num_trips)) +
  geom_point(aes(color = split)) +
  geom_line(aes(y=pred)) +
  xlab("Minimum temperature") +
  ylab("Daily trips") +
  scale_y_continuous()

# to make any comments on this model, we must run the model on a new set of data( not from training or validate)


## k-fold cross validation


set.seed(4)
num_folds <- 5
num_days <- nrow(trips_per_day)
ndx <- sample(1:num_days, num_days, replace=F)# we only want 292 data points, why? there are 365 from the data set
trips_per_day <- trips_per_day[ndx,] %>% 
  mutate(fold = (row_number()%%num_folds)+1)

nrow(trips_per_day)
K <- 1:8
avg_validate_err <- c()
se_validate_err <- c()
for (k in K){# k is for degree of polynomial
  validate_err <- c()
  for ( f in 1:num_folds){
    # training data
    trips_per_day_train <- filter(trips_per_day, fold !=f)# training data are those not in fold f
    # fit training data
    model <- lm(num_trips ~ poly(tmin, k, raw = T), data = trips_per_day_train)
    
    #get validation data
    trips_per_day_validate <- filter(trips_per_day, fold == f)
    # evaluate on the validation data
    validate_err[f] <- sqrt(mean((predict(model, trips_per_day_validate)-trips_per_day_validate$num_trips)^2))
    
  }
  avg_validate_err[k] <- mean(validate_err)
  se_validate_err[k] <- sd(validate_err)/ sqrt(num_folds)
}

# plot the results

plot_data <- data.frame(K, avg_validate_err, se_validate_err)

ggplot(plot_data, aes(x=K, y=avg_validate_err))+
  geom_pointrange(aes(ymin = avg_validate_err-se_validate_err,
                      ymax = avg_validate_err+se_validate_err,
                      color = avg_validate_err == min(avg_validate_err))) +
  geom_line(color = "red")+
  scale_x_continuous(breaks = 1:12)+
  theme(legend.position = "none")+
  xlab('Polynomial Degrees') +
  ylab('RMSE')


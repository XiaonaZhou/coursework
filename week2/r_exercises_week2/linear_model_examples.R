library(scales)
library(broom)
library(modelr)
library(tidyverse)
options(na.action = na.warn) # ignore the warnings
theme_set(theme_bw())
options(repr.plot.width=4, repr.plot.height=3)

#load data
users <- read_tsv(gzfile('users.tsv.gz'))
head(users)

# plot page view on a log scale

ggplot(users, aes(x = daily.views)) +
  geom_histogram(bins = 50) +
  scale_x_log10(label = comma, breaks = 10^(1:ceiling(log10(max(users$daily.views))))) +
                  scale_y_continuous(label = comma) +
                  xlab('Daily pageviews') +
                  ylab('')


# descriptives by age and gender

ggplot(data = users, aes(x = age, y = daily.views)) +
  geom_point()+
  facet_wrap(~ gender) +
  xlab('Age') +
  ylab('Daily pageviews')


# filter out users that have no views
users <- filter(users, daily.views>0)

# calculate and count median views
views_by_age_and_gender <- users %>% 
  filter(age <= 90) %>% 
  group_by(age, gender) %>% 
  summarize(count = n(),
            median_daily_views = median(daily.views))
head(views_by_age_and_gender)


# plot median and use count to indicate size

ggplot(views_by_age_and_gender, aes(x= age, y = median_daily_views, color = gender)) +
  geom_line(aes(linetype = gender)) +
  geom_point(aes(size = count)) +
  xlab('Age') +
  ylab('Daily pageviews') +
  scale_size_area(guide = F) +
  theme(legend.title = element_blank())


# focus people with age between 18 and 65
model_data <- filter(users, age>=18 & age<=65)

# use geom_smooth() 
ggplot(model_data, aes(x = age, y = daily.views)) +
  geom_smooth(method = "lm") +
  scale_y_log10(breaks = 1:100)

# use lm()

model <- lm(log10(daily.views)~age, model_data)
model
summary(model)


# use tidy() to turn summary(model) into data frame, and 
# glance() to see all statistic values, like r^2, p_value
tidy(model)
tidy(model)[1]
glance(model)


#R used our formula to convert the model_data data frame
# to a matrix
M <- model.matrix(log10(daily.views)~age, model_data)
head(M)
M

# two ways to get predicted value

# the old way that uses predict()
plot_data <- model_data %>% 
  distinct(age)
plot_data$predicted <- 10^predict(model, plot_data) # 10^  undo the log10
head(plot_data)


#modelr package 
# data_grid() takes all combinations of all of the unique values in each column
# provide to it(here just a list of every age)
# add_predictions() adds the prediction values

plot_data <- model_data %>% 
  data_grid(age) %>% 
  add_predictions(model) %>% 
  mutate(pred = 10^pred)
head(plot_data)

ggplot(plot_data, aes(x = age, y=pred)) +
  geom_line()

# plot the model with data

plot_data <- model_data %>% 
  group_by(age) %>% 
  summarize(count = n(),
            geom_mean_daily_view = 10^(mean(log10(daily.views)))) %>%
  add_predictions(model) %>% 
  mutate(pred = 10^pred)

ggplot(plot_data, aes(x = age, y = pred))+
  geom_line(aes(y = pred))+
  geom_point(aes(y = geom_mean_daily_view, size = count)) +
  scale_size_area(guide = F)# removed the lengend
  
# model with quadratic features

model <- lm(log10(daily.views)~age + I(age^2), model_data)
model

tidy(model)
glance(model)
M <- model.matrix(log10(daily.views) ~ age +I(age^2), model_data)
head(M)


#plot the data with new model

plot_data <- model_data %>% 
  group_by(age) %>% 
  summarize(count = n(),
            geom_mean_daily_views = 10^(mean(log10(daily.views)))) %>% 
  add_predictions(model) %>% 
  mutate(pred = 10^pred)

ggplot(plot_data, aes(x = age , y = pred )) +
  geom_line(aes(y=pred))+
  geom_point(aes(y = geom_mean_daily_views, size = count)) +
  scale_size_area(guide = F)


# model with age and gender with interactions

model <- lm(log10(daily.views) ~ gender*(age + I(age^2)), data = model_data)
model

# plot the new model

plot_data <- model_data %>% 
  group_by(age, gender) %>% 
  summarize(count = n(),
            geom_mean_daily_views = 10^(mean(log10(daily.views)))) %>% 
  add_predictions(model) %>% 
  mutate(pred = 10^pred)
ggplot(plot_data, aes(x = age, y = pred, color = gender))+
  geom_line(aes(y=pred))+
  geom_point(aes(y=geom_mean_daily_views, size = count))+
  scale_size_area(guide = F)


# evalution of the model

# plotting predicted bs actual values

ggplot(plot_data, aes(x = pred, y = geom_mean_daily_views, color = gender))+
  geom_point()+
  geom_abline(linetype = "dashed")+
  xlab('Predicted')+
  ylab('Actual')


ggplot(plot_data, aes(x = pred, y = geom_mean_daily_views, color = age))+
  geom_point()+
  geom_abline(linetype = "dashed")+
  xlab('Predicted')+
  ylab('Actual')+
  facet_wrap(~gender)


# RMSE

pred_actual <- model_data %>% 
  add_predictions(model) %>% 
  mutate(actual = log10(daily.views))
# rmse with different unite than y
pred_actual %>% 
  summarise(rmse = sqrt(mean((pred-actual)^2)))
#rmse with same unite as y

pred_actual %>% 
  summarize(rmse = sqrt(mean((10^pred-10^actual)^2)))

#R^2  is the square of correlation

pred_actual %>% 
  summarise(rmse = sqrt(mean((pred-actual)^2)),
                        cor = cor(pred,actual),
            r2 = cor^2)
# use rmse() and rsquare() from modelr

rmse(model, model_data)
rsquare(model, model_data)

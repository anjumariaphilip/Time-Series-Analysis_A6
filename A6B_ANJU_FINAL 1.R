library(tidyverse)
library(lubridate)
library(ggplot2)
library(forecast)
library(tseries)
library(caret)

library(rpart)
library(keras)
library(tensorflow)
install.packages("randomForest")
library(randomForest)
install.packages("keras")
data <- read.csv('C:\\Users\\HP\\Documents\\ipl\\BERGEPAINT.NS.csv')

data$Date <- as.Date(data$Date, format="%Y-%m-%d")

summary(data)
any(is.na(data))
boxplot(data$Close)

ggplot(data, aes(x=Date, y=Close)) +
  geom_line() +
  ggtitle("Closing Price Over Time") +
  xlab("Date") +
  ylab("Close Price")

monthly_data <- data %>%
  mutate(month = floor_date(Date, "month")) %>%
  group_by(month) %>%
  summarize(Close = mean(Close))

num_periods <- nrow(monthly_data)
num_periods
head(monthly_data)
if (num_periods >= 24) 
  ts_data <- ts(monthly_data$Close, frequency = 12, start = c(year(min(monthly_data$month)), month(min(monthly_data$month))))
  decomposed_data <- decompose(ts_data, type = "multiplicative")
  autoplot(decomposed_data)
  
  
  
  arima_model <- auto.arima(data$Close)
  arima_forecast <- forecast(arima_model, h = 90)
  autoplot(arima_forecast)
  
  sarima_model <- auto.arima(data$Close, seasonal = TRUE)
  sarima_forecast <- forecast(sarima_model, h = 90)
  autoplot(sarima_forecast)
  
  arima_monthly_model <- auto.arima(monthly_data$Close)
  arima_monthly_forecast <- forecast(arima_monthly_model, h = 12)
  autoplot(arima_monthly_forecast)
  
  
  data_scaled <- scale(data$Close)
  x_train <- array(data_scaled[1:(length(data_scaled) - 1)], dim = c(length(data_scaled) - 1, 1, 1))
  y_train <- data_scaled[2:length(data_scaled)]
  
  lstm_model <- keras_model_sequential() %>%
    layer_lstm(units = 50, return_sequences = TRUE, input_shape = c(1, 1)) %>%
    layer_lstm(units = 50) %>%
    layer_dense(units = 1)
  
  lstm_model %>% compile(
    loss = 'mean_squared_error',
    optimizer = 'adam'
  )
  
  lstm_model %>% fit(
    x_train, y_train,
    epochs = 100,
    batch_size = 32
  )
  
  lstm_predictions <- lstm_model %>% predict(x_train)
  lstm_predictions <- unscale(lstm_predictions, attr(data_scaled, "scaled:center"), attr(data_scaled, "scaled:scale"))
  
  plot(data$Date[2:length(data$Date)], lstm_predictions, type = "l", col = "red", xlab = "Date", ylab = "Close Price")
  lines(data$Date[2:length(data$Date)], data$Close[2:length(data$Date)], col = "blue")
  
  plot(data$Date[2:length(data$Date)], lstm_predictions, type = "l", col = "red", xlab = "Date", ylab = "Close Price")
  lines(data$Date[2:length(data$Date)], data$Close[2:length(data$Date)], col = "blue")  
  
  rf_model <- randomForest(Close ~ ., data = train_data)
  rf_predictions <- predict(rf_model, test_data)
  plot(test_data$Date, test_data$Close, type = "l", col = "blue")
  lines(test_data$Date, rf_predictions, col = "red")  
  
  tree_model <- rpart(Close ~ ., data = train_data)
  tree_predictions <- predict(tree_model, test_data)
  plot(test_data$Date, test_data$Close, type = "l", col = "blue")
  lines(test_data$Date, tree_predictions, col = "red")
  
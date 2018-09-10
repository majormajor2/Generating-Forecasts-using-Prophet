
#load packages

#install.packages("prophet")
#install.packages("Metrics")
#install.packages("glue")
#install.packages("caret")
#install.packages('readr')
#install.packages('reshape2')
#install.packages('dplyr')


library(prophet)
library(caret)
library(Metrics)
library(readr)
library(glue)
library(reshape2)
library(dplyr)


loadTrainingData = function(df1) {
  df = read.csv(file = "csVolume_train2.csv", sep = ";")
  return(df)
}

loadTestData = function(df2) {
  df = read.csv(file = "csVolume_test2.csv", sep = ";", strip.white = TRUE, blank.lines.skip = TRUE)
  return(df)
}

createModelObject = function(df1) {
  m = prophet(df1)
  return(m)
}

createForecast = function(model_object, days_to_forecast) {
  future = make_future_dataframe(model_object, periods = days_to_forecast)
  f = predict(m, future)
  return(f)
}

addPredictionsToTest = function(test_df, forecast_df, days_to_forecast) {
  test_df$predictions_prophet = tail(forecast_df$yhat, days_to_forecast)
  return(test_df$predictions_prophet)
}

errorMetric = function(y, yhat) {
  msg = 'The RMSE (Root Mean Squared Error) is {rmse(actual = y, predicted = yhat)}'
  return(glue(msg))
}

dailyError = function(y, yhat) {
  x = 100*(yhat - y)/y
  summary(x)
}

plotPredictions = function(test_df, id) {
  test_df_long = melt(test_df, id = id)
  test_df_long$ds = as.Date(test_df_long$ds)
  ggplot(test_df_long, aes(x = ds, y = value, colour = variable)) + geom_line()
}


saveOutput = function(forecast_df, days_to_forecast) {
  
  columns_to_keep = c('ds', 'trend', 'seasonal', 'yhat')
  df = select(forecast_df, columns_to_keep)
  df = tail(df, days_to_forecast)
  write.csv2(x = df, file = 'forecast.csv', row.names = FALSE)
  return(df)
}

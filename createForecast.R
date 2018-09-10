rm(list = ls())

#####------------------------------------------------------------------------------------------------------------#####

#USER INPUT REQUIRED

#Set wd e.g. "C:/Users/HamayunKhan/Documents/DemandForecasting" - everything should/will be saved here
setwd("C:/Users/HamayunKhan/Documents/DemandForecasting")
#Specify days i.e numbers of days to forecast
days = 30


#####------------------------------------------------------------------------------------------------------------#####

#SELECT ALL AND RUN
source("forecastFunctions.R")
#load data
df = loadTrainingData(df)
test = loadTestData(test)

#create model
m = createModelObject(df1 = df)
forecast = createForecast(model_object = m, days_to_forecast = days)

#evaluation of the test set
test$predictions_prophet = addPredictionsToTest(test_df = test, forecast_df = forecast, days_to_forecast = days)
errorMetric(y = test$y, yhat = test$predictions_prophet)
dailyError(y = test$y, yhat = test$predictions_prophet)

#output
output = saveOutput(forecast_df = forecast, days_to_forecast = days)

#####------------------------------------------------------------------------------------------------------------#####

#OPTIONAL - SELECT ALL AND RUN

#plots [optional]
plot(m, forecast)
prophet_plot_components(m, forecast)
plotPredictions(test_df = test, id = "ds")

#####------------------------------------------------------------------------------------------------------------#####



test_df_long = melt(test, id = "ds")
test_df_long$ds = as.Date(test_df_long$ds)
ggplot(test_df_long, aes(x = ds, y = value, colour = variable)) + geom_line()

test$predictions_prophet_2016 = as.numeric(test$predictions_prophet_2016)
str(test)

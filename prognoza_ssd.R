setwd("C:/Users/Alessia/Desktop/proiect_ssd")
date<- read.csv(file='setdedate.csv', header=TRUE, sep=',', dec='.', row.names = 1)
View(date) 


#install.packages('forecast')
#install.packages('ggplot2')
library(forecast)
library(ggplot2)

daily_sales <- aggregate(Sales ~ Date, data = date, sum)
sales_ts <- ts(daily_sales$Sales, frequency = 7)

#metoda 1: modelul arima(autoregressive integrated moving average)
fit_arima <- auto.arima(sales_ts)
forecast_arima <- forecast(fit_arima, h = 14)
plot(forecast_arima, main = "Prognoză Vânzări - Model ARIMA", 
     xlab = "Timp", ylab = "Vânzări", col = "purple4")

#metoda 2: modelul holt-winters(exponential soothing)
fit_hw <- HoltWinters(sales_ts, gamma = FALSE)
forecast_hw <- forecast(fit_hw, h = 14)
plot(forecast_hw, main = "Prognoză Vânzări - Model Holt-Winters", 
     xlab = "Timp", ylab = "Vânzări", col = "pink")

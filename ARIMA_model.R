# Install forecast and tseries and load them before arima model

# Sorting global trend by year
global_trend <- global_trend[order(global_trend$Year), ]

# Converting to time series
temp_ts <- ts(global_trend$GlobalAvgTemp, start = min(global_trend$Year), frequency = 1)

# Visualizing
plot(temp_ts, main = "Global Average Temperature Time Series", ylab = "Temp (°C)", xlab = "Year")

# test for stationary (arima model needs a stationary time series)
adf.test(temp_ts)
# if p-value is >0.05(series is non stationary), so we'll difference it
diff_ts <- diff(temp_ts)
adf.test(diff_ts)

# Fitting the arima model
fit_arima <- auto.arima(temp_ts)
summary(fit_arima)

# Forecasting temperatures of 10 years ahead
future_forecast <- forecast(fit_arima, h = 10)
plot(future_forecast, main = "ARIMA Forecast of Global Avg Temperature")

# accuracy of the model
accuracy(fit_arima)

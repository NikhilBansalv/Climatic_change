# Getting predictions of both models for the same period
arima_pred <- fitted(fit_arima)
lm_pred <- predict(lm_model, newdata = test_data)

comparison <- data.frame(
  Year = global_trend$Year,
  Actual = global_trend$GlobalAvgTemp,
  ARIMA_Pred = as.numeric(arima_pred)
)

# Add regression predictions (for the same years)
# Suppose lm_model is trained with annual_data (avg per region)
lm_pred_yearly <- annual_data %>%
  group_by(Year) %>%
  summarise(AvgTemp_Pred = mean(predict(lm_model, newdata = cur_data_all())))

comparison <- merge(comparison, lm_pred_yearly, by = "Year", all.x = TRUE)

# Computing comparision metrics
rmse_arima <- rmse(comparison$Actual, comparison$ARIMA_Pred)
rmse_reg <- rmse(comparison$Actual, comparison$AvgTemp_Pred)

cat("ARIMA RMSE:", rmse_arima, "\n")
cat("Regression RMSE:", rmse_reg, "\n")

ggplot(comparison, aes(x = Year)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = ARIMA_Pred, color = "ARIMA")) +
  geom_line(aes(y = AvgTemp_Pred, color = "Regression")) +
  labs(title = "ARIMA vs Regression Temperature Predictions",
       y = "Temperature (°C)", color = "Model")

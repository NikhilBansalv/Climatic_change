# Selecting only necessary columns
model_data <- annual_data %>% select(Year, Region, AvgTemp)

# Encoding region as a factor
model_data$Region <- as.factor(model_data$Region)

# Splitting into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(model_data), 0.8 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Fitting the regression model
lm_model <- lm(AvgTemp ~ poly(Year, 3) * Region, data = train_data) # Polynomial regression to capture non linear trends.
summary(lm_model)

# Making predictions
predictions <- predict(lm_model, newdata = test_data)

# Evaluating our model
rmse_value <- rmse(test_data$AvgTemp, predictions)
cat("RMSE:", rmse_value, "\n")

# Visualizing prediction vs actual
ggplot(data = test_data, aes(x = AvgTemp, y = predictions)) +
  geom_point(color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predicted vs Actual Temperatures", x = "Actual Temp", y = "Predicted Temp")

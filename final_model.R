folder_path <- "C:/Users/nikhi/OneDrive/Documents/global_dataset"
files <- list.files(path=folder_path,pattern="\\.csv$",full.names=TRUE)

data_list <- lapply(files,read.csv)
merged_data <- dplyr::bind_rows(data_list,.id="source")

# converting dt to proper date type

merged_data$dt <- as.Date(merged_data$dt)

# Checking for the missing values
summary(merged_data)
colSums(is.na(merged_data))

# Removing the rows where average temperature is not mentioned
cleaned_data <- merged_data %>% filter(!is.na(AverageTemperature))

# Removing the duplicates
cleaned_data <- cleaned_data %>% distinct()

# Converting country/city/state to a single religion column
cleaned_data <- cleaned_data %>% mutate(Region = coalesce(Country, City, State))
#Removing unnecessary columns
cleaned_data <- cleaned_data %>% select(-State,-LandAverageTemperature, -LandAverageTemperatureUncertainty, -X, -X.1,-X.2,-X.3,-X.4,-X.5)

# Creating different year and month columns for easier plotting
cleaned_data <- cleaned_data %>% mutate(Year = year(dt), Month = month(dt))
cleaned_data <- cleaned_data %>% drop_na() # install tidyr package

# Splitting data into groups by (year,region,source) and computing avg temp /year for each region-source combination
annual_data <- cleaned_data %>%
  group_by(Year, source, Region) %>%
  summarise(
    AvgTemp = mean(AverageTemperature, na.rm = TRUE),
    AvgUncertainty = mean(AverageTemperatureUncertainty, na.rm = TRUE),
    .groups = "drop"
  )

# EDA begins
# analyzing global trend over time
global_trend <- cleaned_data %>%
  group_by(Year) %>%
  summarise(GlobalAvgTemp = mean(AverageTemperature, na.rm = TRUE))
# To see the warming trend
ggplot(global_trend, aes(x = Year, y = GlobalAvgTemp)) +
  geom_line(color = "red") +
  labs(title = "Global Average Temperature Over Time",
       x = "Year", y = "Temperature (°C)")
# To compare temp of certain regions
region_trend <- annual_data %>%
  filter(Region %in% c("India", "United States", "China"))  # choose a few regions

ggplot(region_trend, aes(x = Year, y = AvgTemp, color = Region)) +
  geom_line() +
  labs(title = "Average Temperature Trends by Region",
       x = "Year", y = "Temperature (°C)")
# this is for whole global temp trend
ggplot(annual_data, aes(x = Year, y = AvgTemp, color = Region)) +
  geom_line(alpha = 0.4) +
  labs(title = "Temperature Trends Across All Regions",
       x = "Year", y = "Avg Temperature (°C)") +
  theme(legend.position = "none")

ggplot(cleaned_data, aes(x = AverageTemperature)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Average Temperatures",
       x = "Temperature (°C)", y = "Count")
# trend of temp vs year for some regions
heatmap_data <- annual_data %>%
  filter(Region %in% c("India", "United States", "China", "Brazil", "Australia"))

ggplot(heatmap_data, aes(x = Year, y = Region, fill = AvgTemp)) +
  geom_tile() +
  labs(title = "Heatmap of Avg Temperature by Region & Year") +
  scale_fill_gradient(low = "blue", high = "red")

region_name <- "India"
region_data <- annual_data %>%
  filter(Region == region_name) %>%
  arrange(Year)

# Create numeric time series
temp_series <- as.numeric(region_data$AvgTemp)

# Normalize data (helps training)
temp_mean <- mean(temp_series)
temp_sd <- sd(temp_series)
temp_series <- scale(temp_series)

# Convert to sequence data (X: last 'timesteps' years → y: next year)
timesteps <- 5
features <- 1
total_samples <- length(temp_series) - timesteps

X_seq <- array(0, dim = c(total_samples, timesteps, features))
y_seq <- array(0, dim = c(total_samples, features))

for (i in 1:total_samples) {
  X_seq[i,,1] <- temp_series[i:(i + timesteps - 1)]
  y_seq[i,1] <- temp_series[i + timesteps]
}

# Split into train/test sets (80/20)
train_size <- round(0.8 * total_samples)
X_train <- X_seq[1:train_size, , , drop = FALSE]
y_train <- y_seq[1:train_size, , drop = FALSE]
X_test  <- X_seq[(train_size+1):total_samples, , , drop = FALSE]
y_test  <- y_seq[(train_size+1):total_samples, , drop = FALSE]

# ===============================================================
# 4. BUILD SEQ2SEQ MODEL (ENCODER–DECODER)
# ===============================================================

latent_dim <- 64

# Encoder
encoder_inputs <- layer_input(shape = c(timesteps, features), name = "encoder_input")
encoder_lstm <- layer_lstm(units = latent_dim, return_state = TRUE, name = "encoder_lstm")
encoder_outputs <- encoder_lstm(encoder_inputs)
state_h <- encoder_outputs[[2]]
state_c <- encoder_outputs[[3]]
encoder_states <- list(state_h, state_c)

# Decoder
decoder_inputs <- layer_input(shape = c(timesteps, features), name = "decoder_input")
decoder_lstm <- layer_lstm(units = latent_dim, return_sequences = TRUE, return_state = TRUE, name = "decoder_lstm")
decoder_outputs <- decoder_lstm(decoder_inputs, initial_state = encoder_states)
decoder_outputs_seq <- decoder_outputs[[1]]

decoder_dense <- layer_dense(units = features, activation = "linear", name = "decoder_output")
decoder_outputs_final <- decoder_dense(decoder_outputs_seq)

# Seq2Seq Model
seq2seq_model <- keras_model(inputs = list(encoder_inputs, decoder_inputs),
                             outputs = decoder_outputs_final)

seq2seq_model$compile(
  optimizer = 'adam',
  loss = 'mse'
)
y = array(rep(y_train, timesteps), dim = c(dim(y_train)[1], timesteps, 1))
seq2seq_model$fit(
  x = list(X_train, X_train),
  y = y,
  epochs = 100L,
  batch_size = 16L,
  validation_split = 0.2,
  verbose = 1L
)

summary(seq2seq_model)

# ===============================================================
# 5. TRAIN THE MODEL
# ===============================================================

# For training, we can set decoder_input same as encoder_input (teacher forcing)
y <- array(
  rep(y_train, timesteps),
  dim = as.integer(c(dim(y_train)[1], timesteps, 1))
)

history <- seq2seq_model$fit(
  x = list(X_train, X_train),
  y = y,
  epochs = as.integer(100),
  batch_size = as.integer(16),
  validation_split = 0.2,
  verbose = as.integer(1)
)

# ===============================================================
# 6. PREDICTION AND VISUALIZATION
# ===============================================================

predicted <- seq2seq_model$predict(list(X_test, X_test))
pred_next <- predicted[,timesteps,1]  # last timestep output

# Denormalize
pred_next <- pred_next * temp_sd + temp_mean
actual_next <- y_test[,1] * temp_sd + temp_mean

# Plot actual vs predicted
df_pred <- data.frame(
  Year = region_data$Year[(train_size+timesteps+1):(length(region_data$Year))],
  Actual = actual_next,
  Predicted = pred_next
)

ggplot(df_pred, aes(x = Year)) +
  geom_line(aes(y = Actual), color = "blue", size = 1.2, linetype = "solid") +
  geom_line(aes(y = Predicted), color = "red", size = 1.2, linetype = "dotdash") +
  labs(
    title = paste("Seq2Seq Temperature Forecast -", region_name),
    y = "Average Temperature (°C)"
  ) +
  theme_minimal()


loss <- history$history$loss
val_loss <- history$history$val_loss
epochs <- 1:length(loss)

# Plot Training Loss
plot(epochs, loss, type = "l", col = "blue", lwd = 2,
     xlab = "Epochs", ylab = "Loss",
     main = "Training and Validation Loss",
     ylim = range(c(loss, val_loss)))  # ensure both fit in same scale

# Add Validation Loss
lines(epochs, val_loss, col = "red", lwd = 2, lty = 2)

# Add Legend
legend("topright",
       legend = c("Training Loss", "Validation Loss"),
       col = c("blue", "red"),
       lwd = 2,
       lty = c(1, 2))

rmse <- sqrt(mean((actual_next - pred_next)^2))
mae <- mean(abs(actual_next - pred_next))
r2 <- 1 - (sum((actual_next - pred_next)^2) / sum((actual_next - mean(actual_next))^2))

cat("RMSE:", rmse, "\nMAE:", mae, "\nR-squared:", r2)

df_metrics <- data.frame(
  Metric = c("RMSE", "MAE", "R-squared"),
  Value = c(rmse, mae, r2)
)

ggplot(df_metrics, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(Value, 3)), vjust = -0.5, size = 5) +
  labs(title = "Model Accuracy Metrics") +
  theme_minimal()

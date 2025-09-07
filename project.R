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

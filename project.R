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

# Splitting data into groups by (year,region,source) and computing avg temp /year for each region-source combination
annual_data <- cleaned_data %>%
  group_by(Year, source, Region) %>%
  summarise(
    AvgTemp = mean(AverageTemperature, na.rm = TRUE),
    AvgUncertainty = mean(AverageTemperatureUncertainty, na.rm = TRUE),
    .groups = "drop"
  )

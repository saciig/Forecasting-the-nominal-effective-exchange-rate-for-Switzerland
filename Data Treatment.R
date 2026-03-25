# ===============================================================
# DATA TREATMENT SCRIPT
# ===============================================================
# This script handles the loading, cleaning, preprocessing, and 
# treatment of raw data for the Forecasting Project. It ensures 
# all steps are reproducible and well-documented.

# Note: The data covers monthly observations from 01/01/1999 to 
# 01/09/2024. Missing values are filled using interpolation and 
# forecasting techniques, with teacher-approved methods such as 
# the "forecast" package.
# ===============================================================

# TABLE OF CONTENTS
# 1. HELPER FUNCTIONS
# 2. DATA LOADING AND FORMATTING
# 3. TRADE SHARES CALCULATION
# 4. INDUSTRIAL PRODUCTION INTERPOLATION AND FORECASTING
# ===============================================================


# ---------------------------------------------------------------
# 1. HELPER FUNCTIONS
# ---------------------------------------------------------------
# These functions standardize date formats and convert numeric 
# columns to a compatible format for processing.
# ===============================================================

# Convert date columns to standard Date format
convert_date <- function(date_column) {
  formats <- c("%d/%m/%Y", "%Y-%m-%d", "%m/%d/%Y")
  for (fmt in formats) {
    date_converted <- as.Date(date_column, format = fmt)
    if (all(!is.na(date_converted) | is.na(date_column))) {
      return(date_converted)
    }
  }
  warning("Date conversion failed for some entries.")
  return(as.Date(date_column))  # Returns as Date but may contain NAs
}

# Format data: convert dates and numeric columns
format_data <- function(data, date_col = "Date") {
  if (date_col %in% colnames(data)) {
    data[[date_col]] <- convert_date(data[[date_col]])
  }
  data[] <- lapply(data, function(col) {
    if (is.character(col)) {
      as.numeric(gsub(",", ".", col))  # Replace commas with periods
    } else {
      col
    }
  })
  return(data)
}


# ---------------------------------------------------------------
# 2. DATA LOADING AND FORMATTING
# ---------------------------------------------------------------
# This section handles the loading and formatting of datasets 
# from various sources, including Trade Shares, Exchange Rates, 
# Inflation, and Industrial Production indices.
# ===============================================================

# 2.1 LOAD TRADE SHARES DATA
# ---------------------------------------------------------------
# Data Source: Swiss Federal Statistical Office (SFO)
# Trade shares were derived using the formula:
#   (Exports + Imports for a country) / (Total Exports + Total Imports)
# This dataset provides annual trade shares for USA, Euro Area, 
# China, and UK, which will later be interpolated to monthly values.
trade_shares_data <- format_data(
  read.csv("Trade_Shares.csv", sep = ";"), 
  date_col = "Date"
)

# 2.2 LOAD NOMINAL EXCHANGE RATES DATA
# ---------------------------------------------------------------
# Data Source: Investing.com
# This dataset contains monthly nominal exchange rates for key 
# trading partners, grouped into a single dataset for analysis.
nominal_exchrate_data <- format_data(
  read.csv("Nominal_Exchange_Rate.csv", sep = ";"), 
  date_col = "Date"
)

# 2.3 LOAD INFLATION DATA
# ---------------------------------------------------------------
# Data Source: Federal Reserve Economic Data (FRED)
# This dataset includes year-over-year percentage changes in 
# inflation for the USA, Euro Area, China, and UK, combined 
# into a single dataset for analysis
inflation_data <- format_data(
  read.csv("Inflation.csv", sep = ";"), 
  date_col = "Date"
)

# 2.4 LOAD QUARTERLY INDUSTRIAL PRODUCTION DATA (SWITZERLAND)
# ---------------------------------------------------------------
# Data Source: Federal Reserve Economic Data (FRED)
# This dataset contains Switzerland's quarterly industrial 
# production index, with Base 100 set to March 2021.
ip_quarterly_FRED <- format_data(
  read_excel("Industrial Index Switzerland Quarterly FRED.xlsx", 
             sheet = "Data"), 
  date_col = "Dates"
)

# 2.5 LOAD MONTHLY INDUSTRIAL PRODUCTION DATA (SWITZERLAND)
# ---------------------------------------------------------------
# Data Source: Swiss Statistical Office (SFO)
# Switzerland's monthly industrial production index is provided 
# with Base 100 set to March 2021 for comparability.
ip_monthly_CONF <- format_data(
  read_excel("Industrial Index Switzerland Monthly CONF.xlsx", 
             sheet = "Data"), 
  date_col = "Dates"
)

# 2.6 LOAD INDUSTRIAL PRODUCTION DATA (CHINA)
# ---------------------------------------------------------------
# Data Source: Federal Reserve Economic Data (FRED)
# China's monthly industrial production index, with Base 100 
# set to March 2021, is included for further analysis.
ip_china <- format_data(
  read.csv("Industrial_Production_China.csv", sep = ";"), 
  date_col = "Date"
)

# 2.7 LOAD INDUSTRIAL PRODUCTION DATA INTERPOLATED
# ---------------------------------------------------------------
# Data Source: Federal Reserve Economic Data (FRED)
# This dataset includes the monthly industrial production indices 
# for all countries, standardized to a base of 100 as of March 2021. 
# Interpolation for Switzerland and China is performed within the code.
ip_interpolated_data <- format_data(
  read.csv("Industrial_Production_Interpolated.csv", sep = ";"), date_col = "Date"
)

# 2.8. LOAD INDUSTRIAL PRODUCTION DATA INTERPOLATED AND FORECASTED
# ---------------------------------------------------------------
# Data Source: Federal Reserve Economic Data (FRED)
# This dataset includes the monthly industrial production indices 
# for all countries, standardized to a base of 100 as of March 2021. 
# Forecasting for Switzerland, Euro and UK is performed within the code.
ip_data <- format_data(
  read.csv("Industrial_Production_Interpolated_Forecasted.csv", sep = ";"), 
  date_col = "Date"
)


# ---------------------------------------------------------------
# 3. TRADE SHARES CALCULATION
# ---------------------------------------------------------------
# This section calculates, normalizes, and interpolates trade 
# shares data for the Forecasting Project. The steps include:
#   - Normalizing trade shares to sum to 1 for each row.
#   - Expanding annual trade shares data to a monthly frequency.
#   - Linearly interpolating trade shares between years.
# ===============================================================

# 3.1 NORMALIZE TRADE SHARES
# ---------------------------------------------------------------
# Ensure that the sum of trade shares for USA, Euro, UK, and 
# China equals 1 for each row in the dataset.

for (i in 1:nrow(trade_shares_data)) {
  row_sum <- sum(trade_shares_data[i, c("USA", "Euro", "UK", "China")], na.rm = TRUE)
  
  if (row_sum > 0) {  # Avoid division by zero
    trade_shares_data[i, "USA"] <- trade_shares_data[i, "USA"] / row_sum
    trade_shares_data[i, "Euro"] <- trade_shares_data[i, "Euro"] / row_sum
    trade_shares_data[i, "UK"] <- trade_shares_data[i, "UK"] / row_sum
    trade_shares_data[i, "China"] <- trade_shares_data[i, "China"] / row_sum
  }
}

# 3.2 EXPAND TRADE SHARES TO MONTHLY FREQUENCY
# ---------------------------------------------------------------
# Create a sequence of monthly dates from 1999-01-01 to 2024-09-01.
# Initialize the expanded dataset with NA values for trade shares.

expanded_dates <- seq(as.Date("1999-01-01"), as.Date("2024-09-01"), by = "month")
expanded_data <- data.frame(Date = expanded_dates, 
                            USA = NA, Euro = NA, UK = NA, China = NA)

# 3.3 LINEAR INTERPOLATION OF TRADE SHARES
# ---------------------------------------------------------------
# Fill in monthly values for trade shares by interpolating between 
# annual data points for each country. Values for 2023-01 to 2024-09 
# are assumed constant (random walk assumption).

for (col in c("USA", "Euro", "UK", "China")) {
  annual_values <- trade_shares_data[[col]]  # Extract annual values
  
  # Initialize interpolated values
  interpolated_values <- numeric(length(expanded_dates))
  
  # Perform linear interpolation
  for (i in 1:(length(annual_values) - 1)) {
    start_value <- annual_values[i]
    end_value <- annual_values[i + 1]
    
    if (!is.na(start_value) && !is.na(end_value)) {
      monthly_increment <- (end_value - start_value) / 12
      for (j in 0:11) {
        index <- (i - 1) * 12 + j + 1
        if (index <= length(interpolated_values)) {
          interpolated_values[index] <- start_value + j * monthly_increment
        }
      }
    }
  }
  
  # Apply random walk assumption for the last period
  last_value <- annual_values[length(annual_values)]
  start_index <- (length(annual_values) - 1) * 12 + 1
  interpolated_values[start_index:length(interpolated_values)] <- last_value
  
  # Assign interpolated values to the expanded dataset
  expanded_data[[col]] <- interpolated_values
}

# 3.4 REPLACE ORIGINAL DATA AND CLEAN UP
# ---------------------------------------------------------------
# Replace the original trade shares dataset with the expanded 
# monthly dataset. Clean up temporary objects to free memory.

trade_shares_data <- expanded_data
rm(expanded_data)


# ---------------------------------------------------------------
# 4. INDUSTRIAL PRODUCTION INTERPOLATION AND FORECASTING
# ---------------------------------------------------------------
# This section performs interpolation and forecasting for the 
# industrial production index (IPI). The steps include:
#   - Interpolating Switzerland's quarterly data to monthly data.
#   - Handling missing values in other datasets with linear 
#     interpolation.
#   - Using ARIMA models for forecasting missing future values.
# ===============================================================

# 4.1 INTERPOLATION: SWITZERLAND
# ---------------------------------------------------------------
# Apply the Denton-Cholette method to interpolate Switzerland's 
# quarterly industrial production index into a monthly series.
# The process ensures that the monthly series matches quarterly 
# benchmarks while maintaining a smooth monthly trend.

# Set the date range for interpolation
start_date <- as.Date("1999-01-01")
end_date <- as.Date("2023-12-01")

# ---------------------------------------------------------------
# Extend monthly data
# ---------------------------------------------------------------
# Extend Switzerland's monthly data by adding missing dates 
# from 1999-01-01 to 2010-09-01 with NA values.

missing_dates <- seq(start_date, end_date, by = "month")
missing_data <- data.frame(Dates = missing_dates[missing_dates < as.Date("2010-10-01")], 
                           Values = NA)

# Combine missing data with existing monthly data
data_monthly_extended <- rbind(
  missing_data, 
  ip_monthly_CONF[ip_monthly_CONF$Dates >= as.Date("2010-10-01"), ]
)

# Restrict the extended data to the specified end date
data_monthly_extended <- data_monthly_extended[
  data_monthly_extended$Dates <= end_date, 
]

# Replace missing values with the mean of the existing data
data_monthly_extended$Values[is.na(data_monthly_extended$Values)] <- 
  mean(ip_monthly_CONF$Values, na.rm = TRUE)

# ---------------------------------------------------------------
# Filter quarterly data
# ---------------------------------------------------------------
# Filter Switzerland's quarterly industrial production index 
# to match the same date range as the monthly data.

data_quarterly_filtered <- ip_quarterly_FRED[
  ip_quarterly_FRED$Dates >= start_date & ip_quarterly_FRED$Dates <= end_date, 
]

# ---------------------------------------------------------------
# Apply Denton-Cholette method
# ---------------------------------------------------------------
# Interpolate Switzerland’s quarterly data to a monthly series 
# using the Denton-Cholette method from the tempdisagg package.

# Create vectors for interpolation
monthly_values <- data_monthly_extended$Values
quarterly_values <- data_quarterly_filtered$Values

# Ensure compatibility between monthly and quarterly data
if (length(monthly_values) != length(quarterly_values) * 3) {
  stop("The length of monthly values must be exactly three times the quarterly values.")
}

# Apply the Denton-Cholette method
denton_model <- td(
  formula = quarterly_values ~ 0 + monthly_values,
  method = "denton-cholette",
  to = 3, 
  conversion = "average"
)

# Generate interpolated monthly values
monthly_interpolated <- predict(denton_model)

# ---------------------------------------------------------------
# Plot interpolated series
# ---------------------------------------------------------------
# Plot the interpolated monthly series alongside the original 
# quarterly data for comparison.

monthly_dates <- seq(start_date, end_date, by = "month")
quarterly_dates <- data_quarterly_filtered$Dates

plot(monthly_dates, monthly_interpolated, type = "l", col = "blue", lwd = 2,
     main = "Interpolated Monthly and Quarterly Series Using Denton-Cholette",
     xlab = "Date", ylab = "Industrial Production Index")

points(quarterly_dates, quarterly_values, col = "red", pch = 19)

legend("topleft", legend = c("Monthly Interpolated", "Quarterly Original"),
       col = c("blue", "red"), lty = c(1, NA), pch = c(NA, 19), lwd = 2)

# ---------------------------------------------------------------
# Extend forecast to 2024
# ---------------------------------------------------------------
# Forecast Switzerland's industrial production index to 2024-06-01 
# using recent growth rates of the monthly series.

extended_dates <- seq(as.Date("2024-01-01"), as.Date("2024-06-01"), by = "month")
growth_rates <- diff(ip_monthly_CONF$Values) / head(ip_monthly_CONF$Values, -1)

last_value <- tail(monthly_interpolated, 1)
forecast_values <- last_value * cumprod(1 + tail(growth_rates, length(extended_dates)))

# Combine interpolated and forecasted values
final_monthly_values <- c(monthly_interpolated, forecast_values)
all_dates <- c(seq(start_date, end_date, by = "month"), extended_dates)

# Plot interpolated and forecasted series
plot(all_dates, final_monthly_values, type = "l", col = "blue", lwd = 2,
     main = "Interpolated Monthly Series with Extended Forecast",
     xlab = "Date", ylab = "Industrial Production Index")

points(quarterly_dates, quarterly_values, col = "red", pch = 19)

legend("topleft", legend = c("Monthly Interpolated & Forecasted", "Quarterly Original"),
       col = c("blue", "red"), lty = c(1, NA), pch = c(NA, 19), lwd = 2)

# Create a data frame with all dates and final monthly values
ip_switzerland_interpolated <- data.frame(Date = all_dates, 
                                          Industrial_Production_Index = final_monthly_values)

# ---------------------------------------------------------------
# Validate interpolation method
# ---------------------------------------------------------------
# Validate the interpolation by comparing quarterly averages of 
# the interpolated series to the original quarterly values.

# Restrict interpolated monthly values to the original end date
final_monthly_values_original <- monthly_interpolated

# Calculate quarterly averages from the interpolated series
interpolated_quarterly_averages <- sapply(
  seq(1, length(final_monthly_values_original), by = 3), 
  function(i) mean(final_monthly_values_original[i:(i + 2)], na.rm = TRUE)
)

# Create a comparison table
comparison <- data.frame(
  Quarter = data_quarterly_filtered$Dates,
  Original_Quarterly = quarterly_values,
  Interpolated_Quarterly_Avg = interpolated_quarterly_averages
)

# Check for discrepancies and display results
comparison$Match <- abs(comparison$Original_Quarterly - comparison$Interpolated_Quarterly_Avg) < 1e-6
print(comparison)

if (all(comparison$Match)) {
  print("The Denton-Cholette interpolation respected the quarterly constraint.")
} else {
  print("There are discrepancies between the interpolated and original quarterly values.")
}

# ---------------------------------------------------------------
# Consistency check (2010-2024)
# ---------------------------------------------------------------
# Compare the interpolated series with the original monthly data 
# for consistency from 2010 to 2024.

# Filter data to include only dates from 2010 to 2024
start_plot_date <- as.Date("2010-01-01")
end_plot_date <- as.Date("2024-06-01")

# Extract the relevant portion of the original monthly series and final series
original_monthly_2010_2024 <- ip_monthly_CONF[
  ip_monthly_CONF$Dates >= start_plot_date & ip_monthly_CONF$Dates <= end_plot_date, 
]
final_interpolated_2010_2024 <- ip_switzerland_interpolated[
  ip_switzerland_interpolated$Date >= start_plot_date & ip_switzerland_interpolated$Date <= end_plot_date, 
]

# Plot the original and interpolated series
plot(
  original_monthly_2010_2024$Dates, 
  original_monthly_2010_2024$Values, 
  type = "l", col = "red", lwd = 2,
  main = "Comparison of Original Monthly Series and Final Interpolated Series (2010-2024)",
  xlab = "Date", ylab = "Industrial Production Index"
)
lines(
  final_interpolated_2010_2024$Date, 
  final_interpolated_2010_2024$Industrial_Production_Index, 
  col = "blue", lwd = 2
)

# Add a legend
legend(
  "topleft", 
  legend = c("Original Monthly Series", "Final Interpolated Series"),
  col = c("red", "blue"), lty = 1, lwd = 2
)

# Add a legend
legend("topleft", legend = c("Original Monthly Series", "Final Interpolated Series"),
       col = c("red", "blue"), lty = 1, lwd = 2)


# ---------------------------------------------------------------
# Save final dataset
# ---------------------------------------------------------------
# Save the interpolated and extended monthly series to a CSV file 
# for further analysis.

ip_switzerland_interpolated <- data.frame(
  Date = all_dates, 
  Industrial_Production_Index = final_monthly_values
)

write.csv(ip_switzerland_interpolated, "Industrial_Production_Switzerland_Monthly.csv", row.names = FALSE)

# ---------------------------------------------------------------
# 4.2 INTERPOLATION: CHINA
# ---------------------------------------------------------------
# The industrial production data for China contained missing 
# values. We used linear interpolation to estimate the missing 
# points, ensuring continuity in the time series.

# Count the number of missing values in China's industrial production
num_na <- sum(is.na(ip_china$China))
cat("Number of missing values in China's industrial production data:", num_na, "\n")

# Define the column with missing values
china <- ip_china$China

# Perform linear interpolation for missing values
for (i in which(is.na(china))) {
  # Find the previous and next non-NA values
  prev_index <- max(which(!is.na(china[1:i])))
  next_index <- min(which(!is.na(china[(i+1):length(china)])) + i)
  
  # Interpolate linearly
  china[i] <- china[prev_index] + ((china[next_index] - china[prev_index]) / 
                                     (next_index - prev_index)) * (i - prev_index)
}

# Update the column in the data frame
ip_china$China <- china

# ---------------------------------------------------------------
# 4.3 FORECASTING: INDUSTRIAL PRODUCTION
# ---------------------------------------------------------------
# ARIMA models are used to forecast missing future values in the 
# industrial production data for Switzerland, UK, and the Euro Area.
# The `forecast` package is employed to ensure robust forecasting.

# Prepare the data by removing unwanted columns
selected_data <- ip_interpolated_data[, -c(1, 8, 9)]

# Convert the date column to Date format
selected_data$Date <- as.Date(selected_data$Date, format = "%d/%m/%Y")

# Convert numeric columns from character to numeric after replacing commas with periods
columns_to_convert <- c("Switzerland", "USA", "China", "Euro.Area", "UK")
for (col in columns_to_convert) {
  selected_data[[col]] <- as.numeric(gsub(",", ".", selected_data[[col]]))
}

# ---------------------------------------------------------------
# Create individual time series for each region
# ---------------------------------------------------------------
# Define a helper function to extract series up to the last non-NA value
create_series_until_na <- function(data, country_col) {
  non_na_index <- which(!is.na(data[[country_col]]))
  end_index <- max(non_na_index)
  data[1:end_index, c("Date", country_col)]
}

switzerland_series <- create_series_until_na(selected_data, "Switzerland")
uk_series <- create_series_until_na(selected_data, "UK")
euro_area_series <- create_series_until_na(selected_data, "Euro.Area")

# ---------------------------------------------------------------
# FORECAST: SWITZERLAND
# ---------------------------------------------------------------
# Use ARIMA to forecast Switzerland's industrial production index

switzerland_arima <- auto.arima(switzerland_series$Switzerland)
summary(switzerland_arima)

# Calculate the forecast horizon
forecast_end_date <- as.Date("2024-09-01")
last_date_Switzerland <- max(switzerland_series$Date)
forecast_horizon_Switzerland <- 12 * (as.numeric(format(forecast_end_date, "%Y")) - 
                                        as.numeric(format(last_date_Switzerland, "%Y"))) + 
  (as.numeric(format(forecast_end_date, "%m")) - 
     as.numeric(format(last_date_Switzerland, "%m")))

# Generate forecasts
switzerland_forecast <- forecast(switzerland_arima, h = forecast_horizon_Switzerland)

# ---------------------------------------------------------------
# FORECAST: UK
# ---------------------------------------------------------------
# Use ARIMA to forecast the UK's industrial production index

UK_arima <- auto.arima(uk_series$UK)
summary(UK_arima)

# Calculate the forecast horizon
last_date_UK <- max(uk_series$Date)
forecast_horizon_UK <- 12 * (as.numeric(format(forecast_end_date, "%Y")) - 
                               as.numeric(format(last_date_UK, "%Y"))) + 
  (as.numeric(format(forecast_end_date, "%m")) - 
     as.numeric(format(last_date_UK, "%m")))

# Generate forecasts
UK_forecast <- forecast(UK_arima, h = forecast_horizon_UK)

# ---------------------------------------------------------------
# FORECAST: EURO AREA
# ---------------------------------------------------------------
# Use ARIMA to forecast the Euro Area's industrial production index

euro_area_arima <- auto.arima(euro_area_series$Euro.Area)
summary(euro_area_arima)

# Calculate the forecast horizon
last_date_euro_area <- max(euro_area_series$Date)
forecast_horizon_euro_area <- 12 * (as.numeric(format(forecast_end_date, "%Y")) - 
                                      as.numeric(format(last_date_euro_area, "%Y"))) + 
  (as.numeric(format(forecast_end_date, "%m")) - 
     as.numeric(format(last_date_euro_area, "%m")))

# Generate forecasts
Euro_area_forecast <- forecast(euro_area_arima, h = forecast_horizon_euro_area)

# ---------------------------------------------------------------
# COMBINE FORECASTED VALUES
# ---------------------------------------------------------------
# Integrate forecasted values into the dataset

selected_data$Switzerland[is.na(selected_data$Switzerland)] <- switzerland_forecast$mean
selected_data$UK[is.na(selected_data$UK)] <- UK_forecast$mean
selected_data$Euro.Area[is.na(selected_data$Euro.Area)] <- Euro_area_forecast$mean

# Save the completed dataset to a CSV file
write.csv(selected_data, "industrial_production_data.csv", row.names = FALSE)



# ---------------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------------
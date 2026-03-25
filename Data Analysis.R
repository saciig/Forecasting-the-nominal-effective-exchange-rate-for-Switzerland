# ===============================================================
# DATA ANALYSIS SCRIPT
# ===============================================================
# This script performs stationarity testing, seasonality adjustment, 
# and visualization of key variables for the Forecasting Project. 
# It includes:
#   - Descriptive statistics and visualizations.
#   - Stationarity tests (ADF tests).
#   - Seasonal adjustments (X-13ARIMA-SEATS).
#   - Calculation of Output Gap using the HP Filter.

# Note: Cointegration testing is skipped as unnecessary for this 
# project. All variables are confirmed to be stationary after first 
# differencing, and the focus is on short- to medium-term forecasts 
# (1-month, 6-month, 12-month, and 18-month horizons). Direct 
# forecasting with rolling window estimation is used, reducing the 
# relevance of long-run equilibrium relationships.
# ===============================================================

# TABLE OF CONTENTS
# 1. NOMINAL EFFECTIVE EXCHANGE RATE (NEER)
#    1.1 Data Preparation and Visualization
#    1.2 Stationarity Testing
#    1.3 Seasonal Adjustment
# 2. INFLATION
#    2.1 Data Preparation and Visualization
#    2.2 Stationarity Testing
#    2.3 Seasonal Adjustment
# 3. OUTPUT GAP
#    3.1 Data Preparation and Visualization
#    3.2 Stationarity Testing
#    3.3 Seasonal Adjustment
#    3.4 HP Filter and Output Gap Calculation
# ===============================================================


# ---------------------------------------------------------------
# 1. NOMINAL EFFECTIVE EXCHANGE RATE (NEER)
# ---------------------------------------------------------------
# This section examines the Nominal Effective Exchange Rate (NEER), 
# representing the relative strength of the Swiss franc against 
# a basket of currencies weighted by trade shares.
# ===============================================================

# 1.1 DATA PREPARATION AND VISUALIZATION
# ---------------------------------------------------------------
# Convert nominal exchange rate columns to time series format and 
# visualize their trends.

# Convert exchange rates to time series
exchange_rate_usd_ts <- ts(nominal_exchrate_data$USD.CHF, frequency = 12, start = c(1999, 1))
exchange_rate_euro_ts <- ts(nominal_exchrate_data$EURO.CHF, frequency = 12, start = c(1999, 1))
exchange_rate_gbp_ts <- ts(nominal_exchrate_data$GPB.CHF, frequency = 12, start = c(1999, 1))
exchange_rate_yuan_ts <- ts(nominal_exchrate_data$Yuan.CHF, frequency = 12, start = c(1999, 1))

# Descriptive statistics for each exchange rate
cat("Descriptive Statistics:\n")
usd_stats <- as.list(summary(exchange_rate_usd_ts))
euro_stats <- as.list(summary(exchange_rate_euro_ts))
gbp_stats <- as.list(summary(exchange_rate_gbp_ts))
yuan_stats <- as.list(summary(exchange_rate_yuan_ts))

# Combine statistics into a data frame
summary_stats <- data.frame(
  Metric = names(usd_stats),
  USD_CHF = unlist(usd_stats),
  EURO_CHF = unlist(euro_stats),
  GBP_CHF = unlist(gbp_stats),
  Yuan_CHF = unlist(yuan_stats)
)
print(summary_stats)

# Visualize nominal exchange rates
data_plot <- data.frame(
  Date = nominal_exchrate_data$Date,
  USD_CHF = nominal_exchrate_data$USD.CHF,
  EURO_CHF = nominal_exchrate_data$EURO.CHF,
  GBP_CHF = nominal_exchrate_data$GPB.CHF,
  Yuan_CHF = nominal_exchrate_data$Yuan.CHF
)

# Plotting USD/CHF
ggplot(data_plot, aes(Date, USD_CHF)) +
  geom_line(color = "darkblue") +
  ggtitle("USD/CHF Exchange Rate") +
  ylab("Exchange Rate") +
  theme_minimal()

# Plotting EUR/CHF
ggplot(data_plot, aes(Date, EURO_CHF)) +
  geom_line(color = "darkgreen") +
  ggtitle("EUR/CHF Exchange Rate") +
  ylab("Exchange Rate") +
  theme_minimal()

# Plotting GBP/CHF
ggplot(data_plot, aes(Date, GBP_CHF)) +
  geom_line(color = "darkred") +
  ggtitle("GBP/CHF Exchange Rate") +
  ylab("Exchange Rate") +
  theme_minimal()

# Plotting Yuan/CHF
ggplot(data_plot, aes(Date, Yuan_CHF)) +
  geom_line(color = "brown") +
  ggtitle("Yuan/CHF Exchange Rate") +
  ylab("Exchange Rate") +
  theme_minimal()

# 1.2 STATIONARITY TESTING
# ---------------------------------------------------------------
# A visual inspection suggests that the series exhibit non-stationary behavior.
# To address this, we apply differencing and use the ADF test to confirm
# whether the transformed series achieve stationarity.

# Differencing the series
exchange_rate_usd_ts <- diff(exchange_rate_usd_ts)
exchange_rate_euro_ts <- diff(exchange_rate_euro_ts)
exchange_rate_gbp_ts <- diff(exchange_rate_gbp_ts)
exchange_rate_yuan_ts <- diff(exchange_rate_yuan_ts)

# Apply ADF test
adf_usd_chf <- adf.test(exchange_rate_usd_ts)
adf_euro_chf <- adf.test(exchange_rate_euro_ts)
adf_gbp_chf <- adf.test(exchange_rate_gbp_ts)
adf_yuan_chf <- adf.test(exchange_rate_yuan_ts)

# Print ADF test results
cat("ADF Test Results:\n")
cat("USD/CHF: p-value =", adf_usd_chf$p.value, "\n")
cat("EURO/CHF: p-value =", adf_euro_chf$p.value, "\n")
cat("GBP/CHF: p-value =", adf_gbp_chf$p.value, "\n")
cat("Yuan/CHF: p-value =", adf_yuan_chf$p.value, "\n")
cat("If p-value < 0.05, the series is stationary after differencing.\n")

# 1.3 SEASONAL ADJUSTMENT
# ---------------------------------------------------------------
# Use X-13ARIMA-SEATS to adjust for seasonality in the differenced 
# exchange rate series.

# Apply X-13ARIMA-SEATS for seasonal adjustment on exchange rate
exchange_rate_usd_seas <- seas(exchange_rate_usd_ts)
exchange_rate_euro_seas <- seas(exchange_rate_euro_ts)
exchange_rate_gbp_seas <- seas(exchange_rate_gbp_ts) # Model used in SEATS is different
exchange_rate_yuan_seas <- seas(exchange_rate_yuan_ts)

# Extract seasonally adjusted nominal exchange rate series
exchange_rate_usd_adj <- final(exchange_rate_usd_seas)
exchange_rate_euro_adj <- final(exchange_rate_euro_seas)
exchange_rate_gbp_adj <- final(exchange_rate_gbp_seas)
exchange_rate_yuan_adj <- final(exchange_rate_yuan_seas)

# Create a data frame with the seasonally adjusted exchange rate series
nominal_exchrate_data_processed <- data.frame(
  Date = nominal_exchrate_data$Date[-1],  # Adjust for differencing (one less observation if differencing was applied)
  USD_CHF_Adj = exchange_rate_usd_adj,
  EURO_CHF_Adj = exchange_rate_euro_adj,
  GBP_CHF_Adj = exchange_rate_gbp_adj,
  Yuan_CHF_Adj = exchange_rate_yuan_adj
)

# Export the data frame to a CSV file
write.csv(nominal_exchrate_data_processed, "Nominal_Exchange_Rate_Processed.csv", row.names = FALSE)


# ---------------------------------------------------------------
# 2. INFLATION
# ---------------------------------------------------------------
# This section examines inflation data for Switzerland, USA, 
# Euro Area, UK, and China. The analysis includes:
#   - Data Preparation and Visualization
#   - Stationarity Testing (ADF Tests)
#   - Seasonal Adjustment (X-13ARIMA-SEATS)
# ===============================================================

# 2.1 DATA PREPARATION AND VISUALIZATION
# ---------------------------------------------------------------
# Convert inflation data to time series format and visualize 
# their trends.

# Convert to time series (assuming monthly data starting in 1999)
inflation_swiss_ts <- ts(inflation_data$Switzerland, frequency = 12, start = c(1999, 1))
inflation_usa_ts <- ts(inflation_data$USA, frequency = 12, start = c(1999, 1))
inflation_euro_ts <- ts(inflation_data$Euro, frequency = 12, start = c(1999, 1))
inflation_uk_ts <- ts(inflation_data$UK, frequency = 12, start = c(1999, 1))
inflation_china_ts <- ts(inflation_data$China, frequency = 12, start = c(1999, 1))

# Descriptive statistics for each inflation series
cat("Descriptive Statistics for Inflation Data:\n")
swiss_stats <- as.list(summary(inflation_swiss_ts))
usa_stats <- as.list(summary(inflation_usa_ts))
euro_stats <- as.list(summary(inflation_euro_ts))
uk_stats <- as.list(summary(inflation_uk_ts))
china_stats <- as.list(summary(inflation_china_ts))

# Combine statistics into a data frame
inflation_summary_stats <- data.frame(
  Metric = names(swiss_stats),
  Switzerland = unlist(swiss_stats),
  USA = unlist(usa_stats),
  Euro = unlist(euro_stats),
  UK = unlist(uk_stats),
  China = unlist(china_stats)
)
print(inflation_summary_stats)

# Convert inflation data to a data frame for plotting
inflation_plot <- data.frame(
  Date = inflation_data$Date,
  Inflation_Switzerland = inflation_data$Switzerland,
  Inflation_USA = inflation_data$USA,
  Inflation_Euro = inflation_data$Euro,
  Inflation_UK = inflation_data$UK,
  Inflation_China = inflation_data$China
)

# Plot Inflation Switzerland
ggplot(inflation_plot, aes(Date, Inflation_Switzerland)) +
  geom_line(color = "blue") +
  ggtitle("Inflation Switzerland") +
  ylab("Inflation Rate") +
  theme_minimal()

# Plot Inflation USA
ggplot(inflation_plot, aes(Date, Inflation_USA)) +
  geom_line(color = "red") +
  ggtitle("Inflation USA") +
  ylab("Inflation Rate") +
  theme_minimal()

# Plot Inflation Euro
ggplot(inflation_plot, aes(Date, Inflation_Euro)) +
  geom_line(color = "green") +
  ggtitle("Inflation Euro Area") +
  ylab("Inflation Rate") +
  theme_minimal()

# Plot Inflation UK
ggplot(inflation_plot, aes(Date, Inflation_UK)) +
  geom_line(color = "purple") +
  ggtitle("Inflation UK") +
  ylab("Inflation Rate") +
  theme_minimal()

# Plot Inflation China
ggplot(inflation_plot, aes(Date, Inflation_China)) +
  geom_line(color = "orange") +
  ggtitle("Inflation China") +
  ylab("Inflation Rate") +
  theme_minimal()

# 2.2 STATIONARITY TESTING
# ---------------------------------------------------------------
# Apply Augmented Dickey-Fuller (ADF) tests to check for stationarity.

# Apply ADF test to each inflation time series
adf_inflation_swiss <- adf.test(inflation_swiss_ts)
adf_inflation_usa <- adf.test(inflation_usa_ts)
adf_inflation_euro <- adf.test(inflation_euro_ts)
adf_inflation_uk <- adf.test(inflation_uk_ts)
adf_inflation_china <- adf.test(inflation_china_ts)

# Print results of ADF tests
cat("ADF Test Results:\n")
cat("Switzerland p-value:", adf_inflation_swiss$p.value, "\n")
cat("USA p-value:", adf_inflation_usa$p.value, "\n")
cat("Euro area p-value:", adf_inflation_euro$p.value, "\n")
cat("UK p-value:", adf_inflation_uk$p.value, "\n")
cat("China p-value:", adf_inflation_china$p.value, "\n")
cat("If p-value < 0.05, the series is stationary. \n")

# 2.3 SEASONAL ADJUSTMENT
# ---------------------------------------------------------------
# Remove seasonal effects using X-13ARIMA-SEATS.

# Apply X-13ARIMA-SEATS for seasonal adjustment on each inflation series
inflation_swiss_seas <- seas(inflation_swiss_ts)
inflation_usa_seas <- seas(inflation_usa_ts)
inflation_euro_seas <- seas(inflation_euro_ts)
inflation_uk_seas <- seas(inflation_uk_ts)
inflation_china_seas <- seas(inflation_china_ts)

# Extract seasonally adjusted inflation series
inflation_swiss_adj <- final(inflation_swiss_seas)
inflation_usa_adj <- final(inflation_usa_seas)  
inflation_euro_adj <- final(inflation_euro_seas)
inflation_uk_adj <- final(inflation_uk_seas)
inflation_china_adj <- final(inflation_china_seas)

# Create a data frame with seasonally adjusted inflation series
inflation_data_processed <- data.frame(
  Date = inflation_data$Date,
  Switzerland = inflation_swiss_adj,
  USA = inflation_usa_adj,
  Euro = inflation_euro_adj,
  UK = inflation_uk_adj,
  China = inflation_china_adj
)

# Export the dataframe to a CSV file
write.csv(inflation_data_processed, "Inflation_data_processed.csv", row.names = FALSE)


# ---------------------------------------------------------------
# 3. OUTPUT GAP
# ---------------------------------------------------------------
# This section examines industrial production data for Switzerland,
# USA, Euro Area, UK, and China. The analysis includes:
#   - Data Preparation and Visualization
#   - Stationarity Testing (ADF Tests)
#   - Seasonal Adjustment (X-13ARIMA-SEATS)
#   - Output Gap Calculation using HP Filter
# ===============================================================

# 3.1 DATA PREPARATION AND VISUALIZATION
# ---------------------------------------------------------------
# Convert industrial production data to time series format and 
# visualize their trends.

# Convert data to time series format
ip_swiss_ts <- ts(ip_data$Switzerland, frequency = 12, start = c(1999, 1))
ip_usa_ts <- ts(ip_data$USA, frequency = 12, start = c(1999, 1))
ip_euro_ts <- ts(ip_data$Euro, frequency = 12, start = c(1999, 1))
ip_uk_ts <- ts(ip_data$UK, frequency = 12, start = c(1999, 1))
ip_china_ts <- ts(ip_data$China, frequency = 12, start = c(1999, 1))

# Visualize industrial production data
ip_plot <- data.frame(
  Date = ip_data$Date,
  Switzerland = ip_data$Switzerland,
  USA = ip_data$USA,
  Euro = ip_data$Euro,
  UK = ip_data$UK,
  China = ip_data$China
)

# Plotting each country's industrial production series
ggplot(ip_plot, aes(Date, Switzerland)) +
  geom_line(color = "blue") +
  ggtitle("Industrial Production - Switzerland") +
  ylab("Index") +
  theme_minimal()

ggplot(ip_plot, aes(Date, USA)) +
  geom_line(color = "red") +
  ggtitle("Industrial Production - USA") +
  ylab("Index") +
  theme_minimal()

ggplot(ip_plot, aes(Date, Euro)) +
  geom_line(color = "green") +
  ggtitle("Industrial Production - Euro Area") +
  ylab("Index") +
  theme_minimal()

ggplot(ip_plot, aes(Date, UK)) +
  geom_line(color = "purple") +
  ggtitle("Industrial Production - UK") +
  ylab("Index") +
  theme_minimal()

ggplot(ip_plot, aes(Date, China)) +
  geom_line(color = "orange") +
  ggtitle("Industrial Production - China") +
  ylab("Index") +
  theme_minimal()

# 3.2 STATIONARITY TESTING
# ---------------------------------------------------------------
# Perform Augmented Dickey-Fuller (ADF) tests on the industrial 
# production time series to check for stationarity.

# Apply ADF tests
adf_swiss <- adf.test(ip_swiss_ts)
adf_usa <- adf.test(ip_usa_ts)
adf_euro <- adf.test(ip_euro_ts)
adf_uk <- adf.test(ip_uk_ts)
adf_china <- adf.test(ip_china_ts)

# Print results of ADF tests
cat("ADF Test Results:\n")
cat("Switzerland p-value:", adf_swiss$p.value, "\n")
cat("USA p-value:", adf_usa$p.value, "\n")
cat("Euro Area p-value:", adf_euro$p.value, "\n")
cat("UK p-value:", adf_uk$p.value, "\n")
cat("China p-value:", adf_china$p.value, "\n")
cat("If p-value < 0.05, the series is stationary. \n")

# Difference the industrial production series to remove the trend
ip_swiss_ts <- diff(ip_swiss_ts)
ip_usa_ts <- diff(ip_usa_ts)
ip_euro_ts <- diff(ip_euro_ts)
ip_uk_ts <- diff(ip_uk_ts)
ip_china_ts <- diff(ip_china_ts)

# Apply again ADF tests
adf_swiss_ip <- adf.test(ip_swiss_ts)
adf_usa_ip <- adf.test(ip_usa_ts)
adf_euro_ip <- adf.test(ip_euro_ts)
adf_uk_ip <- adf.test(ip_uk_ts)
adf_china_ip <- adf.test(ip_china_ts)

# Print results of ADF tests
# If the p-value is < 0.05, we reject the null hypothesis of a unit root, meaning the series is stationary.
cat("ADF Test Results for Industrial Production Time Series:\n")
cat("Industrial Production Switzerland: p-value =", adf_swiss_ip$p.value, "\n")
cat("Industrial Production USA: p-value =", adf_usa_ip$p.value, "\n")
cat("Industrial Production Euro area: p-value =", adf_euro_ip$p.value, "\n")
cat("Industrial Production UK: p-value =", adf_uk_ip$p.value, "\n")
cat("Industrial Production China: p-value =", adf_china_ip$p.value, "\n")

# 3.3 SEASONAL ADJUSTMENT
# ---------------------------------------------------------------
# Apply seasonal adjustment using X-13ARIMA-SEATS to the 
# industrial production series where needed and save the dataframe. 

# Seasonal adjustment for China (example for one country)
ip_china_seas <- seas(ip_china_ts)
ip_china_adj <- final(ip_china_seas)

# Create a data frame with the differenced and seasonally adjusted series
ip_data_processed <- data.frame(
  Date = ip_data$Date[-1],  # Adjust for differencing (one less observation)
  Industrial_Prod_USA = ip_usa_ts,
  Industrial_Prod_Euro = ip_euro_ts,
  Industrial_Prod_UK = ip_uk_ts,
  Industrial_Prod_Switzerland = ip_swiss_ts,
  Industrial_Prod_China = ip_china_adj
)

# Write the data frame to a CSV file
write.csv(ip_data_processed, "Industrial_Production_Processed.csv", row.names = FALSE)


# 3.4 HP FILTER AND OUTPUT GAP CALCULATION
# ---------------------------------------------------------------
# Calculate the output gap for each country using the HP filter.

# Calculate the output gap for each country using the HP filter.
# The output gap is the difference between the actual and trend values,
# where the trend is extracted using the HP filter.

# Function to calculate HP Filter and output gap
calculate_output_gap <- function(series) {
  T <- length(series)
  I <- diag(T)
  D <- diff(diag(T), differences = 2)
  lambda <- 14400  # Smoothing parameter for monthly data
  HP_matrix <- I + lambda * t(D) %*% D
  trend <- solve(HP_matrix, series)
  output_gap <- series - trend
  list(trend = trend, output_gap = output_gap)
}

# ---------------------------------------------------------------
# Output Gap: Switzerland
# ---------------------------------------------------------------
result_swiss <- calculate_output_gap(as.numeric(ip_swiss_ts))
output_gap_swiss <- result_swiss$output_gap
trend_swiss <- result_swiss$trend

# Plot the output gap for Switzerland
plot(output_gap_swiss, type = "l", col = "green", lwd = 2,
     main = "Output Gap - Switzerland", xlab = "Time", ylab = "Output Gap")

# ---------------------------------------------------------------
# Output Gap: USA
# ---------------------------------------------------------------
result_usa <- calculate_output_gap(as.numeric(ip_usa_ts))
output_gap_usa <- result_usa$output_gap
trend_usa <- result_usa$trend

# Plot the output gap for USA
plot(output_gap_usa, type = "l", col = "blue", lwd = 2,
     main = "Output Gap - USA", xlab = "Time", ylab = "Output Gap")

# ---------------------------------------------------------------
# Output Gap: Euro Area
# ---------------------------------------------------------------
result_euro <- calculate_output_gap(as.numeric(ip_euro_ts))
output_gap_euro <- result_euro$output_gap
trend_euro <- result_euro$trend

# Plot the output gap for Euro Area
plot(output_gap_euro, type = "l", col = "red", lwd = 2,
     main = "Output Gap - Euro Area", xlab = "Time", ylab = "Output Gap")

# ---------------------------------------------------------------
# Output Gap: UK
# ---------------------------------------------------------------
result_uk <- calculate_output_gap(as.numeric(ip_uk_ts))
output_gap_uk <- result_uk$output_gap
trend_uk <- result_uk$trend

# Plot the output gap for UK
plot(output_gap_uk, type = "l", col = "purple", lwd = 2,
     main = "Output Gap - UK", xlab = "Time", ylab = "Output Gap")

# ---------------------------------------------------------------
# Output Gap: China
# ---------------------------------------------------------------
result_china <- calculate_output_gap(as.numeric(ip_china_ts))
output_gap_china <- result_china$output_gap
trend_china <- result_china$trend

# Plot the output gap for China
plot(output_gap_china, type = "l", col = "orange", lwd = 2,
     main = "Output Gap - China", xlab = "Time", ylab = "Output Gap")

# ---------------------------------------------------------------
# Export the Output Gap Data
# ---------------------------------------------------------------
# Create a data frame with the output gap values for all countries.
output_gap_data_processed <- data.frame(
  Date = ip_data$Date[-1],  # Adjust for differencing (one less observation)
  Output_Gap_CH = output_gap_swiss,
  Output_Gap_USA = output_gap_usa,
  Output_Gap_Euro = output_gap_euro,
  Output_Gap_UK = output_gap_uk,
  Output_Gap_China = output_gap_china
)

# Save the output gap data to a CSV file
write.csv(output_gap_data_processed, "Output_Gap_Processed.csv", row.names = FALSE)



# ---------------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------------
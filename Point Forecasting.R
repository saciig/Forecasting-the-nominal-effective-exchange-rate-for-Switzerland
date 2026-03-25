# ===============================================================
# POINT FORECASTING SCRIPT
# ===============================================================
# This script computes point forecasts for NEER changes across
# multiple horizons (1, 6, 12, and 18 months) using the rolling 
# estimation results. It includes:
#   - Out-of-sample forecasting with a rolling window approach.
#   - Visualization of forecast errors and comparison with a 
#     Random Walk (RW) benchmark.
#   - Forecasting NEER changes for specific periods based on
#     the latest observed data and rolling beta coefficients.
# ===============================================================

# NOTE:
# This script relies on data processed in the Model Estimation 
# script. Forecasts are made using direct estimation, ensuring
# horizon-specific beta coefficients. 
# The Random Walk model is implemented as a benchmark for comparison. It is
# a widely accepted benchmark in exchange rate forecasting literature. 
# It assumes that changes in the exchange rate are unpredictable, making 
# it a useful baseline for comparing the performance of model-based forecasts.
# ===============================================================

# TABLE OF CONTENTS
# 1. Out-of-Sample Forecasting
#    1.1 Out-of-Sample Forecasting Function
#    1.2 Forecasts for each horizon
# 2. Computation of NEER in Levels
#    2.1 Calculation of NEER in Levels
#    2.2 Visualization of NEER
# 3. Forecasting NEER Changes at Horizons
#    3.1 Forecasting Changes for Specific Horizons
#    3.2 Visualization of Forecasts
#    3.3 Shifting Forecast for December
#    3.4 Forecasted NEER levels with 95% CI
# 4. Random Walk Benchmark
#    4.1 Compute RW benchmark for each horizon
#    4.2 Visualization for each horizon
# ===============================================================

# ===============================================================
# 1. OUT-OF-SAMPLE FORECASTING
# ===============================================================

# 1.1 OUT-OF-SAMPLE FORECASTING FUNCTION
# ---------------------------------------------------------------
# This function generates NEER forecasts for specific horizons 
# using the rolling beta estimates and computes forecast errors.
# ---------------------------------------------------------------

out_of_sample_forecast <- function(estimation_data, rolling_betas, horizon, y_col, lagged_cols, window_size) {
  
  # Initialize vectors to store forecasts and forecast errors
  forecasted_NEER <- numeric()
  forecast_errors <- numeric()
  
  # Define the starting point for out-of-sample evaluation, after the rolling window ends
  evaluation_start <- window_size + 1
  
  # Loop through each forecast time point
  for (t in evaluation_start:(nrow(estimation_data))) {
    # Retrieve the latest rolling beta estimates up to time `t`
    beta_intercept_t <- rolling_betas$beta_intercept[t - window_size]
    beta_inflation_t <- rolling_betas$beta_inflation[t - window_size]
    beta_output_gap_t <- rolling_betas$beta_output_gap[t - window_size]
    
    # Extract predictors at time `t` for forecasting NEER change at time `t + horizon`
    forecast_data <- estimation_data[t, ]
    lagged_inflation_forecast <- forecast_data[[lagged_cols[1]]]
    lagged_output_gap_forecast <- forecast_data[[lagged_cols[2]]]
    
    # Generate forecast for NEER change
    forecasted_NEER_t <- beta_intercept_t +
      beta_inflation_t * lagged_inflation_forecast +
      beta_output_gap_t * lagged_output_gap_forecast
    
    # Store the forecasted value
    forecasted_NEER <- c(forecasted_NEER, forecasted_NEER_t)
    
    # Compute the forecast error at time `t`
    actual_NEER <- estimation_data[[y_col]][t]  # Actual NEER change
    forecast_error <- actual_NEER - forecasted_NEER_t  # Forecast error
    
    # Store the forecast error
    forecast_errors <- c(forecast_errors, forecast_error)
  }
  
  # Return a list containing forecasted values and errors
  return(list(
    forecasted_NEER = forecasted_NEER,
    forecast_errors = forecast_errors
  ))
}

# ---------------------------------------------------------------
# 1.2. FORECASTS FOR EACH HORIZON
# ---------------------------------------------------------------
# Forecasts are computed for 1, 6, 12, and 18-month horizons.
# ---------------------------------------------------------------

# Forecast for 1-Month Horizon
forecast_results_1 <- out_of_sample_forecast(
  estimation_data = estimation_data_1,
  rolling_betas = rolling_betas_1,
  horizon = 1,
  y_col = "nominal_effective_exchrate",
  lagged_cols = c("lagged_inflation_1", "lagged_output_gap_1"),
  window_size = window_size
)

# Forecast for 6-Month Horizon
forecast_results_6 <- out_of_sample_forecast(
  estimation_data = estimation_data_6,
  rolling_betas = rolling_betas_6,
  horizon = 6,
  y_col = "NEER_6_month_change",
  lagged_cols = c("lagged_inflation_6", "lagged_output_gap_6"),
  window_size = window_size
)

# Forecast for 12-Month Horizon
forecast_results_12 <- out_of_sample_forecast(
  estimation_data = estimation_data_12,
  rolling_betas = rolling_betas_12,
  horizon = 12,
  y_col = "NEER_12_month_change",
  lagged_cols = c("lagged_inflation_12", "lagged_output_gap_12"),
  window_size = window_size
)

# Forecast for 18-Month Horizon
forecast_results_18 <- out_of_sample_forecast(
  estimation_data = estimation_data_18,
  rolling_betas = rolling_betas_18,
  horizon = 18,
  y_col = "NEER_18_month_change",
  lagged_cols = c("lagged_inflation_18", "lagged_output_gap_18"),
  window_size = window_size
)

# ---------------------------------------------------------------
# 1.3. EVALUATION AND VISUALIZATION
# ---------------------------------------------------------------
# This section evaluates forecast errors and visualizes results.
# ---------------------------------------------------------------

# Evaluation: 1-Month horizon
# ---------------------------------------------------------------
forecasted_NEER_1 <- forecast_results_1$forecasted_NEER
forecast_errors_1 <- forecast_results_1$forecast_errors

# Calculate error metrics
mean_forecast_error_1 <- mean(forecast_errors_1, na.rm = TRUE)
variance_forecast_error_1 <- var(forecast_errors_1, na.rm = TRUE)
MSFE_1 <- mean((forecast_errors_1)^2, na.rm = TRUE)

# Print results
cat("1-Month Horizon:\n")
cat("Mean of Forecast Errors:", mean_forecast_error_1, "\n")
cat("Variance of Forecast Errors:", variance_forecast_error_1, "\n")
cat("Mean Squared Forecast Error (MSFE):", MSFE_1, "\n")

# Visualizations
plot(forecast_errors_1, type = "l", main = "1-Month Horizon Forecast Errors Over Time",
     xlab = "Forecast Time", ylab = "Forecast Error")
hist(forecast_errors_1, breaks = 20, probability = TRUE,
     main = "1-Month Horizon Density of Forecast Errors", xlab = "Forecast Error")
curve(dnorm(x, mean = mean_forecast_error_1, sd = sqrt(variance_forecast_error_1)),
      add = TRUE, col = "red", lwd = 2)

# Plot Actual vs. Forecasted NEER Changes
time_periods_1 <- seq(window_size + 1, length(forecasted_NEER_1) + window_size)
actual_NEER_realizations_1 <- estimation_data_1$nominal_effective_exchrate[time_periods_1]
plot(time_periods_1, actual_NEER_realizations_1, type = "l", col = "blue", 
     ylim = range(c(actual_NEER_realizations_1, forecasted_NEER_1), na.rm = TRUE),
     xlab = "Time Period", ylab = "NEER Change",
     main = "1-Month Horizon: Forecasts vs Actual Realizations")
lines(time_periods_1, forecasted_NEER_1, col = "red", lty = 2)
legend("topright", legend = c("Actual NEER", "Forecasted NEER"), 
       col = c("blue", "red"), lty = c(1, 2))


# Evaluation: 6-Month Horizon
# ---------------------------------------------------------------
forecasted_NEER_6 <- forecast_results_6$forecasted_NEER
forecast_errors_6 <- forecast_results_6$forecast_errors

# Calculate error metrics
mean_forecast_error_6 <- mean(forecast_errors_6, na.rm = TRUE)
variance_forecast_error_6 <- var(forecast_errors_6, na.rm = TRUE)
MSFE_6 <- mean((forecast_errors_6)^2, na.rm = TRUE)

# Print results
cat("6-Month Horizon:\n")
cat("Mean of Forecast Errors:", mean_forecast_error_6, "\n")
cat("Variance of Forecast Errors:", variance_forecast_error_6, "\n")
cat("Mean Squared Forecast Error (MSFE):", MSFE_6, "\n")

# Visualizations
plot(forecast_errors_6, type = "l", main = "6-Month Horizon Forecast Errors Over Time",
     xlab = "Forecast Time", ylab = "Forecast Error")
hist(forecast_errors_6, breaks = 20, probability = TRUE,
     main = "6-Month Horizon Density of Forecast Errors", xlab = "Forecast Error")
curve(dnorm(x, mean = mean_forecast_error_6, sd = sqrt(variance_forecast_error_6)),
      add = TRUE, col = "red", lwd = 2)

# Plot Actual vs. Forecasted NEER Changes
time_periods_6 <- seq(window_size + 1, length(forecasted_NEER_6) + window_size)
actual_NEER_realizations_6 <- estimation_data_6$NEER_6_month_change[time_periods_6]
plot(time_periods_6, actual_NEER_realizations_6, type = "l", col = "blue", 
     ylim = range(c(actual_NEER_realizations_6, forecasted_NEER_6), na.rm = TRUE),
     xlab = "Time Period", ylab = "NEER Change",
     main = "6-Month Horizon: Forecasts vs Actual Realizations")
lines(time_periods_6, forecasted_NEER_6, col = "red", lty = 2)
legend("topright", legend = c("Actual NEER", "Forecasted NEER"), 
       col = c("blue", "red"), lty = c(1, 2))


# Evaluation: 12-Month Horizon
# ---------------------------------------------------------------
forecasted_NEER_12 <- forecast_results_12$forecasted_NEER
forecast_errors_12 <- forecast_results_12$forecast_errors

# Calculate error metrics
mean_forecast_error_12 <- mean(forecast_errors_12, na.rm = TRUE)
variance_forecast_error_12 <- var(forecast_errors_12, na.rm = TRUE)
MSFE_12 <- mean((forecast_errors_12)^2, na.rm = TRUE)

# Print results
cat("12-Month Horizon:\n")
cat("Mean of Forecast Errors:", mean_forecast_error_12, "\n")
cat("Variance of Forecast Errors:", variance_forecast_error_12, "\n")
cat("Mean Squared Forecast Error (MSFE):", MSFE_12, "\n")

# Visualizations
plot(forecast_errors_12, type = "l", main = "12-Month Horizon Forecast Errors Over Time",
     xlab = "Forecast Time", ylab = "Forecast Error")
hist(forecast_errors_12, breaks = 20, probability = TRUE,
     main = "12-Month Horizon Density of Forecast Errors", xlab = "Forecast Error")
curve(dnorm(x, mean = mean_forecast_error_12, sd = sqrt(variance_forecast_error_12)),
      add = TRUE, col = "red", lwd = 2)

# Plot Actual vs. Forecasted NEER Changes
time_periods_12 <- seq(window_size + 1, length(forecasted_NEER_12) + window_size)
actual_NEER_realizations_12 <- estimation_data_12$NEER_12_month_change[time_periods_12]
plot(time_periods_12, actual_NEER_realizations_12, type = "l", col = "blue", 
     ylim = range(c(actual_NEER_realizations_12, forecasted_NEER_12), na.rm = TRUE),
     xlab = "Time Period", ylab = "NEER Change",
     main = "12-Month Horizon: Forecasts vs Actual Realizations")
lines(time_periods_12, forecasted_NEER_12, col = "red", lty = 2)
legend("topright", legend = c("Actual NEER", "Forecasted NEER"), 
       col = c("blue", "red"), lty = c(1, 2))


# Evaluation: 18-Month Horizon
# ---------------------------------------------------------------
forecasted_NEER_18 <- forecast_results_18$forecasted_NEER
forecast_errors_18 <- forecast_results_18$forecast_errors

# Calculate error metrics
mean_forecast_error_18 <- mean(forecast_errors_18, na.rm = TRUE)
variance_forecast_error_18 <- var(forecast_errors_18, na.rm = TRUE)
MSFE_18 <- mean((forecast_errors_18)^2, na.rm = TRUE)

# Print results
cat("18-Month Horizon:\n")
cat("Mean of Forecast Errors:", mean_forecast_error_18, "\n")
cat("Variance of Forecast Errors:", variance_forecast_error_18, "\n")
cat("Mean Squared Forecast Error (MSFE):", MSFE_18, "\n")

# Visualizations
plot(forecast_errors_18, type = "l", main = "18-Month Horizon Forecast Errors Over Time",
     xlab = "Forecast Time", ylab = "Forecast Error")
hist(forecast_errors_18, breaks = 20, probability = TRUE,
     main = "18-Month Horizon Density of Forecast Errors", xlab = "Forecast Error")
curve(dnorm(x, mean = mean_forecast_error_18, sd = sqrt(variance_forecast_error_18)),
      add = TRUE, col = "red", lwd = 2)

# Plot Actual vs. Forecasted NEER Changes
time_periods_18 <- seq(window_size + 1, length(forecasted_NEER_18) + window_size)
actual_NEER_realizations_18 <- estimation_data_18$NEER_18_month_change[time_periods_18]
plot(time_periods_18, actual_NEER_realizations_18, type = "l", col = "blue", 
     ylim = range(c(actual_NEER_realizations_18, forecasted_NEER_18), na.rm = TRUE),
     xlab = "Time Period", ylab = "NEER Change",
     main = "18-Month Horizon: Forecasts vs Actual Realizations")
lines(time_periods_18, forecasted_NEER_18, col = "red", lty = 2)
legend("topright", legend = c("Actual NEER", "Forecasted NEER"), 
       col = c("blue", "red"), lty = c(1, 2))


# ===============================================================
# 2. COMPUTATION OF NEER IN LEVELS
# ===============================================================

# 2.1 Calculation of NEER in Levels
# ---------------------------------------------------------------
# This section calculates the NEER in levels using nominal
# exchange rates and trade shares.
# ---------------------------------------------------------------

# Define time series for nominal exchange rates
exchange_rate_usd_ts <- ts(nominal_exchrate_data$USD.CHF, frequency = 12, start = c(1999, 1))
exchange_rate_euro_ts <- ts(nominal_exchrate_data$EURO.CHF, frequency = 12, start = c(1999, 1))
exchange_rate_gbp_ts <- ts(nominal_exchrate_data$GPB.CHF, frequency = 12, start = c(1999, 1))
exchange_rate_yuan_ts <- ts(nominal_exchrate_data$Yuan.CHF, frequency = 12, start = c(1999, 1))

# Merge nominal exchange rates with trade shares data
neer_levels_data <- merge(nominal_exchrate_data, trade_shares_data, by = "Date")

# Calculate weighted exchange rates
neer_levels_data$Weighted_USD_CHF <- neer_levels_data$USD.CHF * neer_levels_data$USA_shares
neer_levels_data$Weighted_EURO_CHF <- neer_levels_data$EURO.CHF * neer_levels_data$Euro_shares
neer_levels_data$Weighted_GBP_CHF <- neer_levels_data$GPB.CHF * neer_levels_data$UK_shares
neer_levels_data$Weighted_Yuan_CHF <- neer_levels_data$Yuan.CHF * neer_levels_data$China_shares

# Compute the NEER as the sum of weighted exchange rates
neer_levels_data$nominal_effective_exchrate <- 
  neer_levels_data$Weighted_USD_CHF + 
  neer_levels_data$Weighted_EURO_CHF + 
  neer_levels_data$Weighted_GBP_CHF + 
  neer_levels_data$Weighted_Yuan_CHF

# 2.2 Visualization of NEER
# ---------------------------------------------------------------
# Plot the nominal effective exchange rate over time.
# ---------------------------------------------------------------

# Plot the nominal effective exchange rate
ggplot(data = neer_levels_data, aes(x = as.Date(Date), y = nominal_effective_exchrate)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "Nominal Effective Exchange Rate (NEER) in Levels",
    x = "Date",
    y = "Nominal Effective Exchange Rate"
  ) +
  theme_minimal()


# ===============================================================
# 3. FORECASTING NEER CHANGES AT HORIZONS
# ===============================================================

# 3.1 Forecasting Changes for Specific Horizons
# ---------------------------------------------------------------
# Forecast NEER changes for horizons h = 1, 6, 12, and 18 months
# using the direct forecasting approach.
# ---------------------------------------------------------------

# Initialize the latest observed data from September 2024
latest_inflation <- estimation_data$inflation_differential[nrow(estimation_data)]
latest_output_gap <- estimation_data$output_gap_differential[nrow(estimation_data)]

# Forecast horizons
forecast_horizons <- c(1, 6, 12, 18)

# Direct Forecasting Function for Specific Horizons
direct_forecast_NEER <- function(rolling_betas, latest_inflation, latest_output_gap) {
  # Extract the most recent rolling coefficients
  beta_intercept <- tail(rolling_betas$beta_intercept, 1)
  beta_inflation <- tail(rolling_betas$beta_inflation, 1)
  beta_output_gap <- tail(rolling_betas$beta_output_gap, 1)
  
  # Compute the direct forecast
  forecast_NEER_change <- beta_intercept +
    beta_inflation * latest_inflation +
    beta_output_gap * latest_output_gap
  
  return(forecast_NEER_change)
}

# Perform direct forecasts for each horizon
forecast_results <- list()
for (horizon in forecast_horizons) {
  if (horizon == 1) {
    rolling_betas <- rolling_betas_1
  } else if (horizon == 6) {
    rolling_betas <- rolling_betas_6
  } else if (horizon == 12) {
    rolling_betas <- rolling_betas_12
  } else if (horizon == 18) {
    rolling_betas <- rolling_betas_18
  }
  
  forecast_results[[paste0("horizon_", horizon)]] <- direct_forecast_NEER(
    rolling_betas = rolling_betas,
    latest_inflation = latest_inflation,
    latest_output_gap = latest_output_gap
  )
}

# Print Forecast Results
for (horizon in forecast_horizons) {
  forecast_time <- if (horizon == 1) "October 2024" else if (horizon == 6) "March 2025" else if (horizon == 12) "September 2025" else "March 2026"
  cat(paste0("Forecasted NEER Change for Horizon h = ", horizon, " (", forecast_time, "):\n"))
  print(forecast_results[[paste0("horizon_", horizon)]])
}

# 3.2 Visualization of Forecasts
# ---------------------------------------------------------------
# Generate plots combining historical and forecasted NEER levels.
# ---------------------------------------------------------------

# Define last observed NEER
last_observed_NEER <- neer_levels_data$nominal_effective_exchrate[neer_levels_data$Date == "2024-09-01"]

combine_historical_and_forecast <- function(historical_data, last_observed_NEER, forecast_value, forecast_date) {
  forecast_point <- data.frame(
    Date = as.Date(forecast_date),
    nominal_effective_exchrate = last_observed_NEER + forecast_value,
    Type = "Forecast"
  )
  
  last_historical_point <- data.frame(
    Date = as.Date("2024-09-01"),
    nominal_effective_exchrate = last_observed_NEER,
    Type = "Forecast"
  )
  
  forecast_line <- rbind(last_historical_point, forecast_point)
  historical_data_filtered <- subset(historical_data, as.Date(Date) >= as.Date("2020-01-01") & as.Date(Date) <= as.Date("2024-09-01"))
  historical_data_filtered$Type <- "Historical"
  
  return(list(historical = historical_data_filtered, forecast = forecast_line))
}

# Generate data for plots
forecast_value_1 <- forecast_results[["horizon_1"]]
forecast_value_6 <- forecast_results[["horizon_6"]]
forecast_value_12 <- forecast_results[["horizon_12"]]
forecast_value_18 <- forecast_results[["horizon_18"]]

forecast_dates <- c("2024-10-01", "2025-03-01", "2025-09-01", "2026-03-01")

combined_data_1 <- combine_historical_and_forecast(neer_levels_data, last_observed_NEER, forecast_value_1, forecast_dates[1])
combined_data_6 <- combine_historical_and_forecast(neer_levels_data, last_observed_NEER, forecast_value_6, forecast_dates[2])
combined_data_12 <- combine_historical_and_forecast(neer_levels_data, last_observed_NEER, forecast_value_12, forecast_dates[3])
combined_data_18 <- combine_historical_and_forecast(neer_levels_data, last_observed_NEER, forecast_value_18, forecast_dates[4])

plot_forecast <- function(combined_data, horizon) {
  ggplot() +
    geom_line(data = combined_data$historical, aes(x = as.Date(Date), y = nominal_effective_exchrate), color = "blue", size = 1.2) +
    geom_line(data = combined_data$forecast, aes(x = as.Date(Date), y = nominal_effective_exchrate), color = "red", size = 1.2) +
    geom_vline(xintercept = as.Date("2024-09-01"), linetype = "dashed", color = "green", size = 1) +
    labs(
      title = paste0("Nominal Effective Exchange Rate: ", horizon, "-Month Horizon"),
      x = "Date",
      y = "Nominal Effective Exchange Rate"
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 months", limits = c(as.Date("2020-01-01"), as.Date("2026-03-01"))) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none"
    )
}

# Plot for each horizon
plot_1 <- plot_forecast(combined_data_1, horizon = 1)
plot_6 <- plot_forecast(combined_data_6, horizon = 6)
plot_12 <- plot_forecast(combined_data_12, horizon = 12)
plot_18 <- plot_forecast(combined_data_18, horizon = 18)

# Display plots
print(plot_1)
print(plot_6)
print(plot_12)
print(plot_18)


# 3.3 Shifting Forecast for December
# ---------------------------------------------------------------
# This section is a bit special as it shifts the forecast to start in 
# December 2024, following the teacher's guidance. Instead of incorporating 
# new data for October, November, and December 2024 across all variables—a process 
# that would be both highly time-consuming and uncertain due to potential data 
# availability issues—we extend the nominal exchange rate dataset to include 
# November 2024. The nominal effective exchange rate (NEER) is plotted up to 
# 01 December 2024, under the assumption that current trends persist. The forecast 
# is then superimposed onto the last recorded value, resulting in a small shift.

# The NEER is calculated in levels and normalized to a base of 100, using 
# November 2024 as the reference month. The 1-Month Horizon forecast is 
# computed for December 2024 (01.12.2024 to 01.01.2025), while the forecast 
# for the 18-Month Horizon extends to 01.06.2026.
# ---------------------------------------------------------------

# Load the new data with nominal exchange rate for October, November and December.
NTR_nominal_exchrate_data <- read.csv("NTR_Nominal_Exchange_Rate.csv", sep = ";")

# Convert 'Date' column to Date format
NTR_nominal_exchrate_data$Date <- as.Date(NTR_nominal_exchrate_data$Date, format = "%d/%m/%Y")

# Replace commas with dots and convert character columns to numeric
NTR_nominal_exchrate_data$USD.CHF <- as.numeric(gsub(",", ".", NTR_nominal_exchrate_data$USD.CHF))
NTR_nominal_exchrate_data$EURO.CHF <- as.numeric(gsub(",", ".", NTR_nominal_exchrate_data$EURO.CHF))
NTR_nominal_exchrate_data$Yuan.CHF <- as.numeric(gsub(",", ".", NTR_nominal_exchrate_data$Yuan.CHF))

# Verify the structure after transformations
str(NTR_nominal_exchrate_data)

# Add dates for October, November, and December 2024
new_dates <- as.Date(c("2024-10-01", "2024-11-01", "2024-12-01"))
last_trade_shares <- tail(trade_shares_data, 1)  # Replicate last trade shares

# Extend trade shares data to include the new dates
new_trade_shares <- data.frame(Date = new_dates,
                               USA_shares = rep(last_trade_shares$USA, length(new_dates)),
                               Euro_shares = rep(last_trade_shares$Euro, length(new_dates)),
                               UK_shares = rep(last_trade_shares$UK, length(new_dates)),
                               China_shares = rep(last_trade_shares$China, length(new_dates)))

# Combine with the existing trade shares data
trade_shares_data <- rbind(trade_shares_data, new_trade_shares)

# Merge nominal exchange rates data with extended trade shares data by Date
NTR_neer_levels_data <- merge(NTR_nominal_exchrate_data, trade_shares_data, by = "Date", all.x = TRUE)

# Ensure unique dates in both datasets
NTR_neer_levels_data <- NTR_neer_levels_data[!duplicated(NTR_neer_levels_data$Date), ]

# Calculate the weighted exchange rates
NTR_neer_levels_data$Weighted_USD_CHF <- NTR_neer_levels_data$USD.CHF * NTR_neer_levels_data$USA
NTR_neer_levels_data$Weighted_EURO_CHF <- NTR_neer_levels_data$EURO.CHF * NTR_neer_levels_data$Euro
NTR_neer_levels_data$Weighted_GBP_CHF <- NTR_neer_levels_data$GPB.CHF * NTR_neer_levels_data$UK
NTR_neer_levels_data$Weighted_Yuan_CHF <- NTR_neer_levels_data$Yuan.CHF * NTR_neer_levels_data$China

# Compute the NEER in levels
NTR_neer_levels_data$nominal_effective_exchrate <- 
  NTR_neer_levels_data$Weighted_USD_CHF + 
  NTR_neer_levels_data$Weighted_EURO_CHF + 
  NTR_neer_levels_data$Weighted_GBP_CHF + 
  NTR_neer_levels_data$Weighted_Yuan_CHF

# Normalize the NEER to base 100 at November 2024
base_neer <- NTR_neer_levels_data$nominal_effective_exchrate[NTR_neer_levels_data$Date == "2024-12-01"]
NTR_neer_levels_data$NEER_base100 <- (NTR_neer_levels_data$nominal_effective_exchrate / base_neer) * 100

# Plot the NEER levels (Base 100)
ggplot(NTR_neer_levels_data, aes(x = Date, y = NEER_base100)) +
  geom_line(color = "blue", size = 1.2) +
  labs(
    title = "Nominal Effective Exchange Rate (NEER) - Base 100",
    x = "Date",
    y = "NEER (Base 100)"
  ) +
  geom_vline(xintercept = as.Date("2024-11-01"), linetype = "dashed", color = "red", size = 1, 
             label = "Base Month: Nov 2024") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

# Define the last observed NEER in November 2024
NTR_last_observed_NEER <- NTR_neer_levels_data$NEER_base100[NTR_neer_levels_data$Date == "2024-12-01"]

# Define the forecasted NEER value for the 18-Month Horizon
absolute_forecast_value_18 <- forecast_results[["horizon_18"]]  # Forecasted NEER change for 18 months
percentage_forecast_value_18 <- absolute_forecast_value_18 * 100
forecast_dates <- 
  seq(as.Date("2024-12-01"), as.Date("2026-06-01"), by = "month")  # Generate monthly dates for the forecast

# Compute forecasted NEER values for each date
forecast_values <- seq(from = NTR_last_observed_NEER, 
                       to = NTR_last_observed_NEER + percentage_forecast_value_18, 
                       length.out = length(forecast_dates))

# Create the forecast line data frame
NTR_forecast_line <- data.frame(
  Date = forecast_dates,
  NEER_base100 = forecast_values,
  Type = "Forecast"
)

# Add the last historical point as part of the forecast line
last_historical_point <- data.frame(
  Date = as.Date("2024-12-01"),
  NEER_base100 = NTR_last_observed_NEER,
  Type = "Historical"
)

# Filter historical data from 2023-01-01 to 2024-12-01
NTR_historical_data_filtered <- 
  subset(NTR_neer_levels_data, as.Date(Date) >= as.Date("2023-01-01") & as.Date(Date) <= as.Date("2024-12-01"))
NTR_historical_data_filtered <- 
  NTR_historical_data_filtered[, c("Date", "NEER_base100")]  # Keep only relevant columns
NTR_historical_data_filtered$Type <- "Historical"

# Ensure all data frames have the same columns
NTR_forecast_line <- NTR_forecast_line[, c("Date", "NEER_base100", "Type")]
last_historical_point <- last_historical_point[, c("Date", "NEER_base100", "Type")]

# Combine historical data and the forecast line
NTR_combined_data <- rbind(
  NTR_historical_data_filtered,
  last_historical_point,
  NTR_forecast_line
)

# Plot the combined historical and forecasted data with legends
ggplot(NTR_combined_data, aes(x = Date, y = NEER_base100, color = Type)) +
  geom_line(data = subset(NTR_combined_data, Type == "Historical"), size = 1.2) +
  geom_line(data = subset(NTR_combined_data, Type == "Forecast"), size = 1.2, linetype = "solid") +
  labs(
    title = "Forecast 18-Month Nominal Effective Exchange Rate (NEER)",
    x = "Date",
    y = "NEER (Base 100)",
    color = "Legend"  # Title for the legend
  ) +
  geom_vline(xintercept = as.Date("2024-12-01"), linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(
    values = c(
      "Historical" = "#4d77eb",  # Light blue for historical
      "Forecast" = "#a62424"    # Burgundy for forecast
    ),
    labels = c(
      "Historical" = "Historical Data",
      "Forecast" = "Forecast Data"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12)
  )



# 3.4 Forecasted NEER Levels with 95% CI
# ---------------------------------------------------------------
# This script calculates and visualizes forecasted NEER levels
# with 95% confidence intervals for multiple horizons (1, 6, 12, 18 months).
# It includes:
# 1. Calculation of confidence intervals
# 2. Construction of forecasted NEER levels and confidence intervals
# 3. Visualization of the forecasted NEER and historical data
# 4. Calculation of NEER's appreciation percentage
# ---------------------------------------------------------------

# Calculate Confidence Intervals
calculate_confidence_intervals <- function(forecast_value, total_variance) {
  se <- sqrt(total_variance)
  data.frame(Lower = forecast_value - 1.96 * se, Upper = forecast_value + 1.96 * se)
}

# Confidence intervals for each forecast horizon
ci_1 <- calculate_confidence_intervals(forecast_value_1, tail(total_variances_1, 1))
ci_6 <- calculate_confidence_intervals(forecast_value_6, tail(total_variances_6, 1))
ci_12 <- calculate_confidence_intervals(forecast_value_12, tail(total_variances_12, 1))
ci_18 <- calculate_confidence_intervals(forecast_value_18, tail(total_variances_18, 1))

# Define forecast dates corresponding to the forecast horizons
forecast_dates <- c(as.Date("2024-10-01"), as.Date("2025-03-01"), as.Date("2025-09-01"), as.Date("2026-03-01"))

# Build the data frame
forecast_path <- data.frame(
  Date = c(as.Date("2024-09-01"), forecast_dates),  # Include last actual NEER date
  nominal_effective_exchrate = c(
    last_observed_NEER,
    last_observed_NEER + forecast_value_1,
    last_observed_NEER + forecast_value_6,
    last_observed_NEER + forecast_value_12,
    last_observed_NEER + forecast_value_18
  ),
  Upper = c(
    NA,
    last_observed_NEER + ci_1$Upper,
    last_observed_NEER + ci_6$Upper,
    last_observed_NEER + ci_12$Upper,
    last_observed_NEER + ci_18$Upper
  ),
  Lower = c(
    NA,
    last_observed_NEER + ci_1$Lower,
    last_observed_NEER + ci_6$Lower,
    last_observed_NEER + ci_12$Lower,
    last_observed_NEER + ci_18$Lower
  )
)

# Plot forecasted NEER levels and Confidence Intervals
p=ggplot() +
  # Plot historical NEER values
  geom_line(data = combined_data_1$historical, aes(x = Date, y = nominal_effective_exchrate), 
            color = "blue", linewidth = 1.2) +
  # Plot forecasted NEER points
  geom_point(data = forecast_path[-1, ], aes(x = Date, y = nominal_effective_exchrate),
             color = "red", size = 3) +
  # Plot confidence intervals as vertical lines for each forecast
  geom_segment(data = forecast_path[-1, ], aes(x = Date, xend = Date, y = Lower, yend = Upper),
               color = "grey", linewidth = 1) +
  # Connect the actual NEER to the forecasted NEER levels
  geom_line(data = forecast_path, aes(x = Date, y = nominal_effective_exchrate), 
            color = "black", linetype = "solid", linewidth = 1) +
  # Connect the upper confidence intervals
  geom_line(data = forecast_path, aes(x = Date, y = Upper), 
            color = "darkred", linetype = "dashed", linewidth = 0.8) +
  # Connect the lower confidence intervals
  geom_line(data = forecast_path, aes(x = Date, y = Lower), 
            color = "darkred", linetype = "dashed", linewidth = 0.8) +
  # Mark the last observed NEER value (green vertical line)
  geom_vline(xintercept = as.Date("2024-09-01"), linetype = "dashed", color = "green", linewidth = 1) +
  # Titles and labels
  labs(
    title = "Forecasted NEER Levels with 95% CI",
    x = "Date",
    y = "NEER"
  ) +
  # Increase x-axis granularity
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months", limits = c(as.Date("2020-01-01"), as.Date("2026-03-01"))) +
  # Increase y-axis granularity
  scale_y_continuous(n.breaks = 10) + # Add more ticks on the y-axis
  # Themes for better visualization
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.border = element_rect(color = "lightgrey", fill = NA, linewidth = 1) # Updated to linewidth
  )
print(p)

# Calculate the percentage change in NEER relative to its value at 2024-09-01.

# Get the NEER value at 2024-09-01
base_neer <- forecast_path$nominal_effective_exchrate[forecast_path$Date == as.Date("2024-09-01")]

# Calculate the appreciation in percentage
forecast_path$percentage_appreciation <- ((forecast_path$nominal_effective_exchrate - base_neer) / base_neer) * 100

# View the updated forecast_path with the percentage appreciation
print(forecast_path)


# ===============================================================
# 4. RANDOM WALK BENCHMARK
# ===============================================================
# The Random Walk (RW) model serves as a baseline comparison 
# for forecasting NEER changes. It assumes that the best predictor 
# for the future NEER change is its most recent observed value. 
# This naive yet effective approach helps assess the performance 
# of model-based forecasts.

# ---------------------------------------------------------------
# 4.1 Compute RW benchmark for each horizon
# ---------------------------------------------------------------

# Function to compute Random Walk Benchmark
rw_benchmark <- function(estimation_data, actual_col, horizon, window_size) {
  # Initialize empty vectors for forecasted NEER and forecast errors
  forecasted_NEER_rw <- numeric()
  forecast_errors_rw <- numeric()
  
  # Define the starting point for out-of-sample evaluation
  evaluation_start <- window_size + 1
  
  # Loop through each forecast time point
  for (t in evaluation_start:(nrow(estimation_data))) {
    # RW model forecast for time t+horizon is the NEER change at time t
    rw_forecast <- estimation_data[[actual_col]][t]
    
    # Store the RW forecasted value for time t+horizon
    forecasted_NEER_rw <- c(forecasted_NEER_rw, rw_forecast)
    
    # Calculate the forecast error for the RW model
    actual_NEER <- estimation_data[[actual_col]][t + horizon]  # Actual NEER change at time t+horizon
    forecast_error_rw <- actual_NEER - rw_forecast  # Forecast error for RW model
    
    # Store the forecast error
    forecast_errors_rw <- c(forecast_errors_rw, forecast_error_rw)
  }
  
  # Return the forecasted values and forecast errors
  return(list(forecasted_NEER_rw = forecasted_NEER_rw, forecast_errors_rw = forecast_errors_rw))
}

# Compute RW benchmark for each horizon
rw_results_1 <- rw_benchmark(estimation_data_1, actual_col = "nominal_effective_exchrate", horizon = 1, window_size = window_size)
rw_results_6 <- rw_benchmark(estimation_data_6, actual_col = "NEER_6_month_change", horizon = 6, window_size = window_size)
rw_results_12 <- rw_benchmark(estimation_data_12, actual_col = "NEER_12_month_change", horizon = 12, window_size = window_size)
rw_results_18 <- rw_benchmark(estimation_data_18, actual_col = "NEER_18_month_change", horizon = 18, window_size = window_size)

# ---------------------------------------------------------------
# 4.2 Visualization for each horizon
# ---------------------------------------------------------------

# Visualization: 1-Month horizon
# ---------------------------------------------------------------

# Define the start
evaluation_start <- window_size + 1

# Define the time periods for plotting
time_periods_1 <- seq(evaluation_start, length(rw_results_1$forecasted_NEER_rw) + evaluation_start - 1)

# Plot for 1-Month Horizon
plot(time_periods_1, estimation_data_1$nominal_effective_exchrate[time_periods_1], type = "l", col = "blue",
     ylim = range(c(rw_results_1$forecasted_NEER_rw, forecasted_NEER_1, estimation_data_1$nominal_effective_exchrate[time_periods_1])),
     main = "NEER Forecasts: 1-Month Horizon", xlab = "Time", ylab = "NEER Change")
lines(time_periods_1, forecasted_NEER_1, col = "red", lty = 2)
lines(time_periods_1, rw_results_1$forecasted_NEER_rw, col = "green", lty = 2)
legend("topright", legend = c("Actual NEER", "Model Forecast", "RW Forecast"),
       col = c("blue", "red", "green"), lty = c(1, 2, 2))

# Plot forecast errors for 1-Month Horizon
plot(forecast_errors_1, type = "l", col = "red", lwd = 2,
     main = "Forecast Errors: 1-Month Horizon", xlab = "Time", ylab = "Error")
lines(rw_results_1$forecast_errors_rw, col = "green", lwd = 1)
legend("topright", legend = c("Model", "RW"), col = c("red", "green"), lty = 1)


# Visualization: 6-Month horizon
# ---------------------------------------------------------------

# Define the time periods for plotting
time_periods_6 <- seq(evaluation_start, length(rw_results_6$forecasted_NEER_rw) + evaluation_start - 1)

# Plot for 6-Month Horizon
plot(time_periods_6, estimation_data_6$NEER_6_month_change[time_periods_6], type = "l", col = "blue",
     ylim = range(c(rw_results_6$forecasted_NEER_rw, forecasted_NEER_6, estimation_data_6$NEER_6_month_change[time_periods_6])),
     main = "NEER Forecasts: 6-Month Horizon", xlab = "Time", ylab = "NEER Change")
lines(time_periods_6, forecasted_NEER_6, col = "red", lty = 2)
lines(time_periods_6, rw_results_6$forecasted_NEER_rw, col = "green", lty = 2)
legend("topright", legend = c("Actual NEER", "Model Forecast", "RW Forecast"),
       col = c("blue", "red", "green"), lty = c(1, 2, 2))

# Plot forecast errors for 6-Month Horizon
plot(forecast_errors_6, type = "l", col = "red", lwd = 2,
     main = "Forecast Errors: 6-Month Horizon", xlab = "Time", ylab = "Error")
lines(rw_results_6$forecast_errors_rw, col = "green", lwd = 1)
legend("topright", legend = c("Model", "RW"), col = c("red", "green"), lty = 1)


# Visualization: 12-Month horizon
# ---------------------------------------------------------------

# Define the time periods for plotting
time_periods_12 <- seq(evaluation_start, length(rw_results_12$forecasted_NEER_rw) + evaluation_start - 1)

# Plot for 12-Month Horizon
plot(time_periods_12, estimation_data_12$NEER_12_month_change[time_periods_12], type = "l", col = "blue",
     ylim = range(c(rw_results_12$forecasted_NEER_rw, forecasted_NEER_12, estimation_data_12$NEER_12_month_change[time_periods_12]), na.rm = TRUE),
     main = "NEER Forecasts: 12-Month Horizon", xlab = "Time", ylab = "NEER Change")
lines(time_periods_12, forecasted_NEER_12, col = "red", lty = 2)
lines(time_periods_12, rw_results_12$forecasted_NEER_rw, col = "green", lty = 2)
legend("topright", legend = c("Actual NEER", "Model Forecast", "RW Forecast"),
       col = c("blue", "red", "green"), lty = c(1, 2, 2))

# Plot forecast errors for 12-Month Horizon
plot(forecast_errors_12, type = "l", col = "red", lwd = 2,
     main = "Forecast Errors: 12-Month Horizon", xlab = "Time", ylab = "Error")
lines(rw_results_12$forecast_errors_rw, col = "green", lwd = 1)
legend("topright", legend = c("Model", "RW"), col = c("red", "green"), lty = 1)


# Visualization: 18-Month horizon
# ---------------------------------------------------------------

# Define the time periods for plotting
time_periods_18 <- seq(evaluation_start, length(rw_results_18$forecasted_NEER_rw) + evaluation_start - 1)

# Plot for 18-Month Horizon
plot(time_periods_18, estimation_data_18$NEER_18_month_change[time_periods_18], type = "l", col = "blue",
     ylim = range(c(rw_results_18$forecasted_NEER_rw, forecasted_NEER_18, estimation_data_18$NEER_18_month_change[time_periods_18]), na.rm = TRUE),
     main = "NEER Forecasts: 18-Month Horizon", xlab = "Time", ylab = "NEER Change")
lines(time_periods_18, forecasted_NEER_18, col = "red", lty = 2)
lines(time_periods_18, rw_results_18$forecasted_NEER_rw, col = "green", lty = 2)
legend("topright", legend = c("Actual NEER", "Model Forecast", "RW Forecast"),
       col = c("blue", "red", "green"), lty = c(1, 2, 2))

# Plot forecast errors for 18-Month Horizon
plot(forecast_errors_18, type = "l", col = "red", lwd = 2,
     main = "Forecast Errors: 18-Month Horizon", xlab = "Time", ylab = "Error")
lines(rw_results_18$forecast_errors_rw, col = "green", lwd = 1)
legend("topright", legend = c("Model", "RW"), col = c("red", "green"), lty = 1)



# ---------------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------------
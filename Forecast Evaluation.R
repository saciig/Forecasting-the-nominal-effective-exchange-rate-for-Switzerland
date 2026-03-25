# ===============================================================
# FORECAST EVALUATION SCRIPT
# ===============================================================
# This script evaluates the accuracy and reliability of forecasts 
# using:
#   - Forecast Unbiasedness Tests
#   - Forecast Efficiency Tests
#   - Mean Squared Forecast Error (MSFE) Analysis
#   - Diebold-Mariano Tests
# ===============================================================

# TABLE OF CONTENTS
# 1. Forecast unbiasedness
# 2. Forecast efficiency
# 3. Mean Squared Forecast Error (MSFE) Ratio
# 4. Diebold-Mariano Test
#    4.1 Loss Differential
#    4.2 Variance Analysis
#    4.3 DM Tests for Forecast Horizons
# 5. Spaghetti Chart
# ===============================================================


# ---------------------------------------------------------------
# 1. FORECAST UNBIASEDNESS
# ---------------------------------------------------------------
# This section evaluates whether forecasts are systematically 
# over- or under-predicting using hypothesis tests for forecast 
# unbiasedness. The null hypothesis states that the mean forecast 
# error (tau) equals zero.
# ===============================================================

# 1-Month horizon
# ---------------------------------------------------------------

# Define the forecast errors for the 1-month horizon
forecast_errors_1month <- forecast_errors_1 

# Estimate tau (mean of forecast errors)
tau_hat <- mean(forecast_errors_1month, na.rm = TRUE)

# Compute the residuals (in this case, they are the deviations from tau)
residuals <- forecast_errors_1month - tau_hat

# Compute robust variance of tau
n <- length(forecast_errors_1month)  # Number of observations
variance_robust <- (1 / (n^2)) * sum(residuals^2, na.rm = TRUE)

# Compute the standard error of tau
se_tau <- sqrt(variance_robust)

# Perform the t-test
t_stat <- tau_hat / se_tau
p_value <- 2 * (1 - pt(abs(t_stat), df = n - 1))  # Two-sided p-value

# Print the results
cat("Forecast Unbiasedness Test for 1-Month Horizon:\n")
cat("Tau (Mean Forecast Error):", tau_hat, "\n")
cat("Standard Error of Tau:", se_tau, "\n")
cat("t-Statistic:", t_stat, "\n")
cat("p-Value:", p_value, "\n")

# Interpretation
if (p_value > 0.05) {
  cat("The p-value is larger than 0.05, therefore we fail to reject the null hypothesis and our forecast is unbiased.\n")
} else {
  cat("The p-value is less than 0.05, therefore we reject the null hypothesis, indicating the forecast is biased.\n")
}

# 6-Month horizon
# ---------------------------------------------------------------

# Define the forecast errors for the 6-month horizon
forecast_errors_6month <- forecast_errors_6

# Estimate tau (mean of forecast errors)
tau_hat_6 <- mean(forecast_errors_6month, na.rm = TRUE)

# Compute the residuals (deviations from tau)
residuals_6 <- forecast_errors_6month - tau_hat_6

# Compute robust variance of tau
n_6 <- length(forecast_errors_6month)  # Number of observations for 6-month horizon
variance_robust_6 <- (1 / (n_6^2)) * sum(residuals_6^2, na.rm = TRUE)

# Compute the standard error of tau
se_tau_6 <- sqrt(variance_robust_6)

# Perform the t-test
t_stat_6 <- tau_hat_6 / se_tau_6
p_value_6 <- 2 * (1 - pt(abs(t_stat_6), df = n_6 - 1))  # Two-sided p-value

# Print the results
cat("Forecast Unbiasedness Test for 6-Month Horizon:\n")
cat("Tau (Mean Forecast Error):", tau_hat_6, "\n")
cat("Standard Error of Tau:", se_tau_6, "\n")
cat("t-Statistic:", t_stat_6, "\n")
cat("p-Value:", p_value_6, "\n")

# Interpretation
if (p_value_6 > 0.05) {
  cat("The p-value is larger than 0.05, therefore we fail to reject the null hypothesis and our forecast is unbiased.\n")
} else {
  cat("The p-value is less than 0.05, therefore we reject the null hypothesis, indicating the forecast is biased.\n")
}


# 12-Month horizon
# ---------------------------------------------------------------

# Define the forecast errors for the 12-month horizon
forecast_errors_12month <- forecast_errors_12

# Estimate tau (mean of forecast errors)
tau_hat_12 <- mean(forecast_errors_12month, na.rm = TRUE)

# Compute the residuals (deviations from tau)
residuals_12 <- forecast_errors_12month - tau_hat_12

# Compute robust variance of tau
n_12 <- length(forecast_errors_12month)  # Number of observations for 12-month horizon
variance_robust_12 <- (1 / (n_12^2)) * sum(residuals_12^2, na.rm = TRUE)

# Compute the standard error of tau
se_tau_12 <- sqrt(variance_robust_12)

# Perform the t-test
t_stat_12 <- tau_hat_12 / se_tau_12
p_value_12 <- 2 * (1 - pt(abs(t_stat_12), df = n_12 - 1))  # Two-sided p-value

# Print the results
cat("Forecast Unbiasedness Test for 12-Month Horizon:\n")
cat("Tau (Mean Forecast Error):", tau_hat_12, "\n")
cat("Standard Error of Tau:", se_tau_12, "\n")
cat("t-Statistic:", t_stat_12, "\n")
cat("p-Value:", p_value_12, "\n")

# Interpretation
if (p_value_12 > 0.05) {
  cat("The p-value is larger than 0.05, therefore we fail to reject the null hypothesis and our forecast is unbiased.\n")
} else {
  cat("The p-value is less than 0.05, therefore we reject the null hypothesis, indicating the forecast is biased.\n")
}


# 18-Month horizon
# ---------------------------------------------------------------

# Define the forecast errors for the 18-month horizon
forecast_errors_18month <- forecast_errors_18

# Estimate tau (mean of forecast errors)
tau_hat_18 <- mean(forecast_errors_18month, na.rm = TRUE)

# Compute the residuals (deviations from tau)
residuals_18 <- forecast_errors_18month - tau_hat_18

# Compute robust variance of tau
n_18 <- length(forecast_errors_18month)  # Number of observations for 18-month horizon
variance_robust_18 <- (1 / (n_18^2)) * sum(residuals_18^2, na.rm = TRUE)

# Compute the standard error of tau
se_tau_18 <- sqrt(variance_robust_18)

# Perform the t-test
t_stat_18 <- tau_hat_18 / se_tau_18
p_value_18 <- 2 * (1 - pt(abs(t_stat_18), df = n_18 - 1))  # Two-sided p-value

# Print the results
cat("Forecast Unbiasedness Test for 18-Month Horizon:\n")
cat("Tau (Mean Forecast Error):", tau_hat_18, "\n")
cat("Standard Error of Tau:", se_tau_18, "\n")
cat("t-Statistic:", t_stat_18, "\n")
cat("p-Value:", p_value_18, "\n")

# Interpretation
if (p_value_18 > 0.05) {
  cat("The p-value is larger than 0.05, therefore we fail to reject the null hypothesis and our forecast is unbiased.\n")
} else {
  cat("The p-value is less than 0.05, therefore we reject the null hypothesis, indicating the forecast is biased.\n")
}

# ---------------------------------------------------------------
# 2. FORECAST EFFICIENCY
# ---------------------------------------------------------------
# Forecast efficiency is tested using a regression approach, 
# where lagged predictors are regressed against forecast errors. 
# HAC-robust statistics are computed to account for autocorrelation.
# ===============================================================

# Forecast Efficiency Function
# ---------------------------------------------------------------

# Define the Forecast Efficiency Test Function with Joint Hypothesis Testing
forecast_efficiency_test_hac <- function(errors, predictors) {
  
  # Define the number of observations (T)
  T <- length(errors)
  
  # Construct the predictors matrix (including a constant term)
  predictors_matrix <- cbind(1, predictors)  # Add intercept to predictors
  
  # Perform OLS regression
  XtX_inv <- solve(t(predictors_matrix) %*% predictors_matrix)  # (X'X)^-1
  XtY <- t(predictors_matrix) %*% errors  # X'Y
  beta_hat <- XtX_inv %*% XtY  # Coefficients
  
  # Compute HAC variance-covariance matrix using sandwich package
  # Fit the model for NeweyWest HAC
  model <- lm(errors ~ predictors_matrix - 1)  # Ensure correct formula
  hac_cov <- NeweyWest(model, prewhite = FALSE, adjust = TRUE)
  
  # Calculate standard errors from HAC covariance matrix
  se_beta <- sqrt(diag(hac_cov))
  
  # Calculate t-statistics and p-values for individual coefficients
  t_stats <- beta_hat / se_beta
  p_values <- 2 * (1 - pt(abs(t_stats), df = T - ncol(predictors_matrix)))
  
  # Joint hypothesis testing for all coefficients (including intercept)
  F_stat <- (t(beta_hat) %*% solve(hac_cov) %*% beta_hat) / ncol(predictors_matrix)
  p_value_joint <- 1 - pf(F_stat, df1 = ncol(predictors_matrix), df2 = T - ncol(predictors_matrix))
  
  # Return results
  return(list(
    coefficients = beta_hat,
    se_beta = se_beta,
    t_stats = t_stats,
    p_values = p_values,
    residuals = errors - predictors_matrix %*% beta_hat,
    F_stat = F_stat,
    p_value_joint = p_value_joint
  ))
}


# 1-Month Horizon
# ---------------------------------------------------------------

# Correct Alignment of Predictors and Errors for h=1
aligned_inflation_1 <- 
  estimation_data_1$lagged_inflation_1[evaluation_start:(evaluation_start + length(forecast_errors_1) - 1)]
aligned_output_gap_1 <- 
  estimation_data_1$lagged_output_gap_1[evaluation_start:(evaluation_start + length(forecast_errors_1) - 1)]

# Extract out-of-sample dates for forecast errors
forecast_error_dates <- 
  nominal_effective_exchrate_data$Date[evaluation_start:(evaluation_start + length(forecast_errors_1) - 1)]

# Extract corresponding dates for aligned predictors
aligned_predictor_dates <- 
  nominal_effective_exchrate_data$Date[evaluation_start:(evaluation_start + length(forecast_errors_1) - 1)]

# Check if dates match
if (!all(forecast_error_dates == aligned_predictor_dates)) {
  stop("Dates for forecast errors and predictors do not match.")
}

# Construct Predictors Matrix
predictors <- cbind(aligned_inflation_1, aligned_output_gap_1)

# Run the Efficiency Test
efficiency_results_hac <- forecast_efficiency_test_hac(forecast_errors_1, predictors)

# Print Results
cat("Forecast Efficiency Test Results (Robust HAC):\n")
cat("Coefficients:\n", efficiency_results_hac$coefficients, "\n")
cat("Standard Errors:\n", efficiency_results_hac$se_beta, "\n")
cat("t-Statistics:\n", efficiency_results_hac$t_stats, "\n")
cat("p-Values:\n", efficiency_results_hac$p_values, "\n")
cat("Robust Joint Test F-Statistic:", efficiency_results_hac$F_stat, "\n")
cat("p-Value for Robust Joint Test:", efficiency_results_hac$p_value_joint, "\n")

# Interpretation
if (efficiency_results_hac$p_value_joint < 0.05) {
  cat("The joint hypothesis of forecast efficiency is rejected at the 5% significance level. The forecasts are not efficient.\n")
} else {
  cat("The joint hypothesis of forecast efficiency is not rejected at the 5% significance level. The forecasts are efficient.\n")
}


# 6-Month Horizon
# ---------------------------------------------------------------

# Correct Alignment of Predictors and Errors for h = 6
aligned_inflation_6 <- 
  estimation_data_6$lagged_inflation_6[evaluation_start:(evaluation_start + length(forecast_errors_6) - 1)]
aligned_output_gap_6 <-
  estimation_data_6$lagged_output_gap_6[evaluation_start:(evaluation_start + length(forecast_errors_6) - 1)]

# Extract out-of-sample dates for forecast errors
forecast_error_dates_6 <- 
  nominal_effective_exchrate_data$Date[evaluation_start:(evaluation_start + length(forecast_errors_6) - 1)]

# Extract corresponding dates for aligned predictors
aligned_predictor_dates_6 <- 
  nominal_effective_exchrate_data$Date[evaluation_start:(evaluation_start + length(forecast_errors_6) - 1)]

# Check if dates match
if (!all(forecast_error_dates_6 == aligned_predictor_dates_6)) {
  stop("Dates for forecast errors and predictors do not match for h = 6.")
}

# Construct Predictors Matrix for h = 6
predictors_6 <- cbind(aligned_inflation_6, aligned_output_gap_6)

# Run the Efficiency Test for h = 6 with HAC Robustness
efficiency_results_hac_6 <- forecast_efficiency_test_hac(forecast_errors_6, predictors_6)

# Print Results for h = 6
cat("Forecast Efficiency Test Results for h = 6 (Robust HAC):\n")
cat("Coefficients:\n", efficiency_results_hac_6$coefficients, "\n")
cat("Standard Errors:\n", efficiency_results_hac_6$se_beta, "\n")
cat("t-Statistics:\n", efficiency_results_hac_6$t_stats, "\n")
cat("p-Values:\n", efficiency_results_hac_6$p_values, "\n")
cat("Robust Joint Test F-Statistic:", efficiency_results_hac_6$F_stat, "\n")
cat("p-Value for Robust Joint Test:", efficiency_results_hac_6$p_value_joint, "\n")

# Interpretation for h = 6
if (efficiency_results_hac_6$p_value_joint < 0.05) {
  cat("The joint hypothesis of forecast efficiency is rejected at the 5% significance level. The forecasts for h = 6 are not efficient.\n")
} else {
  cat("The joint hypothesis of forecast efficiency is not rejected at the 5% significance level. The forecasts for h = 6 are efficient.\n")
}


# 12-Month Horizon
# ---------------------------------------------------------------

# Correct Alignment of Predictors and Errors for h = 12
aligned_inflation_12 <- 
  estimation_data_12$lagged_inflation_12[evaluation_start:(evaluation_start + length(forecast_errors_12) - 1)]
aligned_output_gap_12 <- 
  estimation_data_12$lagged_output_gap_12[evaluation_start:(evaluation_start + length(forecast_errors_12) - 1)]

# Extract out-of-sample dates for forecast errors
forecast_error_dates_12 <- 
  nominal_effective_exchrate_data$Date[evaluation_start:(evaluation_start + length(forecast_errors_12) - 1)]

# Extract corresponding dates for aligned predictors
aligned_predictor_dates_12 <- 
  nominal_effective_exchrate_data$Date[evaluation_start:(evaluation_start + length(forecast_errors_12) - 1)]

# Check if dates match
if (!all(forecast_error_dates_12 == aligned_predictor_dates_12)) {
  stop("Dates for forecast errors and predictors do not match for h = 12.")
}

# Construct Predictors Matrix for h = 12
predictors_12 <- cbind(aligned_inflation_12, aligned_output_gap_12)

# Run the Efficiency Test for h = 12 with HAC Robustness
efficiency_results_hac_12 <- forecast_efficiency_test_hac(forecast_errors_12, predictors_12)

# Print Results for h = 12
cat("Forecast Efficiency Test Results for h = 12 (Robust HAC):\n")
cat("Coefficients:\n", efficiency_results_hac_12$coefficients, "\n")
cat("Standard Errors:\n", efficiency_results_hac_12$se_beta, "\n")
cat("t-Statistics:\n", efficiency_results_hac_12$t_stats, "\n")
cat("p-Values:\n", efficiency_results_hac_12$p_values, "\n")
cat("Robust Joint Test F-Statistic:", efficiency_results_hac_12$F_stat, "\n")
cat("p-Value for Robust Joint Test:", efficiency_results_hac_12$p_value_joint, "\n")

# Interpretation for h = 12
if (efficiency_results_hac_12$p_value_joint < 0.05) {
  cat("The joint hypothesis of forecast efficiency is rejected at the 5% significance level. The forecasts for h = 12 are not efficient.\n")
} else {
  cat("The joint hypothesis of forecast efficiency is not rejected at the 5% significance level. The forecasts for h = 12 are efficient.\n")
}


# 18-Month Horizon
# ---------------------------------------------------------------

# Correct Alignment of Predictors and Errors for h = 18
aligned_inflation_18 <- 
  estimation_data_18$lagged_inflation_18[evaluation_start:(evaluation_start + length(forecast_errors_18) - 1)]
aligned_output_gap_18 <- 
  estimation_data_18$lagged_output_gap_18[evaluation_start:(evaluation_start + length(forecast_errors_18) - 1)]

# Extract out-of-sample dates for forecast errors
forecast_error_dates_18 <- 
  nominal_effective_exchrate_data$Date[evaluation_start:(evaluation_start + length(forecast_errors_18) - 1)]

# Extract corresponding dates for aligned predictors
aligned_predictor_dates_18 <- 
  nominal_effective_exchrate_data$Date[evaluation_start:(evaluation_start + length(forecast_errors_18) - 1)]

# Check if dates match
if (!all(forecast_error_dates_18 == aligned_predictor_dates_18)) {
  stop("Dates for forecast errors and predictors do not match for h = 18.")
}

# Construct Predictors Matrix for h = 18
predictors_18 <- cbind(aligned_inflation_18, aligned_output_gap_18)

# Run the Efficiency Test for h = 18 with HAC Robustness
efficiency_results_hac_18 <- forecast_efficiency_test_hac(forecast_errors_18, predictors_18)

# Print Results for h = 18
cat("Forecast Efficiency Test Results for h = 18 (Robust HAC):\n")
cat("Coefficients:\n", efficiency_results_hac_18$coefficients, "\n")
cat("Standard Errors:\n", efficiency_results_hac_18$se_beta, "\n")
cat("t-Statistics:\n", efficiency_results_hac_18$t_stats, "\n")
cat("p-Values:\n", efficiency_results_hac_18$p_values, "\n")
cat("Robust Joint Test F-Statistic:", efficiency_results_hac_18$F_stat, "\n")
cat("p-Value for Robust Joint Test:", efficiency_results_hac_18$p_value_joint, "\n")

# Interpretation for h = 18
if (efficiency_results_hac_18$p_value_joint < 0.05) {
  cat("The joint hypothesis of forecast efficiency is rejected at the 5% significance level. The forecasts for h = 18 are not efficient.\n")
} else {
  cat("The joint hypothesis of forecast efficiency is not rejected at the 5% significance level. The forecasts for h = 18 are efficient.\n")
}


# ---------------------------------------------------------------
# 3. MEAN SQUARED FORECAST ERROR (MSFE) RATIO
# ---------------------------------------------------------------
# This section evaluates the forecast accuracy using the Mean Squared 
# Forecast Error (MSFE) and MSFE ratio for comparison with a benchmark 
# model (Random Walk).
# ===============================================================

# ---------------------------------------------------------------
# MSFE and MSFE Ratio Calculation Function
# ---------------------------------------------------------------

# Function to calculate MSFE and MSFE Ratio
calculate_msfe_and_ratio <- function(model_errors, benchmark_errors) {
  
  # Calculate MSFE for the model and benchmark
  msfe_model <- mean(model_errors^2, na.rm = TRUE)
  msfe_benchmark <- mean(benchmark_errors^2, na.rm = TRUE)
  
  # Calculate the MSFE ratio
  msfe_ratio <- msfe_model / msfe_benchmark
  
  # Return the results
  return(list(
    msfe_model = msfe_model,
    msfe_benchmark = msfe_benchmark,
    msfe_ratio = msfe_ratio
  ))
}


# 1-Month Horizon
# ---------------------------------------------------------------

# Extract forecast errors for the 1-month horizon
forecast_errors_1month <- forecast_results_1$forecast_errors
benchmark_errors_1month <- rw_results_1$forecast_errors_rw

# Compute MSFE and ratio
msfe_results_1month <- calculate_msfe_and_ratio(forecast_errors_1month, benchmark_errors_1month)

# Print results
cat("\n1-Month Horizon:\n")
cat("MSFE (Model):", msfe_results_1month$msfe_model, "\n")
cat("MSFE (Benchmark):", msfe_results_1month$msfe_benchmark, "\n")
cat("MSFE Ratio:", msfe_results_1month$msfe_ratio, "\n")

# Interpretation
if (msfe_results_1month$msfe_ratio < 1) {
  cat("Our model outperforms the random walk benchmark.\n")
} else if (msfe_results_1month$msfe_ratio > 1) {
  cat("The random walk benchmark outperforms our model.\n")
} else {
  cat("Our model and benchmark have equal forecast accuracy.\n")
}


# 6-Month Horizon
# ---------------------------------------------------------------

# Extract forecast errors for the 6-month horizon
forecast_errors_6month <- forecast_results_6$forecast_errors
benchmark_errors_6month <- rw_results_6$forecast_errors_rw

# Compute MSFE and ratio using the function
msfe_results_6month <- calculate_msfe_and_ratio(forecast_errors_6month, benchmark_errors_6month)

# Print the results
cat("6-Month Horizon:\n")
cat("MSFE (Model):", msfe_results_6month$msfe_model, "\n")
cat("MSFE (Benchmark):", msfe_results_6month$msfe_benchmark, "\n")
cat("MSFE Ratio:", msfe_results_6month$msfe_ratio, "\n")

# Interpretation based on the ratio
if (msfe_results_6month$msfe_ratio < 1) {
  cat("Our model outperforms the random walk benchmark.\n")
} else if (msfe_results_6month$msfe_ratio > 1) {
  cat("The random walk benchmark outperforms our model.\n")
} else {
  cat("Our model and benchmark have equal forecast accuracy.\n")
}


# 12-Month Horizon
# ---------------------------------------------------------------

# Extract forecast errors for the 12-month horizon
forecast_errors_12month <- forecast_results_12$forecast_errors
benchmark_errors_12month <- rw_results_12$forecast_errors_rw

# Compute MSFE and ratio using the function
msfe_results_12month <- calculate_msfe_and_ratio(forecast_errors_12month, benchmark_errors_12month)

# Print the results
cat("12-Month Horizon:\n")
cat("MSFE (Model):", msfe_results_12month$msfe_model, "\n")
cat("MSFE (Benchmark):", msfe_results_12month$msfe_benchmark, "\n")
cat("MSFE Ratio:", msfe_results_12month$msfe_ratio, "\n")

# Interpretation based on the ratio
if (msfe_results_12month$msfe_ratio < 1) {
  cat("Our model outperforms the random walk benchmark.\n")
} else if (msfe_results_12month$msfe_ratio > 1) {
  cat("The random walk benchmark outperforms our model.\n")
} else {
  cat("Our model and benchmark have equal forecast accuracy.\n")
}


# 18-Month Horizon
# ---------------------------------------------------------------

# Extract forecast errors for the 18-month horizon
forecast_errors_18month <- forecast_results_18$forecast_errors
benchmark_errors_18month <- rw_results_18$forecast_errors_rw

# Compute MSFE and ratio using the function
msfe_results_18month <- calculate_msfe_and_ratio(forecast_errors_18month, benchmark_errors_18month)

# Print the results
cat("18-Month Horizon:\n")
cat("MSFE (Model):", msfe_results_18month$msfe_model, "\n")
cat("MSFE (Benchmark):", msfe_results_18month$msfe_benchmark, "\n")
cat("MSFE Ratio:", msfe_results_18month$msfe_ratio, "\n")

# Interpretation based on the ratio
if (msfe_results_18month$msfe_ratio < 1) {
  cat("Our model outperforms the random walk benchmark.\n")
} else if (msfe_results_18month$msfe_ratio > 1) {
  cat("The random walk benchmark outperforms our model.\n")
} else {
  cat("Our model and benchmark have equal forecast accuracy.\n")
}


# ===============================================================
# 4. DIEBOLD-MARIANO TEST
# ===============================================================
# This section compares forecast accuracy between the model and 
# the benchmark (Random Walk) using the Diebold-Mariano test. 
# ===============================================================

# ---------------------------------------------------------------
# 4.1 Loss Differential
# ---------------------------------------------------------------
# The loss differential is calculated as the difference in squared 
# forecast errors between the model and the benchmark. Plotting this 
# over time helps identify periods when the model consistently 
# underperforms or outperforms the benchmark.

# Calculate the loss differential
loss_diff <- (forecast_results_1$forecast_errors^2) - (rw_results_1$forecast_errors_rw^2)

# Extract the out-of-sample dates for plotting
out_of_sample_dates <- nominal_effective_exchrate_data$Date[evaluation_start:(evaluation_start + length(loss_diff) - 1)]

# Plot the loss differential over time
plot(out_of_sample_dates, loss_diff, type = "l", col = "blue",
     main = "Loss Differential Over Time (Model vs Benchmark)",
     xlab = "Date", ylab = "Loss Differential (Model - Benchmark)")
abline(h = 0, col = "red", lty = 2)  # Reference line at 0
legend("topright", legend = c("Loss Differential", "Zero Line"), col = c("blue", "red"), lty = c(1, 2))


# ---------------------------------------------------------------
# 4.2 Variance Analysis
# ---------------------------------------------------------------
# Compare the variance of the forecast errors for the model and the 
# benchmark. This can help understand if higher variability in the 
# model’s errors affects the DM test.

# Calculate variance of forecast errors
variance_model_errors <- var(forecast_results_1$forecast_errors, na.rm = TRUE)
variance_benchmark_errors <- var(rw_results_1$forecast_errors_rw, na.rm = TRUE)

# Print variances
cat("Variance of Model Forecast Errors:", variance_model_errors, "\n")
cat("Variance of Benchmark Forecast Errors:", variance_benchmark_errors, "\n")

# Plot histogram of forecast errors for the model
hist(forecast_results_1$forecast_errors, breaks = 20, col = "lightblue",
     main = "Histogram of Model Forecast Errors",
     xlab = "Forecast Error", probability = TRUE)
curve(dnorm(x, mean = mean(forecast_results_1$forecast_errors, na.rm = TRUE),
            sd = sqrt(variance_model_errors)),
      col = "blue", lwd = 2, add = TRUE)

# Plot histogram of forecast errors for the benchmark
hist(rw_results_1$forecast_errors_rw, breaks = 20, col = "lightgreen",
     main = "Histogram of Benchmark Forecast Errors",
     xlab = "Forecast Error", probability = TRUE)
curve(dnorm(x, mean = mean(rw_results_1$forecast_errors_rw, na.rm = TRUE),
            sd = sqrt(variance_benchmark_errors)),
      col = "darkgreen", lwd = 2, add = TRUE)


# ---------------------------------------------------------------
# 4.3 Diebold-Mariano Test Function
# ---------------------------------------------------------------

diebold_mariano_test <- function(model_errors, benchmark_errors, loss_function = function(x) x^2) {
  
  # Calculate the loss differential
  loss_diff <- loss_function(model_errors) - loss_function(benchmark_errors)
  
  # Mean of the loss differential
  mean_loss_diff <- mean(loss_diff, na.rm = TRUE)
  
  # Newey-West HAC variance estimator
  n <- length(loss_diff)
  lag <- floor(1.5 * (n^(1/3)))  # Suggested lag length
  hac_variance <- sum((loss_diff - mean_loss_diff)^2, na.rm = TRUE) / n
  for (k in 1:lag) {
    gamma_k <- sum((loss_diff[-(1:k)] - mean_loss_diff) * (loss_diff[-((n-k+1):n)] - mean_loss_diff), na.rm = TRUE) / n
    hac_variance <- hac_variance + 2 * (1 - k / (lag + 1)) * gamma_k
  }
  
  # DM test statistic
  dm_stat <- mean_loss_diff / sqrt(hac_variance / n)
  
  # One-sided p-value (our model is better)
  p_value <- 1 - pnorm(dm_stat)
  
  # Return results
  return(list(dm_stat = dm_stat, p_value = p_value))
}


# 1-Month Horizon
# ---------------------------------------------------------------

# Compute DM test for 1-month horizon
dm_test_1month <- diebold_mariano_test(
  model_errors = forecast_results_1$forecast_errors,
  benchmark_errors = rw_results_1$forecast_errors_rw
)

# Print Results
cat("Diebold-Mariano Test for 1-Month Horizon (One-Sided):\n")
cat("DM Statistic:", dm_test_1month$dm_stat, "\n")
cat("One-Sided p-Value:", dm_test_1month$p_value, "\n")

# Interpretation based on DM Statistic
if (dm_test_1month$p_value < 0.05) {
  cat("For the 1-Month Horizon, our model significantly outperforms the benchmark (p < 0.05).\n")
} else {
  cat("For the 1-Month Horizon, our model does not significantly outperform the benchmark (p >= 0.05).\n")
}


# 6-Month Horizon
# ---------------------------------------------------------------

# Compute DM test for 6-month horizon
dm_test_6month <- diebold_mariano_test(
  model_errors = forecast_results_6$forecast_errors,
  benchmark_errors = rw_results_6$forecast_errors_rw
)

# Print Results
cat("Diebold-Mariano Test for 6-Month Horizon:\n")
cat("DM Statistic:", dm_test_6month$dm_stat, "\n")
cat("p-Value:", dm_test_6month$p_value, "\n")

# Interpretation based on DM Statistic
if (dm_test_6month$p_value < 0.05) {
  cat("For the 6-Month Horizon, the model significantly differs from the benchmark (p < 0.05).\n")
} else {
  cat("For the 6-Month Horizon, our model does not significantly outperform the benchmark (p >= 0.05).\n")
}


# 12-Month Horizon
# ---------------------------------------------------------------

# Compute DM test for 12-month horizon
dm_test_12month <- diebold_mariano_test(
  model_errors = forecast_results_12$forecast_errors,
  benchmark_errors = rw_results_12$forecast_errors_rw
)

# Print Results
cat("Diebold-Mariano Test for 12-Month Horizon:\n")
cat("DM Statistic:", dm_test_12month$dm_stat, "\n")
cat("p-Value:", dm_test_12month$p_value, "\n")

# Interpretation based on DM Statistic
if (dm_test_12month$p_value < 0.05) {
  cat("For the 12-Month Horizon, the model significantly differs from the benchmark (p < 0.05).\n")
} else {
  cat("For the 12-Month Horizon, the model does not significantly differ from the benchmark (p >= 0.05).\n")
}


# 18-Month Horizon
# ---------------------------------------------------------------

# Compute DM test for 18-month horizon
dm_test_18month <- diebold_mariano_test(
  model_errors = forecast_results_18$forecast_errors,
  benchmark_errors = rw_results_18$forecast_errors_rw
)

# Print Results
cat("Diebold-Mariano Test for 18-Month Horizon:\n")
cat("DM Statistic:", dm_test_18month$dm_stat, "\n")
cat("p-Value:", dm_test_18month$p_value, "\n")

# Interpretation based on DM Statistic
if (dm_test_18month$p_value < 0.05) {
  cat("For the 18-Month Horizon, the model significantly differs from the benchmark (p < 0.05).\n")
} else {
  cat("For the 18-Month Horizon, the model does not significantly differ from the benchmark (p >= 0.05).\n")
}


# ===============================================================
# 5. Spaghetti Chart
# ===============================================================
# This section visualizes the actual NEER (Nominal Effective Exchange Rate)
# levels alongside the forecasted NEER levels for various horizons.
# The chart demonstrates the progression of forecasts over time
# relative to the actual NEER values.
# ===============================================================
# ===============================================================

# Initialize spaghetti data
spaghetti_data <- data.frame(
  Date = neer_levels_data$Date, # Dates of the observations
  Actual_NEER = neer_levels_data$nominal_effective_exchrate # Actual NEER levels
)

# Start from the first forecast point (e.g., "2009-02-01")
start_index <- which(neer_levels_data$Date == as.Date("2009-02-01"))

# Initialize the forecast_lines data frame to store forecast lines
forecast_lines <- data.frame(
  Start_Date = as.Date(character()),
  Horizon_Date = as.Date(character()),
  Forecast_Level = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each forecast point to calculate forecast levels
for (t in start_index:(nrow(neer_levels_data) - 12)) {
  base_neer <- neer_levels_data$nominal_effective_exchrate[t] # Base NEER value at forecast start

  # Temporary storage for this forecast point
  temp_line <- data.frame(
    Start_Date = as.Date(character()),
    Horizon_Date = as.Date(character()),
    Forecast_Level = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 1-Month forecast
  if (!is.na(forecasted_NEER_1[t - start_index + 1]) && (t + 1) <= nrow(neer_levels_data)) {
    temp_line <- rbind(
      temp_line,
      data.frame(
        Start_Date = neer_levels_data$Date[t],
        Horizon_Date = neer_levels_data$Date[t + 1],
        Forecast_Level = base_neer + forecasted_NEER_1[t - start_index + 1]
      )
    )
  }
  
  # 6-Month forecast
  if (!is.na(forecasted_NEER_6[t - start_index + 1]) && (t + 6) <= nrow(neer_levels_data)) {
    temp_line <- rbind(
      temp_line,
      data.frame(
        Start_Date = neer_levels_data$Date[t],
        Horizon_Date = neer_levels_data$Date[t + 6],
        Forecast_Level = base_neer + forecasted_NEER_6[t - start_index + 1]
      )
    )
  }
  
  # 12-Month forecast
  if (!is.na(forecasted_NEER_12[t - start_index + 1]) && (t + 12) <= nrow(neer_levels_data)) {
    temp_line <- rbind(
      temp_line,
      data.frame(
        Start_Date = neer_levels_data$Date[t],
        Horizon_Date = neer_levels_data$Date[t + 12],
        Forecast_Level = base_neer + forecasted_NEER_12[t - start_index + 1]
      )
    )
  }
  
  # 18-Month forecast
  if (!is.na(forecasted_NEER_18[t - start_index + 1]) && (t + 18) <= nrow(neer_levels_data)) {
    temp_line <- rbind(
      temp_line,
      data.frame(
        Start_Date = neer_levels_data$Date[t],
        Horizon_Date = neer_levels_data$Date[t + 18],
        Forecast_Level = base_neer + forecasted_NEER_18[t - start_index + 1]
      )
    )
  }
  
  # Append the temporary line to the forecast_lines data frame
  if (nrow(temp_line) > 0) {  # Only append if temp_line has rows
    forecast_lines <- rbind(forecast_lines, temp_line)
  }
}


# ---------------------------------------------------------------
# Plot the spaghetti graph
# ---------------------------------------------------------------

# Plot the actual NEER levels
plot(neer_levels_data$Date, neer_levels_data$nominal_effective_exchrate, type = "l", col = "black", lwd = 2,
     main = "Spaghetti Graph: Forecasted NEER Levels",
     xlab = "Date", ylab = "NEER Levels", xlim = range(forecast_lines$Horizon_Date, na.rm = TRUE))

# Add forecast lines for each start date
unique_start_dates <- unique(forecast_lines$Start_Date)

for (start_date in unique_start_dates) {
  temp_forecasts <- forecast_lines[forecast_lines$Start_Date == start_date, ]
  lines(temp_forecasts$Horizon_Date, temp_forecasts$Forecast_Level, col = "blue", lty = 2)
}

# Add a legend to the plot
legend("topright", legend = c("Actual NEER", "Forecasted NEER (All Horizons)"),
       col = c("black", "blue"), lty = c(1, 2), lwd = 2)



# ---------------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------------
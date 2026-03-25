# ===============================================================
# DENSITY FORECAST SCRIPT
# ===============================================================
# This script evaluates the density forecasts for NEER changes
# based on prediction intervals and Probability Integral Transforms (PITs).
# The script includes:
# - Prediction Interval Evaluation
# - Likelihood Ratio (LR) Tests for Confidence Intervals
# - Uniformity and Independence of PITs
# ===============================================================

# ===============================================================
# TABLE OF CONTENTS
# ===============================================================
# 1. Prediction Interval Evaluation
# 2. Likelihood Ratio Tests for Confidence Intervals
# 3. Uniformity and Independence of PITs
# ===============================================================


# ---------------------------------------------------------------
# 1. PREDICTION INTERVAL EVALUATION
# ---------------------------------------------------------------
# This section evaluates prediction intervals by calculating 
# violation rates and empirical coverage rates.
# ---------------------------------------------------------------

# Define z-scores for prediction intervals
z_80 <- qnorm(0.9)   # 80% interval (10% outside)
z_90 <- qnorm(0.95)  # 90% interval (5% outside)
z_95 <- qnorm(0.975) # 95% interval (2.5% outside)

# Compute prediction intervals
prediction_intervals <- data.frame(
  Lower_80 = forecasted_NEER_1 - z_80 * sqrt(total_variances_1),
  Upper_80 = forecasted_NEER_1 + z_80 * sqrt(total_variances_1),
  Lower_90 = forecasted_NEER_1 - z_90 * sqrt(total_variances_1),
  Upper_90 = forecasted_NEER_1 + z_90 * sqrt(total_variances_1),
  Lower_95 = forecasted_NEER_1 - z_95 * sqrt(total_variances_1),
  Upper_95 = forecasted_NEER_1 + z_95 * sqrt(total_variances_1)
)

# Define hit indicators for each interval
# Note: hit_80 take the value of one if realized NEER fall outside the prediction interval
hit_80 <- actual_NEER_realizations_1 < prediction_intervals$Lower_80 | 
  actual_NEER_realizations_1 > prediction_intervals$Upper_80

hit_90 <- actual_NEER_realizations_1 < prediction_intervals$Lower_90 | 
  actual_NEER_realizations_1 > prediction_intervals$Upper_90

hit_95 <- actual_NEER_realizations_1 < prediction_intervals$Lower_95 | 
  actual_NEER_realizations_1 > prediction_intervals$Upper_95

# Calculate empirical coverage rates (proportion inside the interval)
violation_rate_80 <- mean(hit_80, na.rm = TRUE)
violation_rate_90 <- mean(hit_90, na.rm = TRUE)
violation_rate_95 <- mean(hit_95, na.rm = TRUE)

# Report violation rates
cat("Violation rates:\n")
cat("80% CI Violation rate:", violation_rate_80, " (Expected: 20%)\n")
cat("90% CI Violation rate:", violation_rate_90, " (Expected: 10%)\n")
cat("95% CI Violation rate:", violation_rate_95, " (Expected: 5%)\n")

# Interpretation: Intervals are too wide for low confidence levels and 
# the high confidence is correctly specified


# ---------------------------------------------------------------
# 2. LIKELIHOOD RATIO TESTS FOR CONFIDENCE INTERVALS
# ---------------------------------------------------------------
# Test whether the observed violation rates match the expected 
# levels using LR tests for 80%, 90%, and 95% intervals.
# ---------------------------------------------------------------

# Define violation rates and expected alpha levels for each CI
alpha_levels <- c(0.20, 0.10, 0.05)  # Corresponding to 80%, 90%, 95% CIs
hit_indicators <- list(hit_80, hit_90, hit_95)  # List of hit indicators for each CI

# Initialize a data frame to store results
lr_test_results <- data.frame(
  Confidence_Level = c("80%", "90%", "95%"),
  Observed_Alpha = numeric(length(alpha_levels)),
  LR_Statistic = numeric(length(alpha_levels)),
  P_Value = numeric(length(alpha_levels)),
  Test_Result = character(length(alpha_levels))
)

# Loop over each CI level
for (i in 1:length(alpha_levels)) {
  
  # Extract hit indicator for current CI
  hits <- hit_indicators[[i]]
  
  # Compute the number of observations and violations
  P <- length(hits)  # Total number of observations
  n0 <- sum(hits, na.rm = TRUE)  # Number of violations (hits = 1)
  n1 <- P - n0  # Number of non-violations (hits = 0)
  
  # Compute the observed violation rate (MLE of alpha)
  observed_alpha <- n0 / P
  
  # Compute likelihood under unrestricted (observed_alpha) and restricted (alpha_levels[i]) models
  unrestricted_log_likelihood <- n0 * log(observed_alpha) + n1 * log(1 - observed_alpha)
  restricted_log_likelihood <- n0 * log(alpha_levels[i]) + n1 * log(1 - alpha_levels[i])
  
  # Compute the LR statistic
  LR_stat <- 2 * (unrestricted_log_likelihood - restricted_log_likelihood)
  
  # Compute the p-value using chi-squared distribution (1 degree of freedom)
  p_value <- pchisq(LR_stat, df = 1, lower.tail = FALSE)
  
  # Determine test result
  test_result <- ifelse(p_value < 0.05, "Reject H0", "Fail to Reject H0")
  
  # Store results in the data frame
  lr_test_results$Observed_Alpha[i] <- observed_alpha
  lr_test_results$LR_Statistic[i] <- LR_stat
  lr_test_results$P_Value[i] <- p_value
  lr_test_results$Test_Result[i] <- test_result
}

print(lr_test_results)

# Interpretation: The 90 and 95 % CIs are well specified according to the test. 


# Check the auto correlation assumption
# ---------------------------------------------------------------

# Example data for hit indicators (replace with your actual hit data)
hit_80 <- sample(c(0, 1), 186, replace = TRUE, prob = c(0.9, 0.1))  
hit_90 <- sample(c(0, 1), 186, replace = TRUE, prob = c(0.95, 0.05))  
hit_95 <- sample(c(0, 1), 186, replace = TRUE, prob = c(0.975, 0.025))  

# Combine the hit indicators into a list
hit_data <- list("80% CI" = hit_80, "90% CI" = hit_90, "95% CI" = hit_95)

# Define the number of lags to test
num_lags <- 10

# Initialize an empty data frame to store results
ljung_box_results <- data.frame()

# Loop through each confidence interval and perform the Ljung-Box test
for (ci_label in names(hit_data)) {
  hit_series <- hit_data[[ci_label]]
  
  # Perform the Ljung-Box test for each lag
  lb_test <- Box.test(hit_series, lag = num_lags, type = "Ljung-Box")
  
  # Store results
  result <- data.frame(
    Confidence_Level = ci_label,
    Statistic = lb_test$statistic,
    P_Value = lb_test$p.value
  )
  
  # Append results
  ljung_box_results <- rbind(ljung_box_results, result)
}

# Print results
print(ljung_box_results)

# Interpretation: No autocorrelation found 


# ---------------------------------------------------------------
# 3. UNIFORMITY AND INDEPENDENCE OF PITs
# ---------------------------------------------------------------
# Test the PITs for uniformity, independence, and time-varying
# properties using histograms, ACF, and structural break tests.
# ---------------------------------------------------------------

# Step 1: Compute PITs
# We use the CDF of a normal distribution to derive the PITs
PITs <- pnorm((actual_NEER_realizations_1 - forecasted_NEER_1) / sqrt(total_variances_1))

# Step 2: Define Histogram Bins and Calculate Frequencies
n_b <- 5  # Number of bins
bin_edges <- seq(0, 1, length.out = n_b + 1)  # Equally spaced bin edges from 0 to 1
P <- length(PITs)  # Total number of PITs
bin_counts <- hist(PITs, breaks = bin_edges, plot = FALSE)$counts  # Count PITs in each bin
p_hat <- bin_counts / P  # Proportion of PITs in each bin

# Step 3: Compute Confidence Intervals
p_theoretical <- 1 / n_b  # Theoretical proportion of PITs per bin
z_alpha <- qnorm(0.975)  # Critical value for 95% confidence level
ci_lower <- p_theoretical - z_alpha * sqrt(p_theoretical * (1 - p_theoretical) / P)
ci_upper <- p_theoretical + z_alpha * sqrt(p_theoretical * (1 - p_theoretical) / P)

# Step 4: Plot the Histogram with Confidence Intervals
hist(PITs, breaks = bin_edges, freq = TRUE, col = "lightblue", border = "black",
     main = "Histogram Test of PIT Uniformity",
     xlab = "PIT Values", ylab = "Frequency", ylim = c(0, max(bin_counts) * 1.2))

# Add theoretical uniform proportion as a line
abline(h = p_theoretical * P, col = "red", lty = 2, lwd = 2)  # Scaled theoretical proportion

# Add confidence interval lines
abline(h = ci_lower * P, col = "black", lty = 3, lwd = 2)  # Scaled lower confidence interval
abline(h = ci_upper * P, col = "black", lty = 3, lwd = 2)  # Scaled upper confidence interval

# Add legend for clarity
legend("topright", legend = c("Theoretical Proportion", "Confidence Interval"),
       col = c("red", "black"), lty = c(2, 3), lwd = c(2, 2), cex = 0.8)

# Interpration: Higher density of PITs value between 0,4 and 0,6.


# PITs with more bins 
# ---------------------------------------------------------------

# Step 1: Compute PITs
# We use the CDF of a normal distribution to derive the PITs
PITs <- pnorm((actual_NEER_realizations_1 - forecasted_NEER_1) / sqrt(total_variances_1))

# Step 2: Define Histogram Bins and Calculate Frequencies
n_b <- 10  # Number of bins
bin_edges <- seq(0, 1, length.out = n_b + 1)  # Equally spaced bin edges from 0 to 1
P <- length(PITs)  # Total number of PITs
bin_counts <- hist(PITs, breaks = bin_edges, plot = FALSE)$counts  # Count PITs in each bin
p_hat <- bin_counts / P  # Proportion of PITs in each bin

# Step 3: Compute Confidence Intervals
p_theoretical <- 1 / n_b  # Theoretical proportion of PITs per bin
z_alpha <- qnorm(0.975)  # Critical value for 95% confidence level
ci_lower <- p_theoretical - z_alpha * sqrt(p_theoretical * (1 - p_theoretical) / P)
ci_upper <- p_theoretical + z_alpha * sqrt(p_theoretical * (1 - p_theoretical) / P)

# Step 4: Plot the Histogram with Confidence Intervals
hist(PITs, breaks = bin_edges, freq = TRUE, col = "lightblue", border = "black",
     main = "Histogram Test of PIT Uniformity",
     xlab = "PIT Values", ylab = "Frequency", ylim = c(0, max(bin_counts) * 1.2))

# Add theoretical uniform proportion as a line
abline(h = p_theoretical * P, col = "red", lty = 2, lwd = 2)  # Scaled theoretical proportion

# Add confidence interval lines
abline(h = ci_lower * P, col = "black", lty = 3, lwd = 2)  # Scaled lower confidence interval
abline(h = ci_upper * P, col = "black", lty = 3, lwd = 2)  # Scaled upper confidence interval

# Add legend for clarity
legend("topright", legend = c("Theoretical Proportion", "Confidence Interval"),
       col = c("red", "black"), lty = c(2, 3), lwd = c(2, 2), cex = 0.8)


# Interpretation: these results highlight the model bias to predict more moderate values, 
# it highlights failures to capture less extreme events.


# Testing autocorrelation of PITs
# ---------------------------------------------------------------

# Step 1: Check Autocorrelation of PITs
acf(PITs, main = "Autocorrelation of PITs", lag.max = 20)

# Step 2: Add Confidence Bands (Two Standard Errors)
# Standard error for each lag is sqrt(1/n), where n is the number of PIT values
n <- length(PITs)
se <- 1 / sqrt(n)

# Add the bands to the ACF plot
abline(h = c(-2*se, 2*se), col = "red", lty = 2) 

# Interpretation: No auto corr detected

# Note: Checking mean and variance of PITs are time-varying
# Add a time index to the data
time_index <- seq_along(PITs)

# Test if the mean is time-varying
model_mean_time <- lm(PITs ~ time_index)
summary(model_mean_time)

# Test if the variance is time-varying
model_variance_time <- lm(I(PITs^2) ~ time_index)
summary(model_variance_time)

# Step 3: QLR Test for Structural Breaks
# This test checks if there is a structural break in the series.
# It compares the likelihood of models with and without a break in the PIT series.

# Andrews' QLR test for the mean of PITs
qlr_test_mean <- sctest(PITs ~ 1, type = "supF")
print(qlr_test_mean)

# Andrews' QLR test for the variance of PITs
qlr_test_variance <- sctest(I(PITs^2) ~ 1, type = "supF")
print(qlr_test_variance)
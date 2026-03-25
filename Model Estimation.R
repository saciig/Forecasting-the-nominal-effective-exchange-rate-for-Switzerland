# ===============================================================
# MODEL ESTIMATION SCRIPT
# ===============================================================
# This script performs model estimation for exchange rate forecasting,
# building on the Taylor Rule and Uncovered Interest Rate Parity (UIP).
# It includes:
#   - Weighted differential calculations for inflation, output gap,
#     and NEER.
#   - Rolling window Ordinary Least Squares (OLS) regressions.
#   - Horizon-specific beta coefficient estimation for NEER forecasts.
# ===============================================================

# NOTE:
# All variables have been processed to ensure stationarity via 
# first differencing (if needed). This script uses the following 
# variables:
#   - Nominal Effective Exchange Rate (NEER)
#   - Inflation Differential
#   - Output Gap Differential
#
# Rolling window OLS estimation is performed for 1-month, 6-month,
# 12-month, and 18-month forecast horizons. Each horizon captures
# horizon-specific sensitivities in the relationships.
# ===============================================================

# TABLE OF CONTENTS
# 1. Methodology Overview
#    1.1 Taylor Rule
#    1.2 Uncovered Interest Rate Parity (UIP)
#    1.3 Exchange Rate Prediction Formula
#    1.4 Choice of Forecasting Method
# 2. Data Preparation
#    2.1 Weighted Differentials for Exchange Rate Prediction
#    2.2 Lagging Variables
#    2.3 Data Frame Preparation for Horizons
# 3. Rolling Estimation
#    3.1 Rolling Window Methodology
#    3.2 Rolling Estimation for 1-Month Horizon
#    3.3 Rolling Estimation for 6-Month Horizon
#    3.4 Rolling Estimation for 12-Month Horizon
#    3.5 Rolling Estimation for 18-Month Horizon
# 4. Results Visualization
#    4.1 Coefficients and Confidence Intervals
#    4.2 Interpretation of Results
# ===============================================================

# ---------------------------------------------------------------
# 1. METHODOLOGY OVERVIEW
# ---------------------------------------------------------------
# This section provides the theoretical foundation for the model estimation process,
# justifies the choice of direct forecasting, and explains the personalized 
# application of economic principles to our project.
# ---------------------------------------------------------------

# 1.1 TAYLOR RULE
# ---------------------------------------------------------------
# In this project, the Taylor Rule is applied to model interest rate behavior for 
# Switzerland and its trading partners (USA, Euro Area, UK, and China). 
# Specifically:
#   - For Switzerland: i_t = μ + ψπ_t + γ(y_t - y_t*)
#   - For trading partners: i*_t = K + ψπ*_t + γ(y*_t - y*_t^*)
# where:
#   - i_t and i*_t: Interest rates for Switzerland and trading partners, respectively.
#   - π_t and π*_t: Inflation rates.
#   - (y_t - y_t*) and (y*_t - y*_t^*): Output gaps for Switzerland and trading partners.
# These equations capture the sensitivity of interest rates to inflation and 
# output gap differentials.

# 1.2 UNCOVERED INTEREST PARITY (UIP)
# ---------------------------------------------------------------
# Using the Uncovered Interest Parity (UIP), we model the expected depreciation 
# or appreciation of the Swiss franc based on the interest rate differential 
# between Switzerland and its trading partners. UIP posits that:
#   i_t - i*_t = ΔS_{t+1}
# where:
#   - i_t: Swiss interest rate.
#   - i*_t: Interest rate for a foreign economy.
#   - ΔS_{t+1}: Expected change in the exchange rate at time t+1.

# 1.3 EXCHANGE RATE PREDICTION FORMULA
# ---------------------------------------------------------------
# The prediction formula combines the Taylor Rule and UIP to forecast changes in 
# the Nominal Effective Exchange Rate (NEER). Weighted differentials for inflation 
# and output gap are used, accounting for trade shares of Switzerland’s key trading partners.
# The formula is:
#   ΔS_{t+1} = α + ψ Σ(w_j(π_t - π*_t^j)) + γ Σ(w_j(y_t - y*_t^j)) + η_t
# where:
#   - ΔS_{t+1}: Change in the NEER.
#   - α: Constant term.
#   - ψ: Weight on inflation differential.
#   - γ: Weight on output gap differential.
#   - w_j: Trade share for trading partner j.
#   - π_t - π*_t^j: Inflation differential between Switzerland and trading partner j.
#   - y_t - y*_t^j: Output gap differential between Switzerland and trading partner j.
#   - η_t: Error term accounting for unexplained variance.

# 1.4 CHOICE OF FORECASTING METHOD
# ---------------------------------------------------------------
# Direct Forecast vs. Iterated Forecast
# ---------------------------------------------------------------
# A direct forecasting approach is chosen for this project due to the following 
# advantages:
#   - Simplicity: Avoids the complexity of forecasting each variable (e.g., inflation 
#     and output gap) in an iterated forecast.
#   - Reduced bias: Direct forecasts produce lower Mean Squared Forecast Errors (MSFE) 
#     under potential model misspecification, minimizing the impact of specification errors.
#   - Error accumulation: Direct forecasts avoid the issue of error accumulation in 
#     iterative steps, which can degrade forecast accuracy.
#   - Literature support: Studies like Moldotsova (2009) emphasize the effectiveness of 
#     direct forecasting for exchange rates, making it a suitable choice for this project.

# Rolling Estimation with a 120-Month Window
# ---------------------------------------------------------------
# The model uses Ordinary Least Squares (OLS) with a rolling estimation method, 
# incorporating a 120-month (10-year) window. This approach dynamically updates 
# coefficients as relationships between variables evolve over time, while maintaining 
# a consistent sample size. Rolling estimation is applied separately for each forecast 
# horizon (1-month, 6-month, 12-month, 18-month).


# ---------------------------------------------------------------
# 2. DATA PREPARATION
# ---------------------------------------------------------------
# This section prepares the data for rolling estimation by:
#   - Calculating weighted differentials for key variables.
#   - Creating lagged variables for predictors.
#   - Preparing separate data frames for each forecast horizon.
# ---------------------------------------------------------------

# 2.1 WEIGHTED DIFFERENTIALS
# ---------------------------------------------------------------
# Compute trade-weighted differentials for:
#   - Nominal Effective Exchange Rate (NEER)
#   - Inflation
#   - Output Gap
# ---------------------------------------------------------------
# ---------------------------------------------------------------
# Nominal Effective Exchange Rate (NEER)
# ---------------------------------------------------------------

# Merge exchange rate data with trade shares
merged_nominal_exchrate_data <- merge(
  nominal_exchrate_data_processed, trade_shares_data, by = "Date"
)

# Calculate weighted exchange rates for each trading partner
merged_nominal_exchrate_data$Weighted_USD_CHF <- 
  merged_nominal_exchrate_data$USD_CHF_Adj * merged_nominal_exchrate_data$USA
merged_nominal_exchrate_data$Weighted_EURO_CHF <- 
  merged_nominal_exchrate_data$EURO_CHF_Adj * merged_nominal_exchrate_data$Euro
merged_nominal_exchrate_data$Weighted_GBP_CHF <- 
  merged_nominal_exchrate_data$GBP_CHF_Adj * merged_nominal_exchrate_data$UK
merged_nominal_exchrate_data$Weighted_Yuan_CHF <- 
  merged_nominal_exchrate_data$Yuan_CHF_Adj * merged_nominal_exchrate_data$China

# Calculate NEER as the sum of weighted exchange rates
merged_nominal_exchrate_data$nominal_effective_exchrate <- 
  rowSums(merged_nominal_exchrate_data[, grep("Weighted_", names(merged_nominal_exchrate_data))])

# Create a new dataframe with the Date and NEER
nominal_effective_exchrate_data <- data.frame(
  Date = merged_nominal_exchrate_data$Date,
  nominal_effective_exchrate = merged_nominal_exchrate_data$nominal_effective_exchrate
)

# ---------------------------------------------------------------
# Inflation Differential
# ---------------------------------------------------------------

# Rename trade share columns for clarity
colnames(trade_shares_data) <- sub("^USA$", "USA_shares", colnames(trade_shares_data))
colnames(trade_shares_data) <- sub("^Euro$", "Euro_shares", colnames(trade_shares_data))
colnames(trade_shares_data) <- sub("^UK$", "UK_shares", colnames(trade_shares_data))
colnames(trade_shares_data) <- sub("^China$", "China_shares", colnames(trade_shares_data))

# Merge inflation data with trade shares
merged_inflation_data <- merge(inflation_data_processed, trade_shares_data, by = "Date")

# Calculate weighted inflation differentials
merged_inflation_data$Weighted_USA <- 
  merged_inflation_data$USA_shares * (merged_inflation_data$Switzerland - merged_inflation_data$USA)
merged_inflation_data$Weighted_Euro <- 
  merged_inflation_data$Euro_shares * (merged_inflation_data$Switzerland - merged_inflation_data$Euro)
merged_inflation_data$Weighted_UK <- 
  merged_inflation_data$UK_shares * (merged_inflation_data$Switzerland - merged_inflation_data$UK)
merged_inflation_data$Weighted_China <- 
  merged_inflation_data$China_shares * (merged_inflation_data$Switzerland - merged_inflation_data$China)

# Calculate total inflation differential
merged_inflation_data$inflation_differential <- rowSums(
  merged_inflation_data[, grep("Weighted_", names(merged_inflation_data))]
)

# Create a new dataframe for inflation differentials
inflation_data <- data.frame(
  Date = merged_inflation_data$Date[-1], 
  inflation_differential = merged_inflation_data$inflation_differential[-1]
)

# ---------------------------------------------------------------
# Output Gap Differential
# ---------------------------------------------------------------

# Merge output gap data with trade shares
merged_output_gap_data <- merge(output_gap_data_processed, trade_shares_data, by = "Date")

# Calculate weighted output gap differentials
merged_output_gap_data$Weighted_USA <- 
  merged_output_gap_data$USA_shares * (merged_output_gap_data$Output_Gap_CH - merged_output_gap_data$Output_Gap_USA)
merged_output_gap_data$Weighted_Euro <- 
  merged_output_gap_data$Euro_shares * (merged_output_gap_data$Output_Gap_CH - merged_output_gap_data$Output_Gap_Euro)
merged_output_gap_data$Weighted_UK <- 
  merged_output_gap_data$UK_shares * (merged_output_gap_data$Output_Gap_CH - merged_output_gap_data$Output_Gap_UK)
merged_output_gap_data$Weighted_China <- 
  merged_output_gap_data$China_shares * (merged_output_gap_data$Output_Gap_CH - merged_output_gap_data$Output_Gap_China)

# Calculate total output gap differential
merged_output_gap_data$output_gap_differential <- rowSums(
  merged_output_gap_data[, grep("Weighted_", names(merged_output_gap_data))]
)

# Create a new dataframe for output gap differentials
output_gap_data <- data.frame(
  Date = merged_output_gap_data$Date,
  output_gap_differential = merged_output_gap_data$output_gap_differential
)

# ---------------------------------------------------------------
# Data frame merging for estimation
# ---------------------------------------------------------------

# Merge the data frames by Date
estimation_data <- Reduce(function(x, y) merge(x, y, by = "Date"), 
                          list(nominal_effective_exchrate_data, 
                               inflation_data, 
                               output_gap_data))

# View the resulting data frame
head(estimation_data)

# 2.2 LAGGING VARIABLES
# ---------------------------------------------------------------
# Creates lagged predictors for each forecast horizon:
#   - Inflation Differential
#   - Output Gap Differential
# Computes NEER changes for 1, 6, 12, and 18-month horizons.
# Ensures predictors align with their forecast horizon.
# ---------------------------------------------------------------

# Lagged variables for 1-month horizon
estimation_data$lagged_inflation_1 <- c(NA, head(estimation_data$inflation_differential, -1))
estimation_data$lagged_output_gap_1 <- c(NA, head(estimation_data$output_gap_differential, -1))

# Initialize columns for NEER changes
estimation_data$NEER_6_month_change <- rep(NA, nrow(estimation_data))
estimation_data$NEER_12_month_change <- rep(NA, nrow(estimation_data))
estimation_data$NEER_18_month_change <- rep(NA, nrow(estimation_data))

# Compute NEER changes for 6-month, 12-month and 18-month horizon
for (i in 6:nrow(estimation_data)) {
  estimation_data$NEER_6_month_change[i] <- 
    sum(estimation_data$nominal_effective_exchrate[(i - 5):i])
}
for (i in 12:nrow(estimation_data)) {
  estimation_data$NEER_12_month_change[i] <- 
    sum(estimation_data$nominal_effective_exchrate[(i - 11):i])
}
for (i in 18:nrow(estimation_data)) {
  estimation_data$NEER_18_month_change[i] <- 
    sum(estimation_data$nominal_effective_exchrate[(i - 17):i])
}

# Create lagged variables for 6-month horizon
estimation_data$lagged_inflation_6 <- c(rep(NA, 6), head(estimation_data$inflation_differential, -6))
estimation_data$lagged_output_gap_6 <- c(rep(NA, 6), head(estimation_data$output_gap_differential, -6))

# Create lagged variables for 12-month horizon
estimation_data$lagged_inflation_12 <- c(rep(NA, 12), head(estimation_data$inflation_differential, -12))
estimation_data$lagged_output_gap_12 <- c(rep(NA, 12), head(estimation_data$output_gap_differential, -12))

# Create lagged variables for 18-month horizon
estimation_data$lagged_inflation_18 <- c(rep(NA, 18), head(estimation_data$inflation_differential, -18))
estimation_data$lagged_output_gap_18 <- c(rep(NA, 18), head(estimation_data$output_gap_differential, -18))

# 2.3 DATA FRAME PREPARATION
# ---------------------------------------------------------------
# Merge variables into estimation data frames for each horizon.
# ---------------------------------------------------------------

# Data frame for 1-month horizon
estimation_data_1 <- estimation_data[!is.na(estimation_data$lagged_inflation_1) & 
                                       !is.na(estimation_data$lagged_output_gap_1), ]

# Data frame for 6-month horizon
estimation_data_6 <- estimation_data[!is.na(estimation_data$lagged_inflation_6) & 
                                       !is.na(estimation_data$lagged_output_gap_6) &
                                       !is.na(estimation_data$NEER_6_month_change), ]

# Data frame for 12-month horizon
estimation_data_12 <- estimation_data[!is.na(estimation_data$lagged_inflation_12) & 
                                        !is.na(estimation_data$lagged_output_gap_12) &
                                        !is.na(estimation_data$NEER_12_month_change), ]

# Data frame for 18-month horizon
estimation_data_18 <- estimation_data[!is.na(estimation_data$lagged_inflation_18) & 
                                        !is.na(estimation_data$lagged_output_gap_18) &
                                        !is.na(estimation_data$NEER_18_month_change), ]


# ---------------------------------------------------------------
# 3. ROLLING ESTIMATION
# ---------------------------------------------------------------
# Perform rolling window regressions to estimate horizon-specific
# beta coefficients.
# ---------------------------------------------------------------

# 3.1 ROLLING WINDOW METHODOLOGY
# ---------------------------------------------------------------
# Defines a rolling estimation function for horizon-specific beta
# coefficients.
# ---------------------------------------------------------------

rolling_estimation <- function(estimation_data, y_col, X_cols, window_size) {
  
  # Initialize vectors to store rolling results
  beta_intercept <- numeric()  # Intercept coefficients
  beta_inflation <- numeric()  # Inflation coefficients
  beta_output_gap <- numeric() # Output gap coefficients
  se_intercept <- numeric()    # Standard error for intercept
  se_inflation <- numeric()    # Standard error for inflation
  se_output_gap <- numeric()   # Standard error for output gap
  residual_variances <- numeric() # Variance of residuals
  total_variances <- numeric()
  
  # Perform rolling window estimation
  for (i in 1:(nrow(estimation_data) - window_size)) {
    
    # Select data for the current rolling window
    rolling_data <- estimation_data[i:(i + window_size - 1), ]
    
    # Skip window if any NA values are present
    if (any(is.na(rolling_data[[y_col]]) | 
            is.na(rolling_data[[X_cols[1]]]) | 
            is.na(rolling_data[[X_cols[2]]]))) {
      beta_intercept[i] <- NA
      beta_inflation[i] <- NA
      beta_output_gap[i] <- NA
      se_intercept[i] <- NA
      se_inflation[i] <- NA
      se_output_gap[i] <- NA
      residual_variances[i] <- NA
      total_variances[i] <- NA
      next
    }
    
    # Extract dependent variable (y) and independent variables (X)
    y <- rolling_data[[y_col]]
    X <- cbind(1, rolling_data[[X_cols[1]]], rolling_data[[X_cols[2]]]) # Add intercept
    
    # Perform OLS regression
    XtX_inv <- solve(t(X) %*% X)      # Compute inverse of X'X
    XtY <- t(X) %*% y                 # Compute X'Y
    beta <- XtX_inv %*% XtY           # Compute coefficients (β = (X'X)^-1 X'Y)
    
    # Store coefficients
    beta_intercept[i] <- beta[1]
    beta_inflation[i] <- beta[2]
    beta_output_gap[i] <- beta[3]
    
    # Calculate residuals and residual variance
    residuals <- y - X %*% beta
    sigma_sq <- sum(residuals^2) / (window_size - ncol(X)) # Variance of residuals
    residual_variances[i] <- sigma_sq
    
    # Calculate standard errors for coefficients
    var_beta <- diag(sigma_sq * XtX_inv)
    se_intercept[i] <- sqrt(var_beta[1])
    se_inflation[i] <- sqrt(var_beta[2])
    se_output_gap[i] <- sqrt(var_beta[3])
  
    # Calculate variance of the optimal forecast
    var_inflation <- var(rolling_data[[X_cols[1]]], na.rm = TRUE)
    var_output_gap <- var(rolling_data[[X_cols[2]]], na.rm = TRUE)
    cov_inflation_output <- cov(rolling_data[[X_cols[1]]], rolling_data[[X_cols[2]]])
    
    var_optimal_forecast <- beta[2]^2 * var_inflation +
      beta[3]^2 * var_output_gap +
      2 * beta[2] * beta[3] * cov_inflation_output
    
    # Total variance = variance of the optimal forecast + residual variance
    total_variances[i] <- var_optimal_forecast + sigma_sq
  }
  
  # Return results as a data frame
  return(data.frame(
    beta_intercept = beta_intercept,
    beta_inflation = beta_inflation,
    beta_output_gap = beta_output_gap,
    se_intercept = se_intercept,
    se_inflation = se_inflation,
    se_output_gap = se_output_gap,
    total_variances = total_variances
  ))
}

# 3.2 ROLLING ESTIMATION FOR EACH HORIZON
# ---------------------------------------------------------------
# Apply the rolling estimation function for 1, 6, 12, and 18-month
# horizons using a rolling window of 120 months.
# ---------------------------------------------------------------

# Define rolling window size
window_size <- 120

# Perform rolling estimation for 1-month horizon
rolling_betas_1 <- rolling_estimation(
  estimation_data_1,
  y_col = "nominal_effective_exchrate",  # Dependent variable
  X_cols = c("lagged_inflation_1", "lagged_output_gap_1"),  # Predictors
  window_size = window_size
)

# Extract total_variances from rolling results for Density Forecast
total_variances_1 <- rolling_betas_1$total_variances

# Perform rolling estimation for 6-month horizon
rolling_betas_6 <- rolling_estimation(
  estimation_data_6,
  y_col = "NEER_6_month_change",  # Dependent variable
  X_cols = c("lagged_inflation_6", "lagged_output_gap_6"),  # Predictors
  window_size = window_size
)
total_variances_6 <- rolling_betas_6$total_variances

# Perform rolling estimation for 12-month horizon
rolling_betas_12 <- rolling_estimation(
  estimation_data_12,
  y_col = "NEER_12_month_change",  # Dependent variable
  X_cols = c("lagged_inflation_12", "lagged_output_gap_12"),  # Predictors
  window_size = window_size
)
total_variances_12 <- rolling_betas_12$total_variances

# Perform rolling estimation for 18-month horizon
rolling_betas_18 <- rolling_estimation(
  estimation_data_18,
  y_col = "NEER_18_month_change",  # Dependent variable
  X_cols = c("lagged_inflation_18", "lagged_output_gap_18"),  # Predictors
  window_size = window_size
)
total_variances_18 <- rolling_betas_18$total_variances

# ---------------------------------------------------------------
# 4. RESULTS VISUALIZATION
# ---------------------------------------------------------------
# Visualize the rolling beta coefficients and confidence intervals 
# for each forecast horizon.
# ---------------------------------------------------------------

# Visualization for 1-Month Horizon
# ---------------------------------------------------------------
# Create a data frame for 1-month horizon
rolling_betas_1_df <- data.frame(
  Date = estimation_data_1$Date[(window_size + 1):nrow(estimation_data_1)],
  beta_intercept_1 = rolling_betas_1$beta_intercept,
  beta_inflation_1 = rolling_betas_1$beta_inflation,
  beta_output_gap_1 = rolling_betas_1$beta_output_gap,
  ci_intercept_upper_1 = rolling_betas_1$beta_intercept + 1.96 * rolling_betas_1$se_intercept,
  ci_intercept_lower_1 = rolling_betas_1$beta_intercept - 1.96 * rolling_betas_1$se_intercept,
  ci_inflation_upper_1 = rolling_betas_1$beta_inflation + 1.96 * rolling_betas_1$se_inflation,
  ci_inflation_lower_1 = rolling_betas_1$beta_inflation - 1.96 * rolling_betas_1$se_inflation,
  ci_output_gap_upper_1 = rolling_betas_1$beta_output_gap + 1.96 * rolling_betas_1$se_output_gap,
  ci_output_gap_lower_1 = rolling_betas_1$beta_output_gap - 1.96 * rolling_betas_1$se_output_gap
)

# Plot Beta for Inflation (1-month horizon)
plot(rolling_betas_1_df$Date, rolling_betas_1_df$beta_inflation, type = "l", col = "blue",
     main = "Beta for Inflation (1-Month Horizon)", xlab = "Date", ylab = "Beta",
     ylim = range(rolling_betas_1_df$ci_inflation_lower, rolling_betas_1_df$ci_inflation_upper, na.rm = TRUE))
lines(rolling_betas_1_df$Date, rolling_betas_1_df$ci_inflation_upper, col = "blue", lty = 2)
lines(rolling_betas_1_df$Date, rolling_betas_1_df$ci_inflation_lower, col = "blue", lty = 2)

# Plot the evolution of beta for Output Gap (1-month horizon)
plot(rolling_betas_1_df$Date, rolling_betas_1_df$beta_output_gap, type = "l", col = "red",
     main = "Beta for Output Gap (1-Month Horizon)", xlab = "Date", ylab = "Beta",
     ylim = range(rolling_betas_1_df$ci_output_gap_lower, rolling_betas_1_df$ci_output_gap_upper, na.rm = TRUE))
lines(rolling_betas_1_df$Date, rolling_betas_1_df$ci_output_gap_upper, col = "red", lty = 2)
lines(rolling_betas_1_df$Date, rolling_betas_1_df$ci_output_gap_lower, col = "red", lty = 2)

# Plot the evolution of beta for Intercept (1-month horizon)
plot(rolling_betas_1_df$Date, rolling_betas_1_df$beta_intercept, type = "l", col = "purple",
     main = "Intercept (1-Month Horizon)", xlab = "Date", ylab = "Intercept",
     ylim = range(rolling_betas_1_df$ci_intercept_lower, rolling_betas_1_df$ci_intercept_upper, na.rm = TRUE))
lines(rolling_betas_1_df$Date, rolling_betas_1_df$ci_intercept_upper, col = "purple", lty = 2)
lines(rolling_betas_1_df$Date, rolling_betas_1_df$ci_intercept_lower, col = "purple", lty = 2)

# Print the last values of the beta coefficients and their confidence intervals
cat("Final values for 1-month horizon:\n")
cat("Beta Intercept:", tail(rolling_betas_1_df$beta_intercept_1, 1), 
    "(95% CI:", 
    tail(rolling_betas_1_df$ci_intercept_lower_1, 1), ",", 
    tail(rolling_betas_1_df$ci_intercept_upper_1, 1), ")\n")
cat("Beta Inflation:", tail(rolling_betas_1_df$beta_inflation_1, 1), 
    "(95% CI:", 
    tail(rolling_betas_1_df$ci_inflation_lower_1, 1), ",", 
    tail(rolling_betas_1_df$ci_inflation_upper_1, 1), ")\n")
cat("Beta Output Gap:", tail(rolling_betas_1_df$beta_output_gap_1, 1), 
    "(95% CI:", 
    tail(rolling_betas_1_df$ci_output_gap_lower_1, 1), ",", 
    tail(rolling_betas_1_df$ci_output_gap_upper_1, 1), ")\n")


# Visualization for 6-Month Horizon
# ---------------------------------------------------------------
# Create a data frame for 6-month horizon
rolling_betas_6 <- data.frame(
  Date = estimation_data_6$Date[(window_size + 1):nrow(estimation_data_6)],
  beta_intercept_6 = rolling_betas_6$beta_intercept,
  beta_inflation_6 = rolling_betas_6$beta_inflation,
  beta_output_gap_6 = rolling_betas_6$beta_output_gap,
  ci_intercept_upper_6 = rolling_betas_6$beta_intercept + 1.96 * rolling_betas_6$se_intercept,
  ci_intercept_lower_6 = rolling_betas_6$beta_intercept - 1.96 * rolling_betas_6$se_intercept,
  ci_inflation_upper_6 = rolling_betas_6$beta_inflation + 1.96 * rolling_betas_6$se_inflation,
  ci_inflation_lower_6 = rolling_betas_6$beta_inflation - 1.96 * rolling_betas_6$se_inflation,
  ci_output_gap_upper_6 = rolling_betas_6$beta_output_gap + 1.96 * rolling_betas_6$se_output_gap,
  ci_output_gap_lower_6 = rolling_betas_6$beta_output_gap - 1.96 * rolling_betas_6$se_output_gap
)

# Plot the evolution of beta for Inflation (6-month horizon)
y_min_inflation_6 <- min(rolling_betas_6$ci_inflation_lower_6, na.rm = TRUE)
y_max_inflation_6 <- max(rolling_betas_6$ci_inflation_upper_6, na.rm = TRUE)
plot(rolling_betas_6$Date, rolling_betas_6$beta_inflation_6, type = "l", col = "blue",
     main = "Evolution of Beta for Inflation (6-Month Horizon)", 
     xlab = "Date", ylab = "Beta for Inflation",
     ylim = c(y_min_inflation_6, y_max_inflation_6))
lines(rolling_betas_6$Date, rolling_betas_6$ci_inflation_upper_6, col = "blue", lty = 2)
lines(rolling_betas_6$Date, rolling_betas_6$ci_inflation_lower_6, col = "blue", lty = 2)

# Plot the evolution of beta for Output Gap (6-month horizon)
y_min_output_gap_6 <- min(rolling_betas_6$ci_output_gap_lower_6, na.rm = TRUE)
y_max_output_gap_6 <- max(rolling_betas_6$ci_output_gap_upper_6, na.rm = TRUE)
plot(rolling_betas_6$Date, rolling_betas_6$beta_output_gap_6, type = "l", col = "red",
     main = "Evolution of Beta for Output Gap (6-Month Horizon)", 
     xlab = "Date", ylab = "Beta for Output Gap",
     ylim = c(y_min_output_gap_6, y_max_output_gap_6))
lines(rolling_betas_6$Date, rolling_betas_6$ci_output_gap_upper_6, col = "red", lty = 2)
lines(rolling_betas_6$Date, rolling_betas_6$ci_output_gap_lower_6, col = "red", lty = 2)

# Plot the evolution of beta for Intercept (6-month horizon)
y_min_intercept_6 <- min(rolling_betas_6$ci_intercept_lower_6, na.rm = TRUE)
y_max_intercept_6 <- max(rolling_betas_6$ci_intercept_upper_6, na.rm = TRUE)
plot(rolling_betas_6$Date, rolling_betas_6$beta_intercept_6, type = "l", col = "purple",
     main = "Evolution of Beta for Intercept (6-Month Horizon)", 
     xlab = "Date", ylab = "Intercept",
     ylim = c(y_min_intercept_6, y_max_intercept_6))
lines(rolling_betas_6$Date, rolling_betas_6$ci_intercept_upper_6, col = "purple", lty = 2)
lines(rolling_betas_6$Date, rolling_betas_6$ci_intercept_lower_6, col = "purple", lty = 2)

# Print the last values of the beta coefficients and their confidence intervals
cat("Final values for 6-month horizon:\n")
cat("Beta Intercept:", tail(rolling_betas_6$beta_intercept_6, 1), 
    "(95% CI:", 
    tail(rolling_betas_6$ci_intercept_lower_6, 1), ",", 
    tail(rolling_betas_6$ci_intercept_upper_6, 1), ")\n")
cat("Beta Inflation:", tail(rolling_betas_6$beta_inflation_6, 1), 
    "(95% CI:", 
    tail(rolling_betas_6$ci_inflation_lower_6, 1), ",", 
    tail(rolling_betas_6$ci_inflation_upper_6, 1), ")\n")
cat("Beta Output Gap:", tail(rolling_betas_6$beta_output_gap_6, 1), 
    "(95% CI:", 
    tail(rolling_betas_6$ci_output_gap_lower_6, 1), ",", 
    tail(rolling_betas_6$ci_output_gap_upper_6, 1), ")\n")


# Visualization for 12-Month Horizon
# ---------------------------------------------------------------
# Create a data frame for 12-month horizon
rolling_betas_12 <- data.frame(
  Date = estimation_data_12$Date[(window_size + 1):nrow(estimation_data_12)],
  beta_intercept_12 = rolling_betas_12$beta_intercept,
  beta_inflation_12 = rolling_betas_12$beta_inflation,
  beta_output_gap_12 = rolling_betas_12$beta_output_gap,
  ci_intercept_upper_12 = rolling_betas_12$beta_intercept + 1.96 * rolling_betas_12$se_intercept,
  ci_intercept_lower_12 = rolling_betas_12$beta_intercept - 1.96 * rolling_betas_12$se_intercept,
  ci_inflation_upper_12 = rolling_betas_12$beta_inflation + 1.96 * rolling_betas_12$se_inflation,
  ci_inflation_lower_12 = rolling_betas_12$beta_inflation - 1.96 * rolling_betas_12$se_inflation,
  ci_output_gap_upper_12 = rolling_betas_12$beta_output_gap + 1.96 * rolling_betas_12$se_output_gap,
  ci_output_gap_lower_12 = rolling_betas_12$beta_output_gap - 1.96 * rolling_betas_12$se_output_gap
)

# Plot the evolution of beta for Inflation (12-month horizon)
y_min_inflation_12 <- min(rolling_betas_12$ci_inflation_lower_12, na.rm = TRUE)
y_max_inflation_12 <- max(rolling_betas_12$ci_inflation_upper_12, na.rm = TRUE)
plot(rolling_betas_12$Date, rolling_betas_12$beta_inflation_12, type = "l", col = "blue",
     main = "Evolution of Beta for Inflation (12-Month Horizon)", 
     xlab = "Date", ylab = "Beta for Inflation",
     ylim = c(y_min_inflation_12, y_max_inflation_12))
lines(rolling_betas_12$Date, rolling_betas_12$ci_inflation_upper_12, col = "blue", lty = 2)
lines(rolling_betas_12$Date, rolling_betas_12$ci_inflation_lower_12, col = "blue", lty = 2)

# Plot the evolution of beta for Output Gap (12-month horizon)
y_min_output_gap_12 <- min(rolling_betas_12$ci_output_gap_lower_12, na.rm = TRUE)
y_max_output_gap_12 <- max(rolling_betas_12$ci_output_gap_upper_12, na.rm = TRUE)
plot(rolling_betas_12$Date, rolling_betas_12$beta_output_gap_12, type = "l", col = "red",
     main = "Evolution of Beta for Output Gap (12-Month Horizon)", 
     xlab = "Date", ylab = "Beta for Output Gap",
     ylim = c(y_min_output_gap_12, y_max_output_gap_12))
lines(rolling_betas_12$Date, rolling_betas_12$ci_output_gap_upper_12, col = "red", lty = 2)
lines(rolling_betas_12$Date, rolling_betas_12$ci_output_gap_lower_12, col = "red", lty = 2)

# Plot the evolution of beta for Intercept (12-month horizon)
y_min_intercept_12 <- min(rolling_betas_12$ci_intercept_lower_12, na.rm = TRUE)
y_max_intercept_12 <- max(rolling_betas_12$ci_intercept_upper_12, na.rm = TRUE)
plot(rolling_betas_12$Date, rolling_betas_12$beta_intercept_12, type = "l", col = "purple",
     main = "Evolution of Beta for Intercept (12-Month Horizon)", 
     xlab = "Date", ylab = "Intercept",
     ylim = c(y_min_intercept_12, y_max_intercept_12))
lines(rolling_betas_12$Date, rolling_betas_12$ci_intercept_upper_12, col = "purple", lty = 2)
lines(rolling_betas_12$Date, rolling_betas_12$ci_intercept_lower_12, col = "purple", lty = 2)

# Print the last values of the beta coefficients and their confidence intervals
cat("Final values for 12-month horizon:\n")
cat("Beta Intercept:", tail(rolling_betas_12$beta_intercept_12, 1), 
    "(95% CI:", 
    tail(rolling_betas_12$ci_intercept_lower_12, 1), ",", 
    tail(rolling_betas_12$ci_intercept_upper_12, 1), ")\n")
cat("Beta Inflation:", tail(rolling_betas_12$beta_inflation_12, 1), 
    "(95% CI:", 
    tail(rolling_betas_12$ci_inflation_lower_12, 1), ",", 
    tail(rolling_betas_12$ci_inflation_upper_12, 1), ")\n")
cat("Beta Output Gap:", tail(rolling_betas_12$beta_output_gap_12, 1), 
    "(95% CI:", 
    tail(rolling_betas_12$ci_output_gap_lower_12, 1), ",", 
    tail(rolling_betas_12$ci_output_gap_upper_12, 1), ")\n")


# Visualization for 18-Month Horizon
# ---------------------------------------------------------------
# Create a data frame for 18-month horizon
rolling_betas_18 <- data.frame(
  Date = estimation_data_18$Date[(window_size + 1):nrow(estimation_data_18)],
  beta_intercept_18 = rolling_betas_18$beta_intercept,
  beta_inflation_18 = rolling_betas_18$beta_inflation,
  beta_output_gap_18 = rolling_betas_18$beta_output_gap,
  ci_intercept_upper_18 = rolling_betas_18$beta_intercept + 1.96 * rolling_betas_18$se_intercept,
  ci_intercept_lower_18 = rolling_betas_18$beta_intercept - 1.96 * rolling_betas_18$se_intercept,
  ci_inflation_upper_18 = rolling_betas_18$beta_inflation + 1.96 * rolling_betas_18$se_inflation,
  ci_inflation_lower_18 = rolling_betas_18$beta_inflation - 1.96 * rolling_betas_18$se_inflation,
  ci_output_gap_upper_18 = rolling_betas_18$beta_output_gap + 1.96 * rolling_betas_18$se_output_gap,
  ci_output_gap_lower_18 = rolling_betas_18$beta_output_gap - 1.96 * rolling_betas_18$se_output_gap
)

# Plot the evolution of beta for Inflation (18-month horizon)
y_min_inflation_18 <- min(rolling_betas_18$ci_inflation_lower_18, na.rm = TRUE)
y_max_inflation_18 <- max(rolling_betas_18$ci_inflation_upper_18, na.rm = TRUE)
plot(rolling_betas_18$Date, rolling_betas_18$beta_inflation_18, type = "l", col = "blue",
     main = "Evolution of Beta for Inflation (18-Month Horizon)", 
     xlab = "Date", ylab = "Beta for Inflation",
     ylim = c(y_min_inflation_18, y_max_inflation_18))
lines(rolling_betas_18$Date, rolling_betas_18$ci_inflation_upper_18, col = "blue", lty = 2)
lines(rolling_betas_18$Date, rolling_betas_18$ci_inflation_lower_18, col = "blue", lty = 2)

# Plot the evolution of beta for Output Gap (18-month horizon)
y_min_output_gap_18 <- min(rolling_betas_18$ci_output_gap_lower_18, na.rm = TRUE)
y_max_output_gap_18 <- max(rolling_betas_18$ci_output_gap_upper_18, na.rm = TRUE)
plot(rolling_betas_18$Date, rolling_betas_18$beta_output_gap_18, type = "l", col = "red",
     main = "Evolution of Beta for Output Gap (18-Month Horizon)", 
     xlab = "Date", ylab = "Beta for Output Gap",
     ylim = c(y_min_output_gap_18, y_max_output_gap_18))
lines(rolling_betas_18$Date, rolling_betas_18$ci_output_gap_upper_18, col = "red", lty = 2)
lines(rolling_betas_18$Date, rolling_betas_18$ci_output_gap_lower_18, col = "red", lty = 2)

# Plot the evolution of beta for Intercept (18-month horizon)
y_min_intercept_18 <- min(rolling_betas_18$ci_intercept_lower_18, na.rm = TRUE)
y_max_intercept_18 <- max(rolling_betas_18$ci_intercept_upper_18, na.rm = TRUE)
plot(rolling_betas_18$Date, rolling_betas_18$beta_intercept_18, type = "l", col = "purple",
     main = "Evolution of Beta for Intercept (18-Month Horizon)", 
     xlab = "Date", ylab = "Intercept",
     ylim = c(y_min_intercept_18, y_max_intercept_18))
lines(rolling_betas_18$Date, rolling_betas_18$ci_intercept_upper_18, col = "purple", lty = 2)
lines(rolling_betas_18$Date, rolling_betas_18$ci_intercept_lower_18, col = "purple", lty = 2)

# Print the last values of the beta coefficients and their confidence intervals
cat("Final values for 18-month horizon:\n")
cat("Beta Intercept:", tail(rolling_betas_18$beta_intercept_18, 1), 
    "(95% CI:", 
    tail(rolling_betas_18$ci_intercept_lower_18, 1), ",", 
    tail(rolling_betas_18$ci_intercept_upper_18, 1), ")\n")
cat("Beta Inflation:", tail(rolling_betas_18$beta_inflation_18, 1), 
    "(95% CI:", 
    tail(rolling_betas_18$ci_inflation_lower_18, 1), ",", 
    tail(rolling_betas_18$ci_inflation_upper_18, 1), ")\n")
cat("Beta Output Gap:", tail(rolling_betas_18$beta_output_gap_18, 1), 
    "(95% CI:", 
    tail(rolling_betas_18$ci_output_gap_lower_18, 1), ",", 
    tail(rolling_betas_18$ci_output_gap_upper_18, 1), ")\n")



# ---------------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------------
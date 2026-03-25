# ===============================================================
# MAIN SCRIPT 
# ===============================================================
# This script orchestrates all sub-scripts related to the project,
# including data preparation, model estimation, forecasting, and evaluation.
# ===============================================================

# ---------------------------------------------------------------
# Set the Working Directory
# ---------------------------------------------------------------
# Set the working directory to the main project folder where all scripts
# and datasets are stored. Ensure this path is correctly set for your system.

setwd("/Users/adriencurrat/Desktop/Project 4")

# ---------------------------------------------------------------
# 1. Loading Packages
# ---------------------------------------------------------------
# This script ensures all required packages for data manipulation, 
# visualization, and statistical analysis are loaded. 

cat("\n--- Loading Packages ---\n")
source("Packages Loading.R")


# ---------------------------------------------------------------
# 2. Data Treatment
# ---------------------------------------------------------------
# This script handles the loading, cleaning, preprocessing, and 
# treatment of raw data. 

cat("\n--- Loading and Preparing Data ---\n")
source("Data Treatment.R")


# ---------------------------------------------------------------
# 3. Data Analysis
# ---------------------------------------------------------------
# # This script performs stationarity testing, seasonality adjustment, 
# and visualization of key variables. 

cat("\n--- Data Analysis ---\n")
source("Data Analysis.R")


# ---------------------------------------------------------------
# 4. Model Estimation
# ---------------------------------------------------------------
# This script performs model estimation for exchange rate forecasting,
# building on the Taylor Rule and Uncovered Interest Rate Parity (UIP).

cat("\n--- Model Estimation ---\n")
source("Model Estimation.R")


# ---------------------------------------------------------------
# 5. Point Forecasting
# ---------------------------------------------------------------
# This script computes point forecasts for NEER changes across
# multiple horizons (1, 6, 12, and 18 months) using the rolling 
# estimation results.

cat("\n--- Point Forecasting ---\n")
source("Point Forecasting.R")


# ---------------------------------------------------------------
# 6. Forecast Evaluation
# ---------------------------------------------------------------
# This script evaluates the accuracy of forecasts by comparing the model-based
# predictions with the random walk benchmark. 

cat("\n--- Forecast Evaluation ---\n")
source("Forecast Evaluation.R")


# ---------------------------------------------------------------
# 7. Density Forecast 
# ---------------------------------------------------------------
# This script focuses on interval and density forecast evaluation, testing
# the uniformity and independence of PITs, and assessing the quality of
# prediction intervals.

cat("\n--- Density Forecast ---\n")
source("Density Forecast.R")


# ---------------------------------------------------------------
# END OF MAIN SCRIPT
# ---------------------------------------------------------------
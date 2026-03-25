# ===============================================================
# PACKAGES LOADING SCRIPT
# ===============================================================
# This script installs and loads all the necessary packages for 
# the forecasting project 
# ===============================================================

# Set CRAN mirror for consistent package installations
options(repos = c(CRAN = "https://cran.rstudio.com"))

# ---------------------------------------------------------------
# 1. INSTALL PACKAGES
# ---------------------------------------------------------------
# The following commands ensure required packages are installed.
# Use `install.packages()` only if a package is not already installed.
# ===============================================================

# Data processing and analysis
install.packages("seasonal")   # Seasonal adjustment for time series
install.packages("tseries")    # Statistical tests for time series
install.packages("tempdisagg") # Temporal disaggregation of quarterly data
install.packages("forecast")   # Time series forecasting models like ARIMA
install.packages("sandwich")   # Robust HAC covariance estimation
install.packages("strucchange") # Structural change analysis in regression models

# Data visualization
install.packages("ggplot2")    # Visualization of trends and distributions

# Data import/export
install.packages("openxlsx")   # Export to Excel
install.packages("readxl")     # Import from Excel
install.packages("writexl")    # Write Excel files

# Report generation
install.packages("knitr")      # Report generation (Markdown to PDF)
install.packages("tinytex")    # PDF generation support for knitr


# ---------------------------------------------------------------
# 2. LOAD PACKAGES
# ---------------------------------------------------------------
# The following libraries are used throughout the project. Ensure 
# they are loaded successfully before proceeding.
# ===============================================================

# knitr, tinytex: Generate reports and PDFs.
library(knitr)
library(tinytex)

# seasonal: Perform seasonal adjustments on time-series data.
library(seasonal)

# tseries: Perform statistical tests (e.g., stationarity)
library(tseries)

# tempdisagg: Temporally disaggregate quarterly data to monthly frequency.
# Example: Using the Denton-Cholette method to interpolate industrial production data.
library(tempdisagg)

# forecast: Build and evaluate time series forecasting models.
# Example: ARIMA, exponential smoothing, and seasonal adjustments to forecast 
# missing values in industrial production data.
library(forecast)

# sandwich: Compute robust HAC variance-covariance matrices for efficiency tests.
library(sandwich)

# strucchange: Structural change analysis and QLR test for series.
library(strucchange)

# ggplot2: Create visualizations for trends, distributions, etc.
library(ggplot2)

# openxlsx, readxl, writexl: Handle Excel file import/export.
library(openxlsx)
library(readxl)
library(writexl)



# ---------------------------------------------------------------
# END OF SCRIPT
# ---------------------------------------------------------------
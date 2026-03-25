# Forecasting the Swiss nominal effective exchange rate

**Authors:** Isaac Graber, Mathéo Bourgeois, Adrien Currat  
**Institution:** University of Lausanne, Msc. in Economics  
**Course:** Economic Forecasting for Decision-Making  

## Introduction

This project replicates a real-world forecasting exercise conducted in the context of the Swiss National Bank (SNB). Acting as a macroeconomic forecasting team, we forecasted the evolution of the Swiss nominal effective exchange rate (NEER) of Switzerland up to 2026.

Using a theoretical framework based on the Taylor rule and the uncovered interest parity (UIP) condition, we assessed the impact of inflation and output gap differentials on bilateral exchange rates with major trading partners (US, EU, China, and UK), weighted by trade importance. The results showed that the model delivered accurate forecasts and outperformed a random walk.

This project received the maximum grade in the course. The work was also highlighted by Professor Grobéty in a public LinkedIn post, acknowledging the quality of the analysis.

## Project Structure

### 1. Project workflow

To run the project:

- Place all scripts and datasets in the same working directory.  
- Open the Main.R script in RStudio.  
- Ensure the working directory is correctly set in Main.R.  
- Source the Main.R script to execute all steps in the correct order.  

### 2. Scripts overview

#### 3.1 Packages Loading.R

**Purpose:** Ensures that all required R packages are installed and loaded. The script sets the CRAN mirror and installs missing packages automatically.

#### 3.2 Data Treatment.R

**Purpose:** Handles data cleaning, preprocessing, and interpolation. Raw macroeconomic data is processed into usable formats for further analysis.

#### 3.3 Data Analysis.R

**Purpose:** Conducts stationarity testing, adjusts for seasonality, and visualizes key variables. Outputs include graphs and summary statistics of the data.

#### 3.4 Model Estimation.R

**Purpose:** Estimates the Taylor Rule and Uncovered Interest Rate Parity (UIP) models to forecast exchange rates. Outputs include estimated coefficients and diagnostic metrics.

#### 3.5 Point Forecasting.R

**Purpose:** Generates point forecasts for NEER (Nominal Effective Exchange Rate) changes across multiple horizons (1, 6, 12, and 18 months).

#### 3.6 Forecast Evaluation.R

**Purpose:** Evaluates the accuracy of the forecasts using comparison metrics against a random walk benchmark.

#### 3.7 Density Forecast.R

**Purpose:** Evaluates the quality of prediction intervals and density forecasts. It includes Likelihood Ratio (LR) tests, Probability Integral Transform (PIT) analysis, and autocorrelation testing.

### 3. List of required packages

#### Data processing and analysis

- seasonal: Seasonal adjustment for time-series data.  
- tseries: Statistical tests for time-series analysis (e.g. stationarity tests).  
- tempdisagg: Temporal disaggregation of quarterly data (e.g. Denton–Cholette method).  
- forecast: Time-series forecasting tools, including ARIMA and related methods.  
- sandwich: Robust (HAC) covariance matrix estimation for regression models.  
- strucchange: Structural change analysis and QLR testing for regression models.  

#### Data visualization

- ggplot2: Visualization of time-series trends, distributions, and forecast results.  

#### Data import / export

- readxl: Import data from Excel files.  
- openxlsx: Export data to Excel files.  
- writexl: Write processed data and results to Excel files.  


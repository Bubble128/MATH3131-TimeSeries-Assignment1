# MATH3131 Time Series Analysis - Assignment 1 EDA

## Author
Chenrui Liu (a1984046)

## Project Description
This repository contains the R scripts and diagnostic visualisations for the Exploratory Data Analysis (EDA) of Singapore's Total Live Births (TLB) and Total Fertility Rate (TFR) from 1960 to 2024. 

## File Structure
* `Assignment1_EDA.R`: The complete, reproducible base R script covering data cleaning, transformation, model identification, and statistical tests (ADF and Ljung-Box Portmanteau tests).
* `01_...` to `05_...`: Initial exploratory plots, and ACF/PACF of the original and differenced series for both variables.
* `06_...` to `07_...`: Residual diagnostic plots for the preliminary models (ARIMA(1,1,1) for TFR and ARIMA(0,1,0) for TLB).
* `08_Portmanteau_LjungBox_Test_Results.png`: Console output verifying the statistical adequacy of the models.

## Key Findings
* Both TFR and TLB require first-order differencing ($d=1$) to achieve stationarity.
* TFR shows a strong short-term autoregressive and moving average pattern, modeled with an ARIMA(1,1,1).
* TLB exhibits a highly significant 12-year cycle (lag 12), corresponding to the cultural "Dragon Year effect" in Singapore, which will be addressed using a Seasonal ARIMA (SARIMA) model in future analyses.

## Usage
To reproduce the analysis, ensure the `BirthsAndFertilityRatesAnnual.csv` data file is in the same working directory as the R script. Execute the script in base R or RStudio.

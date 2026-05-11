library(forecast)

# Read the CSV file
raw_data <- read.csv("BirthsAndFertilityRatesAnnual.csv", header = FALSE, stringsAsFactors = FALSE)

# Extract TFR (row 2) and TLB (row 16), from the year 2024 (column 3) to 1960 (column 67).
# Reverse the data to arrange it in ascending chronological order. (1960 -> 2024)
tfr_values <- rev(as.numeric(unlist(raw_data[2, 3:67])))
tlb_values <- rev(as.numeric(unlist(raw_data[16, 3:67])))

# Create a time series object
tfr_ts <- ts(tfr_values, start = 1960, frequency = 1)
tlb_ts <- ts(tlb_values, start = 1960, frequency = 1)

# Extract the training set (1960-2012) for modeling.
tfr_train <- window(tfr_ts, start = 1960, end = 2012)
tlb_train <- window(tlb_ts, start = 1960, end = 2012)

# Draw the time series diagram of the raw data
par(mfrow = c(2, 1))
plot(tlb_train, main = "Singapore TLB Training Set (1960-2012)", ylab = "TLB", col = "blue", lwd = 2)
plot(tfr_train, main = "Singapore TFR Training Set (1960-2012)", ylab = "TFR", col = "red", lwd = 2)

# Examine the ACF and PACF of the first-differenced series with lag.max = 30.
# This is used to identify possible short-lag and long-lag ARIMA candidate models.
tfr_train_diff <- diff(tfr_train)
tlb_train_diff <- diff(tlb_train)

par(mfrow = c(2, 2))
# Plot the ACF and PACF after TFR difference
acf(tfr_train_diff, main = "ACF of Differenced TFR (d=1)", lag.max = 30)
pacf(tfr_train_diff, main = "PACF of Differenced TFR (d=1)", lag.max = 30)

# Plot the ACF and PACF after TLB difference
acf(tlb_train_diff, main = "ACF of Differenced TLB (d=1)", lag.max = 30)
pacf(tlb_train_diff, main = "PACF of Differenced TLB (d=1)", lag.max = 30)

# TFR candidate models
fit_tfr_110 <- Arima(tfr_train, order = c(1,1,0))
fit_tfr_011 <- Arima(tfr_train, order = c(0,1,1))
fit_tfr_111 <- Arima(tfr_train, order = c(1,1,1))
fit_tfr_120 <- Arima(tfr_train, order = c(12,1,0))
fit_tfr_130 <- Arima(tfr_train, order = c(13,1,0))

# TLB candidate models
fit_tlb_010 <- Arima(tlb_train, order = c(0,1,0))
fit_tlb_011 <- Arima(tlb_train, order = c(0,1,1))
fit_tlb_110 <- Arima(tlb_train, order = c(1,1,0))
fit_tlb_120 <- Arima(tlb_train, order = c(12,1,0))
fit_tlb_130 <- Arima(tlb_train, order = c(13,1,0))

# Print candidate model summaries
print("--- Initial candidate models for TFR ---")
print(fit_tfr_110)
print(fit_tfr_011)
print(fit_tfr_111)
print(fit_tfr_120)
print(fit_tfr_130)

print("--- Initial candidate models for TLB ---")
print(fit_tlb_010)
print(fit_tlb_011)
print(fit_tlb_110)
print(fit_tlb_120)
print(fit_tlb_130)

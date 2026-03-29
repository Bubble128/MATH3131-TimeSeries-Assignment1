# Load raw data, header=FALSE to avoid parsing issues
raw_data <- read.csv("BirthsAndFertilityRatesAnnual.csv", header = FALSE, stringsAsFactors = FALSE)

# Extract TFR (row 2) and TLB (row 16) from cols 3 to 67 (2024 to 1960)
# Reverse the order to chronological (1960 -> 2024)
tfr_values <- rev(as.numeric(unlist(raw_data[2, 3:67])))
tlb_values <- rev(as.numeric(unlist(raw_data[16, 3:67])))

# Create time series objects
tfr_ts <- ts(tfr_values, start = 1960, frequency = 1)
tlb_ts <- ts(tlb_values, start = 1960, frequency = 1)

# 1. Plot raw time series (EDA)
par(mfrow = c(2, 1))
plot(tlb_ts, main = "Singapore Total Live Births (1960-2024)", ylab = "Total Live Births", xlab = "Year", col = "blue", lwd = 2)
plot(tfr_ts, main = "Singapore Total Fertility Rate (1960-2024)", ylab = "Total Fertility Rate", xlab = "Year", col = "red", lwd = 2)
par(mfrow = c(1, 1))

# Extract training set (1960 - 2012) for modeling
tfr_train <- window(tfr_ts, start = 1960, end = 2012)

# 2. Check stationarity of the training set (Identify d)
par(mfrow = c(2, 1))
acf(tfr_train, main = "ACF of Original TFR (1960-2012) - Shows Trend")
pacf(tfr_train, main = "PACF of Original TFR (1960-2012)")
par(mfrow = c(1, 1))

# Apply first-order differencing to achieve stationarity
tfr_train_diff <- diff(tfr_train)

# 3. Plot differenced data (Identify p and q)
par(mfrow = c(2, 1))
acf(tfr_train_diff, main = "ACF of Differenced TFR (d=1)")
pacf(tfr_train_diff, main = "PACF of Differenced TFR (d=1)")
par(mfrow = c(1, 1))

# 4. Fit the preliminary ARIMA(1,1,1) model
tfr_model <- arima(tfr_train, order = c(1, 1, 1))
print(tfr_model)

# 5. Residual diagnostics to check for white noise
res_tfr <- tfr_model$residuals
par(mfrow = c(2, 2))

# Residual plot(2 standard deviation bounds)
plot(res_tfr, main = "Residuals of ARIMA(1,1,1)")
abline(h = 0, lwd = 1)
abline(h = 2 * sd(res_tfr), col = "red", lty = 2)
abline(h = -2 * sd(res_tfr), col = "red", lty = 2)

# Q-Q plot
qqnorm(res_tfr)
qqline(res_tfr, col = "blue")

# ACF of residuals
acf(res_tfr, main = "ACF of Residuals")

par(mfrow = c(1, 1))
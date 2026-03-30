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
tlb_train <- window(tlb_ts, start = 1960, end = 2012)

# 2. Check stationarity of the training set (Identify d)
par(mfrow = c(2, 1))
acf(tfr_train, main = "ACF of Original TFR (1960-2012)")
pacf(tfr_train, main = "PACF of Original TFR (1960-2012)")
par(mfrow = c(1, 1))
par(mfrow = c(2, 1))
acf(tlb_train, main = "ACF of Original TLB (1960-2012)")
pacf(tlb_train, main = "PACF of Original TLB (1960-2012)")
par(mfrow = c(1, 1))

# Apply first-order differencing to achieve stationarity
tfr_train_diff <- diff(tfr_train)
tlb_train_diff <- diff(tlb_train)

# 3. Plot differenced data (Identify p and q)
par(mfrow = c(2, 1))
acf(tfr_train_diff, main = "ACF of Differenced TFR (d=1)")
pacf(tfr_train_diff, main = "PACF of Differenced TFR (d=1)")
par(mfrow = c(1, 1))
par(mfrow = c(2, 1))
acf(tlb_train_diff, main = "ACF of Differenced TLB (d=1)")
pacf(tlb_train_diff, main = "PACF of Differenced TLB (d=1)")
par(mfrow = c(1, 1))

# 4. Fit the preliminary ARIMA model
tfr_model <- arima(tfr_train, order = c(1, 1, 1))
print(tfr_model)

tlb_model <- arima(tlb_train, order = c(0, 1, 0))
print(tlb_model)

# 5. Residual diagnostics to check for white noise (TFR)
res_tfr <- tfr_model$residuals
par(mfrow = c(2, 2))

# Residual plot (2 standard deviation bounds)
plot(res_tfr, main = "Residuals of TFR ARIMA")
abline(h = 0, lwd = 1)
abline(h = 2 * sd(res_tfr), col = "red", lty = 2)
abline(h = -2 * sd(res_tfr), col = "red", lty = 2)

# Q-Q plot (Normality check)
qqnorm(res_tfr)
qqline(res_tfr, col = "blue")

# ACF of residuals (Remaining autocorrelation)
acf(res_tfr, main = "ACF of Residuals")

# PACF of residuals
pacf(res_tfr, main = "PACF of Residuals")

par(mfrow = c(1, 1))

# 6. Residual diagnostics to check for white noise (TLB)
res_tlb <- tlb_model$residuals
par(mfrow = c(2, 2))

# Residual plot (2 standard deviation bounds)
plot(res_tlb, main = "Residuals of TLB ARIMA")
abline(h = 0, lwd = 1)
abline(h = 2 * sd(res_tlb), col = "red", lty = 2)
abline(h = -2 * sd(res_tlb), col = "red", lty = 2)

# Q-Q plot (Normality check)
qqnorm(res_tlb, main = "Normal Q-Q Plot (TLB)")
qqline(res_tlb, col = "blue")

# ACF of residuals (Remaining autocorrelation)
acf(res_tlb, main = "ACF of TLB Residuals")

# PACF of residuals
pacf(res_tlb, main = "PACF of TLB Residuals")

par(mfrow = c(1, 1))

# Hypothesis Testing
print("--- Portmanteau Test (Ljung-Box) for TFR ARIMA(1,1,1) Residuals ---")
Box.test(res_tfr, lag = 10, type = "Ljung-Box")

print("--- Portmanteau Test (Ljung-Box) for TLB ARIMA(0,1,0) Residuals ---")
Box.test(res_tlb, lag = 10, type = "Ljung-Box")
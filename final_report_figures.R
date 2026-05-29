library(forecast)

# 1. Read data

raw_data <- read.csv("BirthsAndFertilityRatesAnnual.csv",
                     header = FALSE,
                     stringsAsFactors = FALSE)

# Extract TFR and TLB, then reverse to chronological order: 1960 -> 2024
tfr_values <- rev(as.numeric(unlist(raw_data[2, 3:67])))
tlb_values <- rev(as.numeric(unlist(raw_data[16, 3:67])))

# Create original annual time series objects
tfr_ts <- ts(tfr_values, start = 1960, frequency = 1)
tlb_ts <- ts(tlb_values, start = 1960, frequency = 1)

# Create log-transformed series
log_tfr_ts <- log(tfr_ts)
log_tlb_ts <- log(tlb_ts)

# Training and testing split
tfr_train_raw <- window(tfr_ts, start = 1960, end = 2012)
tlb_train_raw <- window(tlb_ts, start = 1960, end = 2012)

tfr_test_raw <- window(tfr_ts, start = 2013, end = 2024)
tlb_test_raw <- window(tlb_ts, start = 2013, end = 2024)

tfr_train <- window(log_tfr_ts, start = 1960, end = 2012)
tlb_train <- window(log_tlb_ts, start = 1960, end = 2012)

tfr_test <- window(log_tfr_ts, start = 2013, end = 2024)
tlb_test <- window(log_tlb_ts, start = 2013, end = 2024)


# 2. Original-scale trend plot

png("fig_original_training_series_TLB_TFR_1960_2012.png",
    width = 1200, height = 900, res = 120)

par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))

plot(tlb_train_raw,
     main = "Singapore TLB Training Set (1960-2012)",
     ylab = "TLB",
     xlab = "Year",
     col = "blue",
     lwd = 2)

plot(tfr_train_raw,
     main = "Singapore TFR Training Set (1960-2012)",
     ylab = "TFR",
     xlab = "Year",
     col = "red",
     lwd = 2)

dev.off()


# 3. Log-transformed trend plot

png("fig_log_training_series_TLB_TFR_1960_2012.png",
    width = 1200, height = 900, res = 120)

par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))

plot(tlb_train,
     main = "Log Singapore TLB Training Set (1960-2012)",
     ylab = "log(TLB)",
     xlab = "Year",
     col = "blue",
     lwd = 2)

plot(tfr_train,
     main = "Log Singapore TFR Training Set (1960-2012)",
     ylab = "log(TFR)",
     xlab = "Year",
     col = "red",
     lwd = 2)

dev.off()


# 4. First-differenced log series plot

tfr_train_diff1 <- diff(tfr_train, differences = 1)
tlb_train_diff1 <- diff(tlb_train, differences = 1)

png("fig_differenced_log_series_TLB_TFR_d1_1960_2012.png",
    width = 1200, height = 900, res = 120)

par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))

plot(tlb_train_diff1,
     main = "First-Differenced log(TLB) Training Series",
     ylab = "Difference of log(TLB)",
     xlab = "Year",
     col = "blue",
     lwd = 2)
abline(h = 0, lty = 2)

plot(tfr_train_diff1,
     main = "First-Differenced log(TFR) Training Series",
     ylab = "Difference of log(TFR)",
     xlab = "Year",
     col = "red",
     lwd = 2)
abline(h = 0, lty = 2)

dev.off()


# 5. ACF/PACF of first-differenced log series, lag.max = 40

png("fig_log_differenced_acf_pacf_TLB_TFR_d1_lag40.png",
    width = 1200, height = 900, res = 120)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

acf(tfr_train_diff1,
    main = "ACF of Differenced log(TFR) (d=1)",
    lag.max = 40)

pacf(tfr_train_diff1,
     main = "PACF of Differenced log(TFR) (d=1)",
     lag.max = 40)

acf(tlb_train_diff1,
    main = "ACF of Differenced log(TLB) (d=1)",
    lag.max = 40)

pacf(tlb_train_diff1,
     main = "PACF of Differenced log(TLB) (d=1)",
     lag.max = 40)

dev.off()

# 6. Regular and seasonal differenced log series for SARIMA identification
# d = 1 and D = 1 with seasonal period 12
# This is used to help identify p, q, P and Q for SARIMA-style models.

tfr_train_diff_d1_D1 <- diff(diff(tfr_train, differences = 1), lag = 12, differences = 1)
tlb_train_diff_d1_D1 <- diff(diff(tlb_train, differences = 1), lag = 12, differences = 1)

png("fig_d1_D1_log_series_TLB_TFR_1960_2012.png",
    width = 1200, height = 900, res = 120)

par(mfrow = c(2, 1), mar = c(4, 4, 3, 2))

plot(tlb_train_diff_d1_D1,
     main = "Regular and Seasonal Differenced log(TLB): d=1, D=1, period=12",
     ylab = "Differenced log(TLB)",
     xlab = "Year",
     col = "blue",
     lwd = 2)
abline(h = 0, lty = 2)

plot(tfr_train_diff_d1_D1,
     main = "Regular and Seasonal Differenced log(TFR): d=1, D=1, period=12",
     ylab = "Differenced log(TFR)",
     xlab = "Year",
     col = "red",
     lwd = 2)
abline(h = 0, lty = 2)

dev.off()


# 7. ACF/PACF after regular and seasonal differencing
# These plots are used to identify SARIMA p, q, P and Q.

png("fig_d1_D1_log_acf_pacf_TLB_TFR_lag40.png",
    width = 1200, height = 900, res = 120)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

acf(tfr_train_diff_d1_D1,
    main = "ACF of log(TFR) after d=1 and D=1",
    lag.max = 40)

pacf(tfr_train_diff_d1_D1,
     main = "PACF of log(TFR) after d=1 and D=1",
     lag.max = 40)

acf(tlb_train_diff_d1_D1,
    main = "ACF of log(TLB) after d=1 and D=1",
    lag.max = 40)

pacf(tlb_train_diff_d1_D1,
     main = "PACF of log(TLB) after d=1 and D=1",
     lag.max = 40)

dev.off()

# 8. Second-order differenced log(TFR) plot and ACF/PACF

tfr_train_diff2 <- diff(tfr_train, differences = 2)

png("fig_second_differenced_log_TFR_d2_lag40.png",
    width = 1200, height = 900, res = 120)

par(mfrow = c(1, 3), mar = c(4, 4, 3, 2))

plot(tfr_train_diff2,
     main = "Second-Differenced log(TFR)",
     ylab = "Second Difference of log(TFR)",
     xlab = "Year",
     col = "red",
     lwd = 2)
abline(h = 0, lty = 2)

acf(tfr_train_diff2,
    main = "ACF of 2nd-Differenced log(TFR)",
    lag.max = 40)

pacf(tfr_train_diff2,
     main = "PACF of 2nd-Differenced log(TFR)",
     lag.max = 40)

dev.off()


print("Figures exported successfully")
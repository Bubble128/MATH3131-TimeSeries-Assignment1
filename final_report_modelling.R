library(forecast)

# 1. Read data
raw_data <- read.csv("BirthsAndFertilityRatesAnnual.csv",
                     header = FALSE,
                     stringsAsFactors = FALSE)

# Extract TFR row and TLB row, then reverse to chronological order: 1960 -> 2024
tfr_values <- rev(as.numeric(unlist(raw_data[2, 3:67])))
tlb_values <- rev(as.numeric(unlist(raw_data[16, 3:67])))

# Create original annual time series objects
tfr_ts <- ts(tfr_values, start = 1960, frequency = 1)
tlb_ts <- ts(tlb_values, start = 1960, frequency = 1)

# 2. Log transformation
# Both TFR and TLB are log-transformed before modelling
# to stabilise variance and reduce scale differences.
log_tfr_ts <- log(tfr_ts)
log_tlb_ts <- log(tlb_ts)

# Training set: 1960-2012
tfr_train <- window(log_tfr_ts, start = 1960, end = 2012)
tlb_train <- window(log_tlb_ts, start = 1960, end = 2012)

# Testing set: 2013-2024
tfr_test <- window(log_tfr_ts, start = 2013, end = 2024)
tlb_test <- window(log_tlb_ts, start = 2013, end = 2024)

# Raw-scale training and testing series for plotting
tfr_train_raw <- window(tfr_ts, start = 1960, end = 2012)
tlb_train_raw <- window(tlb_ts, start = 1960, end = 2012)

tfr_test_raw <- window(tfr_ts, start = 2013, end = 2024)
tlb_test_raw <- window(tlb_ts, start = 2013, end = 2024)

# 4. Candidate model fitting

# Helper function: safely fit models
safe_arima <- function(series, order, seasonal = NULL) {
  tryCatch(
    {
      if (is.null(seasonal)) {
        Arima(series, order = order)
      } else {
        Arima(series, order = order, seasonal = seasonal)
      }
    },
    error = function(e) {
      message("Model failed: ARIMA(",
              paste(order, collapse = ","),
              ")")
      return(NULL)
    }
  )
}


# 4. Candidate model fitting

# TLB candidate models

# TLB is modelled on the log scale.
# The first-differenced log(TLB) series suggests d = 1.
# The additional period-12 seasonal differencing suggests D = 1.
# After applying d = 1 and D = 1, the PACF shows a notable spike
# around lag 4, while the ACF shows seasonal structure around lag 12.
# Therefore, SARIMA candidates with p = 4, q = 0 or 1,
# P = 0, D = 1 and Q = 1 are considered.

# Traditional ARIMA baseline models
fit_tlb_010 <- safe_arima(tlb_train, order = c(0, 1, 0))
fit_tlb_011 <- safe_arima(tlb_train, order = c(0, 1, 1))
fit_tlb_110 <- safe_arima(tlb_train, order = c(1, 1, 0))

# SARIMA candidates identified after d = 1 and D = 1
fit_tlb_sarima_110_011 <- safe_arima(
  tlb_train,
  order = c(1, 1, 0),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tlb_sarima_011_011 <- safe_arima(
  tlb_train,
  order = c(0, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tlb_sarima_410_011 <- safe_arima(
  tlb_train,
  order = c(4, 1, 0),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tlb_sarima_411_011 <- safe_arima(
  tlb_train,
  order = c(4, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 12)
)


# TFR candidate models

# TFR is also modelled on the log scale.
# The first-differenced log(TFR) series suggests d = 1.
# The d = 1 and D = 1 ACF/PACF suggests p = 1 or 4,
# q = 1 or 2, P = 0 and Q = 1.
# Therefore, several SARIMA candidates are considered.

# Traditional ARIMA baseline models
fit_tfr_110 <- safe_arima(tfr_train, order = c(1, 1, 0))
fit_tfr_011 <- safe_arima(tfr_train, order = c(0, 1, 1))
fit_tfr_111 <- safe_arima(tfr_train, order = c(1, 1, 1))

# SARIMA candidates identified after d = 1 and D = 1
fit_tfr_sarima_111_011 <- safe_arima(
  tfr_train,
  order = c(1, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tfr_sarima_112_011 <- safe_arima(
  tfr_train,
  order = c(1, 1, 2),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tfr_sarima_410_011 <- safe_arima(
  tfr_train,
  order = c(4, 1, 0),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tfr_sarima_411_011 <- safe_arima(
  tfr_train,
  order = c(4, 1, 1),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tfr_sarima_412_011 <- safe_arima(
  tfr_train,
  order = c(4, 1, 2),
  seasonal = list(order = c(0, 1, 1), period = 12)
)


# Auto-selected ARIMA benchmark models

# Since the data are annual with frequency = 1, auto.arima is used here
# as a non-seasonal automatic ARIMA benchmark.

auto_tlb <- auto.arima(
  tlb_train,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

auto_tfr <- auto.arima(
  tfr_train,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

print("--- Auto-selected ARIMA model for log(TLB) ---")
print(auto_tlb)

print("--- Auto-selected ARIMA model for log(TFR) ---")
print(auto_tfr)


# 5. Model comparison functions

# Ljung-Box p-value
# H0: residuals are independently distributed.
# p-value > 0.05 suggests no strong evidence of remaining autocorrelation.
lb_pvalue <- function(model, lag_value = 24) {
  if (is.null(model)) {
    return(NA)
  }
  
  Box.test(
    residuals(model),
    lag = lag_value,
    type = "Ljung-Box",
    fitdf = length(coef(model))
  )$p.value
}

# Forecast accuracy on the log scale
get_forecast_accuracy <- function(model, test_data) {
  if (is.null(model)) {
    return(c(SSE = NA, RMSE = NA, MAE = NA))
  }
  
  fc <- forecast(model, h = length(test_data))
  errors <- as.numeric(test_data) - as.numeric(fc$mean)
  
  sse <- sum(errors^2)
  rmse <- sqrt(mean(errors^2))
  mae <- mean(abs(errors))
  
  return(c(SSE = sse, RMSE = rmse, MAE = mae))
}

# Build comparison table
make_model_table <- function(series_name, model_names, model_objects, test_data) {
  aic_values <- sapply(model_objects, function(m) if (is.null(m)) NA else AIC(m))
  bic_values <- sapply(model_objects, function(m) if (is.null(m)) NA else BIC(m))
  lb_values <- sapply(model_objects, lb_pvalue)
  
  accuracy_values <- t(sapply(model_objects, get_forecast_accuracy, test_data = test_data))
  
  table <- data.frame(
    Series = series_name,
    Model = model_names,
    AIC = as.numeric(aic_values),
    BIC = as.numeric(bic_values),
    Ljung_Box_pvalue = as.numeric(lb_values),
    SSE = accuracy_values[, "SSE"],
    RMSE = accuracy_values[, "RMSE"],
    MAE = accuracy_values[, "MAE"]
  )
  
  return(table)
}


# 6. TLB model comparison table

tlb_model_names <- c(
  "ARIMA(0,1,0)",
  "ARIMA(0,1,1)",
  "ARIMA(1,1,0)",
  "ARIMA(1,1,0)(0,1,1)[12]",
  "ARIMA(0,1,1)(0,1,1)[12]",
  "ARIMA(4,1,0)(0,1,1)[12]",
  "ARIMA(4,1,1)(0,1,1)[12]",
  "auto.arima"
)

tlb_model_objects <- list(
  fit_tlb_010,
  fit_tlb_011,
  fit_tlb_110,
  fit_tlb_sarima_110_011,
  fit_tlb_sarima_011_011,
  fit_tlb_sarima_410_011,
  fit_tlb_sarima_411_011,
  auto_tlb
)

tlb_model_table <- make_model_table(
  series_name = "log(TLB)",
  model_names = tlb_model_names,
  model_objects = tlb_model_objects,
  test_data = tlb_test
)

print("--- Final TLB model comparison table ---")
print(tlb_model_table)


# 7. TFR model comparison table

tfr_model_names <- c(
  "ARIMA(1,1,0)",
  "ARIMA(0,1,1)",
  "ARIMA(1,1,1)",
  "ARIMA(1,1,1)(0,1,1)[12]",
  "ARIMA(1,1,2)(0,1,1)[12]",
  "ARIMA(4,1,0)(0,1,1)[12]",
  "ARIMA(4,1,1)(0,1,1)[12]",
  "ARIMA(4,1,2)(0,1,1)[12]",
  "auto.arima"
)

tfr_model_objects <- list(
  fit_tfr_110,
  fit_tfr_011,
  fit_tfr_111,
  fit_tfr_sarima_111_011,
  fit_tfr_sarima_112_011,
  fit_tfr_sarima_410_011,
  fit_tfr_sarima_411_011,
  fit_tfr_sarima_412_011,
  auto_tfr
)

tfr_model_table <- make_model_table(
  series_name = "log(TFR)",
  model_names = tfr_model_names,
  model_objects = tfr_model_objects,
  test_data = tfr_test
)

print("--- Final TFR model comparison table ---")
print(tfr_model_table)


# 8. Export model comparison tables

write.csv(
  tlb_model_table,
  "tlb_model_comparison_log.csv",
  row.names = FALSE
)

write.csv(
  tfr_model_table,
  "tfr_model_comparison_log.csv",
  row.names = FALSE
)

print("Model comparison tables exported successfully.")

# 8. Forecast plots for selected final models

# Preferred final models:
# TFR: ARIMA(4,1,0)(0,1,1)[12]
# TLB: ARIMA(1,1,0)(0,1,1)[12]

final_tfr_model <- fit_tfr_sarima_410_011
final_tlb_model <- fit_tlb_sarima_110_011

h_tfr <- length(tfr_test)
h_tlb <- length(tlb_test)

fc_tfr_log <- forecast(final_tfr_model, h = h_tfr)
fc_tlb_log <- forecast(final_tlb_model, h = h_tlb)

# Convert forecasts back to original scale for presentation
fc_tfr_mean <- exp(fc_tfr_log$mean)
fc_tfr_lower <- exp(fc_tfr_log$lower[, 2])  # 95% lower
fc_tfr_upper <- exp(fc_tfr_log$upper[, 2])  # 95% upper

fc_tlb_mean <- exp(fc_tlb_log$mean)
fc_tlb_lower <- exp(fc_tlb_log$lower[, 2])
fc_tlb_upper <- exp(fc_tlb_log$upper[, 2])

years_test <- 2013:2024

# TFR forecast plot

png("fig_tfr_forecast.png", width = 1200, height = 800, res = 120)

plot(
  tfr_ts,
  xlim = c(1960, 2024),
  ylim = range(c(tfr_ts, fc_tfr_lower, fc_tfr_upper), na.rm = TRUE),
  main = "Forecast of Singapore TFR, 2013-2024",
  ylab = "TFR",
  xlab = "Year",
  col = "black",
  lwd = 2
)

# Add testing actual values
lines(tfr_test_raw, col = "red", lwd = 2)

# Add forecast mean and interval
lines(ts(fc_tfr_mean, start = 2013, frequency = 1), col = "blue", lwd = 2)
lines(ts(fc_tfr_lower, start = 2013, frequency = 1), col = "blue", lty = 2)
lines(ts(fc_tfr_upper, start = 2013, frequency = 1), col = "blue", lty = 2)

abline(v = 2012, lty = 3)

legend(
  "topright",
  legend = c("Observed series", "Testing actual", "Forecast mean", "95% interval"),
  col = c("black", "red", "blue", "blue"),
  lty = c(1, 1, 1, 2),
  lwd = c(2, 2, 2, 1),
  bty = "n"
)

dev.off()


# TLB forecast plot

png("fig_tlb_forecast.png", width = 1200, height = 800, res = 120)

plot(
  tlb_ts,
  xlim = c(1960, 2024),
  ylim = range(c(tlb_ts, fc_tlb_lower, fc_tlb_upper), na.rm = TRUE),
  main = "Forecast of Singapore Total Live Births, 2013-2024",
  ylab = "Total Live Births",
  xlab = "Year",
  col = "black",
  lwd = 2
)

# Add testing actual values
lines(tlb_test_raw, col = "red", lwd = 2)

# Add forecast mean and interval
lines(ts(fc_tlb_mean, start = 2013, frequency = 1), col = "blue", lwd = 2)
lines(ts(fc_tlb_lower, start = 2013, frequency = 1), col = "blue", lty = 2)
lines(ts(fc_tlb_upper, start = 2013, frequency = 1), col = "blue", lty = 2)

abline(v = 2012, lty = 3)

legend(
  "topright",
  legend = c("Observed series", "Testing actual", "Forecast mean", "95% interval"),
  col = c("black", "red", "blue", "blue"),
  lty = c(1, 1, 1, 2),
  lwd = c(2, 2, 2, 1),
  bty = "n"
)

dev.off()

print("Forecast plots exported successfully.")

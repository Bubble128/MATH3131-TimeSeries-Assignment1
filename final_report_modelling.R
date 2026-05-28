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


# TLB candidate models

# TLB is mainly considered with d = 1.
# The first-differenced log(TLB) series is reasonably stable.
# Low-order ARIMA models are used as baselines.
# Period-12 SARIMA-style models are included because lag 12 is visible
# in the ACF/PACF and may correspond to a 12-year demographic cycle.

fit_tlb_010 <- safe_arima(tlb_train, order = c(0, 1, 0))
fit_tlb_011 <- safe_arima(tlb_train, order = c(0, 1, 1))
fit_tlb_110 <- safe_arima(tlb_train, order = c(1, 1, 0))

fit_tlb_sarima_010_011 <- safe_arima(
  tlb_train,
  order = c(0, 1, 0),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tlb_sarima_110_011 <- safe_arima(
  tlb_train,
  order = c(1, 1, 0),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tlb_sarima_010_110 <- safe_arima(
  tlb_train,
  order = c(0, 1, 0),
  seasonal = list(order = c(1, 1, 0), period = 12)
)


# TFR candidate models

# TFR is more complex.
# d = 1 is the main starting point because first-differenced log(TFR)
# is already roughly centred around zero.
# d = 2 is also included as an alternative candidate because TFR has a
# strong long-term decline and second-order differencing is a defensible
# sensitivity check.

fit_tfr_111 <- safe_arima(tfr_train, order = c(1, 1, 1))
fit_tfr_011 <- safe_arima(tfr_train, order = c(0, 1, 1))
fit_tfr_110 <- safe_arima(tfr_train, order = c(1, 1, 0))

fit_tfr_021 <- safe_arima(tfr_train, order = c(0, 2, 1))
fit_tfr_120_d2 <- safe_arima(tfr_train, order = c(12, 2, 0))

fit_tfr_sarima_110_011 <- safe_arima(
  tfr_train,
  order = c(1, 1, 0),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tfr_sarima_010_011 <- safe_arima(
  tfr_train,
  order = c(0, 1, 0),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

fit_tfr_sarima_620_110 <- safe_arima(
  tfr_train,
  order = c(6, 2, 0),
  seasonal = list(order = c(1, 1, 0), period = 12)
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
  "ARIMA(0,1,0)(0,1,1)[12]",
  "ARIMA(1,1,0)(0,1,1)[12]",
  "ARIMA(0,1,0)(1,1,0)[12]",
  "auto.arima"
)

tlb_model_objects <- list(
  fit_tlb_010,
  fit_tlb_011,
  fit_tlb_110,
  fit_tlb_sarima_010_011,
  fit_tlb_sarima_110_011,
  fit_tlb_sarima_010_110,
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
  "ARIMA(1,1,1)",
  "ARIMA(0,1,1)",
  "ARIMA(1,1,0)",
  "ARIMA(0,2,1)",
  "ARIMA(12,2,0)",
  "ARIMA(1,1,0)(0,1,1)[12]",
  "ARIMA(0,1,0)(0,1,1)[12]",
  "ARIMA(6,2,0)(1,1,0)[12]",
  "auto.arima"
)

tfr_model_objects <- list(
  fit_tfr_111,
  fit_tfr_011,
  fit_tfr_110,
  fit_tfr_021,
  fit_tfr_120_d2,
  fit_tfr_sarima_110_011,
  fit_tfr_sarima_010_011,
  fit_tfr_sarima_620_110,
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

library(forecast)

raw_data <- read.csv("BirthsAndFertilityRatesAnnual.csv",
                     header = FALSE,
                     stringsAsFactors = FALSE)

tfr_values <- rev(as.numeric(unlist(raw_data[2, 3:67])))
tlb_values <- rev(as.numeric(unlist(raw_data[16, 3:67])))

tfr_ts <- ts(tfr_values, start = 1960, frequency = 1)
tlb_ts <- ts(tlb_values, start = 1960, frequency = 1)

tfr_train <- window(tfr_ts, start = 1960, end = 2012)
tlb_train <- window(tlb_ts, start = 1960, end = 2012)

tfr_test <- window(tfr_ts, start = 2013, end = 2024)
tlb_test <- window(tlb_ts, start = 2013, end = 2024)

# TFR ARIMA baseline candidates
fit_tfr_110 <- Arima(tfr_train, order = c(1,1,0))
fit_tfr_011 <- Arima(tfr_train, order = c(0,1,1))
fit_tfr_111 <- Arima(tfr_train, order = c(1,1,1))
fit_tfr_120 <- Arima(tfr_train, order = c(12,1,0))
fit_tfr_130 <- Arima(tfr_train, order = c(13,1,0))

# TLB ARIMA baseline candidates
fit_tlb_010 <- Arima(tlb_train, order = c(0,1,0))
fit_tlb_011 <- Arima(tlb_train, order = c(0,1,1))
fit_tlb_110 <- Arima(tlb_train, order = c(1,1,0))
fit_tlb_120 <- Arima(tlb_train, order = c(12,1,0))
fit_tlb_130 <- Arima(tlb_train, order = c(13,1,0))

# TFR SARIMA-style candidates
fit_tfr_sarima_110_011 <- Arima(
  tfr_train,
  order = c(1,1,0),
  seasonal = list(order = c(0,1,1), period = 12)
)

fit_tfr_sarima_510_011 <- Arima(
  tfr_train,
  order = c(5,1,0),
  seasonal = list(order = c(0,1,1), period = 12)
)

# TLB SARIMA-style candidates
fit_tlb_sarima_011_011 <- Arima(
  tlb_train,
  order = c(0,1,1),
  seasonal = list(order = c(0,1,1), period = 12)
)

fit_tlb_sarima_110_011 <- Arima(
  tlb_train,
  order = c(1,1,0),
  seasonal = list(order = c(0,1,1), period = 12)
)

# Model comparison: AIC and BIC
tfr_model_table <- data.frame(
  Series = "TFR",
  Model = c(
    "ARIMA(1,1,0)",
    "ARIMA(0,1,1)",
    "ARIMA(1,1,1)",
    "ARIMA(12,1,0)",
    "ARIMA(13,1,0)",
    "ARIMA(1,1,0)(0,1,1)[12]",
    "ARIMA(5,1,0)(0,1,1)[12]"
  ),
  AIC = c(
    AIC(fit_tfr_110),
    AIC(fit_tfr_011),
    AIC(fit_tfr_111),
    AIC(fit_tfr_120),
    AIC(fit_tfr_130),
    AIC(fit_tfr_sarima_110_011),
    AIC(fit_tfr_sarima_510_011)
  ),
  BIC = c(
    BIC(fit_tfr_110),
    BIC(fit_tfr_011),
    BIC(fit_tfr_111),
    BIC(fit_tfr_120),
    BIC(fit_tfr_130),
    BIC(fit_tfr_sarima_110_011),
    BIC(fit_tfr_sarima_510_011)
  )
)

tlb_model_table <- data.frame(
  Series = "TLB",
  Model = c(
    "ARIMA(0,1,0)",
    "ARIMA(0,1,1)",
    "ARIMA(1,1,0)",
    "ARIMA(12,1,0)",
    "ARIMA(13,1,0)",
    "ARIMA(0,1,1)(0,1,1)[12]",
    "ARIMA(1,1,0)(0,1,1)[12]"
  ),
  AIC = c(
    AIC(fit_tlb_010),
    AIC(fit_tlb_011),
    AIC(fit_tlb_110),
    AIC(fit_tlb_120),
    AIC(fit_tlb_130),
    AIC(fit_tlb_sarima_011_011),
    AIC(fit_tlb_sarima_110_011)
  ),
  BIC = c(
    BIC(fit_tlb_010),
    BIC(fit_tlb_011),
    BIC(fit_tlb_110),
    BIC(fit_tlb_120),
    BIC(fit_tlb_130),
    BIC(fit_tlb_sarima_011_011),
    BIC(fit_tlb_sarima_110_011)
  )
)

print("--- TFR AIC/BIC comparison ---")
print(tfr_model_table)

print("--- TLB AIC/BIC comparison ---")
print(tlb_model_table)
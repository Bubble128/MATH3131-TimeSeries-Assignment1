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
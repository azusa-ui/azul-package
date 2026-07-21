test_that("ARIMA model reports order, differencing and AIC", {
  set.seed(1); x <- ts(cumsum(rnorm(120)) + sin((1:120)/6), frequency = 12)
  m <- arima(x, order = c(1,1,1), seasonal = list(order = c(0,1,1), period = 12))
  s <- interpret(m, outcome = "cases")
  expect_match(txt(s), "ARIMA\\(1,1,1\\)\\(0,1,1\\)\\[12\\]")
  expect_match(txt(s), "differenced")
  expect_match(txt(s), "AIC")
})

test_that("Ljung-Box, ADF and KPSS htests are interpreted", {
  set.seed(2); x <- ts(rnorm(100))
  lb <- Box.test(x, lag = 10, type = "Ljung-Box")
  expect_match(txt(interpret(lb)), "white noise")
  adf <- structure(list(statistic = c("Dickey-Fuller" = -2.1), p.value = 0.5,
    method = "Augmented Dickey-Fuller Test"), class = "htest")
  expect_match(txt(interpret(adf)), "stationarity")
  kp <- structure(list(statistic = c("KPSS Level" = 0.6), p.value = 0.02,
    method = "KPSS Test for Level Stationarity"), class = "htest")
  expect_match(txt(interpret(kp)), "KPSS")
})

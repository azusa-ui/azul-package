test_that("azul_plot draws a forest plot and returns an interpretation", {
  d <- within(mtcars, am <- factor(am))
  f <- tempfile(fileext = ".png"); grDevices::png(f)
  i <- azul_plot(glm(am ~ wt + hp, binomial, d))
  grDevices::dev.off()
  expect_true(file.exists(f))
  expect_s3_class(i, "azul_interpretation")
  expect_match(as.character(i), "Forest plot")
})

test_that("azul_plot handles km, residuals and roc", {
  skip_if_not_installed("survival")
  library(survival)
  f <- tempfile(fileext = ".png"); grDevices::png(f)
  azul_plot(survfit(Surv(time, status) ~ sex, lung))
  azul_plot(lm(mpg ~ wt, mtcars), type = "residuals")
  grDevices::dev.off()
  expect_true(file.exists(f))
  skip_if_not_installed("pROC")
  d <- within(mtcars, am <- factor(am)); pr <- predict(glm(am ~ wt, binomial, d), type = "response")
  g <- tempfile(fileext = ".png"); grDevices::png(g)
  ri <- azul_plot(pROC::roc(d$am, pr, quiet = TRUE))
  grDevices::dev.off()
  expect_true(file.exists(g))
})

test_that("azul_plot draws time-series ACF/PACF and ARIMA diagnostics", {
  set.seed(1); x <- ts(cumsum(rnorm(120)) + sin((1:120)/6), frequency = 12)
  f <- tempfile(fileext = ".png"); grDevices::png(f)
  s1 <- azul_plot(x)                              # ts -> series + ACF + PACF
  s2 <- azul_plot(x, type = "acf")               # ACF + PACF
  m <- arima(x, order = c(1, 1, 1))
  s3 <- azul_plot(m)                             # tsdiag -> residual ACF/PACF
  grDevices::dev.off()
  expect_true(file.exists(f))
  expect_match(as.character(s1), "ACF")
  expect_match(as.character(s3), "white noise")
})

test_that("azul_plot draws mixed-model caterpillar and Cox Schoenfeld plots", {
  skip_if_not_installed("lme4"); skip_if_not_installed("survival")
  set.seed(1); n <- 300
  d <- data.frame(x = rnorm(n), g = factor(sample(1:20, n, TRUE)))
  d$y <- 2 * d$x + rnorm(20)[as.integer(d$g)] * 2 + rnorm(n)
  f <- tempfile(fileext = ".png"); grDevices::png(f)
  s1 <- azul_plot(lme4::lmer(y ~ x + (1 | g), d))
  cx <- survival::coxph(survival::Surv(time, status) ~ age + sex, survival::lung)
  s2 <- azul_plot(cx, type = "schoenfeld")
  grDevices::dev.off()
  expect_match(as.character(s1), "aterpillar|BLUP")
  expect_match(as.character(s2), "Schoenfeld|proportional hazards")
})

test_that("azul_plot draws a calibration plot for logistic models", {
  f <- tempfile(fileext = ".png"); grDevices::png(f)
  s <- azul_plot(glm(vs ~ wt + hp + mpg, binomial, mtcars), type = "calibration")
  grDevices::dev.off()
  expect_match(as.character(s), "alibrat")
})

test_that("azul_plot draws effect, qqrand and scree figures", {
  set.seed(1); n <- 300
  d <- data.frame(age = rnorm(n, 50, 10), sex = factor(sample(c("F","M"), n, TRUE)))
  d$dz <- rbinom(n, 1, plogis(-2 + 0.02*d$age + ifelse(d$sex=="M", 0.06*d$age, 0)))
  f <- tempfile(fileext = ".png"); grDevices::png(f)
  s1 <- azul_plot(glm(dz ~ age * sex, binomial, d), type = "effect")
  grDevices::dev.off()
  expect_match(as.character(s1), "predicted|interaction")
  skip_if_not_installed("lme4")
  dd <- data.frame(x = rnorm(200), g = factor(sample(1:20, 200, TRUE)))
  dd$y <- 2*dd$x + rnorm(20)[as.integer(dd$g)]*2 + rnorm(200)
  g <- tempfile(fileext = ".png"); grDevices::png(g)
  s2 <- azul_plot(lme4::lmer(y ~ x + (1|g), dd), type = "qqrand")
  s3 <- azul_plot(cor(mtcars), type = "scree")
  grDevices::dev.off()
  expect_match(as.character(s2), "normal")
  expect_match(as.character(s3), "Eigenvalues|Kaiser")
})

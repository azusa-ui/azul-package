test_that("azul_survtable builds a flextable for a Weibull survreg model", {
  skip_if_not_installed("survival")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  m <- survival::survreg(survival::Surv(time, status) ~ sex + ph.ecog,
                         data = survival::lung, dist = "weibull")
  ft <- azul_survtable(m)
  expect_s3_class(ft, "flextable")
})

test_that("azul_survtable handles exponential and log-logistic distributions", {
  skip_if_not_installed("survival"); skip_if_not_installed("flextable")
  ex <- survival::survreg(survival::Surv(time, status) ~ sex,
                          data = survival::lung, dist = "exponential")
  ll <- survival::survreg(survival::Surv(time, status) ~ sex,
                          data = survival::lung, dist = "loglogistic")
  expect_s3_class(azul_survtable(ex), "flextable")
  expect_s3_class(azul_survtable(ll), "flextable")
})

test_that("azul_survtable supports log-normal (AFT-only) models", {
  skip_if_not_installed("survival"); skip_if_not_installed("flextable")
  m <- survival::survreg(survival::Surv(time, status) ~ sex,
                         data = survival::lung, dist = "lognormal")
  ft <- azul_survtable(m)
  expect_s3_class(ft, "flextable")
})

test_that("azul_survcompare ranks distributions by AIC", {
  skip_if_not_installed("survival"); skip_if_not_installed("flextable")
  ft <- azul_survcompare(survival::Surv(time, status) ~ sex + ph.ecog,
                         data = survival::lung)
  expect_s3_class(ft, "flextable")
  tab <- attr(ft, "table")
  expect_true(all(diff(tab$AIC) >= 0))          # sorted ascending by AIC
  expect_equal(min(tab$dAIC), 0)                # best model has dAIC 0
})

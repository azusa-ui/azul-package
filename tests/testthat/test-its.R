test_that("ITS reports level change, trend and slope change", {
  set.seed(1); n <- 120
  d <- data.frame(time = 1:n, smokban = as.integer(1:n > 60), pop = 1e5)
  lp <- -6 + 0.002*d$time - 0.15*d$smokban - 0.004*d$smokban*d$time
  d$aces <- rpois(n, exp(lp + log(d$pop)))
  m <- glm(aces ~ offset(log(pop)) + smokban*time, family = quasipoisson, data = d)
  s <- interpret_its(m, level = "smokban", trend = "time", period = 12,
                     unit = "month", period_label = "year", outcome = "events")
  expect_match(txt(s), "interrupted time series")
  expect_match(txt(s), "level \\(step\\) change")
  expect_match(txt(s), "post-intervention trend")
  expect_match(txt(s), "segmented")
})

test_that("step-only ITS notes no slope change", {
  set.seed(2); n <- 120
  d <- data.frame(time = 1:n, smokban = as.integer(1:n > 60), pop = 1e5)
  d$aces <- rpois(n, exp(-6 + 0.002*d$time - 0.15*d$smokban + log(d$pop)))
  m <- glm(aces ~ offset(log(pop)) + smokban + time, family = quasipoisson, data = d)
  s <- interpret_its(m, level = "smokban", trend = "time", outcome = "events")
  expect_match(txt(s), "no change in slope|level-change")
})

test_that("nested model comparison is interpreted", {
  set.seed(3); n <- 120
  d <- data.frame(time = 1:n, smokban = as.integer(1:n > 60), pop = 1e5)
  d$aces <- rpois(n, exp(-6 + 0.002*d$time - 0.15*d$smokban + log(d$pop)))
  m1 <- glm(aces ~ offset(log(pop)) + smokban + time, family = quasipoisson, data = d)
  m2 <- glm(aces ~ offset(log(pop)) + smokban*time, family = quasipoisson, data = d)
  s <- interpret(anova(m1, m2, test = "F"))
  expect_match(txt(s), "nested models|improve")
})

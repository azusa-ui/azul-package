test_that("2x2 diagnostic table gives sens/spec/PPV/NPV with CIs", {
  tb <- matrix(c(85,15,20,180), 2, byrow = TRUE,
               dimnames = list(Test = c("Pos","Neg"), Disease = c("Pos","Neg")))
  s <- interpret_diagnostic(tb, test = "the rapid test")
  expect_match(txt(s), "sensitivity was 81")
  expect_match(txt(s), "specificity was 92")
  expect_match(txt(s), "likelihood ratio")
  expect_match(txt(s), "95% CI")
})

test_that("ROC object reports AUC with 95% CI and a verdict", {
  skip_if_not_installed("pROC")
  set.seed(1); n <- 400
  d <- data.frame(x = rnorm(n)); d$y <- rbinom(n, 1, plogis(-0.4 + 1.3 * d$x))
  pr <- predict(glm(y ~ x, d, family = binomial), type = "response")
  s <- interpret(pROC::roc(d$y, pr, quiet = TRUE))
  expect_match(txt(s), "AUC")
  expect_match(txt(s), "discrimination")
})

test_that("Hosmer-Lemeshow htest is interpreted as calibration", {
  skip_if_not_installed("ResourceSelection")
  set.seed(2); n <- 400
  d <- data.frame(x = rnorm(n)); d$y <- rbinom(n, 1, plogis(-0.4 + 1.3 * d$x))
  pr <- predict(glm(y ~ x, d, family = binomial), type = "response")
  s <- interpret(ResourceSelection::hoslem.test(d$y, pr, g = 10))
  expect_match(txt(s), "Hosmer-Lemeshow")
  expect_match(txt(s), "calibration")
})

test_that("caret confusionMatrix is interpreted", {
  skip_if_not_installed("caret")
  set.seed(3); n <- 300
  d <- data.frame(x = rnorm(n)); d$y <- rbinom(n, 1, plogis(-0.4 + 1.3 * d$x))
  pr <- predict(glm(y ~ x, d, family = binomial), type = "response")
  cm <- caret::confusionMatrix(factor(ifelse(pr > 0.5, 1, 0), levels = c(0,1)),
                               factor(d$y, levels = c(0,1)), positive = "1")
  s <- interpret(cm)
  expect_match(txt(s), "sensitivity")
  expect_match(txt(s), "kappa")
})

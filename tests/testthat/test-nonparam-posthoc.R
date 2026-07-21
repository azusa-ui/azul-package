test_that("Friedman, KS and sign tests interpret", {
  set.seed(2); mat <- matrix(rnorm(60), 20, 3)
  expect_match(txt(interpret(friedman.test(mat))), "Friedman")
  expect_match(txt(interpret(suppressWarnings(ks.test(rnorm(50), rnorm(50, 1))))), "Kolmogorov-Smirnov")
})

test_that("TukeyHSD post-hoc is interpreted with adjusted CIs", {
  set.seed(1); d <- data.frame(y = rnorm(90), g = factor(rep(c("A","B","C"), each = 30)))
  d$y <- d$y + as.numeric(d$g) * 3
  s <- interpret(TukeyHSD(aov(y ~ g, d)))
  expect_match(txt(s), "Tukey")
  expect_match(txt(s), "mean difference")
  expect_match(txt(s), "95% CI")
})

test_that("emmeans contrasts are interpreted", {
  skip_if_not_installed("emmeans")
  set.seed(1); d <- data.frame(y = rnorm(90), g = factor(rep(c("A","B","C"), each = 30)))
  d$y <- d$y + as.numeric(d$g) * 3
  emm <- emmeans::emmeans(lm(y ~ g, d), "g")
  s <- interpret(emmeans::contrast(emm, "pairwise"))
  expect_match(txt(s), "contrast")
})

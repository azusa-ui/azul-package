test_that("KMO and Bartlett interpret and are disambiguated", {
  skip_if_not_installed("psych")
  d <- na.omit(psych::bfi[, 1:15])
  expect_match(txt(interpret(psych::KMO(d))), "Kaiser-Meyer-Olkin")
  b <- psych::cortest.bartlett(cor(d), n = nrow(d))
  expect_match(txt(interpret(b)), "sphericity")
})

test_that("EFA (psych::fa) reports variance, loadings and fit", {
  skip_if_not_installed("psych")
  d <- na.omit(psych::bfi[, 1:15])
  s <- interpret(psych::fa(d, nfactors = 3, rotate = "varimax", fm = "minres"))
  expect_match(txt(s), "exploratory factor analysis")
  expect_match(txt(s), "total variance")
  expect_match(txt(s), "RMSEA")
})

test_that("Cronbach's alpha reports reliability with a verdict", {
  skip_if_not_installed("psych")
  d <- na.omit(psych::bfi[, 1:5])
  s <- interpret(psych::alpha(d, check.keys = TRUE))
  expect_match(txt(s), "Cronbach")
  expect_match(txt(s), "internal consistency")
})

test_that("factanal EFA interprets with the sufficiency test", {
  skip_if_not_installed("psych")
  d <- na.omit(psych::bfi[, 1:15])
  s <- interpret(factanal(d, factors = 3))
  expect_match(txt(s), "maximum-likelihood")
  expect_match(txt(s), "factor")
})

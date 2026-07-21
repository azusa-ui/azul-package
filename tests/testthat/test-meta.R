test_that("metafor rma meta-analysis is interpreted", {
  skip_if_not_installed("metafor")
  dat <- metafor::escalc(measure = "OR", ai = c(10,12,8,15), bi = c(40,38,42,35),
                         ci = c(5,7,6,9), di = c(45,43,44,41))
  m <- metafor::rma(yi, vi, data = dat)
  s <- interpret(m)
  expect_match(txt(s), "meta-analysis")
  expect_match(txt(s), "pooled OR")
  expect_match(txt(s), "I-squared")
})

test_that("meta package object is interpreted", {
  skip_if_not_installed("meta")
  mb <- meta::metabin(c(10,12,8,15), c(50,50,50,50), c(5,7,6,9), c(50,50,50,50), sm = "RR")
  s <- interpret(mb)
  expect_match(txt(s), "pooled RR")
  expect_match(txt(s), "heterogeneity")
})

make_df <- function(n = 150, seed = 1) {
  set.seed(seed)
  d <- data.frame(
    age = round(rnorm(n, 45, 12)),
    bmi = round(rlnorm(n, log(25), 0.2), 1),
    sex = factor(sample(c("Male","Female"), n, TRUE)),
    smoker = sample(c(0, 1), n, TRUE),
    group = factor(sample(c("Control","Treatment"), n, TRUE)))
  d$age[c(3, 9)] <- NA
  d
}

test_that("raw data frame is interpreted descriptively", {
  s <- interpret(make_df())
  expect_s3_class(s, "azul_interpretation")
  expect_match(txt(s), "observations on 5 variables")
  expect_match(txt(s), "mean age")
  expect_match(txt(s), "For sex")
  expect_match(txt(s), "missing", ignore.case = TRUE)
})

test_that("interpret_descriptive stratifies by a group", {
  s <- interpret_descriptive(make_df(), by = "group")
  expect_match(txt(s), "stratified by group")
  expect_match(txt(s), "group = Control")
  expect_match(txt(s), "group = Treatment")
})

test_that("a results table still routes to interpret_table", {
  tab <- data.frame(term = c("Male","Age"), OR = c(1.49, 1.03),
                    LL = c(1.24, 1.01), UL = c(1.78, 1.05), p = c(4e-4, 0.02))
  s <- interpret(tab)
  expect_match(txt(s), "times the odds")
})

test_that("tibble (spec_tbl_df) dispatches to descriptive", {
  d <- make_df()
  class(d) <- c("spec_tbl_df", "tbl_df", "tbl", "data.frame")
  s <- interpret(d)
  expect_match(txt(s), "observations")
})

test_that("descriptive handles Date and ordered factor types", {
  set.seed(1); n <- 60
  d <- data.frame(
    dt = as.Date("2024-01-01") + sample(0:300, n, TRUE),
    sev = ordered(sample(c("mild","mod","sev"), n, TRUE), levels = c("mild","mod","sev")))
  s <- interpret(d)
  expect_match(txt(s), "date/time")
  expect_match(txt(s), "ordinal")
  expect_match(txt(s), "median category")
})

test_that("table objects are interpreted (2-D association, 1-D frequency)", {
  m <- matrix(c(30, 10, 12, 28), 2, dimnames = list(c("a","b"), c("x","y")))
  expect_match(txt(interpret(as.table(m))), "association|chi-squared")
  tb <- table(factor(rep(c("A","B","C"), c(10, 20, 30))))
  expect_match(txt(interpret(tb)), "frequency distribution")
})

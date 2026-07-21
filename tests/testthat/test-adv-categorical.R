test_that("binary logistic yields Crude/Adjusted OR with 95% CI", {
  d <- make_cat_data()
  s <- interpret(glm(y_bin ~ x, d, family = binomial), outcome = "disease")
  expect_s3_class(s, "azul_interpretation")
  expect_match(txt(s), "Simple logistic regression")
  expect_match(txt(s), "Crude OR")
  expect_match(txt(s), "95% CI")
  m <- interpret(glm(y_bin ~ grp3 + x, d, family = binomial), outcome = "disease")
  expect_match(txt(m), "Adjusted OR")
  expect_match(txt(m), "grp3B"); expect_match(txt(m), "grp3C")
  expect_false(grepl("P = NA", txt(m)))
})

test_that("Poisson and negative binomial yield IRR", {
  d <- make_cat_data()
  p <- interpret(glm(cnt ~ x + offset(log(ptime)), d, family = poisson), outcome = "events")
  expect_match(txt(p), "IRR")
  q <- interpret(glm(cnt ~ x, d, family = quasipoisson), outcome = "events")
  expect_match(txt(q), "Quasi-Poisson")
  skip_if_not_installed("MASS")
  nb <- interpret(MASS::glm.nb(cnt ~ x, d), outcome = "events")
  expect_match(txt(nb), "Negative binomial")
  expect_match(txt(nb), "IRR")
})

test_that("ordinal logistic returns unified OR with computed P values", {
  skip_if_not_installed("MASS")
  d <- make_cat_data()
  s <- interpret(MASS::polr(sev ~ x, d, Hess = TRUE), outcome = "severity")
  expect_match(txt(s), "proportional odds")
  expect_false(grepl("P = NA", txt(s)))
})

test_that("multinomial names each outcome category vs reference", {
  skip_if_not_installed("nnet")
  d <- make_cat_data()
  s <- suppressWarnings(interpret(nnet::multinom(grp3 ~ x, d, trace = FALSE), outcome = "group"))
  expect_match(txt(s), "reference outcome category")
  expect_match(txt(s), "RRR")
})

test_that("conditional logistic flags the matched design", {
  skip_if_not_installed("survival")
  library(survival)
  cc <- data.frame(set = rep(1:60, each = 2), case = rep(c(1, 0), 60),
                   exp = rbinom(120, 1, .5), age = rnorm(120, 60, 8))
  s <- interpret(clogit(case ~ exp + age + strata(set), cc),
                 outcome = "case status")
  expect_match(txt(s), "Conditional logistic")
  expect_match(txt(s), "matched")
})

test_that("GEE gives a population-averaged marginal effect", {
  skip_if_not_installed("geepack")
  d <- make_cat_data(); d <- d[order(d$clust), ]
  g <- geepack::geeglm(y_bin ~ x, id = clust, data = d,
                       family = binomial, corstr = "exchangeable")
  s <- interpret(g, outcome = "disease")
  expect_match(txt(s), "generalised estimating equations", ignore.case = TRUE)
  expect_match(txt(s), "population-averaged", ignore.case = TRUE)
})

test_that("zero-inflated model interprets both components", {
  skip_if_not_installed("pscl")
  set.seed(7); n <- 300
  zdf <- data.frame(x = rnorm(n))
  zdf$cnt <- ifelse(rbinom(n, 1, .3) == 1, 0, rpois(n, 3))
  s <- interpret(pscl::zeroinfl(cnt ~ x | x, data = zdf), outcome = "events")
  expect_match(txt(s), "count component")
  expect_match(txt(s), "zero-inflation component")
})

test_that("chi-square and Fisher htest are interpreted", {
  expect_match(txt(interpret(chisq.test(matrix(c(30, 20, 10, 40), 2)))), "chi-squared")
  expect_match(txt(interpret(fisher.test(matrix(c(8, 2, 1, 9), 2)))), "Fisher")
})

test_that("interpret_table reads a supplied OR table", {
  tab <- data.frame(term = c("Male", "Age"), OR = c(1.49, 1.03),
                    LL = c(1.24, 1.01), UL = c(1.78, 1.05), p = c(0.0004, 0.02))
  s <- interpret_table(tab, type = "OR", outcome = "coronary artery disease")
  expect_match(txt(s), "Male")
  expect_match(txt(s), "1.49 times the odds")
  expect_match(txt(s), "95% CI")
})

test_that("VGAM vglm: cumulative gives OR, multinomial gives RRR", {
  skip_if_not_installed("VGAM")
  set.seed(1); n <- 200
  d <- data.frame(sev = ordered(sample(c("mild","mod","sev"), n, TRUE),
                                 levels = c("mild","mod","sev")),
                  grp = factor(sample(c("A","B","C"), n, TRUE)), x = rnorm(n))
  m1 <- VGAM::vglm(sev ~ x + grp,
                   VGAM::cumulative(parallel = TRUE, reverse = TRUE), data = d)
  s1 <- interpret(m1, outcome = "disease severity")
  expect_match(txt(s1), "proportional odds")
  expect_match(txt(s1), "OR")
  expect_false(grepl("P = NA", txt(s1)))
  m2 <- VGAM::vglm(grp ~ x, VGAM::multinomial, data = d)
  s2 <- interpret(m2, outcome = "group membership")
  expect_match(txt(s2), "RRR")
})

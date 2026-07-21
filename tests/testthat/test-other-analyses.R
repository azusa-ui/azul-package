test_that("linear regression separates continuous and factor terms", {
  set.seed(1); n <- 120
  d <- data.frame(x = rnorm(n, 10, 3), g = factor(sample(c("A","B","C"), n, TRUE)))
  d$y <- 20 + 1.5 * d$x + as.numeric(d$g) * 2 + rnorm(n, 0, 5)
  s <- interpret(lm(y ~ x + g, d), outcome = "score", unit = "point")
  expect_match(txt(s), "Each one-unit increase in x")
  expect_match(txt(s), "group had a score")
  expect_match(txt(s), "R-squared")
})

test_that("t-tests, correlation and non-parametric htests interpret", {
  set.seed(2); d <- data.frame(y = rnorm(80, 50, 10), g = factor(sample(c("F","M"), 80, TRUE)), x = rnorm(80))
  expect_match(txt(interpret(t.test(y ~ g, d, var.equal = TRUE))), "independent t-test")
  expect_match(txt(interpret(t.test(y ~ g, d))), "Welch")
  expect_match(txt(interpret(cor.test(d$x, d$y))), "Pearson")
  expect_match(txt(interpret(wilcox.test(y ~ g, d))), "Mann-Whitney")
  expect_match(txt(interpret(kruskal.test(y ~ g, d))), "Kruskal-Wallis")
})

test_that("ANOVA and MANOVA interpret", {
  set.seed(3); d <- data.frame(y = rnorm(90), x = rnorm(90), g = factor(rep(c("A","B","C"), 30)))
  expect_match(txt(interpret(aov(y ~ g, d), outcome = "score")), "ANOVA")
  expect_match(txt(interpret(aov(y ~ g + x, d), outcome = "score")), "ANCOVA|factorial")
  expect_match(txt(interpret(manova(cbind(y, x) ~ g, data = d))), "Wilks")
})

test_that("survival: KM, log-rank and Cox interpret", {
  skip_if_not_installed("survival")
  library(survival)
  set.seed(4); sd <- data.frame(time = rexp(150, .1), status = rbinom(150, 1, .7),
                                grp = factor(sample(c("ctrl","trt"), 150, TRUE)), age = rnorm(150, 60, 10))
  expect_match(txt(interpret(survfit(Surv(time, status) ~ 1, sd), outcome = "death")), "Kaplan-Meier")
  expect_match(txt(interpret(survdiff(Surv(time, status) ~ grp, sd))), "log-rank")
  cx <- interpret(coxph(Surv(time, status) ~ grp + age, sd), outcome = "death")
  expect_match(txt(cx), "hazard")
  expect_match(txt(cx), "HR")
})

test_that("mixed models extract fixed effects and ICC", {
  skip_if_not_installed("lme4")
  set.seed(5); md <- data.frame(x = rnorm(200), cl = factor(sample(1:20, 200, TRUE)))
  md$y <- 2 * md$x + rnorm(20)[as.integer(md$cl)] + rnorm(200)
  s <- interpret(lme4::lmer(y ~ x + (1 | cl), md))
  expect_match(txt(s), "mixed-effects")
  expect_match(txt(s), "fixed-effect estimate")
})

test_that("SEM / CFA interpret with fit indices", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa(' visual =~ x1+x2+x3 ; textual =~ x4+x5+x6 ',
                     data = lavaan::HolzingerSwineford1939)
  s <- interpret(fit)
  expect_match(txt(s), "CFI")
  expect_match(txt(s), "RMSEA")
})

test_that("survreg AFT and its summary interpret with TR and HR", {
  skip_if_not_installed("survival")
  m <- survival::survreg(survival::Surv(futime, fustat) ~ age,
                         data = survival::ovarian, dist = "weibull")
  expect_match(txt(interpret(m, outcome = "death")), "time ratio|TR")
  expect_match(txt(interpret(summary(m), outcome = "death")), "AFT")
})

test_that("Poisson goodness-of-fit list interprets", {
  skip_if_not_installed("epiDisplay")
  set.seed(6); d <- data.frame(y = rpois(100, 3), x = rnorm(100))
  g <- epiDisplay::poisgof(glm(y ~ x, d, family = poisson))
  expect_match(txt(interpret(g)), "goodness-of-fit")
})

test_that("ordinal assumption test (nominal_test) interprets", {
  skip_if_not_installed("ordinal")
  set.seed(7); n <- 200
  d <- data.frame(y = ordered(sample(c("lo","mid","hi"), n, TRUE), levels = c("lo","mid","hi")),
                  x = rnorm(n))
  s <- interpret(ordinal::nominal_test(ordinal::clm(y ~ x, data = d)))
  expect_match(txt(s), "proportional odds")
})

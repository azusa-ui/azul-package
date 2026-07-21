test_that("linear regression assumption checks run", {
  s <- check_assumptions(lm(mpg ~ wt + hp + disp, mtcars))
  expect_s3_class(s, "azul_interpretation")
  expect_match(txt(s), "Normality of residuals")
  expect_match(txt(s), "Shapiro-Wilk")
  expect_match(txt(s), "assumption met|MAY BE VIOLATED")
})

test_that("logistic assumption checks include Hosmer-Lemeshow and EPV", {
  skip_if_not_installed("ResourceSelection")
  d <- within(mtcars, am <- factor(am))
  s <- check_assumptions(glm(am ~ wt + hp, binomial, d))
  expect_match(txt(s), "Hosmer-Lemeshow")
  expect_match(txt(s), "events per variable|EPV")
})

test_that("Poisson checks overdispersion and Cox checks proportional hazards", {
  sp <- check_assumptions(glm(count ~ spray, poisson, InsectSprays))
  expect_match(txt(sp), "overdispersion|dispersion")
  skip_if_not_installed("survival")
  cx <- check_assumptions(survival::coxph(survival::Surv(time, status) ~ age + sex,
                                          survival::lung))
  expect_match(txt(cx), "Proportional hazards")
})

test_that("ANOVA checks normality and homogeneity", {
  s <- check_assumptions(aov(weight ~ group, PlantGrowth))
  expect_match(txt(s), "Normality")
  expect_match(txt(s), "Homogeneity|Levene")
})

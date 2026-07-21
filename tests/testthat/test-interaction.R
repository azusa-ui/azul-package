test_that("significant interaction triggers stratified simple effects", {
  skip_if_not_installed("emmeans"); skip_if_not_installed("car")
  set.seed(1); n <- 300
  d <- data.frame(sex = factor(sample(c("F","M"), n, TRUE)),
                  trt = factor(sample(c("placebo","drug"), n, TRUE)), age = rnorm(n, 50, 10))
  d$y <- 10 + 3*(d$trt=="drug") + 2*(d$sex=="M") + 8*(d$trt=="drug")*(d$sex=="M") + rnorm(n,0,4)
  s <- interpret(lm(y ~ sex*trt + age, d), outcome = "score")
  expect_match(txt(s), "statistically significant interaction was present")
  expect_match(txt(s), "Stratified effects")
  expect_match(txt(s), "Among")
})

test_that("non-significant interaction is interpreted marginally with a note", {
  skip_if_not_installed("car")
  set.seed(3); n <- 300
  d <- data.frame(x = rnorm(n), g = factor(sample(c("A","B"), n, TRUE)))
  d$y <- 2 * d$x + rnorm(n)
  s <- interpret(lm(y ~ x*g, d), outcome = "score")
  expect_match(txt(s), "not statistically significant")
  expect_match(txt(s), "interpreted marginally")
})

test_that("two-way ANOVA stratifies on a significant interaction", {
  skip_if_not_installed("emmeans"); skip_if_not_installed("car")
  set.seed(2); n <- 240
  d <- data.frame(a = factor(sample(c("lo","hi"), n, TRUE)),
                  b = factor(sample(c("x","y"), n, TRUE)))
  d$y <- 5 + 4*(d$a=="hi")*(d$b=="y") + rnorm(n)
  s <- interpret(aov(y ~ a*b, d), outcome = "score")
  expect_match(txt(s), "Stratified effects|simple")
})

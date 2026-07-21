test_that("print shows an AT A GLANCE summary derived from estimates", {
  d <- within(mtcars, am <- factor(am))
  s <- interpret(glm(am ~ wt + hp, binomial, d), outcome = "manual")
  out <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(out, "AT A GLANCE")
  expect_match(out, "reached statistical significance")
})

test_that("summary() gives a compact takeaway", {
  s <- interpret(lm(mpg ~ wt + hp, mtcars), outcome = "fuel economy")
  out <- paste(capture.output(summary(s)), collapse = "\n")
  expect_match(out, "Multiple linear regression")
  expect_match(out, "At a glance|linear regression")
})

test_that("at-a-glance degrades gracefully when no estimates", {
  s <- interpret(cor.test(mtcars$mpg, mtcars$wt))
  expect_s3_class(s, "azul_interpretation")
  # should not error even though there is no estimates table
  expect_silent(invisible(capture.output(print(s))))
})

test_that("additive interaction reports RERI, AP and S with CIs", {
  set.seed(1); n <- 1200
  d <- data.frame(a = rbinom(n,1,.5), b = rbinom(n,1,.5))
  d$y <- rbinom(n, 1, plogis(-2 + 0.5*d$a + 0.4*d$b + 1.2*d$a*d$b))
  d$comb <- factor(ifelse(d$a==0&d$b==0,"00",ifelse(d$a==1&d$b==0,"10",
              ifelse(d$a==0&d$b==1,"01","11"))), levels=c("00","10","01","11"))
  fit <- glm(y ~ comb, data = d, family = binomial)
  s <- interpret_additive_interaction(fit, c("comb10","comb01","comb11"), outcome = "disease")
  expect_match(txt(s), "RERI")
  expect_match(txt(s), "synergy index")
  expect_match(txt(s), "95% CI")
})

test_that("chi-square goodness-of-fit is worded distinctly", {
  s <- interpret(chisq.test(c(20, 30, 25, 25)))
  expect_match(txt(s), "goodness-of-fit")
})

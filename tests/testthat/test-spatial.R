test_that("Moran's I and Geary's C htests are interpreted", {
  mor <- structure(list(statistic=c("Moran I statistic standard deviate"=3.42),
    p.value=0.0003, estimate=c("Moran I statistic"=0.28,"Expectation"=-0.01,"Variance"=0.007),
    method="Moran I test under randomisation", alternative="greater"), class="htest")
  s <- interpret(mor)
  expect_match(txt(s), "Moran's I")
  expect_match(txt(s), "positive spatial autocorrelation")
  gea <- structure(list(statistic=c("Geary C statistic standard deviate"=2.7),
    p.value=0.004, estimate=c("Geary C statistic"=0.72,"Expectation"=1,"Variance"=0.01),
    method="Geary C test under randomisation", alternative="greater"), class="htest")
  expect_match(txt(interpret(gea)), "Geary's C")
})

test_that("SMR reports ratio, CI and direction", {
  s <- interpret_smr(observed = c(120, 45), expected = c(90, 52),
                     area = c("A", "B"))
  expect_match(txt(s), "SMR of 1.33")
  expect_match(txt(s), "excess")
  expect_match(txt(s), "95% CI")
})

test_that("LISA and Getis-Ord summarise clusters/hot spots", {
  set.seed(1)
  m <- cbind(Ii = rnorm(40, 0.1, 0.3), "Pr(z != E(Ii))" = runif(40))
  colnames(m) <- c("Ii", "Pr(z != E(Ii))")
  expect_match(txt(interpret_lisa(m)), "LISA|local spatial association")
  expect_match(txt(interpret_getis(rnorm(50))), "hot spot|cold spot")
})

test_that("spatial lag regression reports rho", {
  sar <- structure(list(type = "lag", rho = 0.42, rho.se = 0.09,
    coefficients = c("(Intercept)" = 1.2, x = 0.5)), class = "Sarlm")
  s <- interpret(sar, outcome = "disease rate")
  expect_match(txt(s), "spatial lag")
  expect_match(txt(s), "rho")
})

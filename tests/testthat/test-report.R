test_that("azul_table builds a formatted estimates table", {
  d <- within(mtcars, am <- factor(am))
  t <- azul_table(glm(am ~ wt + hp, binomial, d))
  expect_s3_class(t, "azul_table")
  expect_true(any(grepl("OR", names(t))))
  expect_true("P-value" %in% names(t))
  expect_equal(nrow(t), 2)
})

test_that("azul_report writes an HTML file", {
  f <- tempfile(fileext = ".html")
  out <- azul_report(interpret(lm(mpg ~ wt + hp, mtcars), outcome = "mpg"),
                     file = f, assumptions = FALSE)
  expect_true(file.exists(out))
  txt <- paste(readLines(out), collapse = "\n")
  expect_match(txt, "Interpretation")
  expect_match(txt, "Results table")
})

test_that("azul_report writes a docx when officer/flextable are available", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  f <- tempfile(fileext = ".docx")
  out <- azul_report(lm(mpg ~ wt + hp, mtcars), file = f, outcome = "mpg")
  expect_true(file.exists(out))
  expect_match(out, "\\.docx$")
})

test_that("azul_table uses fixed decimals (no scientific notation)", {
  t2 <- azul_table(glm(vs ~ wt + hp, binomial, mtcars), digits = 2)
  t3 <- azul_table(glm(vs ~ wt + hp, binomial, mtcars), digits = 3)
  col2 <- t2[[2]]; col3 <- t3[[2]]
  expect_false(any(grepl("e[+-]", col2)))   # no scientific notation
  expect_false(any(grepl("e[+-]", col3)))
  expect_match(col3[1], "\\.[0-9]{3}")        # 3 decimals when digits = 3
})

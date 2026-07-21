test_that("azul_help prints a grouped table of contents", {
  out <- paste(capture.output(azul_help()), collapse = "\n")
  expect_match(out, "interpret\\(\\)")
  expect_match(out, "azul_plot\\(\\)")
  expect_match(out, "\\?\\?azul")
})

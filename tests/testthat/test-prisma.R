test_that("interpret_prisma builds the flow narrative and checks consistency", {
  s <- interpret_prisma(list(identified = 2450, other_sources = 30, duplicates = 610,
    screened = 1870, excluded_screen = 1690, sought = 180, assessed = 180,
    excluded_fulltext = 158, included = 22),
    databases = c("PubMed", "Scopus"),
    reasons = c("wrong outcome" = 70, "wrong design" = 88),
    review = "scoping")
  expect_s3_class(s, "azul_interpretation")
  expect_match(as.character(s), "2480 records were identified")
  expect_match(as.character(s), "610 duplicates")
  expect_match(as.character(s), "22 studies met the inclusion")
  expect_match(as.character(s), "scoping review")
})

test_that("interpret_prisma flags counts that do not reconcile", {
  s <- interpret_prisma(list(assessed = 100, excluded_fulltext = 60, included = 30))
  expect_true(any(grepl("does not equal included", s$notes)))
})

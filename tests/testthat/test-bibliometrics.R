test_that("interpret_bibliometrix works from a metrics list", {
  s <- interpret_bibliometrix(list(documents = 1240, sources = 210,
    timespan = "2010:2023", annual_growth = 12.4, avg_citations_per_doc = 18.6,
    authors = 3150, coauthors_per_doc = 3.8, international_coauthorship = 24.5,
    collaboration_index = 3.2, author_keywords = 2680, keywords_plus = 1950),
    field = "digital health")
  expect_s3_class(s, "azul_interpretation")
  expect_match(as.character(s), "1240 documents")
  expect_match(as.character(s), "12.4% per year")
  expect_match(as.character(s), "international collaboration")
  expect_match(as.character(s), "author keywords")
})

test_that("interpret handles a bibliometrix-style object", {
  obj <- structure(list(Articles = 500, nAuthors = 1400, AuthorsSingle = 60,
    Collaboration = 2.9, References = 18000, nAUperPaper = rep(3.5, 500),
    Sources = stats::setNames(rep(1, 120), paste0("J", 1:120))),
    class = "bibliometrix")
  s <- interpret(obj, field = "TB epidemiology")
  expect_match(as.character(s), "500 documents")
  expect_match(as.character(s), "collaboration index was 2.9")
})

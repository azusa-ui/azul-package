# ---------------------------------------------------------------------------
# Bibliometric analysis interpretation. Works from a bibliometrix
# biblioAnalysis object, or from a named list of the "Main Information" metrics
# (documents, sources, timespan, annual_growth, avg_citations_per_doc,
# references, author_keywords, keywords_plus, authors, authors_single,
# single_authored_docs, coauthors_per_doc, international_coauthorship,
# collaboration_index). Any subset may be supplied.
# Reference: Aria & Cuccurullo (2017), J Informetrics; Donthu et al. (2021),
# J Business Research (bibliometric analysis guidelines).
# ---------------------------------------------------------------------------

# pull the interpretable metrics out of a bibliometrix biblioAnalysis object
.biblio_from_object <- function(object) {
  g <- function(nm) tryCatch(object[[nm]], error = function(e) NULL)
  nAU <- g("nAUperPaper")
  list(
    documents        = g("Articles"),
    authors          = g("nAuthors"),
    authors_single   = g("AuthorsSingle"),
    collaboration_index = g("Collaboration"),
    references       = g("References"),
    coauthors_per_doc = if (!is.null(nAU)) mean(as.numeric(nAU), na.rm = TRUE) else NULL,
    sources          = if (!is.null(g("Sources"))) length(g("Sources")) else NULL)
}

#' Interpret a bibliometric analysis
#'
#' Turns the key figures of a bibliometric analysis into a manuscript-style
#' paragraph. Accepts a \code{bibliometrix} \code{biblioAnalysis} object, or a
#' named list of the "Main Information" metrics (any subset).
#'
#' @param x A \code{bibliometrix} object, or a named list with any of:
#'   \code{documents}, \code{sources}, \code{timespan}, \code{annual_growth}
#'   (percent), \code{avg_citations_per_doc}, \code{references},
#'   \code{author_keywords}, \code{keywords_plus}, \code{authors},
#'   \code{authors_single}, \code{single_authored_docs},
#'   \code{coauthors_per_doc}, \code{international_coauthorship} (percent),
#'   \code{collaboration_index}.
#' @param field Optional research field/topic label for the prose.
#' @param ... Styling args (markup, alpha).
#' @return An \code{azul_interpretation} object.
#' @examples
#' interpret_bibliometrix(list(documents = 1240, sources = 210,
#'   timespan = "2010:2023", annual_growth = 12.4, avg_citations_per_doc = 18.6,
#'   authors = 3150, coauthors_per_doc = 3.8, international_coauthorship = 24.5,
#'   collaboration_index = 3.2, author_keywords = 2680, keywords_plus = 1950),
#'   field = "digital health")
#' @export
interpret_bibliometrix <- function(x, field = "the field", ...) {
  a <- azul_args(...)
  m <- if (inherits(x, "bibliometrix")) .biblio_from_object(x)
       else if (is.list(x)) x
       else stop("x must be a bibliometrix object or a named list of metrics.", call. = FALSE)
  has <- function(nm) !is.null(m[[nm]]) && !is.na(m[[nm]][1])
  n1 <- function(v, dp = 0) fmt_num(v, dp)

  paras <- character(0)
  # corpus scope
  scope <- "A bibliometric analysis was conducted"
  if (has("documents")) scope <- paste0(scope, " on ", n1(m$documents), " documents")
  if (has("sources")) scope <- paste0(scope, " published across ", n1(m$sources), " sources")
  if (has("timespan")) scope <- paste0(scope, " over ", gsub(":", " to ", as.character(m$timespan)))
  scope <- paste0(scope, " in ", field, ".")
  if (has("annual_growth"))
    scope <- paste0(scope, " The annual scientific production grew at ", n1(m$annual_growth, 1),
                    "% per year, indicating ",
                    if (m$annual_growth > 0) "an expanding" else "a contracting", " literature.")
  paras <- c(paras, scope)

  # impact
  if (has("avg_citations_per_doc") || has("references")) {
    imp <- "In terms of impact,"
    if (has("avg_citations_per_doc")) imp <- paste0(imp, " the documents received on average ",
      n1(m$avg_citations_per_doc, 1), " citations each")
    if (has("references")) imp <- paste0(imp, if (has("avg_citations_per_doc")) ", and drew on " else " the corpus drew on ",
      n1(m$references), " cited references")
    paras <- c(paras, paste0(imp, "."))
  }

  # authorship and collaboration
  if (any(vapply(c("authors","coauthors_per_doc","international_coauthorship","collaboration_index",
                   "single_authored_docs","authors_single"), has, logical(1)))) {
    au <- "Authorship and collaboration:"
    if (has("authors")) au <- paste0(au, " ", n1(m$authors), " authors contributed;")
    if (has("coauthors_per_doc")) au <- paste0(au, " documents had on average ",
      n1(m$coauthors_per_doc, 1), " co-authors,")
    if (has("international_coauthorship")) au <- paste0(au, " ", n1(m$international_coauthorship, 1),
      "% involved international collaboration,")
    if (has("collaboration_index")) au <- paste0(au, " and the collaboration index was ",
      n1(m$collaboration_index, 1), ",")
    au <- sub("[,;]$", ".", au)
    if (has("single_authored_docs")) au <- paste0(au, " ", n1(m$single_authored_docs),
      " documents were single-authored.")
    paras <- c(paras, au)
  }

  # keywords
  if (has("author_keywords") || has("keywords_plus")) {
    kw <- "The conceptual structure was described by"
    if (has("author_keywords")) kw <- paste0(kw, " ", n1(m$author_keywords), " author keywords")
    if (has("keywords_plus")) kw <- paste0(kw, if (has("author_keywords")) " and " else " ",
      n1(m$keywords_plus), " Keywords Plus")
    paras <- c(paras, paste0(kw, "."))
  }

  new_interpretation("Bibliometric analysis", paras,
    c("Performance analysis (productivity, citations) and science mapping (co-citation, co-word, collaboration networks) answer different questions; report both where relevant.",
      "Consider the three bibliometric laws: Lotka's law (author productivity), Bradford's law (source concentration) and Zipf's law (word frequency).",
      "State the database (e.g. Scopus, Web of Science), the search query, and the inclusion/exclusion criteria.",
      "Reference: Aria & Cuccurullo (2017); Donthu et al. (2021), bibliometric analysis guidelines."))
}

#' @rdname interpret
#' @export
interpret.bibliometrix <- function(object, ...) interpret_bibliometrix(object, ...)

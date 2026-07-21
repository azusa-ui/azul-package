# ---------------------------------------------------------------------------
# Publication-ready output: azul_table() builds a formatted estimates table
# (data.frame or flextable) in MJMS style; azul_report() writes a one-call
# Word (.docx) or HTML document bundling the interpretation, the table and the
# assumption checks.
# ---------------------------------------------------------------------------

# P value in MJMS table style: 3 dp, "<0.001", no leading "P ="
.p_plain <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  formatC(round(p, 3), format = "f", digits = 3)
}

# infer the effect-measure label from the method string
.measure_label <- function(method) {
  m <- tolower(method %||% "")
  if (grepl("logistic|odds", m)) "OR"
  else if (grepl("cox|hazard", m)) "HR"
  else if (grepl("poisson|log-linear|negative binomial|rate", m)) "IRR"
  else if (grepl("multinomial", m)) "RRR"
  else if (grepl("linear", m)) "b"
  else "Estimate"
}

#' Build a publication-ready estimates table
#'
#' Turns the estimates stored in an \code{azul_interpretation} (or a fitted
#' model) into a tidy, MJMS-style table: one row per term with the point
#' estimate and its 95\% confidence interval, and the P value.
#'
#' @param x An \code{azul_interpretation} or a fitted model.
#' @param measure Optional effect-measure label (e.g. "OR"); inferred if NULL.
#' @param flextable Logical; if TRUE and the 'flextable' package is available,
#'   return a \code{flextable} ready for Word/HTML; otherwise a data frame.
#' @param ... Passed to \code{\link{interpret}} when \code{x} is a model.
#' @return A data frame (class \code{azul_table}) or a \code{flextable}.
#' @examples
#' azul_table(glm(am ~ wt + hp, within(mtcars, am <- factor(am)), family = binomial))
#' @export
azul_table <- function(x, measure = NULL, flextable = FALSE, ...) {
  int <- if (inherits(x, "azul_interpretation")) x else interpret(x, ...)
  est <- int$estimates
  if (is.null(est) || !is.data.frame(est) || !("estimate" %in% names(est)))
    stop("No estimates table is available for this object.", call. = FALSE)
  meas <- measure %||% .measure_label(int$method)
  term <- if ("term" %in% names(est)) est$term else rownames(est)
  ci <- if (all(c("conf.low", "conf.high") %in% names(est)))
    paste0(fmt_num(est$estimate), " (", fmt_num(est$conf.low), ", ", fmt_num(est$conf.high), ")")
  else fmt_num(est$estimate)
  pv <- if ("p.value" %in% names(est)) vapply(est$p.value, .p_plain, character(1)) else NA
  df <- data.frame(Variable = term, X = ci, `P-value` = pv,
                   check.names = FALSE, stringsAsFactors = FALSE)
  names(df)[2] <- paste0(meas, " (95% CI)")
  attr(df, "method") <- int$method
  attr(df, "measure") <- meas
  class(df) <- c("azul_table", "data.frame")
  if (flextable && requireNamespace("flextable", quietly = TRUE)) {
    ft <- flextable::flextable(as.data.frame(df))
    ft <- flextable::add_footer_lines(ft,
      paste0(meas, ", ", switch(meas, OR = "odds ratio", HR = "hazard ratio",
             IRR = "incidence rate ratio", RRR = "relative risk ratio",
             b = "regression coefficient", "estimate"),
             "; CI, confidence interval. ", int$method, "."))
    ft <- flextable::autofit(ft)
    return(ft)
  }
  df
}

#' Write a one-call report (Word or HTML)
#'
#' Produces a document containing the interpretation, a publication-ready
#' estimates table and, optionally, the assumption checks.
#'
#' @param x An \code{azul_interpretation} or a fitted model.
#' @param file Output path; \code{.docx} (needs 'officer' + 'flextable') or
#'   \code{.html} (no extra dependencies).
#' @param assumptions Logical; append \code{\link{check_assumptions}} output
#'   when the object is a supported model.
#' @param ... Passed to \code{\link{interpret}} when \code{x} is a model.
#' @return The output file path, invisibly.
#' @examples
#' \dontrun{
#' azul_report(lm(mpg ~ wt + hp, mtcars), "report.docx")
#' }
#' @export
azul_report <- function(x, file = "azul_report.docx", assumptions = TRUE, ...) {
  is_model <- !inherits(x, "azul_interpretation")
  int <- if (is_model) interpret(x, ...) else x
  tbl <- tryCatch(azul_table(int), error = function(e) NULL)
  chk <- if (assumptions && is_model)
    tryCatch(check_assumptions(x), error = function(e) NULL) else NULL
  ext <- tolower(tools::file_ext(file))

  if (ext == "docx" && requireNamespace("officer", quietly = TRUE) &&
      requireNamespace("flextable", quietly = TRUE)) {
    doc <- officer::read_docx()
    doc <- officer::body_add_par(doc, int$method, style = "heading 1")
    doc <- officer::body_add_par(doc, "Interpretation", style = "heading 2")
    for (p in .split_sentences(int$paragraph))
      doc <- officer::body_add_par(doc, p, style = "Normal")
    if (!is.null(tbl)) {
      doc <- officer::body_add_par(doc, "Results table", style = "heading 2")
      doc <- flextable::body_add_flextable(doc, azul_table(int, flextable = TRUE))
    }
    if (length(int$assumptions)) {
      doc <- officer::body_add_par(doc, "Assumptions to check", style = "heading 2")
      for (aln in int$assumptions) doc <- officer::body_add_par(doc, paste0("• ", aln))
    }
    if (!is.null(chk)) {
      doc <- officer::body_add_par(doc, "Assumption checks", style = "heading 2")
      for (p in chk$paragraph) doc <- officer::body_add_par(doc, p)
    }
    print(doc, target = file)
    return(invisible(file))
  }

  # ---- HTML fallback (no dependencies) -----------------------------------
  if (ext == "docx") file <- sub("\\.docx$", ".html", file)
  esc <- function(s) gsub("<", "&lt;", gsub("&", "&amp;", s))
  h <- c("<!doctype html><html><head><meta charset='utf-8'><title>azul report</title>",
    "<style>body{font-family:Georgia,serif;max-width:820px;margin:40px auto;line-height:1.5;color:#222}",
    "h1{font-size:1.4em}h2{font-size:1.1em;border-bottom:1px solid #ddd;padding-bottom:3px}",
    "table{border-collapse:collapse;margin:12px 0}th,td{border:1px solid #bbb;padding:6px 10px;text-align:left}",
    "th{background:#f2f2f2}.note{color:#666;font-size:.9em}</style></head><body>",
    paste0("<h1>", esc(int$method), "</h1>"), "<h2>Interpretation</h2>")
  for (p in .split_sentences(int$paragraph)) h <- c(h, paste0("<p>", esc(p), "</p>"))
  if (!is.null(tbl)) {
    h <- c(h, "<h2>Results table</h2>", "<table><tr>",
      paste0(vapply(names(tbl), function(nm) paste0("<th>", esc(nm), "</th>"), character(1)), collapse = ""), "</tr>")
    for (i in seq_len(nrow(tbl))) h <- c(h, paste0("<tr>",
      paste0(vapply(tbl[i, ], function(v) paste0("<td>", esc(as.character(v)), "</td>"), character(1)), collapse = ""), "</tr>"))
    h <- c(h, "</table>")
  }
  if (length(int$assumptions)) h <- c(h, "<h2>Assumptions to check</h2><ul>",
    paste0("<li>", esc(int$assumptions), "</li>"), "</ul>")
  if (!is.null(chk)) h <- c(h, "<h2>Assumption checks</h2>",
    paste0("<p>", esc(chk$paragraph), "</p>"))
  h <- c(h, "<p class='note'>Generated by the azul R package.</p></body></html>")
  writeLines(h, file)
  invisible(file)
}

#' @export
print.azul_table <- function(x, ...) {
  cat("azul results table (", attr(x, "measure"), "):\n", sep = "")
  print.data.frame(x)
  invisible(x)
}

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
#' @param digits Decimal places for the estimate and CI (default 2; fixed, never scientific).
#' @param flextable Logical; if TRUE and the 'flextable' package is available,
#'   return a \code{flextable} ready for Word/HTML; otherwise a data frame.
#' @param ... Passed to \code{\link{interpret}} when \code{x} is a model.
#' @return A data frame (class \code{azul_table}) or a \code{flextable}.
#' @examples
#' azul_table(glm(am ~ wt + hp, within(mtcars, am <- factor(am)), family = binomial))
#' @export
azul_table <- function(x, measure = NULL, flextable = FALSE, digits = 2, ...) {
  int <- if (inherits(x, "azul_interpretation")) x else interpret(x, ...)
  est <- int$estimates
  if (is.null(est) || !is.data.frame(est) || !("estimate" %in% names(est)))
    stop("No estimates table is available for this object.", call. = FALSE)
  meas <- measure %||% .measure_label(int$method)
  term <- if ("term" %in% names(est)) est$term else rownames(est)
  # fixed-decimal formatting only (never scientific notation) for the table
  ff <- function(v) formatC(round(as.numeric(v), digits), format = "f", digits = digits)
  ci <- if (all(c("conf.low", "conf.high") %in% names(est)))
    paste0(ff(est$estimate), " (", ff(est$conf.low), ", ", ff(est$conf.high), ")")
  else ff(est$estimate)
  pv <- if ("p.value" %in% names(est)) vapply(est$p.value, .p_plain, character(1)) else NA
  df <- data.frame(Variable = term, X = ci, `P-value` = pv,
                   check.names = FALSE, stringsAsFactors = FALSE)
  names(df)[2] <- paste0(meas, " (95% CI)")
  attr(df, "method") <- int$method
  attr(df, "measure") <- meas
  attr(df, "adjust") <- if (length(term) > 1) term else character(0)
  class(df) <- c("azul_table", "data.frame")
  if (flextable && requireNamespace("flextable", quietly = TRUE)) return(.azul_flextable(df, int$method, meas))
  df
}

# APA three-line table: bold predictor names, adjustment footnote, only the
# top / header / bottom rules.
.azul_flextable <- function(df, method, meas) {
  ft <- flextable::flextable(as.data.frame(df))
  meas_long <- switch(meas, OR = "odds ratio", HR = "hazard ratio",
    IRR = "incidence rate ratio", RRR = "relative risk ratio",
    b = "regression coefficient", "estimate")
  adj <- attr(df, "adjust")
  foot <- paste0(meas, ", ", meas_long, "; CI, confidence interval. ", method, ".")
  if (length(adj) > 1)
    foot <- c(foot, paste0("All estimates are mutually adjusted for the other predictors in the model (",
                           paste(adj, collapse = ", "), ")."))
  ft <- flextable::add_footer_lines(ft, values = foot)
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bold(ft, j = 1, part = "body")          # bold predictor names
  ft <- flextable::align(ft, j = 1, align = "left", part = "all")
  ft <- flextable::align(ft, j = seq(2, ncol(df)), align = "center", part = "all")
  if (requireNamespace("officer", quietly = TRUE)) {
    bd <- officer::fp_border(color = "black", width = 1)
    ft <- flextable::border_remove(ft)
    ft <- flextable::hline_top(ft, part = "header", border = bd)     # top rule
    ft <- flextable::hline_bottom(ft, part = "header", border = bd)  # under header
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)    # bottom rule
  }
  ft <- flextable::fontsize(ft, size = 9, part = "footer")
  flextable::autofit(ft)
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
azul_report <- function(x, file = "azul_report.docx", assumptions = TRUE,
                        figure = TRUE, digits = 2, ...) {
  is_model <- !inherits(x, "azul_interpretation")
  int <- if (is_model) interpret(x, ...) else x
  tbl <- tryCatch(azul_table(int, digits = digits), error = function(e) NULL)
  chk <- if (assumptions && is_model)
    tryCatch(check_assumptions(x), error = function(e) NULL) else NULL
  fig <- if (figure && is_model) .azul_render_png(x) else NULL
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
    if (!is.null(fig)) {
      doc <- officer::body_add_par(doc, "Figure", style = "heading 2")
      doc <- tryCatch(officer::body_add_img(doc, src = fig, width = 6, height = 3.75),
                      error = function(e) doc)
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
    # APA three-line table: rules only at top, under header, and bottom
    "table{border-collapse:collapse;margin:12px 0;border-top:2px solid #000;border-bottom:2px solid #000}",
    "th,td{padding:6px 12px;text-align:center;border:none}",
    "th{border-bottom:1px solid #000}td:first-child,th:first-child{text-align:left;font-weight:bold}",
    ".note{color:#555;font-size:.85em;margin-top:4px}</style></head><body>",
    paste0("<h1>", esc(int$method), "</h1>"), "<h2>Interpretation</h2>")
  for (p in .split_sentences(int$paragraph)) h <- c(h, paste0("<p>", esc(p), "</p>"))
  if (!is.null(tbl)) {
    h <- c(h, "<h2>Results table</h2>", "<table><tr>",
      paste0(vapply(names(tbl), function(nm) paste0("<th>", esc(nm), "</th>"), character(1)), collapse = ""), "</tr>")
    for (i in seq_len(nrow(tbl))) h <- c(h, paste0("<tr>",
      paste0(vapply(tbl[i, ], function(v) paste0("<td>", esc(as.character(v)), "</td>"), character(1)), collapse = ""), "</tr>"))
    h <- c(h, "</table>")
    adj <- attr(tbl, "adjust")
    if (length(adj) > 1)
      h <- c(h, paste0("<p class='note'>All estimates are mutually adjusted for the other predictors in the model (",
                       esc(paste(adj, collapse = ", ")), ").</p>"))
  }
  if (!is.null(fig)) {
    uri <- tryCatch(knitr::image_uri(fig), error = function(e) NULL)
    if (!is.null(uri)) h <- c(h, "<h2>Figure</h2>",
      paste0("<img src='", uri, "' style='max-width:100%;height:auto'/>"))
  }
  if (length(int$assumptions)) h <- c(h, "<h2>Assumptions to check</h2><ul>",
    paste0("<li>", esc(int$assumptions), "</li>"), "</ul>")
  if (!is.null(chk)) h <- c(h, "<h2>Assumption checks</h2>",
    paste0("<p>", esc(chk$paragraph), "</p>"))
  h <- c(h, "<p class='note'>Generated by the azul R package.</p></body></html>")
  writeLines(h, file)
  invisible(file)
}

# render the model's azul_plot() figure to a temporary PNG (NULL on failure)
.azul_render_png <- function(x) {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = 1600, height = 1050, res = 200)
  ok <- tryCatch({ azul_plot(x); TRUE }, error = function(e) FALSE)
  grDevices::dev.off()
  if (isTRUE(ok) && file.exists(tmp)) tmp else NULL
}

#' @export
print.azul_table <- function(x, ...) {
  cat("azul results table (", attr(x, "measure"), "):\n", sep = "")
  print.data.frame(x)
  invisible(x)
}

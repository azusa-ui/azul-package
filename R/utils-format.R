# ---------------------------------------------------------------------------
# Formatting helpers implementing MJMS (Arifin et al. 2016) and SAMPL rules.
# These are internal but exported so power users can reuse them.
# ---------------------------------------------------------------------------

#' Format a number to a fixed number of decimals
#'
#' @param x Numeric vector.
#' @param dp Decimal places (default 2).
#' @return Character vector.
#' @export
fmt_num <- function(x, dp = 2) {
  x <- as.numeric(x)
  out <- formatC(round(x, dp), format = "f", digits = dp)
  # keep 2 significant figures for very small non-zero values that would
  # otherwise collapse to "0.00" (e.g. a tiny odds ratio)
  small <- is.finite(x) & x != 0 & abs(round(x, dp)) < 10^(-dp)
  if (any(small)) out[small] <- formatC(signif(x[small], 2), format = "g")
  out
}

#' Format a P value in MJMS style
#'
#' Three decimal places; the smallest reported value is \code{P < 0.001}.
#' The letter P is capitalised, per MJMS. Set \code{markup = "markdown"} to
#' italicise it.
#'
#' @param p Numeric P value.
#' @param markup One of "none" or "markdown".
#' @param label The letter to use (default "P").
#' @return A character string such as "P = 0.032" or "P < 0.001".
#' @export
fmt_p <- function(p, markup = c("none", "markdown"), label = "P") {
  markup <- match.arg(markup)
  lab <- if (markup == "markdown") paste0("*", label, "*") else label
  if (is.na(p)) return(paste0(lab, " = NA"))
  if (p < 0.001) return(paste0(lab, " < 0.001"))
  paste0(lab, " = ", formatC(round(p, 3), format = "f", digits = 3))
}

#' Format an estimate with its 95 percent confidence interval (MJMS style)
#'
#' Produces \code{estimate (95\% CI: lower, upper)}.
#'
#' @param est Point estimate.
#' @param lower,upper Confidence limits.
#' @param dp Decimal places.
#' @param label Optional label placed before the estimate (e.g. "Adjusted OR").
#' @param ci_sep Separator inside the CI ("," default, or "to").
#' @return Character string.
#' @export
fmt_ci <- function(est, lower, upper, dp = 2, label = NULL, ci_sep = ",") {
  sep <- if (identical(ci_sep, "to")) " to " else ", "
  core <- paste0(fmt_num(est, dp), " (95% CI: ",
                 fmt_num(lower, dp), sep, fmt_num(upper, dp), ")")
  if (is.null(label)) core else paste0(label, " ", core)
}

#' Verbal strength of a correlation coefficient
#'
#' Uses common epidemiology thresholds on the absolute value: < 0.3 weak,
#' 0.3 to < 0.5 moderate, 0.5 to < 0.7 moderately strong, >= 0.7 strong.
#'
#' @param r Correlation coefficient.
#' @return Character string.
#' @export
cor_strength <- function(r) {
  a <- abs(r)
  if (is.na(a)) return("undetermined")
  if (a < 0.3) "weak"
  else if (a < 0.5) "moderate"
  else if (a < 0.7) "moderately strong"
  else "strong"
}

#' Direction word for a signed quantity
#' @param x Numeric.
#' @param pos,neg Words for positive and negative.
#' @return Character string.
#' @keywords internal
direction <- function(x, pos = "positive", neg = "negative") {
  if (is.na(x)) return("undetermined") else if (x >= 0) pos else neg
}

#' Significance clause given a P value and alpha
#' @param p P value.
#' @param alpha Significance level (default 0.05).
#' @return "statistically significant" or "not statistically significant".
#' @keywords internal
sig_phrase <- function(p, alpha = 0.05) {
  if (is.na(p)) return("of undetermined statistical significance")
  if (p < alpha) "statistically significant" else "not statistically significant"
}

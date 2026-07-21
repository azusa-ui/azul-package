# ---------------------------------------------------------------------------
# Shared helpers for pulling tidy coefficient tables from model objects,
# without hard-depending on broom (used if available, base fallback otherwise).
# ---------------------------------------------------------------------------

#' Tidy a model's coefficients with 95 percent CIs
#'
#' @param model A fitted model.
#' @param exponentiate Logical; exponentiate estimates and CIs (for OR/HR/IRR/RRR).
#' @param conf.level Confidence level.
#' @return data.frame with columns term, estimate, conf.low, conf.high, p.value.
#' @keywords internal
coef_table <- function(model, exponentiate = FALSE, conf.level = 0.95) {
  if (requireNamespace("broom", quietly = TRUE)) {
    tt <- tryCatch(
      broom::tidy(model, conf.int = TRUE, conf.level = conf.level,
                  exponentiate = exponentiate),
      error = function(e) NULL)
    if (!is.null(tt) && all(c("term", "estimate") %in% names(tt))) {
      tt <- as.data.frame(tt)
      if (!"conf.low"  %in% names(tt)) tt$conf.low  <- NA_real_
      if (!"conf.high" %in% names(tt)) tt$conf.high <- NA_real_
      if (!"p.value"   %in% names(tt)) tt$p.value   <- NA_real_
      return(tt[, c("term", "estimate", "conf.low", "conf.high", "p.value")])
    }
  }
  # base fallback -----------------------------------------------------------
  sm <- tryCatch(stats::coef(summary(model)), error = function(e) NULL)
  est <- stats::coef(model)
  ci  <- tryCatch(suppressMessages(stats::confint(model, level = conf.level)),
                  error = function(e) NULL)
  terms <- names(est)
  pcol <- if (!is.null(sm) && ncol(sm) >= 4) sm[, 4] else rep(NA_real_, length(est))
  out <- data.frame(
    term = terms,
    estimate = as.numeric(est),
    conf.low = if (!is.null(ci)) as.numeric(ci[terms, 1]) else NA_real_,
    conf.high = if (!is.null(ci)) as.numeric(ci[terms, 2]) else NA_real_,
    p.value = as.numeric(pcol[terms]),
    stringsAsFactors = FALSE)
  if (exponentiate) {
    out$estimate  <- exp(out$estimate)
    out$conf.low  <- exp(out$conf.low)
    out$conf.high <- exp(out$conf.high)
  }
  rownames(out) <- NULL
  out
}

#' Drop the intercept row from a coefficient table
#' @keywords internal
drop_intercept <- function(tab) {
  tab[!grepl("Intercept", tab$term, ignore.case = TRUE), , drop = FALSE]
}

#' Human label for a term ("male sex", "each one-unit increase in age", ...)
#' Kept simple: returns the raw term; wording is added by the caller.
#' @keywords internal
term_label <- function(term) term

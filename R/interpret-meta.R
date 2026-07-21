# ---------------------------------------------------------------------------
# Meta-analysis: metafor (rma) and meta (metabin/metacont/metagen/...).
# Reports the pooled effect with 95% CI, heterogeneity (I-squared, tau-squared,
# Cochran's Q), and the number of studies.
# Reference: Borenstein, Hedges, Higgins & Rothstein, Introduction to
# Meta-Analysis (Wiley); Higgins & Thompson (2002); Cochrane Handbook.
# ---------------------------------------------------------------------------

.i2_verdict <- function(i2) {  # i2 as percentage
  if (is.na(i2)) return("undetermined")
  if (i2 < 25) "low" else if (i2 < 50) "moderate" else if (i2 < 75) "substantial" else "considerable"
}

.meta_ratio <- function(sm) sm %in% c("OR","RR","IRR","HR","PETO","ratio")

.meta_body <- function(k, est, lo, hi, p, sm, ratio, i2, tau2, Q, Qp, method, a) {
  if (ratio) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
  eff <- if (ratio) paste0("pooled ", sm, " of ", fmt_num(est), " (95% CI: ", fmt_num(lo), ", ", fmt_num(hi), ")")
         else paste0("pooled ", sm, " (effect size) of ", fmt_ci(est, lo, hi))
  pooled <- paste0("A ", if (grepl("random|REML|DL|EB|SJ", method, ignore.case = TRUE)) "random-effects" else "fixed-effect",
    " meta-analysis of ", k, " studies gave a ", eff, " (", fmt_p(p, a$markup), "), a ",
    sig_phrase(p, a$alpha), " overall effect.")
  het <- paste0("Between-study heterogeneity was ", .i2_verdict(i2), " (I-squared = ", fmt_num(i2, 1), "%",
    if (!is.na(tau2)) paste0(", tau-squared = ", fmt_num(tau2, 3)) else "",
    if (!is.na(Q)) paste0("; Cochran's Q = ", fmt_num(Q), ", ", fmt_p(Qp, a$markup)) else "", ").")
  c(pooled, het)
}

#' @rdname interpret
#' @export
interpret.rma <- function(object, ...) {
  a <- azul_args(...)
  sm <- object$measure
  paras <- .meta_body(object$k, object$beta[1], object$ci.lb, object$ci.ub, object$pval,
    sm, .meta_ratio(sm), object$I2, object$tau2, object$QE, object$QEp, object$method, a)
  new_interpretation("Meta-analysis (metafor)", paras,
    c("State the effect measure and model (fixed vs random effects) and the tau-squared estimator.",
      "Assess publication/small-study bias (funnel plot, Egger's test) when there are enough studies (>= 10).",
      "With few studies the heterogeneity variance (tau-squared) is imprecise; interpret I-squared cautiously.",
      "Reference: Borenstein et al., Introduction to Meta-Analysis; Cochrane Handbook."))
}

#' @rdname interpret
#' @export
interpret.meta <- function(object, ...) {
  a <- azul_args(...)
  sm <- object$sm %||% "effect"
  i2 <- object$I2; if (!is.null(i2) && !is.na(i2) && i2 <= 1) i2 <- i2 * 100
  # prefer random-effects summary
  te <- object$TE.random %||% object$TE.fixed
  lo <- object$lower.random %||% object$lower.fixed
  hi <- object$upper.random %||% object$upper.fixed
  p  <- object$pval.random %||% object$pval.fixed
  method <- if (!is.null(object$TE.random)) "random" else "fixed"
  paras <- .meta_body(object$k, te, lo, hi, p, sm, .meta_ratio(sm), i2,
                      object$tau2 %||% NA_real_, object$Q %||% NA_real_, object$pval.Q %||% NA_real_, method, a)
  new_interpretation("Meta-analysis (meta)", paras,
    c("State the effect measure and whether the fixed- or random-effects estimate is reported.",
      "Assess publication/small-study bias (funnel plot, Egger's test) with >= 10 studies.",
      "Consider subgroup or sensitivity analyses to explain substantial heterogeneity.",
      "Reference: Borenstein et al., Introduction to Meta-Analysis; Cochrane Handbook."))
}

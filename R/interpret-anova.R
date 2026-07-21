# ---------------------------------------------------------------------------
# anova / data.frame tables. Special-cases the ordinal assumption tests from
# the 'ordinal' package (nominal_test / scale_test), which return an anova
# table with an LRT column and a "<none>" reference row. Falls back to a
# generic likelihood-ratio / ANOVA-table interpretation otherwise.
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @param test For an ordinal assumption table, either "nominal" (proportional
#'   odds, the default) or "scale" (equal scale / homogeneity of variances).
#' @export
interpret.anova <- function(object, test = c("nominal", "scale"), ...) {
  a <- azul_args(...)
  df <- as.data.frame(object)
  cn <- colnames(df)
  pcol <- grep("^Pr\\(", cn, value = TRUE)

  is_ord_assump <- "LRT" %in% cn && length(pcol) && "<none>" %in% rownames(df)

  if (is_ord_assump) {
    test <- match.arg(test)
    terms <- setdiff(rownames(df), "<none>")
    p <- df[terms, pcol[1]]
    viol <- terms[!is.na(p) & p < a$alpha]
    assumption <- if (identical(test, "scale"))
      "equal scale (homogeneity of variance) assumption"
    else "proportional odds (nominal effects) assumption"

    per <- vapply(seq_along(terms), function(i) {
      paste0("for '", terms[i], "', ", fmt_p(p[i], a$markup), " (",
             if (!is.na(p[i]) && p[i] < a$alpha) "assumption violated" else "assumption not violated", ")")
    }, character(1))

    overall <- if (length(viol) == 0)
      paste0("The ", assumption, " was supported for all predictors (all P >= ",
             a$alpha, "), so the proportional odds model is appropriate.")
    else
      paste0("The ", assumption, " was violated for ",
             paste(sprintf("'%s'", viol), collapse = ", "), " (P < ", a$alpha,
             "); consider relaxing proportional odds for ",
             if (length(viol) == 1) "that predictor" else "those predictors",
             " (a partial proportional odds term via nominal = ~ ., or a multinomial model).")

    body <- paste0("A likelihood-ratio test assessed the ", assumption,
                   " in the ordinal (cumulative link) model. Testing each predictor in turn: ",
                   paste(per, collapse = "; "), ". ", overall)
    return(new_interpretation(
      paste0("Assumption test: ", assumption), body,
      notes = "nominal_test() and scale_test() share this table structure; if this table came from scale_test(), pass test = 'scale' for the correct wording.",
      estimates = df))
  }

  # ---- model-vs-model comparison (anova(m1, m2, test = ...)) --------------
  if (any(grepl("Resid", cn)) && nrow(df) >= 2 && length(pcol)) {
    last <- nrow(df)
    pv <- df[[pcol[1]]][last]
    statcol <- intersect(c("F", "Deviance", "Chisq", "LRT"), cn)
    stat <- if (length(statcol)) df[[statcol[1]]][last] else NA_real_
    improved <- !is.na(pv) && pv < a$alpha
    body <- paste0("Two nested models were compared. The larger model ",
      if (improved) "significantly improved the fit over the reduced model"
      else "did not significantly improve the fit over the reduced model",
      if (length(statcol) && !is.na(stat)) paste0(" (", statcol[1], " = ", fmt_num(stat), ", ") else " (",
      fmt_p(pv, a$markup), "). ",
      if (improved) "The additional term(s) are therefore retained."
      else "The additional term(s) do not add explanatory value and may be dropped for parsimony.")
    return(new_interpretation("Model comparison (nested models)", body,
      c("Nested-model comparison: use an F-test for (quasi-)Poisson/Gaussian with overdispersion, or a likelihood-ratio test otherwise.",
        "The models must be nested and fitted to the same observations."),
      estimates = df))
  }

  # ---- generic anova / LRT table -----------------------------------------
  if (!length(pcol)) {
    return(new_interpretation("ANOVA table",
      "An analysis-of-variance / deviance table was supplied but no P-value column was found; report the sums of squares or deviances directly.",
      estimates = df))
  }
  rows <- rownames(df); keep <- !grepl("Residual|<none>|NULL", rows, ignore.case = TRUE)
  statcol <- intersect(c("F", "LRT", "Chisq", "Deviance", "F value"), cn)
  sentences <- vapply(which(keep), function(i) {
    pv <- df[i, pcol[1]]
    stat <- if (length(statcol)) paste0(" (", statcol[1], " = ", fmt_num(df[i, statcol[1]]), ")") else ""
    paste0("the term '", rows[i], "' was ", sig_phrase(pv, a$alpha), stat, ", ", fmt_p(pv, a$markup))
  }, character(1))
  body <- paste0("Based on the analysis-of-variance / likelihood-ratio table, ",
                 paste(sentences, collapse = "; "), ".")
  new_interpretation("ANOVA / likelihood-ratio table", body,
    notes = "Generic interpretation of an anova table; confirm whether the comparison is sequential (Type I) or a model-vs-model likelihood-ratio test.",
    estimates = df)
}

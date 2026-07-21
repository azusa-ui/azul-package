# ---------------------------------------------------------------------------
# List-shaped results from epiDisplay and similar helpers that do not carry a
# dedicated S3 class. interpret.list inspects the contents and dispatches to a
# tailored interpreter, or falls back to the standard "unsupported" message.
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @export
interpret.list <- function(object, ...) {
  a <- azul_args(...)
  nm <- names(object)

  # psych::cortest.bartlett() -> list(chisq, p.value, df) with NO 'results':
  # Bartlett's test of sphericity (factorability of the correlation matrix).
  if (setequal(nm, c("chisq", "p.value", "df")) && !("results" %in% nm)) {
    x2 <- as.numeric(object$chisq); df <- as.numeric(object$df); p <- as.numeric(object$p.value)
    ok <- !is.na(p) && p < a$alpha
    body <- paste0("Bartlett's test of sphericity examined whether the correlation matrix departs from an identity matrix. ",
      "The result was ", if (ok) "statistically significant" else "not statistically significant",
      " (chi-squared = ", fmt_num(x2), ", df = ", fmt_num(df, 0), ", ", fmt_p(p, a$markup), "). ",
      if (ok) "The significant result indicates sufficient correlations among items, so factor analysis is appropriate."
      else "The non-significant result suggests the items are largely uncorrelated, so factor analysis may not be warranted.")
    return(new_interpretation("Bartlett's test of sphericity", body,
      c("Pair with the KMO measure of sampling adequacy (>= 0.60).",
        "Bartlett's test is sensitive to sample size and tends to be significant with large N.")))
  }

  # epiDisplay::poisgof() -> list(results, chisq, df, p.value):
  # goodness-of-fit test for the Poisson assumption (residual deviance).
  if (all(c("chisq", "df", "p.value") %in% nm)) {
    x2 <- as.numeric(object$chisq); df <- as.numeric(object$df); p <- as.numeric(object$p.value)
    good <- is.na(p) || p >= a$alpha
    verdict <- if (good)
      "The non-significant P value provides no evidence of lack of fit, so the Poisson assumption appears reasonable."
    else
      "The significant P value indicates lack of fit, most commonly due to overdispersion (variance exceeding the mean); a negative binomial or quasi-Poisson model should be considered."
    body <- paste0(
      "A goodness-of-fit test for the Poisson assumption (based on the residual deviance) was performed. ",
      "The model showed ", if (good) "adequate" else "poor", " fit (chi-squared = ", fmt_num(x2),
      ", df = ", fmt_num(df, 0), ", ", fmt_p(p, a$markup), "). ", verdict)
    return(new_interpretation("Poisson goodness-of-fit test", body,
      c("The deviance goodness-of-fit test compares the residual deviance with a chi-squared distribution; it is unreliable with sparse counts or many covariate patterns.",
        "A significant result usually signals overdispersion rather than a specific structural mis-specification.",
        "Confirm overdispersion directly with the ratio of residual deviance to its degrees of freedom (values well above 1 suggest overdispersion).")))
  }

  interpret.default(object, ...)
}

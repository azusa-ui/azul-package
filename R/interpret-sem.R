# ---------------------------------------------------------------------------
# Structural equation models / CFA (lavaan). Reports global fit indices and
# standardised loadings/paths against conventional thresholds.
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @export
interpret.lavaan <- function(object, ...) {
  a <- azul_args(...)
  fm <- tryCatch(lavaan::fitMeasures(object,
        c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower",
          "rmsea.ci.upper","srmr")), error = function(e) NULL)
  if (is.null(fm))
    return(new_interpretation("Structural equation model (lavaan)",
      "A lavaan model was fitted; extract fit with lavaan::fitMeasures() and standardised estimates with standardizedSolution().",
      c("Report chi-square, CFI/TLI, RMSEA [90% CI] and SRMR.")))
  cfi <- fm[["cfi"]]; tli <- fm[["tli"]]; rmsea <- fm[["rmsea"]]; srmr <- fm[["srmr"]]
  verdict <- function(ok) if (isTRUE(ok)) "acceptable" else "below the conventional threshold"
  fit_txt <- paste0(
    "The model chi-square was ", fmt_num(fm[["chisq"]]), " (df = ", fmt_num(fm[["df"]], 0),
    ", ", fmt_p(fm[["pvalue"]], a$markup), "). ",
    "Approximate fit indices were CFI = ", fmt_num(cfi, 3), " (", verdict(cfi >= 0.90),
    "), TLI = ", fmt_num(tli, 3), " (", verdict(tli >= 0.90),
    "), RMSEA = ", fmt_num(rmsea, 3), " (90% CI: ", fmt_num(fm[["rmsea.ci.lower"]], 3), ", ",
    fmt_num(fm[["rmsea.ci.upper"]], 3), "; ", verdict(rmsea < 0.08),
    "), and SRMR = ", fmt_num(srmr, 3), " (", verdict(srmr < 0.08), ").")

  loads <- tryCatch(lavaan::standardizedSolution(object), error = function(e) NULL)
  load_txt <- ""
  if (!is.null(loads)) {
    ff <- loads[loads$op == "=~", , drop = FALSE]
    if (nrow(ff)) {
      weak <- ff[abs(ff$est.std) < 0.40, , drop = FALSE]
      load_txt <- paste0(" Standardised factor loadings ranged from ", fmt_num(min(ff$est.std), 2),
        " to ", fmt_num(max(ff$est.std), 2), "; loadings at or above 0.40 are conventionally retained.",
        if (nrow(weak)) paste0(" ", nrow(weak), " indicator(s) loaded below 0.40 and may warrant review.") else "")
    }
  }
  body <- paste0("A structural equation / confirmatory factor model was estimated with lavaan. ",
                 fit_txt, load_txt,
                 " Fit thresholds follow convention (good: CFI/TLI >= 0.95, RMSEA < 0.06, SRMR < 0.08; acceptable: CFI/TLI >= 0.90, RMSEA < 0.08).")
  assum <- c("Adequate sample size (rules of thumb: >= 10 cases per free parameter, or N >= 200).",
             "Multivariate normality (or use a robust estimator such as MLR).",
             "Correctly specified measurement and structural model; consider modification indices cautiously and theory-driven.")
  new_interpretation("Structural equation / factor model (lavaan)", body, assum)
}

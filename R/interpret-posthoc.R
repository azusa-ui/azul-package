# ---------------------------------------------------------------------------
# Post-hoc pairwise comparisons: base TukeyHSD and emmeans contrast grids.
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @export
interpret.TukeyHSD <- function(object, ...) {
  a <- azul_args(...)
  blocks <- character(0)
  for (fac in names(object)) {
    mtx <- object[[fac]]
    cmp <- rownames(mtx)
    sig <- mtx[, "p adj"] < a$alpha
    lead <- paste0("Tukey HSD post-hoc comparisons for '", fac,
                   "' (family-wise 95% confidence, adjusted P):")
    lines <- vapply(seq_len(nrow(mtx)), function(i) {
      paste0("The ", cmp[i], " comparison showed a mean difference of ",
             fmt_num(mtx[i, "diff"]), " (95% CI: ", fmt_num(mtx[i, "lwr"]), ", ",
             fmt_num(mtx[i, "upr"]), "; ", fmt_p(mtx[i, "p adj"], a$markup), "), ",
             if (sig[i]) "a statistically significant difference." else "not statistically significant.")
    }, character(1))
    summ <- if (any(sig))
      paste0("After adjustment, ", sum(sig), " of ", nrow(mtx),
             " pairwise comparisons remained statistically significant.")
    else "No pairwise comparison remained statistically significant after adjustment."
    blocks <- c(blocks, lead, lines, summ)
  }
  new_interpretation("Tukey HSD post-hoc comparisons", blocks,
    c("The confidence intervals and P values are already adjusted for multiple comparisons (family-wise).",
      "Use only after a significant omnibus ANOVA; report the mean difference and 95% CI for each pair."))
}

#' @rdname interpret
#' @export
interpret.emmGrid <- function(object, ...) {
  a <- azul_args(...)
  sm <- tryCatch(as.data.frame(summary(object, infer = c(TRUE, TRUE))),
                 error = function(e) as.data.frame(summary(object)))
  nm <- names(sm)
  keycol <- intersect(c("contrast"), nm)
  ratio <- any(c("odds.ratio", "ratio", "risk.ratio", "hazard.ratio") %in% nm)
  by_cols <- setdiff(intersect(nm, nm), c("contrast","estimate","odds.ratio","ratio","risk.ratio",
                     "hazard.ratio","SE","df","z.ratio","t.ratio","lower.CL","upper.CL",
                     "asymp.LCL","asymp.UCL","p.value","null"))
  lines <- vapply(seq_len(nrow(sm)), function(i) {
    r <- .emm_row(sm, i)
    lab <- if (length(keycol)) as.character(sm[[keycol]][1L * i]) else paste("comparison", i)
    strat <- if (length(by_cols)) paste0(" (at ",
      paste(vapply(by_cols, function(b) paste0(b, " = ", sm[[b]][i]), character(1)), collapse = ", "), ")") else ""
    meas <- if (ratio) paste0("ratio ", fmt_num(r$est), " (95% CI: ", fmt_num(r$lo), ", ", fmt_num(r$hi), ")")
            else paste0("difference ", fmt_ci(r$est, r$lo, r$hi))
    paste0("The ", lab, " contrast", strat, " gave a ", meas, " (", fmt_p(r$p, a$markup), "), ",
           sig_phrase(r$p, a$alpha), ".")
  }, character(1))
  new_interpretation("Estimated marginal means / contrasts (emmeans)", lines,
    c("State the P-value adjustment used (e.g. Tukey, Bonferroni, none).",
      if (ratio) "Ratios are on the response scale (odds/rate/hazard ratios)." else "Differences are on the response scale.",
      "Report each contrast with its 95% CI."))
}

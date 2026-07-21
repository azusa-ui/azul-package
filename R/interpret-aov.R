# ---------------------------------------------------------------------------
# ANOVA / ANCOVA (aov) and MANOVA / MANCOVA (manova).
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @export
interpret.aov <- function(object, ...) {
  a <- azul_args(...); outcome <- a$outcome %||% all.vars(stats::formula(object))[1]
  st <- summary(object)[[1]]
  rows <- rownames(st); rows <- trimws(rows)
  keep <- rows != "Residuals"
  sentences <- vapply(which(keep), function(i) {
    f <- st[["F value"]][i]; p <- st[["Pr(>F)"]][i]
    df1 <- st[["Df"]][i]; df2 <- st[["Df"]][rows == "Residuals"][1]
    paste0("The effect of ", rows[i], " on ", outcome, " was ", sig_phrase(p, a$alpha),
           " (F(", fmt_num(df1, 0), ", ", fmt_num(df2, 0), ") = ", fmt_num(f), ", ",
           fmt_p(p, a$markup), ")")
  }, character(1))
  ancova <- sum(keep) > 1
  lead <- paste0(if (ancova) "An analysis of covariance (ANCOVA) / factorial ANOVA" else "A one-way ANOVA",
                 " was used to compare ", outcome, " across groups.")
  tail <- "Significant factors should be followed up with post-hoc pairwise comparisons (e.g. Tukey or Bonferroni), each reported with its mean difference and 95% CI."

  # stratify when the interaction is significant
  ia <- tryCatch(.interaction_analysis(object, a), error = function(e) NULL)
  extra_assum <- NULL
  if (!is.null(ia) && ia$any_sig) {
    caveat <- paste0("Because a statistically significant interaction was present, the effect of ",
      "each factor depends on the level of the other; the simple (stratified) effects are reported below rather than the main effects.")
    strat <- unlist(lapply(ia$sig, function(t)
      c(paste0("Stratified effects for the ", gsub(":", " x ", t), " interaction:"),
        .stratified_paras(object, t, a))), use.names = FALSE)
    body <- c(lead, sentences, caveat, strat, tail)
    extra_assum <- "Significant interaction: interpret simple effects stratified by the moderator, not the marginal main effects."
  } else {
    body <- c(lead, sentences, tail)
  }
  assum <- c("Independent observations.",
             "Approximately normal residuals (Q-Q plot).",
             "Homogeneity of variance (Levene's test); if violated, use Welch's ANOVA or a non-parametric Kruskal-Wallis test.",
             if (ancova) "For ANCOVA: linearity between covariate and outcome, and homogeneity of regression slopes." else NULL,
             extra_assum)
  new_interpretation(if (ancova) "ANCOVA / factorial ANOVA" else "One-way ANOVA",
                     body, stats::na.omit(assum))
}

#' @rdname interpret
#' @export
interpret.manova <- function(object, ...) {
  a <- azul_args(...)
  st <- tryCatch(summary(object, test = "Wilks")$stats, error = function(e) NULL)
  if (is.null(st)) return(new_interpretation("MANOVA",
      "A MANOVA was fitted; extract Wilks' Lambda from summary(model, test = 'Wilks').",
      c("Multivariate normality; homogeneity of covariance matrices (Box's M).")))
  rows <- rownames(st); keep <- rows != "Residuals"
  sentences <- vapply(which(keep), function(i) {
    lam <- st[i, "Wilks"]; f <- st[i, "approx F"]; p <- st[i, "Pr(>F)"]
    df1 <- st[i, "num Df"]; df2 <- st[i, "den Df"]
    paste0("The multivariate effect of ", rows[i], " was ", sig_phrase(p, a$alpha),
           " (Wilks' Lambda = ", fmt_num(lam, 3), ", approximate F(", fmt_num(df1, 0), ", ",
           fmt_num(df2, 0), ") = ", fmt_num(f), ", ", fmt_p(p, a$markup), ")")
  }, character(1))
  body <- paste0("A multivariate analysis of variance (MANOVA) tested the joint effect on the outcome set. ",
                 paste(sentences, collapse = ". "),
                 ". Significant multivariate effects should be decomposed with univariate ANOVAs (with a multiple-comparison correction) and, where relevant, discriminant analysis.")
  assum <- c("Multivariate normality of the outcome set.",
             "Homogeneity of variance-covariance matrices (Box's M test).",
             "Linearity among outcomes and absence of multicollinearity/singularity.",
             "Independent observations.")
  new_interpretation("MANOVA / MANCOVA", body, assum)
}

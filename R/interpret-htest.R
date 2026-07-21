# ---------------------------------------------------------------------------
# htest objects: t-test, correlation, chi-square, Fisher, Wilcoxon,
# Kruskal-Wallis, McNemar. Dispatch is on the $method string inside htest.
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @export
interpret.htest <- function(object, ...) {
  a <- azul_args(...)
  m <- object$method
  outcome <- a$outcome %||% "the outcome"

  # ---- t tests -----------------------------------------------------------
  if (grepl("t-test", m, ignore.case = TRUE)) {
    est <- object$estimate
    ci  <- object$conf.int
    tval <- unname(object$statistic); df <- unname(object$parameter)
    p <- object$p.value
    if (grepl("Paired", m)) {
      d <- unname(est)  # mean of differences
      para <- paste0(
        "A paired t-test was used to compare ", outcome, " before and after. ",
        "The mean difference was ", fmt_num(d), " (95% CI: ",
        fmt_num(ci[1]), ", ", fmt_num(ci[2]), "); the change was ",
        sig_phrase(p, a$alpha), " (paired t-test, t(", fmt_num(df, 0), ") = ",
        fmt_num(tval), ", ", fmt_p(p, a$markup), ").")
      assum <- c("Differences approximately normally distributed (inspect a histogram / Q-Q plot of the paired differences).",
                 "Observations paired and independent across pairs.")
      return(new_interpretation("Paired t-test", para, assum))
    }
    if (grepl("One Sample", m)) {
      para <- paste0(
        "A one-sample t-test compared the mean ", outcome, " (", fmt_num(unname(est)),
        ", 95% CI: ", fmt_num(ci[1]), ", ", fmt_num(ci[2]),
        ") against the reference value. The difference was ", sig_phrase(p, a$alpha),
        " (t(", fmt_num(df, 0), ") = ", fmt_num(tval), ", ", fmt_p(p, a$markup), ").")
      return(new_interpretation("One-sample t-test", para,
              c("Outcome approximately normally distributed.")))
    }
    # independent / Welch two sample
    md <- unname(est[1] - est[2])
    para <- paste0(
      "The mean ", outcome, " was ", fmt_num(unname(est[1])), " in the first group ",
      "compared with ", fmt_num(unname(est[2])), " in the second group. ",
      "The mean difference was ", fmt_num(md), " (95% CI: ", fmt_num(ci[1]), ", ",
      fmt_num(ci[2]), "); the difference was ", sig_phrase(p, a$alpha),
      " (", if (grepl("Welch", m)) "Welch " else "independent ", "t-test, t(",
      fmt_num(df, 1), ") = ", fmt_num(tval), ", ", fmt_p(p, a$markup), ").")
    assum <- c("Outcome approximately normally distributed within each group.",
               "Independent observations.",
               if (!grepl("Welch", m)) "Homogeneity of variance (Levene's test); if violated use Welch's t-test." else NULL)
    return(new_interpretation(if (grepl("Welch", m)) "Welch two-sample t-test" else "Independent t-test",
                              para, stats::na.omit(assum)))
  }

  # ---- correlation -------------------------------------------------------
  if (grepl("correlation", m, ignore.case = TRUE)) {
    r <- unname(object$estimate); p <- object$p.value
    is_pearson <- grepl("Pearson", m)
    coef_name <- if (is_pearson) "Pearson r" else if (grepl("Spearman", m)) "Spearman rho" else "Kendall tau"
    ci_txt <- if (!is.null(object$conf.int))
      paste0(", 95% CI: ", fmt_num(object$conf.int[1]), ", ", fmt_num(object$conf.int[2])) else ""
    para <- paste0(
      "There was a ", cor_strength(r), " ", direction(r), " correlation between the two variables (",
      coef_name, " = ", fmt_num(r), ci_txt, "; ", fmt_p(p, a$markup), "). The association was ",
      sig_phrase(p, a$alpha), ".")
    assum <- if (is_pearson)
      c("Linear relationship and approximate bivariate normality (Pearson).",
        "If skewed or non-linear, report Spearman's rho instead.") else
      c("Monotonic relationship (rank-based); no normality assumption.")
    return(new_interpretation(paste0(sub(".*", "", ""), coef_name, " correlation"), para, assum))
  }

  # ---- Hosmer-Lemeshow goodness-of-fit ----------------------------------
  if (grepl("Hosmer", m, ignore.case = TRUE)) {
    x2 <- unname(object$statistic); df <- unname(object$parameter); p <- object$p.value
    good <- is.na(p) || p >= a$alpha
    para <- paste0(
      "The Hosmer-Lemeshow goodness-of-fit test was used to assess calibration of the logistic model. ",
      "The model showed ", if (good) "adequate" else "poor", " calibration (chi-squared = ", fmt_num(x2),
      ", df = ", fmt_num(df, 0), ", ", fmt_p(p, a$markup), "). ",
      if (good) "A non-significant result indicates no evidence of lack of fit, so observed and predicted probabilities agree well."
      else "A significant result indicates a discrepancy between observed and predicted probabilities; reconsider the functional form or add terms.")
    return(new_interpretation("Hosmer-Lemeshow goodness-of-fit test", para,
      c("The test is sensitive to the number of groups (commonly g = 10) and to sample size.",
        "Calibration (this test) is distinct from discrimination (ROC/AUC); report both.")))
  }

  # ---- chi-square / McNemar ---------------------------------------------
  if (grepl("Chi-squared", m, ignore.case = TRUE) || grepl("McNemar", m, ignore.case = TRUE)) {
    x2 <- unname(object$statistic); df <- unname(object$parameter); p <- object$p.value
    if (grepl("McNemar", m)) {
      para <- paste0(
        "A McNemar's test examined the change in paired proportions. The change was ",
        sig_phrase(p, a$alpha), " (McNemar's chi-squared = ", fmt_num(x2), ", df = ",
        fmt_num(df, 0), ", ", fmt_p(p, a$markup), ").")
      return(new_interpretation("McNemar's test", para,
              c("Paired/matched binary observations.",
                "Use continuity correction or exact test when discordant cells are few.")))
    }
    gof <- grepl("given probabilities|goodness", m, ignore.case = TRUE)
    if (gof) {
      para <- paste0(
        "A chi-squared goodness-of-fit test compared the observed category frequencies with the expected proportions. ",
        "The observed distribution ", if (!is.na(p) && p < a$alpha) "differed significantly from" else "did not differ significantly from",
        " the expected (chi-squared(", fmt_num(df, 0), ") = ", fmt_num(x2), ", ", fmt_p(p, a$markup), ").")
      return(new_interpretation("Chi-squared goodness-of-fit test", para,
              c("Expected count >= 5 in at least 80% of cells; otherwise use an exact test.",
                "Independent observations; the expected proportions should be specified a priori.")))
    }
    para <- paste0(
      "There was ", if (!is.na(p) && p < a$alpha) "a statistically significant" else "no statistically significant",
      " association between the two categorical variables (chi-squared(", fmt_num(df, 0), ") = ",
      fmt_num(x2), ", ", fmt_p(p, a$markup), ").")
    return(new_interpretation("Pearson chi-squared test", para,
            c("Expected count >= 5 in at least 80% of cells and no cell < 1; otherwise use Fisher's exact test.",
              "Independent observations.")))
  }

  # ---- Fisher ------------------------------------------------------------
  if (grepl("Fisher", m, ignore.case = TRUE)) {
    p <- object$p.value
    or_txt <- if (!is.null(object$estimate))
      paste0(" The estimated odds ratio was ", fmt_num(unname(object$estimate)),
             if (!is.null(object$conf.int)) paste0(" (95% CI: ", fmt_num(object$conf.int[1]),
                    ", ", fmt_num(object$conf.int[2]), ")") else "", ".") else ""
    para <- paste0(
      "Fisher's exact test was used because of small expected cell counts. The association was ",
      sig_phrase(p, a$alpha), " (", fmt_p(p, a$markup), ").", or_txt)
    return(new_interpretation("Fisher's exact test", para,
            c("Appropriate when expected cell counts are small.", "Independent observations.")))
  }

  # ---- Wilcoxon / Mann-Whitney ------------------------------------------
  if (grepl("Wilcoxon", m, ignore.case = TRUE) || grepl("Mann-Whitney", m, ignore.case = TRUE)) {
    p <- object$p.value; W <- unname(object$statistic)
    paired <- grepl("signed rank", m, ignore.case = TRUE)
    nm <- if (paired) "Wilcoxon signed-rank test" else "Mann-Whitney U test (Wilcoxon rank-sum)"
    para <- paste0(
      "A ", nm, " was used as a non-parametric comparison of ", outcome, ". The difference in ",
      if (paired) "the paired distributions" else "distributions between groups", " was ",
      sig_phrase(p, a$alpha), " (", nm, ", ", fmt_p(p, a$markup), ").")
    return(new_interpretation(nm, para,
            c("Distribution-free; report median (IQR) as the accompanying descriptive statistic.")))
  }

  # ---- Kruskal-Wallis ----------------------------------------------------
  if (grepl("Kruskal-Wallis", m, ignore.case = TRUE)) {
    p <- object$p.value; H <- unname(object$statistic); df <- unname(object$parameter)
    para <- paste0(
      "A Kruskal-Wallis test compared ", outcome, " across the groups. The difference was ",
      sig_phrase(p, a$alpha), " (H(", fmt_num(df, 0), ") = ", fmt_num(H), ", ",
      fmt_p(p, a$markup), ").",
      if (!is.na(p) && p < a$alpha) " Dunn's test with a multiple-comparison correction is recommended for pairwise follow-up." else "")
    return(new_interpretation("Kruskal-Wallis test", para,
            c("Non-parametric alternative to one-way ANOVA; report median (IQR) per group.")))
  }

  # ---- Ljung-Box / Box-Pierce (residual white-noise) ---------------------
  if (grepl("Box-Ljung|Box-Pierce", m, ignore.case = TRUE)) {
    x2 <- unname(object$statistic); df <- unname(object$parameter); p <- object$p.value
    white <- is.na(p) || p >= a$alpha
    para <- paste0("A ", if (grepl("Ljung", m)) "Ljung-Box" else "Box-Pierce",
      " test examined the residuals for remaining autocorrelation. The residuals were ",
      if (white) "consistent with white noise" else "NOT consistent with white noise",
      " (chi-squared(", fmt_num(df, 0), ") = ", fmt_num(x2), ", ", fmt_p(p, a$markup), "). ",
      if (white) "A non-significant result indicates no residual autocorrelation, so the model has captured the temporal structure adequately."
      else "A significant result indicates residual autocorrelation remains; the model order should be revised.")
    return(new_interpretation("Ljung-Box test (residual autocorrelation)", para,
      c("Set the lag and the fitdf (number of estimated ARMA parameters) appropriately.",
        "Complement with the ACF/PACF of residuals.",
        "Reference: Ljung & Box (1978); Hyndman & Athanasopoulos, Forecasting: Principles and Practice.")))
  }

  # ---- Augmented Dickey-Fuller (stationarity) ----------------------------
  if (grepl("Dickey-Fuller", m, ignore.case = TRUE)) {
    p <- object$p.value; stat <- unname(object$statistic)
    stat_ok <- !is.na(p) && p < a$alpha
    para <- paste0("An Augmented Dickey-Fuller (ADF) test assessed stationarity (null hypothesis: a unit root, i.e. non-stationary). ",
      "The series ", if (stat_ok) "appeared stationary" else "did not appear stationary",
      " (Dickey-Fuller = ", fmt_num(stat), ", ", fmt_p(p, a$markup), "). ",
      if (stat_ok) "The null of a unit root was rejected."
      else "The null of a unit root was not rejected; consider differencing before ARIMA modelling.")
    return(new_interpretation("Augmented Dickey-Fuller test (stationarity)", para,
      c("The ADF null is non-stationarity; pair it with the KPSS test (whose null is stationarity).",
        "Reference: Dickey & Fuller (1979); Hyndman & Athanasopoulos.")))
  }

  # ---- KPSS (stationarity, opposite null) --------------------------------
  if (grepl("KPSS", m, ignore.case = TRUE)) {
    p <- object$p.value; stat <- unname(object$statistic)
    nonstat <- !is.na(p) && p < a$alpha
    para <- paste0("A KPSS test assessed stationarity (null hypothesis: the series is stationary). ",
      "The series ", if (nonstat) "appeared non-stationary" else "was consistent with stationarity",
      " (KPSS = ", fmt_num(stat), ", ", fmt_p(p, a$markup), "). ",
      if (nonstat) "The null of stationarity was rejected; differencing is indicated."
      else "The null of stationarity was not rejected.")
    return(new_interpretation("KPSS test (stationarity)", para,
      c("The KPSS null is stationarity (opposite to ADF); use both for a firmer conclusion.",
        "Reference: Kwiatkowski et al. (1992); Hyndman & Athanasopoulos.")))
  }

  # ---- Moran's I (global spatial autocorrelation) ------------------------
  if (grepl("Moran I", m, ignore.case = TRUE)) {
    I <- unname(object$estimate[1]); E <- unname(object$estimate[2])
    zz <- unname(object$statistic); p <- object$p.value
    dir <- if (is.na(I) || is.na(E)) "undetermined" else if (I > E) "positive" else if (I < E) "negative" else "no"
    verdict <- if (dir == "positive") "clustering of similar values (nearby areas tend to be alike)"
      else if (dir == "negative") "a dispersed / checkerboard pattern (nearby areas tend to differ)"
      else "a spatially random pattern"
    para <- paste0("Moran's I was used to test for global spatial autocorrelation. ",
      "The observed Moran's I was ", fmt_num(I, 3), " against an expectation of ", fmt_num(E, 3),
      " under spatial randomness (standard deviate = ", fmt_num(zz), ", ", fmt_p(p, a$markup), "). ",
      "This indicates ", if (!is.na(p) && p < a$alpha) "statistically significant " else "no statistically significant ",
      dir, " spatial autocorrelation, consistent with ", verdict, ".")
    return(new_interpretation("Moran's I (global spatial autocorrelation)", para,
      c("Results depend on the spatial weights matrix (contiguity vs distance, row-standardised or not); state how it was defined.",
        "Global Moran's I summarises the whole map; use local indicators (LISA) to locate clusters.",
        "Reference: Moran (1950); Waller & Gotway (2004), Applied Spatial Statistics for Public Health Data, Wiley.")))
  }

  # ---- Geary's C (global spatial autocorrelation) ------------------------
  if (grepl("Geary C", m, ignore.case = TRUE)) {
    C <- unname(object$estimate[1]); p <- object$p.value; zz <- unname(object$statistic)
    dir <- if (is.na(C)) "undetermined" else if (C < 1) "positive" else if (C > 1) "negative" else "no"
    para <- paste0("Geary's C was used to test for global spatial autocorrelation. ",
      "The observed Geary's C was ", fmt_num(C, 3), " (values below 1 indicate positive autocorrelation, ",
      "1 indicates randomness, above 1 negative; standard deviate = ", fmt_num(zz), ", ", fmt_p(p, a$markup), "). ",
      "This indicates ", if (!is.na(p) && p < a$alpha) "statistically significant " else "no statistically significant ",
      dir, " spatial autocorrelation.")
    return(new_interpretation("Geary's C (global spatial autocorrelation)", para,
      c("Geary's C is more sensitive to local (short-range) autocorrelation than Moran's I.",
        "Depends on the spatial weights matrix; state its definition.",
        "Reference: Geary (1954); Waller & Gotway (2004), Applied Spatial Statistics for Public Health Data, Wiley.")))
  }

  # ---- Friedman (non-parametric repeated measures) -----------------------
  if (grepl("Friedman", m, ignore.case = TRUE)) {
    p <- object$p.value; x2 <- unname(object$statistic); df <- unname(object$parameter)
    para <- paste0(
      "A Friedman test (non-parametric alternative to repeated-measures ANOVA) compared ", outcome,
      " across the related conditions. The difference across conditions was ", sig_phrase(p, a$alpha),
      " (Friedman chi-squared(", fmt_num(df, 0), ") = ", fmt_num(x2), ", ", fmt_p(p, a$markup), ").",
      if (!is.na(p) && p < a$alpha) " Follow up with pairwise Wilcoxon signed-rank tests (Bonferroni) or a Nemenyi post-hoc." else "")
    return(new_interpretation("Friedman test", para,
      c("Repeated/related measurements on the same subjects; report median (IQR) per condition.",
        "Complete blocks required (no missing cells).")))
  }

  # ---- Kolmogorov-Smirnov ------------------------------------------------
  if (grepl("Kolmogorov-Smirnov", m, ignore.case = TRUE)) {
    p <- object$p.value; D <- unname(object$statistic)
    two <- grepl("Two-sample", m)
    para <- paste0("A ", if (two) "two-sample" else "one-sample", " Kolmogorov-Smirnov test compared ",
      if (two) "the two distributions" else "the distribution against the reference", ". The distributions ",
      if (!is.na(p) && p < a$alpha) "differed significantly" else "did not differ significantly",
      " (D = ", fmt_num(D, 3), ", ", fmt_p(p, a$markup), ").")
    return(new_interpretation("Kolmogorov-Smirnov test", para,
      c("Compares entire distributions (location and shape), not only the median.",
        if (!two) "The one-sample test assumes the reference distribution is fully specified a priori." else NULL)))
  }

  # ---- Sign test ---------------------------------------------------------
  if (grepl("Sign[- ]?Test", m, ignore.case = TRUE) || grepl("Sign Test", m, ignore.case = TRUE)) {
    p <- object$p.value
    para <- paste0("A sign test (a distribution-free test based only on the direction of differences) was used. ",
      "The result was ", sig_phrase(p, a$alpha), " (", fmt_p(p, a$markup), ").",
      " It is less powerful than the Wilcoxon signed-rank test but makes no symmetry assumption.")
    return(new_interpretation("Sign test", para,
      c("Uses only the sign of the differences; report the median difference.")))
  }

  # ---- fallback ----------------------------------------------------------
  para <- paste0("The ", m, " gave ", fmt_p(object$p.value, a$markup),
                 "; the result was ", sig_phrase(object$p.value, a$alpha), ".")
  new_interpretation(m, para)
}

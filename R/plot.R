# ---------------------------------------------------------------------------
# azul_plot(): draw the appropriate figure for a model and return a short
# interpretation of what it shows. Base graphics only (no extra dependencies).
# ---------------------------------------------------------------------------

.azul_plot_type <- function(x) {
  if (inherits(x, "survfit")) return("km")
  if (inherits(x, "roc")) return("roc")
  if (inherits(x, c("rma", "meta"))) return("forest_meta")
  if (inherits(x, c("Arima", "arima"))) return("tsdiag")
  if (inherits(x, "ts")) return("ts")
  if (inherits(x, c("lmerMod", "glmerMod", "lmerModLmerTest", "lme"))) return("caterpillar")
  if (inherits(x, c("fa", "principal", "scree"))) return("scree")
  if (inherits(x, c("coxph", "glm", "lm", "negbin", "polr", "clm", "multinom",
                    "azul_interpretation"))) return("forest")
  "forest"
}

#' Draw and interpret the figure for a model
#'
#' Produces the figure that best matches the analysis and returns a short
#' interpretation of it. Supported figures: a forest plot of the estimates
#' (regression models), a Kaplan-Meier curve (\code{survfit}), an ROC curve
#' (\code{pROC}), residual-diagnostic panels (\code{lm}/\code{glm}), and a
#' forest plot for meta-analysis (\code{metafor}/\code{meta}).
#'
#' @param x A fitted model, an \code{azul_interpretation}, a \code{survfit},
#'   \code{roc}, or meta-analysis object.
#' @param type One of "auto" (default), "forest", "km", "roc", "residuals",
#'   "forest_meta".
#' @param main Optional plot title.
#' @param ... Passed to the underlying plot.
#' @return Invisibly, an \code{azul_interpretation} describing the figure (the
#'   plot is drawn as a side effect).
#' @examples
#' azul_plot(glm(am ~ wt + hp, within(mtcars, am <- factor(am)), family = binomial))
#' @export
azul_plot <- function(x, type = c("auto", "forest", "km", "roc", "residuals",
                                  "forest_meta", "ts", "tsdiag", "acf",
                                  "caterpillar", "schoenfeld", "funnel", "calibration",
                                  "effect", "qqrand", "scree"),
                      main = NULL, ...) {
  type <- match.arg(type)
  if (type == "auto") type <- .azul_plot_type(x)
  switch(type,
    forest       = .azul_forest(x, main = main, ...),
    km           = .azul_km(x, main = main, ...),
    roc          = .azul_roc(x, main = main, ...),
    residuals    = .azul_residuals(x, ...),
    forest_meta  = .azul_forest_meta(x, main = main, ...),
    ts           = .azul_ts(x, main = main, ...),
    tsdiag       = .azul_tsdiag(x, ...),
    acf          = .azul_acf(x, ...),
    caterpillar  = .azul_caterpillar(x, main = main, ...),
    schoenfeld   = .azul_schoenfeld(x, ...),
    funnel       = .azul_funnel(x, main = main, ...),
    calibration  = .azul_calibration(x, main = main, ...),
    effect       = .azul_effect(x, main = main, ...),
    qqrand       = .azul_qqrand(x, main = main, ...),
    scree        = .azul_scree(x, main = main, ...),
    stop("Unknown plot type '", type, "'.", call. = FALSE))
}

# effect / interaction plot: predicted outcome across a focal predictor,
# split by a moderator when the model contains their interaction.
.azul_effect <- function(x, focal = NULL, moderator = NULL, main = NULL, ...) {
  if (!inherits(x, c("lm", "glm")) || inherits(x, "mlm"))
    stop("Effect plots are supported for lm and glm models.", call. = FALSE)
  mf <- stats::model.frame(x)
  resp <- names(mf)[1]
  preds <- all.vars(stats::delete.response(stats::terms(x)))
  preds <- intersect(preds, names(mf))
  is_fac <- vapply(mf[preds], function(z) is.factor(z) || is.character(z), logical(1))
  if (is.null(focal)) focal <- (preds[!is_fac])[1] %||% preds[1]
  if (is.na(focal)) stop("No usable predictor found for an effect plot.", call. = FALSE)
  ilabs <- attr(stats::terms(x), "term.labels"); ilabs <- ilabs[grepl(":", ilabs)]
  if (is.null(moderator)) {
    for (t in ilabs) { pp <- strsplit(t, ":")[[1]]
      if (focal %in% pp) { other <- setdiff(pp, focal)[1]
        if (!is.na(other) && isTRUE(is_fac[other])) { moderator <- other; break } } }
  }
  base_row <- lapply(preds, function(p) if (is_fac[p]) factor(levels(factor(mf[[p]]))[1], levels = levels(factor(mf[[p]]))) else mean(mf[[p]], na.rm = TRUE))
  names(base_row) <- preds
  fx <- if (is_fac[focal]) levels(factor(mf[[focal]])) else seq(min(mf[[focal]], na.rm = TRUE), max(mf[[focal]], na.rm = TRUE), length.out = 100)
  mods <- if (!is.null(moderator)) levels(factor(mf[[moderator]])) else NA
  cols <- grDevices::palette.colors(max(length(mods), 1))
  first <- TRUE; ry <- c()
  preds_list <- list()
  for (mi in seq_along(mods)) {
    nd <- base_row[rep(1, length(fx))]; nd <- as.data.frame(lapply(preds, function(p) rep(base_row[[p]], length(fx)))); names(nd) <- preds
    nd[[focal]] <- fx
    if (!is.null(moderator)) nd[[moderator]] <- factor(mods[mi], levels = levels(factor(mf[[moderator]])))
    pr <- as.numeric(stats::predict(x, newdata = nd, type = "response"))
    preds_list[[mi]] <- pr; ry <- range(c(ry, pr), na.rm = TRUE)
  }
  ylab <- if (inherits(x, "glm") && stats::family(x)$family == "binomial") "Predicted probability" else paste0("Predicted ", resp)
  op <- graphics::par(mar = c(4, 4, 3, 1)); on.exit(graphics::par(op))
  xf <- if (is_fac[focal]) seq_along(fx) else fx
  graphics::plot(xf, preds_list[[1]], type = if (is_fac[focal]) "b" else "l", ylim = ry,
    xaxt = if (is_fac[focal]) "n" else "s", lwd = 2, col = cols[1],
    xlab = focal, ylab = ylab, main = main %||% paste0("Effect plot: ", resp, " vs ", focal), ...)
  if (is_fac[focal]) graphics::axis(1, at = seq_along(fx), labels = fx)
  if (length(mods) > 1) for (mi in 2:length(mods)) graphics::lines(xf, preds_list[[mi]], lwd = 2, col = cols[mi], type = if (is_fac[focal]) "b" else "l")
  if (!is.null(moderator)) graphics::legend("topleft", legend = paste0(moderator, " = ", mods), col = cols[seq_along(mods)], lwd = 2, bty = "n", cex = 0.9)
  body <- paste0("Model-predicted ", tolower(ylab), " across ", focal,
    if (!is.null(moderator)) paste0(", separately for each level of ", moderator,
      ". Non-parallel lines show the interaction: the effect of ", focal, " differs across ", moderator, ".")
    else ", with the other predictors held at their mean or reference level.")
  invisible(new_interpretation("Effect plot", body, notes = "Figure drawn by azul_plot()."))
}

# Q-Q plot of the random effects (mixed-model normality)
.azul_qqrand <- function(x, main = NULL, ...) {
  re <- if (inherits(x, c("lmerMod", "glmerMod", "lmerModLmerTest")) && requireNamespace("lme4", quietly = TRUE))
    lme4::ranef(x)[[1]] else if (inherits(x, "lme") && requireNamespace("nlme", quietly = TRUE)) nlme::ranef(x) else NULL
  if (is.null(re)) stop("Could not extract random effects.", call. = FALSE)
  v <- as.numeric(as.data.frame(re)[[1]])
  op <- graphics::par(mar = c(4, 4, 3, 1)); on.exit(graphics::par(op))
  stats::qqnorm(v, main = main %||% "Normal Q-Q of random effects", pch = 19, ...); stats::qqline(v, col = "red")
  invisible(new_interpretation("Q-Q plot of random effects",
    c("Points close to the line support the assumption that the random effects are normally distributed.",
      "Systematic departures (curved or S-shaped patterns) indicate non-normal random effects."),
    notes = "Figure drawn by azul_plot()."))
}

# scree plot of eigenvalues for factor analysis / PCA
.azul_scree <- function(x, main = NULL, ...) {
  ev <- if (inherits(x, "scree")) x$pcv
    else if (inherits(x, c("fa", "principal"))) x$values %||% x$e.values
    else if (is.matrix(x) && nrow(x) == ncol(x)) eigen(x, only.values = TRUE)$values
    else eigen(stats::cor(as.data.frame(x), use = "pairwise"), only.values = TRUE)$values
  if (is.null(ev)) stop("Could not obtain eigenvalues for a scree plot.", call. = FALSE)
  op <- graphics::par(mar = c(4, 4, 3, 1)); on.exit(graphics::par(op))
  graphics::plot(seq_along(ev), ev, type = "b", pch = 19, xlab = "Component / factor",
    ylab = "Eigenvalue", main = main %||% "Scree plot", ...)
  graphics::abline(h = 1, lty = 2, col = "red")
  k <- sum(ev > 1)
  invisible(new_interpretation("Scree plot",
    paste0("Eigenvalues by component. By the Kaiser criterion (eigenvalue > 1, red line) ", k,
      " component", if (k != 1) "s" else "", " would be retained; the 'elbow' where the curve flattens is an alternative cue. Cross-check with parallel analysis."),
    notes = "Figure drawn by azul_plot()."))
}

# caterpillar plot of mixed-model random effects (BLUPs with intervals)
.azul_caterpillar <- function(x, main = NULL, ...) {
  re <- NULL; condvar <- FALSE
  if (inherits(x, c("lmerMod", "glmerMod", "lmerModLmerTest")) &&
      requireNamespace("lme4", quietly = TRUE)) {
    re <- tryCatch(lme4::ranef(x, condVar = TRUE), error = function(e) NULL); condvar <- TRUE
  } else if (inherits(x, "lme") && requireNamespace("nlme", quietly = TRUE)) {
    re <- tryCatch(nlme::ranef(x), error = function(e) NULL)
  }
  if (is.null(re) || !length(re)) stop("Could not extract random effects for a caterpillar plot.", call. = FALSE)
  g <- names(re)[1]; df <- as.data.frame(re[[g]])
  term <- if ("(Intercept)" %in% names(df)) "(Intercept)" else names(df)[1]
  est <- df[[term]]
  se <- rep(NA_real_, length(est))
  if (condvar) {
    pv <- attr(re[[g]], "postVar")
    if (!is.null(pv)) { j <- match(term, names(df)); se <- sqrt(pv[j, j, ]) }
  }
  ord <- order(est); est <- est[ord]; se <- se[ord]; n <- length(est)
  lo <- est - 1.96 * se; hi <- est + 1.96 * se
  ylim <- range(c(est, lo, hi, 0), na.rm = TRUE)
  op <- graphics::par(mar = c(4, 4, 3, 1)); on.exit(graphics::par(op))
  graphics::plot(seq_len(n), est, ylim = ylim, pch = 19, xlab = paste0(g, " (sorted)"),
    ylab = paste0("Random ", tolower(term), " (BLUP)"),
    main = main %||% paste0("Caterpillar plot: ", g, " random effects"), ...)
  if (any(is.finite(se))) graphics::segments(seq_len(n), lo, seq_len(n), hi, col = "grey50")
  graphics::abline(h = 0, lty = 2, col = "red")
  excl <- if (any(is.finite(se))) sum(lo > 0 | hi < 0, na.rm = TRUE) else NA
  body <- paste0("Caterpillar plot of the ", n, " '", g, "' random ", tolower(term),
    "s (best linear unbiased predictors), sorted low to high",
    if (condvar) " with 95% prediction intervals" else "", ". The dashed line is the overall average (0). ",
    if (!is.na(excl)) paste0(excl, " cluster(s) had an interval excluding 0, i.e. departing from the average. ") else "",
    "A wide spread indicates substantial between-cluster variation (quantified by the ICC).")
  invisible(new_interpretation(paste0("Caterpillar plot: ", g, " random effects"), body,
    notes = "Figure drawn by azul_plot()."))
}

# Schoenfeld residuals for the Cox proportional-hazards assumption
.azul_schoenfeld <- function(x, ...) {
  if (!requireNamespace("survival", quietly = TRUE)) stop("Package 'survival' is required.", call. = FALSE)
  zph <- survival::cox.zph(x)
  np <- nrow(zph$table) - 1
  op <- graphics::par(mfrow = c(ceiling(np / 2), min(np, 2)), mar = c(4, 4, 2, 1)); on.exit(graphics::par(op))
  graphics::plot(zph, ...)
  gp <- zph$table["GLOBAL", "p"]
  invisible(new_interpretation("Schoenfeld residuals (proportional hazards)",
    paste0("Scaled Schoenfeld residuals against time for each covariate. A roughly horizontal (flat) smooth ",
      "supports proportional hazards; a clear time trend indicates a violation. The global test gave ",
      fmt_p(gp), " (a non-significant result supports the assumption)."),
    notes = "Figure drawn by azul_plot(); see also check_assumptions() on the Cox model."))
}

# funnel plot for meta-analysis (small-study / publication bias)
.azul_funnel <- function(x, main = NULL, ...) {
  if (inherits(x, "rma") && requireNamespace("metafor", quietly = TRUE))
    metafor::funnel(x, main = main %||% "Funnel plot", ...)
  else if (inherits(x, "meta") && requireNamespace("meta", quietly = TRUE))
    meta::funnel(x, ...)
  else stop("Install 'metafor' or 'meta' for a funnel plot.", call. = FALSE)
  invisible(new_interpretation("Funnel plot (small-study effects)",
    c("Each point is a study: effect size (x) against its precision (y, larger = more precise).",
      "A symmetric, inverted-funnel shape argues against publication/small-study bias.",
      "Asymmetry (gaps at the bottom) suggests missing small negative studies; confirm with Egger's test."),
    notes = "Figure drawn by azul_plot()."))
}

# calibration plot for a logistic model (observed vs predicted by decile)
.azul_calibration <- function(x, main = NULL, groups = 10, ...) {
  if (!inherits(x, "glm") || stats::family(x)$family != "binomial")
    stop("Calibration plots apply to binomial glm (logistic) models.", call. = FALSE)
  p <- stats::fitted(x); y <- x$y
  br <- unique(stats::quantile(p, probs = seq(0, 1, length.out = groups + 1), na.rm = TRUE))
  bin <- cut(p, breaks = br, include.lowest = TRUE)
  obs <- tapply(y, bin, mean); pred <- tapply(p, bin, mean)
  op <- graphics::par(mar = c(4, 4, 3, 1)); on.exit(graphics::par(op))
  graphics::plot(pred, obs, xlim = 0:1, ylim = 0:1, pch = 19,
    xlab = "Predicted probability", ylab = "Observed proportion",
    main = main %||% "Calibration plot"); graphics::abline(0, 1, lty = 2, col = "grey40")
  graphics::lines(pred, obs, col = "steelblue")
  invisible(new_interpretation("Calibration plot (logistic model)",
    c("Points on the diagonal indicate well-calibrated predictions (observed = predicted).",
      "Points below the line indicate over-prediction; above, under-prediction.",
      "Pair with the Hosmer-Lemeshow test for a formal calibration check."),
    notes = "Figure drawn by azul_plot()."))
}

# time series identification: the series, its ACF and PACF
.azul_ts <- function(x, main = NULL, ...) {
  op <- graphics::par(mfrow = c(3, 1), mar = c(4, 4, 2, 1)); on.exit(graphics::par(op))
  graphics::plot(x, ylab = "Value", xlab = "Time", main = main %||% "Time series", ...)
  stats::acf(x, main = "ACF")
  stats::pacf(x, main = "PACF")
  invisible(new_interpretation("Time series identification (ACF / PACF)",
    c("A slowly-decaying ACF suggests non-stationarity, so difference the series (d) until it settles.",
      "ACF cutting off after lag q with a tailing PACF suggests an MA(q) term.",
      "PACF cutting off after lag p with a tailing ACF suggests an AR(p) term.",
      "Spikes at seasonal lags (e.g. 12, 24) indicate a seasonal component."),
    notes = "Figure drawn by azul_plot(); confirm stationarity with an ADF/KPSS test."))
}

# ARIMA residual diagnostics: residuals over time, ACF and PACF of residuals
.azul_tsdiag <- function(x, ...) {
  r <- stats::residuals(x)
  op <- graphics::par(mfrow = c(3, 1), mar = c(4, 4, 2, 1)); on.exit(graphics::par(op))
  graphics::plot(r, ylab = "Residuals", xlab = "Time", main = "Residuals over time")
  graphics::abline(h = 0, lty = 2, col = "grey40")
  stats::acf(r, main = "ACF of residuals", na.action = stats::na.pass)
  stats::pacf(r, main = "PACF of residuals", na.action = stats::na.pass)
  invisible(new_interpretation("ARIMA residual diagnostics",
    c("For an adequate model the residuals should be white noise: nearly all ACF and PACF bars fall within the confidence bands.",
      "A pattern or spikes beyond the bands indicate remaining autocorrelation; revise the (p, d, q) orders.",
      "The residuals over time should look random around zero with roughly constant variance."),
    notes = "Figure drawn by azul_plot(); confirm with the Ljung-Box test (Box.test, type = 'Ljung-Box')."))
}

# ACF + PACF of a series or of a model's residuals
.azul_acf <- function(x, ...) {
  v <- if (inherits(x, c("Arima", "arima", "lm", "glm"))) stats::residuals(x) else x
  op <- graphics::par(mfrow = c(1, 2)); on.exit(graphics::par(op))
  stats::acf(v, main = "ACF", na.action = stats::na.pass, ...)
  stats::pacf(v, main = "PACF", na.action = stats::na.pass, ...)
  invisible(new_interpretation("Autocorrelation (ACF / PACF)",
    c("Bars within the confidence bands indicate no significant autocorrelation at that lag.",
      "ACF identifies MA order (cut-off), PACF identifies AR order (cut-off)."),
    notes = "Figure drawn by azul_plot()."))
}

# forest plot from a model's estimates table
.azul_forest <- function(x, main = NULL, ...) {
  int <- if (inherits(x, "azul_interpretation")) x else interpret(x)
  est <- int$estimates
  if (is.null(est) || !is.data.frame(est) || !all(c("estimate","conf.low","conf.high") %in% names(est)))
    stop("No estimates with confidence intervals are available for a forest plot.", call. = FALSE)
  meas <- .measure_label(int$method)
  ratio <- meas %in% c("OR","HR","IRR","RRR")
  term <- if ("term" %in% names(est)) est$term else rownames(est)
  n <- nrow(est); yy <- n:1                      # top row at the top
  null_v <- if (ratio) 1 else 0
  xs <- c(est$estimate, est$conf.low, est$conf.high, null_v)
  xs <- xs[is.finite(xs) & (!ratio | xs > 0)]
  xlim <- range(xs, na.rm = TRUE)
  op <- graphics::par(mar = c(5, max(6, max(nchar(term)) * 0.6), 4, 2)); on.exit(graphics::par(op))
  graphics::plot(est$estimate, yy, xlim = xlim, ylim = c(0.5, n + 0.5), yaxt = "n",
    xlab = paste0(meas, " (95% CI)"), ylab = "", pch = 19,
    log = if (ratio) "x" else "", main = main %||% int$method, ...)
  graphics::segments(est$conf.low, yy, est$conf.high, yy, lwd = 2)
  graphics::axis(2, at = yy, labels = term, las = 1, cex.axis = 0.9)
  graphics::abline(v = null_v, lty = 2, col = "grey40")
  nonsig <- sum(est$conf.low <= null_v & est$conf.high >= null_v, na.rm = TRUE)
  body <- paste0("Forest plot of ", n, " ", meas, " estimate", if (n != 1) "s" else "",
    " with 95% confidence intervals. The dashed line marks the null value (",
    meas, " = ", null_v, "); an estimate whose interval crosses it is not statistically significant. ",
    n - nonsig, " of ", n, " estimate", if (n != 1) "s" else "", " excluded the null.")
  invisible(new_interpretation(paste0("Forest plot: ", int$method), body,
    notes = "Figure drawn by azul_plot()."))
}

.azul_km <- function(x, main = NULL, ...) {
  graphics::plot(x, xlab = "Time", ylab = "Survival probability",
    main = main %||% "Kaplan-Meier survival curve",
    col = if (!is.null(x$strata)) seq_along(x$strata) else 1, ...)
  if (!is.null(x$strata))
    graphics::legend("topright", legend = names(x$strata),
      col = seq_along(x$strata), lty = 1, bty = "n", cex = 0.9)
  invisible(interpret(x))
}

.azul_roc <- function(x, main = NULL, ...) {
  if (!requireNamespace("pROC", quietly = TRUE)) stop("Package 'pROC' is required.", call. = FALSE)
  graphics::plot(x, main = main %||% "ROC curve", legacy.axes = TRUE, ...)
  invisible(interpret(x))
}

.azul_residuals <- function(x, ...) {
  op <- graphics::par(mfrow = c(2, 2)); on.exit(graphics::par(op))
  graphics::plot(x, ...)
  invisible(new_interpretation("Residual diagnostics",
    c("Top-left (residuals vs fitted): look for non-linearity or funnelling (heteroscedasticity).",
      "Top-right (Q-Q): points on the line support normality of residuals.",
      "Bottom-left (scale-location): a flat trend supports constant variance.",
      "Bottom-right (residuals vs leverage): points beyond Cook's distance are influential."),
    notes = "Figure drawn by azul_plot(); pair with check_assumptions()."))
}

.azul_forest_meta <- function(x, main = NULL, ...) {
  if (inherits(x, "rma") && requireNamespace("metafor", quietly = TRUE)) {
    metafor::forest(x, main = main %||% "Meta-analysis forest plot", ...)
  } else if (inherits(x, "meta") && requireNamespace("meta", quietly = TRUE)) {
    meta::forest(x, ...)
  } else stop("Install 'metafor' or 'meta' to draw a meta-analysis forest plot.", call. = FALSE)
  invisible(interpret(x))
}

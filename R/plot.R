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
                                  "forest_meta", "ts", "tsdiag", "acf"),
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
    stop("Unknown plot type '", type, "'.", call. = FALSE))
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

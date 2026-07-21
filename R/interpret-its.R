# ---------------------------------------------------------------------------
# Interrupted time series (ITS) / segmented regression.
# Reference: Lopez Bernal J, Cummins S, Gasparrini A. Interrupted time series
# regression for the evaluation of public health interventions: a tutorial.
# Int J Epidemiol. 2017;46(1):348-355.
#
# An ITS model is a (quasi-)Poisson or linear segmented regression with:
#   - a continuous time term (underlying/pre-intervention trend),
#   - an intervention indicator (0 before, 1 after) -> LEVEL (step) change,
#   - optionally an intervention x time interaction -> SLOPE change,
#   - usually an offset(log(pop)) and harmonic() seasonality terms.
# interpret_its() names which term is which and reports them accordingly.
# ---------------------------------------------------------------------------

#' Interpret an interrupted time series (segmented regression) model
#'
#' Reports the pre-intervention trend, the immediate level (step) change and,
#' when present, the change in slope, following the Lopez Bernal / Gasparrini
#' ITS tutorial. Works with a (quasi-)Poisson model (rate/count outcome, log
#' link -> incidence rate ratios) or a linear model (absolute changes).
#'
#' @param model A fitted \code{glm} (Poisson / quasi-Poisson, log link) or
#'   \code{lm} segmented-regression model.
#' @param level Name of the intervention indicator coefficient (the step /
#'   level-change term), e.g. \code{"smokban"}.
#' @param trend Name of the continuous time coefficient (the pre-intervention
#'   trend), e.g. \code{"time"}.
#' @param slope Name of the intervention-by-time interaction coefficient (the
#'   slope-change term). If \code{NULL}, azul looks for
#'   \code{level:trend} / \code{trend:level} automatically.
#' @param period Number of time units over which to express the trend
#'   (e.g. 12 for a per-year trend from monthly data). Default 1.
#' @param unit Name of one time unit (e.g. "month").
#' @param period_label Label for the trend period (e.g. "year"). Default = unit.
#' @param outcome Name of the outcome for the prose.
#' @param ... Styling args (alpha, markup).
#' @return An \code{azul_interpretation} object.
#' @examples
#' set.seed(1); n <- 120
#' d <- data.frame(time = 1:n, smokban = as.integer(1:n > 60), pop = 1e5)
#' lp <- -6 + 0.002*d$time - 0.15*d$smokban - 0.004*d$smokban*d$time
#' d$aces <- rpois(n, exp(lp + log(d$pop)))
#' m <- glm(aces ~ offset(log(pop)) + smokban*time, family = quasipoisson, data = d)
#' interpret_its(m, level = "smokban", trend = "time", period = 12,
#'               unit = "month", period_label = "year", outcome = "acute coronary events")
#' @export
interpret_its <- function(model, level, trend = "time", slope = NULL, period = 1,
                          unit = "time unit", period_label = NULL, outcome = "the outcome", ...) {
  a <- azul_args(...)
  if (is.null(period_label)) period_label <- unit
  b <- stats::coef(model); V <- stats::vcov(model); nm <- names(b)
  if (!level %in% nm) stop("Level term '", level, "' not found in the model.", call. = FALSE)
  if (!trend %in% nm) stop("Trend term '", trend, "' not found in the model.", call. = FALSE)
  if (is.null(slope)) {
    cand <- c(paste(level, trend, sep = ":"), paste(trend, level, sep = ":"))
    slope <- cand[cand %in% nm][1]
    if (is.na(slope)) slope <- NULL
  }
  z <- stats::qnorm(1 - a$alpha / 2)
  se <- sqrt(diag(V))
  fam <- tryCatch(stats::family(model)$family, error = function(e) "gaussian")
  link <- tryCatch(stats::family(model)$link, error = function(e) "identity")
  ratio <- link == "log"
  overdisp <- grepl("quasi", fam)

  eff <- function(coefname, mult = 1) {
    est <- b[[coefname]] * mult; s <- se[[coefname]] * abs(mult)
    lo <- est - z * s; hi <- est + z * s
    if (ratio) c(exp(est), exp(lo), exp(hi)) else c(est, lo, hi)
  }
  meas <- if (ratio) "IRR" else "coefficient"

  ## pre-intervention trend (per period)
  tr <- eff(trend, period)
  tr_pct <- (tr[1] - 1) * 100
  trend_txt <- if (ratio)
    paste0("Before the intervention, ", outcome, " was changing by a factor of ", fmt_num(tr[1]),
           " per ", period_label, " (", meas, " ", fmt_num(tr[1]), ", 95% CI: ", fmt_num(tr[2]), ", ",
           fmt_num(tr[3]), "), i.e. an underlying ", fmt_num(abs(tr_pct), 1), "% ",
           if (tr[1] >= 1) "increase" else "decrease", " per ", period_label, ".")
  else
    paste0("Before the intervention, ", outcome, " was changing by ", fmt_num(tr[1]),
           " units per ", period_label, " (95% CI: ", fmt_num(tr[2]), ", ", fmt_num(tr[3]), ").")

  ## level (step) change
  lv <- eff(level)
  p_lv <- 2 * stats::pnorm(abs(b[[level]] / se[[level]]), lower.tail = FALSE)
  lv_pct <- (lv[1] - 1) * 100
  level_txt <- if (ratio)
    paste0("Immediately after the intervention there was a level (step) change: the rate changed by a factor of ",
           fmt_num(lv[1]), " (", meas, " ", fmt_num(lv[1]), ", 95% CI: ", fmt_num(lv[2]), ", ", fmt_num(lv[3]),
           "; ", fmt_p(p_lv, a$markup), "), corresponding to an immediate ", fmt_num(abs(lv_pct), 1), "% ",
           if (lv[1] >= 1) "increase" else "decrease", " in ", outcome, ".")
  else
    paste0("Immediately after the intervention there was a level (step) change of ", fmt_num(lv[1]),
           " units (95% CI: ", fmt_num(lv[2]), ", ", fmt_num(lv[3]), "; ", fmt_p(p_lv, a$markup), ").")

  paras <- c(
    paste0("An interrupted time series analysis using segmented ",
           if (overdisp) "quasi-Poisson" else if (ratio) "Poisson" else "linear",
           " regression was used to evaluate the effect of the intervention on ", outcome, "."),
    trend_txt, level_txt)

  ## slope change (if present)
  if (!is.null(slope)) {
    sl <- eff(slope)
    p_sl <- 2 * stats::pnorm(abs(b[[slope]] / se[[slope]]), lower.tail = FALSE)
    post <- eff2 <- NULL
    if (ratio) {
      post_est <- (b[[trend]] + b[[slope]]) * period
      post_rr <- exp(post_est)
      slope_txt <- paste0("The post-intervention trend changed relative to the pre-intervention trend by a factor of ",
        fmt_num(sl[1]), " per ", unit, " (", meas, " ", fmt_num(sl[1]), ", 95% CI: ", fmt_num(sl[2]), ", ",
        fmt_num(sl[3]), "; ", fmt_p(p_sl, a$markup), "). ",
        "The resulting post-intervention trend was about ", fmt_num(post_rr), " per ", period_label, ".")
    } else {
      slope_txt <- paste0("The post-intervention slope changed by ", fmt_num(sl[1]), " units per ", unit,
        " relative to the pre-intervention slope (95% CI: ", fmt_num(sl[2]), ", ", fmt_num(sl[3]), "; ",
        fmt_p(p_sl, a$markup), ").")
    }
    paras <- c(paras, slope_txt)
  } else {
    paras <- c(paras, paste0("This was a level-change (step) model; no change in slope was included, ",
      "so the intervention effect is interpreted as an immediate change in level with the trend assumed unchanged."))
  }

  assum <- c(
    "The counterfactual assumption: the pre-intervention trend would have continued unchanged without the intervention.",
    if (overdisp) "Overdispersion was handled with a quasi-Poisson model (variance proportional to the mean)."
      else if (ratio) "Check for overdispersion (deviance/df); use quasi-Poisson or negative binomial if present." else NULL,
    "Check for autocorrelation of residuals (plot residuals over time, ACF and PACF; adjust if present).",
    "Adjust for seasonality where relevant (e.g. harmonic terms or month indicators).",
    "Ensure enough time points before and after the interruption, and no other concurrent interventions.",
    "If a change-in-slope term is added, compare models with an F-test (accounting for overdispersion) or likelihood-ratio test.")
  new_interpretation("Interrupted time series (segmented regression)", paras, stats::na.omit(assum),
    estimates = data.frame(term = c(trend, level, if (!is.null(slope)) slope),
      estimate = c(tr[1], lv[1], if (!is.null(slope)) eff(slope)[1]),
      conf.low = c(tr[2], lv[2], if (!is.null(slope)) eff(slope)[2]),
      conf.high = c(tr[3], lv[3], if (!is.null(slope)) eff(slope)[3])))
}

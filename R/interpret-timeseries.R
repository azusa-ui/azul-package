# ---------------------------------------------------------------------------
# Time series: ARIMA / seasonal ARIMA models (stats::arima, forecast::Arima).
# Stationarity and white-noise tests (ADF, KPSS, Ljung-Box) are htest objects
# handled in interpret.htest.
# Reference: Hyndman RJ, Athanasopoulos G. Forecasting: Principles and Practice
# (3rd ed), OTexts; Box, Jenkins & Reinsel, Time Series Analysis.
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @export
interpret.Arima <- function(object, ...) {
  a <- azul_args(...)
  outcome <- a$outcome %||% "the series"
  arma <- object$arma  # c(p, q, P, Q, period, d, D)
  p <- arma[1]; q <- arma[2]; P <- arma[3]; Q <- arma[4]; s <- arma[5]; d <- arma[6]; D <- arma[7]
  seasonal <- (P + D + Q) > 0
  order_txt <- paste0("ARIMA(", p, ",", d, ",", q, ")",
                      if (seasonal) paste0("(", P, ",", D, ",", Q, ")[", s, "]") else "")

  # coefficients with Wald tests
  co <- object$coef
  se <- tryCatch(sqrt(diag(object$var.coef)), error = function(e) rep(NA_real_, length(co)))
  z <- stats::qnorm(1 - a$alpha / 2)
  sig <- vapply(seq_along(co), function(i) {
    est <- co[i]; s_i <- se[i]
    pv <- if (!is.na(s_i) && s_i > 0) 2 * stats::pnorm(abs(est / s_i), lower.tail = FALSE) else NA_real_
    paste0(names(co)[i], " = ", fmt_num(est, 3),
           if (!is.na(s_i)) paste0(" (95% CI: ", fmt_num(est - z*s_i, 3), ", ", fmt_num(est + z*s_i, 3),
                                   "; ", fmt_p(pv, a$markup), ")") else "")
  }, character(1))

  diff_txt <- if (d + D > 0)
    paste0("The series was differenced ", d, " time", if (d != 1) "s" else "",
           if (D > 0) paste0(" (plus ", D, " seasonal difference", if (D != 1) "s" else "", ")") else "",
           " to achieve stationarity.") else "The series was modelled without differencing (assumed stationary)."

  paras <- c(
    paste0("An ", order_txt, " model was fitted to ", outcome, "."),
    diff_txt,
    paste0("The model comprised ", p, " autoregressive (AR) and ", q, " moving-average (MA) term",
           if (max(p, q) != 1) "s" else "",
           if (seasonal) paste0(", with ", P, " seasonal AR and ", Q, " seasonal MA term",
                                if (max(P, Q) != 1) "s" else "", " at period ", s) else "", "."),
    if (length(co)) paste0("Estimated coefficients: ", paste(sig, collapse = "; "), ".") else NULL,
    paste0("Model fit: AIC = ", fmt_num(object$aic, 1),
           if (!is.null(object$bic)) paste0(", BIC = ", fmt_num(object$bic, 1)) else "",
           " (lower is better when comparing models on the same differenced series)."))

  new_interpretation(paste0("Time series model (", order_txt, ")"), stats::na.omit(unlist(paras)),
    c("Confirm the residuals are white noise (Ljung-Box test non-significant; ACF/PACF of residuals).",
      "Confirm stationarity of the differenced series (ADF / KPSS tests) before selecting p, d, q.",
      "Compare candidate models by AIC/BIC only when fitted to the same order of differencing.",
      "AIC/BIC measure in-sample fit; assess forecasts on a held-out period.",
      "Reference: Hyndman & Athanasopoulos, Forecasting: Principles and Practice; Box & Jenkins."))
}

#' @rdname interpret
#' @export
interpret.arima <- function(object, ...) interpret.Arima(object, ...)

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


#' Suggest ARIMA/SARIMA orders from the data
#'
#' Reports the data-driven building blocks for a Box-Jenkins model: whether a
#' variance-stabilising transformation is indicated (Box-Cox lambda), how many
#' regular and seasonal differences are needed (unit-root tests), and a starting
#' model chosen by \code{forecast::auto.arima}. Use it alongside the manual
#' ACF/PACF reading from \code{\link{azul_plot}} rather than as a replacement.
#'
#' @param x A time series (\code{ts}) or numeric vector.
#' @param ... Styling args (markup, alpha).
#' @return An \code{azul_interpretation} object.
#' @examples
#' azul_arima_suggest(AirPassengers)
#' @export
azul_arima_suggest <- function(x, ...) {
  a <- azul_args(...)
  if (!requireNamespace("forecast", quietly = TRUE))
    stop("Package 'forecast' is required for azul_arima_suggest().", call. = FALSE)
  if (!stats::is.ts(x)) x <- stats::as.ts(x)
  f <- stats::frequency(x)
  seasonal <- f > 1

  lambda <- tryCatch(forecast::BoxCox.lambda(x), error = function(e) NA_real_)
  transf <- if (is.na(lambda)) "A Box-Cox transformation could not be assessed."
    else if (abs(lambda) < 0.25)
      paste0("The Box-Cox lambda was ", fmt_num(lambda, 2),
             " (close to 0), suggesting a log transformation to stabilise the variance.")
    else if (abs(lambda - 1) < 0.25)
      paste0("The Box-Cox lambda was ", fmt_num(lambda, 2),
             " (close to 1), so no transformation is needed.")
    else paste0("The Box-Cox lambda was ", fmt_num(lambda, 2),
             ", suggesting a power transformation (x^lambda) may stabilise the variance.")

  d <- tryCatch(forecast::ndiffs(x), error = function(e) NA_integer_)
  D <- if (seasonal) tryCatch(forecast::nsdiffs(x), error = function(e) 0L) else 0L
  fit <- tryCatch(forecast::auto.arima(x), error = function(e) NULL)
  order_txt <- if (!is.null(fit)) {
    ar <- fit$arma
    paste0("ARIMA(", ar[1], ",", ar[6], ",", ar[2], ")",
           if ((ar[3] + ar[7] + ar[4]) > 0) paste0("(", ar[3], ",", ar[7], ",", ar[4], ")[", ar[5], "]") else "")
  } else "unavailable"

  paras <- c(
    paste0("Data-driven identification for this series (frequency ", f,
           if (seasonal) ", i.e. seasonal" else ", i.e. non-seasonal", ")."),
    transf,
    paste0("Unit-root testing suggested ", d, " regular difference",
           if (!is.na(d) && d != 1) "s" else "",
           if (seasonal) paste0(" and ", D, " seasonal difference", if (D != 1) "s" else "",
                                " at period ", f) else "",
           " to achieve stationarity (this is d",
           if (seasonal) " and D" else "", ")."),
    paste0("An automatic search (auto.arima) selected a starting model of ", order_txt,
           if (!is.null(fit)) paste0(" (AIC = ", fmt_num(fit$aic, 1), ")") else "", "."))
  new_interpretation("ARIMA/SARIMA order suggestion", paras,
    c("These are automated suggestions; confirm them by reading the ACF and PACF (azul_plot(series) and azul_plot(differenced, type = 'acf')).",
      "Compare a few candidate orders by AIC/BIC on the same order of differencing.",
      "After fitting, verify the residuals are white noise (azul_plot(model); Ljung-Box test).",
      "Reference: Hyndman & Athanasopoulos, Forecasting: Principles and Practice."))
}

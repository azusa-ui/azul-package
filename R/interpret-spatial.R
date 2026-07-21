# ---------------------------------------------------------------------------
# Spatial epidemiology. Interpretation follows long-established, widely-cited
# conventions (Moran 1950; Geary 1954; Anselin 1995 LISA; Getis & Ord 1992;
# Breslow & Day SMR; Anselin spatial econometrics). Global Moran's I and
# Geary's C are htest objects (see interpret.htest). Here: SMR, local Moran
# (LISA), Getis-Ord Gi*, and spatial lag / error regression.
# ---------------------------------------------------------------------------

#' Interpret a standardised morbidity/mortality ratio (SMR)
#'
#' @param observed Observed count(s) of events.
#' @param expected Expected count(s) (e.g. from indirect standardisation).
#' @param area Optional area label(s).
#' @param conf.level Confidence level (default 0.95).
#' @param ... Styling args.
#' @return An \code{azul_interpretation} object.
#' @examples
#' interpret_smr(observed = 120, expected = 90, area = "District A")
#' @export
interpret_smr <- function(observed, expected, area = NULL, conf.level = 0.95, ...) {
  a <- azul_args(...)
  z <- stats::qnorm(1 - (1 - conf.level) / 2)
  n <- length(observed)
  if (is.null(area)) area <- if (n == 1) "the area" else paste("area", seq_len(n))
  lines <- vapply(seq_len(n), function(i) {
    O <- observed[i]; E <- expected[i]; smr <- O / E
    # Byar's approximation to the exact Poisson CI for the count, scaled by E
    lo <- O * (1 - 1/(9*O) - z/(3*sqrt(O)))^3 / E
    hi <- (O + 1) * (1 - 1/(9*(O+1)) + z/(3*sqrt(O+1)))^3 / E
    dir <- if (smr > 1) "excess" else if (smr < 1) "deficit" else "no departure"
    sig <- if (lo > 1 || hi < 1) "statistically significant" else "not statistically significant"
    paste0("In ", area[i], ", ", O, " events were observed against ", fmt_num(E, 1),
           " expected, giving an SMR of ", fmt_num(smr, 2), " (95% CI: ", fmt_num(lo, 2), ", ",
           fmt_num(hi, 2), "), a ", fmt_num(abs(smr - 1) * 100, 1), "% ", dir,
           " that was ", sig, ".")
  }, character(1))
  new_interpretation("Standardised morbidity/mortality ratio (SMR)", lines,
    c("SMR compares observed with expected events after indirect standardisation; an SMR of 1 means as expected.",
      "SMRs from different areas are not directly comparable if the reference standards differ.",
      "For small areas, SMRs are unstable; consider smoothing (e.g. empirical/full Bayes) before mapping.",
      "Confidence intervals use Byar's approximation. Reference: Waller & Gotway (2004), ch. 2; Breslow & Day (1987)."))
}

#' Interpret local Moran's I (LISA) results
#'
#' @param x A \code{localmoran} result (matrix/data frame) with an \code{Ii}
#'   column and a p-value column, or a data frame with those.
#' @param quadr Optional factor/character vector of cluster types per location
#'   (e.g. "High-High", "Low-Low", "High-Low", "Low-High"), as returned by
#'   recent \code{spdep::localmoran} attributes.
#' @param alpha Significance level for flagging locations.
#' @param ... Styling args.
#' @return An \code{azul_interpretation} object.
#' @export
interpret_lisa <- function(x, quadr = NULL, alpha = 0.05, ...) {
  a <- azul_args(...)
  df <- as.data.frame(x)
  pcol <- grep("^Pr|p.value|Pr\\(", names(df), value = TRUE)[1]
  if (is.na(pcol)) stop("Could not find a p-value column in the LISA result.", call. = FALSE)
  sig <- df[[pcol]] < alpha
  nsig <- sum(sig, na.rm = TRUE)
  body <- paste0("A local Moran's I (LISA) analysis identified ", nsig, " of ", nrow(df),
                 " locations with statistically significant local spatial association (P < ", alpha, ").")
  if (!is.null(quadr)) {
    q <- as.character(quadr)[sig]
    tb <- table(q)
    part <- paste(vapply(names(tb), function(k) paste0(tb[[k]], " ", k), character(1)), collapse = ", ")
    body <- paste0(body, " Among the significant locations, the cluster types were: ", part,
      ". High-High and Low-Low denote clusters of similar values; High-Low and Low-High denote spatial outliers.")
  } else {
    body <- paste0(body, " Classify each significant location as High-High or Low-Low (clusters) or High-Low / Low-High (spatial outliers) using the value and its spatial lag.")
  }
  new_interpretation("Local Moran's I (LISA)", body,
    c("Adjust p-values for multiple testing across locations (e.g. FDR); LISA runs one test per area.",
      "Results depend on the spatial weights and on the conditioning/permutation scheme.",
      "Reference: Anselin (1995), LISA; Waller & Gotway (2004), ch. 7."))
}

#' Interpret Getis-Ord Gi* hot-spot z-scores
#'
#' @param z Numeric vector of Gi* z-scores (e.g. from \code{spdep::localG}).
#' @param alpha Significance level (two-sided).
#' @param ... Styling args.
#' @return An \code{azul_interpretation} object.
#' @export
interpret_getis <- function(z, alpha = 0.05, ...) {
  a <- azul_args(...)
  zc <- stats::qnorm(1 - alpha / 2)
  hot <- sum(z > zc, na.rm = TRUE); cold <- sum(z < -zc, na.rm = TRUE)
  body <- paste0("A Getis-Ord Gi* hot-spot analysis identified ", hot, " statistically significant hot spot",
    if (hot != 1) "s" else "", " (high-value clusters, z > ", fmt_num(zc, 2), ") and ", cold,
    " cold spot", if (cold != 1) "s" else "", " (low-value clusters, z < ", fmt_num(-zc, 2),
    ") out of ", length(z), " locations.")
  new_interpretation("Getis-Ord Gi* (hot-spot analysis)", body,
    c("Gi* uses a fixed distance band or contiguity; state the neighbourhood definition.",
      "Consider a multiple-comparison correction (e.g. FDR) across locations.",
      "Reference: Getis & Ord (1992); Waller & Gotway (2004), ch. 7."))
}

#' @rdname interpret
#' @export
interpret.Sarlm <- function(object, ...) {
  a <- azul_args(...)
  type <- tryCatch(object$type, error = function(e) "spatial")
  outcome <- a$outcome %||% "the outcome"
  paras <- paste0("A spatial ", switch(type, lag = "lag (SAR)", error = "error (SEM)",
                  sac = "lag + error (SAC)", type), " regression was fitted for ", outcome,
                  " to account for spatial dependence.")
  # spatial parameter: rho (lag) or lambda (error)
  if (!is.null(object$rho)) {
    se <- tryCatch(object$rho.se, error = function(e) NA_real_)
    p <- if (!is.na(se)) 2 * stats::pnorm(abs(object$rho / se), lower.tail = FALSE) else NA_real_
    paras <- c(paras, paste0("The spatial autoregressive parameter (rho) was ", fmt_num(object$rho, 3),
      if (!is.na(se)) paste0(" (95% CI: ", fmt_num(object$rho - 1.96*se, 3), ", ",
        fmt_num(object$rho + 1.96*se, 3), "; ", fmt_p(p, a$markup), ")") else "",
      ", indicating ", if (!is.na(p) && p < a$alpha) "significant " else "no significant ",
      "spatial spillover: outcomes in an area are associated with outcomes in neighbouring areas."))
  }
  if (!is.null(object$lambda)) {
    se <- tryCatch(object$lambda.se, error = function(e) NA_real_)
    p <- if (!is.na(se)) 2 * stats::pnorm(abs(object$lambda / se), lower.tail = FALSE) else NA_real_
    paras <- c(paras, paste0("The spatial error parameter (lambda) was ", fmt_num(object$lambda, 3),
      if (!is.na(se)) paste0(" (", fmt_p(p, a$markup), ")") else "",
      ", indicating ", if (!is.na(p) && p < a$alpha) "significant " else "no significant ",
      "spatial autocorrelation in the residuals (unmeasured spatially-structured factors)."))
  }
  paras <- c(paras, "Interpret the regression coefficients as in ordinary regression, but note that in a spatial lag model a predictor also has indirect (spillover) effects; report direct, indirect and total impacts where available.")
  new_interpretation(paste0("Spatial ", type, " regression"), paras,
    c("State the spatial weights matrix (contiguity/distance, row-standardisation).",
      "Choose lag vs error using Lagrange Multiplier tests (LMlag, LMerror and their robust versions).",
      "For a lag model, report impact measures (direct/indirect/total), not just the raw coefficients.",
      "Reference: Anselin (1988), Spatial Econometrics; Waller & Gotway (2004), ch. 9."))
}

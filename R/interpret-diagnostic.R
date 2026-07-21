# ---------------------------------------------------------------------------
# Diagnostic test accuracy and model discrimination:
#   - interpret_diagnostic(): sensitivity/specificity/PPV/NPV/accuracy/LRs
#     from a 2x2 table (or counts), with Wilson 95% CIs.
#   - interpret.confusionMatrix(): the same, from a caret confusionMatrix.
#   - interpret.roc(): AUC with 95% CI and a discrimination verdict (pROC).
# Hosmer-Lemeshow goodness-of-fit is handled in interpret.htest.
# ---------------------------------------------------------------------------

# Wilson score interval for a proportion x / n
.wilson <- function(x, n, conf = 0.95) {
  if (is.na(n) || n == 0) return(c(NA_real_, NA_real_))
  z <- stats::qnorm(1 - (1 - conf) / 2); phat <- x / n
  denom <- 1 + z^2 / n
  centre <- (phat + z^2 / (2 * n)) / denom
  half <- z * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2)) / denom
  c(centre - half, centre + half)
}

.pctci <- function(x, n, markup) {
  ci <- .wilson(x, n)
  paste0(fmt_num(100 * x / n, 1), "% (95% CI: ", fmt_num(100 * ci[1], 1), ", ",
         fmt_num(100 * ci[2], 1), "%)")
}

# Build the metric sentences from the four cell counts.
.diag_metrics <- function(tp, fp, fn, tn, a, extra = NULL) {
  N <- tp + fp + fn + tn
  sens_n <- tp + fn; spec_n <- tn + fp; ppv_n <- tp + fp; npv_n <- tn + fn
  sens <- tp / sens_n; spec <- tn / spec_n
  lrp <- if (spec < 1) sens / (1 - spec) else NA_real_
  lrn <- if (spec > 0) (1 - sens) / spec else NA_real_
  s <- c(
    paste0("The sensitivity was ", .pctci(tp, sens_n, a$markup),
           " and the specificity was ", .pctci(tn, spec_n, a$markup), "."),
    paste0("The positive predictive value was ", .pctci(tp, ppv_n, a$markup),
           " and the negative predictive value was ", .pctci(tn, npv_n, a$markup),
           " (both depend on the prevalence in this sample)."),
    paste0("The overall accuracy was ", .pctci(tp + tn, N, a$markup), "."),
    paste0("The positive likelihood ratio was ", if (is.na(lrp)) "undefined" else fmt_num(lrp),
           " and the negative likelihood ratio was ", if (is.na(lrn)) "undefined" else fmt_num(lrn), ".")
  )
  c(s, extra)
}

#' Interpret a 2x2 diagnostic test table
#'
#' @param x A 2x2 table or matrix of counts. By default rows are the test
#'   result and columns are the true (gold-standard) status, each ordered
#'   positive then negative. Set \code{layout} if your table differs.
#' @param tp,fp,fn,tn Alternatively, supply the four counts directly.
#' @param layout Cell layout of \code{x}: "test_rows" (default; rows = test,
#'   cols = truth, positive first) or "truth_rows".
#' @param test Name of the index test (for the prose).
#' @param ... Styling args (markup, ci_sep).
#' @return An \code{azul_interpretation} object.
#' @export
interpret_diagnostic <- function(x = NULL, tp = NULL, fp = NULL, fn = NULL, tn = NULL,
                                 layout = c("test_rows", "truth_rows"),
                                 test = "the index test", ...) {
  a <- azul_args(...)
  layout <- match.arg(layout)
  if (is.null(tp)) {
    m <- as.matrix(x)
    if (!all(dim(m) == c(2, 2))) stop("x must be a 2x2 table of counts.", call. = FALSE)
    if (layout == "test_rows") { tp <- m[1,1]; fp <- m[1,2]; fn <- m[2,1]; tn <- m[2,2] }
    else                        { tp <- m[1,1]; fn <- m[1,2]; fp <- m[2,1]; tn <- m[2,2] }
  }
  lead <- paste0("The diagnostic accuracy of ", test,
                 " was evaluated against the reference standard (n = ", tp + fp + fn + tn, ").")
  paras <- c(lead, .diag_metrics(tp, fp, fn, tn, a))
  new_interpretation("Diagnostic test accuracy", paras,
    c("Predictive values are prevalence-dependent and apply only at this sample's prevalence.",
      "Likelihood ratios are prevalence-independent (LR+ > 10 or LR- < 0.1 give strong diagnostic shifts).",
      "Confirm the reference (gold) standard and the chosen cut-off; report the cut-off used."),
    notes = paste0("Assumed layout: ", if (layout == "test_rows") "rows = test result, columns = true status"
                   else "rows = true status, columns = test result", ", positive level first. Set 'layout' if this differs."))
}

#' @rdname interpret
#' @export
interpret.confusionMatrix <- function(object, ...) {
  a <- azul_args(...)
  tb <- object$table                      # rows = Prediction, cols = Reference
  pos <- object$positive %||% rownames(tb)[1]
  lv <- rownames(tb); neg <- setdiff(lv, pos)[1]
  tp <- tb[pos, pos]; fp <- tb[pos, neg]; fn <- tb[neg, pos]; tn <- tb[neg, neg]
  kap <- tryCatch(object$overall[["Kappa"]], error = function(e) NA_real_)
  accCI <- tryCatch(object$overall[c("AccuracyLower","AccuracyUpper")], error = function(e) c(NA,NA))
  extra <- c(
    paste0("Cohen's kappa was ", fmt_num(kap, 3),
           ", indicating ", .kappa_verdict(kap), " agreement beyond chance."))
  lead <- paste0("Classification performance was summarised against the reference labels (positive class = '",
                 pos, "').")
  paras <- c(lead, .diag_metrics(tp, fp, fn, tn, a, extra = extra))
  new_interpretation("Classification / diagnostic performance (confusion matrix)", paras,
    c("Predictive values depend on the prevalence in the evaluation sample.",
      "Report the classification threshold; the default 0.5 is rarely optimal.",
      "Prefer an independent test set or cross-validation over resubstitution accuracy."))
}

.kappa_verdict <- function(k) {
  if (is.na(k)) return("undetermined")
  if (k < 0.20) "slight" else if (k < 0.40) "fair" else if (k < 0.60) "moderate"
  else if (k < 0.80) "substantial" else "almost perfect"
}

#' @rdname interpret
#' @export
interpret.roc <- function(object, ...) {
  a <- azul_args(...)
  if (!requireNamespace("pROC", quietly = TRUE))
    stop("Package 'pROC' is required to interpret a roc object.", call. = FALSE)
  auc <- as.numeric(pROC::auc(object))
  ci  <- tryCatch(as.numeric(pROC::ci.auc(object)), error = function(e) c(NA, auc, NA))
  verdict <-
    if (auc < 0.6) "poor (little better than chance)"
    else if (auc < 0.7) "poor to fair"
    else if (auc < 0.8) "acceptable"
    else if (auc < 0.9) "excellent"
    else "outstanding"
  body <- paste0(
    "The area under the ROC curve (AUC) was ", fmt_num(auc, 3), " (95% CI: ",
    fmt_num(ci[1], 3), ", ", fmt_num(ci[3], 3), "), indicating ", verdict, " discrimination. ",
    "The model correctly ranked a randomly chosen case above a randomly chosen non-case ",
    fmt_num(auc * 100, 1), "% of the time.")
  new_interpretation("ROC / AUC (discrimination)", body,
    c("AUC thresholds (Hosmer-Lemeshow): 0.7 to 0.8 acceptable, 0.8 to 0.9 excellent, >= 0.9 outstanding.",
      "AUC measures discrimination only; assess calibration separately (e.g. Hosmer-Lemeshow, calibration plot).",
      "Report the AUC on validation or cross-validated data, not only the training sample."))
}

# ---------------------------------------------------------------------------
# Linear / generalised linear mixed models: lme4 (lmerMod, glmerMod) and
# nlme (lme). Fixed-effect interpretation plus ICC and random-effect note.
# ---------------------------------------------------------------------------

# Extract a fixed-effects table (term, estimate, conf.low, conf.high, p.value)
# straight from the model summary, without needing broom.mixed. Works for
# lme4 (lmerMod/glmerMod) and nlme (lme).
.fixef_table <- function(object, exponentiate = FALSE) {
  cf <- tryCatch(stats::coef(summary(object)), error = function(e) NULL)
  if (is.null(cf) || is.null(dim(cf))) return(NULL)
  cn <- colnames(cf)
  est_c <- intersect(c("Estimate", "Value"), cn)[1]
  se_c  <- intersect(c("Std. Error", "Std.Error"), cn)[1]
  if (is.na(est_c) || is.na(se_c)) return(NULL)
  est <- cf[, est_c]; se <- cf[, se_c]
  pc <- grep("^Pr\\(|p-value|Pr\\.\\.\\.|^p\\.value$", cn, value = TRUE)
  p <- if (length(pc)) cf[, pc[1]] else 2 * stats::pnorm(abs(est / se), lower.tail = FALSE)
  z <- stats::qnorm(0.975)
  lo <- est - z * se; hi <- est + z * se
  if (exponentiate) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
  out <- data.frame(term = rownames(cf), estimate = est, conf.low = lo,
                    conf.high = hi, p.value = p, stringsAsFactors = FALSE)
  drop_intercept(out)
}

.mixed_body <- function(object, a, exponentiate = FALSE, effword = NULL) {
  outcome <- a$outcome %||% "the outcome"
  tab <- .fixef_table(object, exponentiate = exponentiate)
  icc_txt <- ""
  if (requireNamespace("performance", quietly = TRUE)) {
    icc <- tryCatch(performance::icc(object)$ICC_adjusted, error = function(e) NA_real_)
    if (!is.na(icc)) icc_txt <- paste0(" The adjusted intraclass correlation coefficient (ICC) was ",
                                       fmt_num(icc, 3), ", indicating the share of variance attributable to clustering.")
  }
  sentences <- if (!is.null(tab) && nrow(tab)) vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    if (exponentiate)
      paste0("For ", r$term, ", the fixed effect corresponded to ", fmt_num(r$estimate), " times ", effword,
             " (", fmt_ci(r$estimate, r$conf.low, r$conf.high), "; ", fmt_p(r$p.value, a$markup),
             "), holding the other fixed effects constant.")
    else
      paste0("For ", r$term, ", the fixed-effect estimate was ",
             fmt_ci(r$estimate, r$conf.low, r$conf.high, label = "b ="), " (", fmt_p(r$p.value, a$markup),
             "), holding the other fixed effects constant.")
  }, character(1)) else "Fixed-effect estimates could not be extracted automatically; read them from summary(model)."
  list(tab = tab, body = c(sentences, if (nzchar(icc_txt)) trimws(icc_txt) else NULL))
}

#' @rdname interpret
#' @export
interpret.lmerMod <- function(object, ...) {
  a <- azul_args(...)
  res <- .mixed_body(object, a, exponentiate = FALSE)
  body <- c("A linear mixed-effects model with random effects for the clustering structure was fitted.", res$body)
  assum <- c("Linearity and homoscedasticity of residuals.",
             "P values for lme4 fixed effects are large-sample (Wald) approximations unless lmerTest/Satterthwaite is used.",
             "Normality of both residuals and random effects.",
             "Correctly specified random-effects structure (random intercept vs. random slope; compare with a likelihood-ratio test).",
             "Justify the mixed model over ordinary regression when ICC > ~0.05.")
  new_interpretation("Linear mixed-effects model", body, assum, estimates = res$tab)
}

#' @rdname interpret
#' @export
interpret.glmerMod <- function(object, ...) {
  a <- azul_args(...)
  fam <- tryCatch(stats::family(object)$family, error = function(e) "binomial")
  is_bin <- fam == "binomial"
  res <- .mixed_body(object, a, exponentiate = TRUE,
                     effword = if (is_bin) "the odds of the outcome" else "the expected count")
  body <- c(paste0("A generalised linear mixed-effects model (", fam,
                 " family) was fitted with random effects for clustering. ",
                 "Exponentiated fixed effects are ", if (is_bin) "odds ratios" else "rate ratios",
                 " with a conditional (cluster-specific) interpretation."), res$body)
  assum <- c("Correct family and link.",
             "Correctly specified random-effects structure.",
             "Normality of random effects.",
             "Cluster-specific (conditional) interpretation of fixed effects.")
  new_interpretation("Generalised linear mixed-effects model", body, assum, estimates = res$tab)
}

#' @rdname interpret
#' @export
interpret.lme <- function(object, ...) {
  a <- azul_args(...)
  res <- .mixed_body(object, a, exponentiate = FALSE)
  body <- c("A linear mixed-effects model (nlme) with random effects for the clustering structure was fitted.", res$body)
  assum <- c("Linearity and homoscedasticity (inspect standardised residuals).",
             "Normality of residuals and random effects.",
             "Correct random-effects and, if specified, correlation/variance structure.")
  new_interpretation("Linear mixed-effects model (nlme)", body, assum, estimates = res$tab)
}

# ---------------------------------------------------------------------------
# Correlated / clustered categorical (GEE: geepack::geeglm) and zero-inflated
# count models (pscl::zeroinfl). Advanced categorical module.
# ---------------------------------------------------------------------------

# Build a tidy exponentiated table from an estimate/std.error link-scale summary
.link_ci_table <- function(terms, est, se, p, z = stats::qnorm(0.975), exponentiate = TRUE) {
  lo <- est - z * se; hi <- est + z * se
  if (exponentiate) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
  data.frame(term = terms, estimate = est, conf.low = lo, conf.high = hi,
             p.value = p, stringsAsFactors = FALSE)
}

#' @rdname interpret
#' @export
interpret.geeglm <- function(object, ...) {
  a <- azul_args(...)
  outcome <- a$outcome %||% all.vars(stats::formula(object))[1]
  fam <- tryCatch(object$family$family, error = function(e) "gaussian")
  link <- tryCatch(object$family$link, error = function(e) "identity")
  corstr <- tryCatch(object$corstr, error = function(e) "independence")
  sm <- summary(object)$coefficients
  terms <- rownames(sm)
  est <- sm[, "Estimate"]; se <- sm[, "Std.err"]
  pcol <- grep("^Pr", colnames(sm)); p <- if (length(pcol)) sm[, pcol[1]] else NA_real_
  is_logit <- fam == "binomial"; is_count <- fam == "poisson" || link == "log"
  expo <- is_logit || is_count
  tab <- drop_intercept(.link_ci_table(terms, est, se, p, exponentiate = expo))
  multiple <- nrow(tab) > 1
  eff <- if (is_logit) list(lab = "OR", word = "the odds of")
         else if (is_count) list(lab = "IRR", word = "the expected count of")
         else list(lab = "b", word = NULL)
  eff_lab <- if (expo) paste0(if (multiple) "Adjusted " else "Crude ", eff$lab) else "b"
  sentences <- vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    if (is.null(eff$word))
      paste0("For ", r$term, ", the estimate was ", fmt_ci(r$estimate, r$conf.low, r$conf.high, label = "b ="),
             " (", fmt_p(r$p.value, a$markup), ")")
    else
      paste0(r$term, " was associated with ", fmt_num(r$estimate), " times ", eff$word, " ", outcome,
             " (", eff_lab, " ", fmt_ci(r$estimate, r$conf.low, r$conf.high), "; ", fmt_p(r$p.value, a$markup), ")")
  }, character(1))
  body <- paste0("A generalised estimating equations (GEE) model (", fam, " family, ",
                 corstr, " working correlation) was used to account for within-cluster correlation in ",
                 outcome, ". Population-averaged (marginal) effects with robust (sandwich) standard errors are reported. ",
                 paste(sentences, collapse = ". "),
                 if (multiple) ", after adjusting for the other predictors." else ".")
  assum <- c("Population-averaged (marginal) interpretation, not cluster-specific.",
             "Robust standard errors are consistent even if the working correlation is misspecified; choose the working correlation (exchangeable, AR-1, ...) to reflect the design.",
             "Requires a reasonable number of clusters (rule of thumb >= 30 to 40) for valid robust inference.",
             "Missing data assumed missing completely at random (MCAR).")
  new_interpretation("GEE (population-averaged) model", body, assum, estimates = tab)
}

#' @rdname interpret
#' @export
interpret.zeroinfl <- function(object, ...) {
  a <- azul_args(...)
  outcome <- a$outcome %||% "the outcome"
  cf <- summary(object)$coefficients
  mk <- function(mat, expo) {
    terms <- rownames(mat)
    drop_intercept(.link_ci_table(terms, mat[, "Estimate"], mat[, "Std. Error"],
                                  mat[, ncol(mat)], exponentiate = expo))
  }
  ct <- mk(cf$count, TRUE)   # count model: IRR
  zt <- mk(cf$zero, TRUE)    # zero-inflation model: OR of being a structural zero
  count_s <- vapply(seq_len(nrow(ct)), function(i) {
    r <- ct[i, ]
    paste0(r$term, " (IRR ", fmt_ci(r$estimate, r$conf.low, r$conf.high), "; ", fmt_p(r$p.value, a$markup), ")")
  }, character(1))
  zero_s <- vapply(seq_len(nrow(zt)), function(i) {
    r <- zt[i, ]
    paste0(r$term, " (OR ", fmt_ci(r$estimate, r$conf.low, r$conf.high), "; ", fmt_p(r$p.value, a$markup), ")")
  }, character(1))
  dist <- object$dist
  body <- paste0("A zero-inflated ", dist, " model was fitted for ", outcome,
                 ", jointly modelling the count process and the excess (structural) zeros. ",
                 "In the count component, incidence rate ratios were: ", paste(count_s, collapse = "; "), ". ",
                 "In the zero-inflation component, the odds of being an excess zero were: ",
                 paste(zero_s, collapse = "; "), ".")
  assum <- c("Excess zeros beyond what the count distribution predicts (compare with a plain count model by Vuong test or AIC).",
             "Correct count distribution (Poisson vs negative binomial for overdispersion).",
             "The two components may share or differ in their predictors; state the specification.")
  new_interpretation(paste0("Zero-inflated ", dist, " model"), body, assum,
                     estimates = list(count = ct, zero = zt))
}

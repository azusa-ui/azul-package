# ---------------------------------------------------------------------------
# Survival: coxph, clogit, survfit (Kaplan-Meier), survdiff (log-rank),
# survreg (parametric AFT). Uses the survival package's summary structures.
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @export
interpret.coxph <- function(object, ...) {
  a <- azul_args(...)
  event <- a$outcome %||% "the event"
  multiple <- length(stats::coef(object)) > 1
  tab <- drop_intercept(coef_table(object, exponentiate = TRUE))
  eff_lab <- paste0(if (multiple) "Adjusted" else "Crude", " HR")
  sentences <- vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    dir <- if (!is.na(r$estimate) && r$estimate < 1) "lower" else "higher"
    paste0(r$term, " was associated with ", fmt_num(r$estimate),
           " times the hazard of ", event, " (a ", dir, " hazard; ", eff_lab, " ",
           fmt_ci(r$estimate, r$conf.low, r$conf.high), "; ", fmt_p(r$p.value, a$markup), ")")
  }, character(1))
  lead <- paste0(if (multiple) "Multiple " else "Simple ",
                 "Cox proportional hazards regression was used to model time to ", event, ".")
  ref_note <- "Categorical predictors are expressed relative to their reference level."
  comp <- .compose_with_interaction(object, a, lead, sentences, tail = ref_note)
  assum <- c("Proportional hazards: check with scaled Schoenfeld residuals (cox.zph), log-minus-log plots, or time-interaction terms.",
             "Correct functional form of continuous covariates (martingale residuals).",
             "Non-informative (independent) censoring.",
             "Adequate events per variable (>= 10).",
             comp$extra_assum)
  new_interpretation("Cox proportional hazards regression", comp$body, stats::na.omit(assum), estimates = tab)
}

#' @rdname interpret
#' @export
interpret.clogit <- function(object, ...) {
  a <- azul_args(...)
  outcome <- a$outcome %||% "the outcome"
  multiple <- length(stats::coef(object)) > 1
  tab <- drop_intercept(coef_table(object, exponentiate = TRUE))
  eff_lab <- paste0(if (multiple) "Adjusted" else "Crude", " OR")
  sentences <- vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    paste0(r$term, " was associated with ", fmt_num(r$estimate), " times the odds of ",
           outcome, " (", eff_lab, " ", fmt_ci(r$estimate, r$conf.low, r$conf.high),
           "; ", fmt_p(r$p.value, a$markup), ")")
  }, character(1))
  body <- paste0("Conditional logistic regression was used to account for the matched case-control design. ",
                 paste(sentences, collapse = ". "),
                 if (multiple) ", after adjusting for the additional covariates." else ".",
                 " The matching variables are adjusted for by design.")
  assum <- c("Matched design analysed with the correct strata (do not use ordinary logistic regression on matched data).",
             "Linearity of continuous predictors on the logit scale.",
             "No serious multicollinearity.")
  new_interpretation("Conditional logistic regression (matched case-control)", body, assum, estimates = tab)
}

#' @rdname interpret
#' @export
interpret.survfit <- function(object, ...) {
  a <- azul_args(...); event <- a$outcome %||% "the event"
  s <- summary(object)$table
  # single-group survfit returns a named vector; multi-group returns a matrix
  para <- if (is.matrix(s)) {
    lines <- vapply(seq_len(nrow(s)), function(i) {
      row <- s[i, ]
      paste0("For ", rownames(s)[i], ", ", fmt_num(row[["records"]], 0), " subjects contributed ",
             fmt_num(row[["events"]], 0), " events; the median survival time was ",
             fmt_num(row[["median"]]), " (95% CI: ", fmt_num(row[["0.95LCL"]]), ", ",
             fmt_num(row[["0.95UCL"]]), ")")
    }, character(1))
    paste0("Kaplan-Meier estimates of time to ", event, " were obtained. ",
           paste(lines, collapse = ". "), ". Compare the strata with a log-rank test.")
  } else {
    paste0("A Kaplan-Meier (product-limit) analysis of time to ", event, " was performed. Out of ",
           fmt_num(s[["records"]], 0), " subjects, ", fmt_num(s[["events"]], 0),
           " events occurred during follow-up. The median survival time was ",
           fmt_num(s[["median"]]), " (95% CI: ", fmt_num(s[["0.95LCL"]]), ", ",
           fmt_num(s[["0.95UCL"]]), ").")
  }
  assum <- c("Non-informative censoring.",
             "Survival estimated by the Kaplan-Meier (product-limit) method; report the number at risk over time.")
  new_interpretation("Kaplan-Meier survival estimate", para, assum)
}

#' @rdname interpret
#' @export
interpret.survdiff <- function(object, ...) {
  a <- azul_args(...); event <- a$outcome %||% "the event"
  df <- length(object$n) - 1
  x2 <- object$chisq
  p <- stats::pchisq(x2, df, lower.tail = FALSE)
  para <- paste0("A log-rank test compared survival across the groups. There was ",
                 if (p < a$alpha) "a statistically significant" else "no statistically significant",
                 " difference in time to ", event, " between groups (log-rank chi-squared(",
                 fmt_num(df, 0), ") = ", fmt_num(x2), ", ", fmt_p(p, a$markup), ").")
  new_interpretation("Log-rank test", para,
                     c("Proportional hazards across groups (the log-rank test is most powerful under proportional hazards).",
                       "Non-informative censoring."))
}

# Shared AFT interpreter working directly from the coefficient table (st),
# the distribution and the scale. Used by both the survreg model method and
# the summary.survreg method.
.aft_interpret <- function(st, dist, scale, event, a, tab = NULL) {
  z <- stats::qnorm(0.975)
  ph_ok <- dist %in% c("weibull", "exponential")

  paras <- character(0)

  # -- baseline (intercept) -------------------------------------------------
  if ("(Intercept)" %in% rownames(st)) {
    b0 <- st["(Intercept)", "Value"]
    paras <- c(paras, paste0(
      "Intercept: the baseline log survival time was ", fmt_num(b0, 3),
      "; the predicted baseline survival time was exp(", fmt_num(b0, 3), ") = ",
      fmt_num(exp(b0)), " time units when all covariates are at 0 or their reference level."))
  }

  # -- covariates -----------------------------------------------------------
  terms <- setdiff(rownames(st), c("(Intercept)", "Log(scale)", "Scale"))
  for (tm in terms) {
    b <- st[tm, "Value"]; se <- st[tm, "Std. Error"]; p <- st[tm, ncol(st)]
    TR <- exp(b); lo <- exp(b - z * se); hi <- exp(b + z * se)
    aft_pct <- (TR - 1) * 100
    aft_dir <- if (TR >= 1) "longer" else "shorter"
    sig <- sig_phrase(p, a$alpha)
    pcmp <- if (!is.na(p) && p < a$alpha) paste0("P < ", a$alpha) else paste0("P >= ", a$alpha)
    paras <- c(paras, paste0(
      "AFT interpretation: adjusting for the other covariates, each one-unit increase in '", tm,
      "' multiplied the expected survival time by TR = ", fmt_num(TR), " (95% CI: ",
      fmt_num(lo), ", ", fmt_num(hi), "; ", fmt_p(p, a$markup),
      "), i.e. survival time was ", fmt_num(abs(aft_pct), 1), "% ", aft_dir,
      ". The effect was ", sig, " (", pcmp, ")."))

    if (ph_ok) {
      HR <- exp(-b / scale)                # Weibull AFT -> PH: HR = TR^(-1/scale)
      hr_pct <- (HR - 1) * 100
      hr_dir <- if (HR >= 1) "higher" else "lower"
      paras <- c(paras, paste0(
        "Equivalent PH interpretation: the hazard (instantaneous risk) of ", event,
        " was multiplied by HR = ", fmt_num(HR), ", a ", fmt_num(abs(hr_pct), 1),
        "% ", hr_dir, " hazard."))
    }
  }

  if (!ph_ok)
    paras <- c(paras, paste0(
      "This ", dist, " AFT model does not have a proportional-hazards interpretation; ",
      "report the time ratio (TR) only."))

  lead <- paste0("A parametric accelerated failure time (AFT) model with a ", dist,
                 " distribution was fitted for time to ", event,
                 ". Coefficients are reported as time ratios (TR)",
                 if (ph_ok) ", with the equivalent hazard ratios (HR) derived from the scale parameter." else ".")
  paras <- c(lead, paras)

  if (is.null(tab)) {
    keep <- setdiff(rownames(st), c("(Intercept)", "Log(scale)", "Scale"))
    tab <- data.frame(term = keep,
                      estimate = exp(st[keep, "Value"]),
                      conf.low = exp(st[keep, "Value"] - z * st[keep, "Std. Error"]),
                      conf.high = exp(st[keep, "Value"] + z * st[keep, "Std. Error"]),
                      p.value = st[keep, ncol(st)], stringsAsFactors = FALSE)
  }
  assum <- c(paste0("The baseline hazard follows the assumed ", dist, " form (compare competing distributions by AIC)."),
             if (ph_ok) "Exponential and Weibull support both AFT (time ratio) and PH (hazard ratio) interpretations." else "Log-normal / log-logistic have non-monotone hazards and an AFT (time ratio) interpretation only.",
             "Non-informative censoring.")
  new_interpretation(paste0("Parametric AFT survival model (", dist, ")"),
                     paras, stats::na.omit(assum), estimates = tab)
}

#' @rdname interpret
#' @export
interpret.survreg <- function(object, ...) {
  a <- azul_args(...); event <- a$outcome %||% "the event"
  # build the estimates table directly from the summary (broom::tidy ignores
  # exponentiate for survreg); .aft_interpret does this when tab = NULL
  .aft_interpret(summary(object)$table, object$dist, object$scale, event, a)
}

#' @rdname interpret
#' @export
interpret.summary.survreg <- function(object, ...) {
  a <- azul_args(...); event <- a$outcome %||% "the event"
  st <- object$table
  # distribution: from the stored call if present, else infer/ default weibull
  dist <- tryCatch(as.character(object$call$dist), error = function(e) NULL)
  if (is.null(dist) || !length(dist)) dist <- "weibull"
  # scale: exp(Log(scale)) if present, else 1 (exponential)
  scale <- if ("Log(scale)" %in% rownames(st)) exp(st["Log(scale)", "Value"]) else 1
  .aft_interpret(st, dist, scale, event, a)
}

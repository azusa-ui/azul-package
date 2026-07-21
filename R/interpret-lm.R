# ---------------------------------------------------------------------------
# Linear regression: simple and multiple (stats::lm).
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @export
interpret.lm <- function(object, ...) {
  # glm dispatches separately; guard in case someone calls interpret.lm on a glm
  if (inherits(object, "glm")) return(interpret.glm(object, ...))
  a <- azul_args(...)
  outcome <- a$outcome %||% all.vars(stats::formula(object))[1]
  tab <- drop_intercept(coef_table(object))
  sm <- summary(object)
  r2 <- sm$r.squared; ar2 <- sm$adj.r.squared
  fst <- sm$fstatistic
  multiple <- nrow(tab) > 1
  xlev <- tryCatch(object$xlevels, error = function(e) list())
  adj <- if (multiple) ", holding the other predictors constant" else ""

  main_idx <- which(!grepl(":", tab$term))   # interaction rows handled separately
  sentences <- vapply(main_idx, function(i) {
    r <- tab[i, ]
    pr <- .parse_pred_term(r$term, xlev)
    ci <- paste0("(adjusted b = ", fmt_ci(r$estimate, r$conf.low, r$conf.high),
                 "; ", fmt_p(r$p.value, a$markup), ")")
    if (pr$is_factor) {
      dir <- if (r$estimate >= 0) "higher" else "lower"
      paste0("The ", pr$level, " group had a ", outcome, " that was ", fmt_num(abs(r$estimate)),
             " ", a$unit, "s ", dir, " than the ", pr$ref, " group ", ci, adj, ".")
    } else {
      paste0("Each one-unit increase in ", r$term, " was associated with a ",
             fmt_num(r$estimate), " ", a$unit, " change in ", outcome, " ", ci, adj, ".")
    }
  }, character(1))

  lead <- if (multiple)
    paste0("Multiple linear regression was used to model ", outcome, ".") else
    paste0("Simple linear regression was used to model ", outcome, ".")

  fit_txt <- paste0("The model explained ", fmt_num(r2 * 100, 1),
                    "% of the variance in ", outcome, " (R-squared = ", fmt_num(r2, 3),
                    ", adjusted R-squared = ", fmt_num(ar2, 3), ")")
  if (!is.null(fst))
    fit_txt <- paste0(fit_txt, " and was ",
                      sig_phrase(stats::pf(fst[1], fst[2], fst[3], lower.tail = FALSE), a$alpha),
                      " overall (F(", fmt_num(fst[2], 0), ", ", fmt_num(fst[3], 0), ") = ",
                      fmt_num(fst[1]), ")")
  fit_txt <- paste0(fit_txt, ".")

  comp <- .compose_with_interaction(object, a, lead, sentences, tail = fit_txt)

  assum <- c("Linearity of the predictor-outcome relationship.",
             "Independence of residuals.",
             "Homoscedasticity (constant residual variance; inspect residuals-vs-fitted).",
             "Normality of residuals (Q-Q plot).",
             if (multiple) "No serious multicollinearity (check variance inflation factors, VIF < 5 to 10)." else NULL,
             "No influential outliers (Cook's distance).",
             comp$extra_assum)

  new_interpretation(if (multiple) "Multiple linear regression" else "Simple linear regression",
                     comp$body, stats::na.omit(assum), estimates = tab)
}

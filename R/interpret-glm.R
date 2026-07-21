# ---------------------------------------------------------------------------
# Generalised linear models: logistic (binomial), Poisson / quasi-Poisson,
# and negative binomial (MASS::glm.nb -> class "negbin").
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @export
interpret.glm <- function(object, ...) {
  a <- azul_args(...)
  fam <- tryCatch(stats::family(object)$family, error = function(e) "gaussian")
  link <- tryCatch(stats::family(object)$link, error = function(e) "identity")
  outcome <- a$outcome %||% all.vars(stats::formula(object))[1]
  multiple <- length(stats::coef(object)) > 2

  is_logit <- fam == "binomial" && link == "logit"
  is_count <- fam %in% c("poisson", "quasipoisson") || link == "log"

  if (is_logit) {
    tab <- drop_intercept(coef_table(object, exponentiate = TRUE))
    effect <- "OR"; effword <- "the odds"
  } else if (is_count) {
    tab <- drop_intercept(coef_table(object, exponentiate = TRUE))
    effect <- "IRR"; effword <- "the expected count/rate"
  } else {
    # gaussian glm -> treat like lm on the linear scale
    tab <- drop_intercept(coef_table(object, exponentiate = FALSE))
    effect <- "b"; effword <- outcome
  }

  adj <- if (multiple) "adjusted " else ""
  eff_lab <- if (multiple && effect != "b") paste0("Adjusted ", effect)
             else if (effect != "b") paste0("Crude ", effect)
             else "b"

  main_idx <- which(!grepl(":", tab$term))   # interaction rows handled separately
  sentences <- vapply(main_idx, function(i) {
    r <- tab[i, ]
    verb <- if (effect == "b") paste0("a ", fmt_num(r$estimate), " ", a$unit, " change in ", outcome)
            else paste0(fmt_num(r$estimate), " times ", effword, " of ", outcome)
    paste0(r$term, " was associated with ", verb,
           " (", eff_lab, " ", fmt_ci(r$estimate, r$conf.low, r$conf.high),
           "; ", fmt_p(r$p.value, a$markup), ")")
  }, character(1))

  method <- if (is_logit) paste0(if (multiple) "Multiple" else "Simple", " logistic regression")
            else if (is_count) paste0(if (fam == "quasipoisson") "Quasi-Poisson" else "Poisson", " (log-linear) regression")
            else "Generalised linear model"

  lead <- paste0(method, " was used to model ", outcome, ".")
  ref_note <- "Categorical predictors are reported relative to their reference level; state the reference explicitly in the table footnote."
  comp <- .compose_with_interaction(object, a, lead, sentences, tail = ref_note)
  body <- comp$body

  assum <- if (is_logit)
    c("Binary outcome and independent observations.",
      "Linearity of continuous predictors on the logit scale.",
      "No serious multicollinearity (VIF).",
      "Adequate events per variable (rule of thumb >= 10).",
      "Assess fit with the Hosmer-Lemeshow test and discrimination with the ROC/AUC.")
  else if (is_count)
    c("Counts with independent observations.",
      "For Poisson, mean equals variance; check overdispersion (deviance/df). If deviance/df > ~1.5, use quasi-Poisson or negative binomial.",
      "Use an offset (log person-time) when exposure/follow-up differs across units.",
      "Consider a zero-inflated model if zeros are in excess.")
  else c("Standard GLM assumptions for the chosen family and link.")

  new_interpretation(method, body, stats::na.omit(c(assum, comp$extra_assum)), estimates = tab)
}

#' @rdname interpret
#' @export
interpret.negbin <- function(object, ...) {
  a <- azul_args(...)
  outcome <- a$outcome %||% all.vars(stats::formula(object))[1]
  multiple <- length(stats::coef(object)) > 2
  tab <- drop_intercept(coef_table(object, exponentiate = TRUE))
  eff_lab <- paste0(if (multiple) "Adjusted" else "Crude", " IRR")
  sentences <- vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    paste0(r$term, " was associated with ", fmt_num(r$estimate),
           " times the expected count of ", outcome,
           " (", eff_lab, " ", fmt_ci(r$estimate, r$conf.low, r$conf.high),
           "; ", fmt_p(r$p.value, a$markup), ")")
  }, character(1))
  theta <- tryCatch(object$theta, error = function(e) NA_real_)
  body <- paste0("Negative binomial regression was used to model ", outcome,
                 " because of overdispersion in the count outcome. ",
                 paste(sentences, collapse = ". "),
                 if (multiple) ", after adjusting for the other predictors." else ".",
                 if (!is.na(theta)) paste0(" The dispersion parameter theta was ", fmt_num(theta, 3), ".") else "")
  assum <- c("Overdispersed counts (variance > mean); negative binomial relaxes the Poisson equidispersion assumption.",
             "Independent observations; use an offset for differing exposure time.",
             "Consider a zero-inflated negative binomial if structural zeros are present.")
  new_interpretation("Negative binomial regression", body, assum, estimates = tab)
}

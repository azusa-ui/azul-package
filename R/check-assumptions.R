# ---------------------------------------------------------------------------
# check_assumptions(): run the standard diagnostic tests for a fitted model
# and report each assumption as met / possibly violated, with the test result.
# Uses base R plus car / ResourceSelection / survival when available.
# ---------------------------------------------------------------------------

.assum <- function(name, detail, met) {
  verdict <- if (is.na(met)) "could not be assessed"
             else if (met) "assumption met" else "ASSUMPTION MAY BE VIOLATED"
  paste0(name, ": ", detail, " -> ", verdict, ".")
}

#' Check the assumptions of a fitted model
#'
#' Runs the standard diagnostic tests for the model and reports each assumption
#' as met or possibly violated. Supported: \code{lm}, \code{glm} (binomial,
#' Poisson/quasi-Poisson), \code{coxph}, and \code{aov}.
#'
#' @param model A fitted model.
#' @param alpha Significance level for the diagnostic tests (default 0.05).
#' @param ... Unused.
#' @return An \code{azul_interpretation} object.
#' @examples
#' check_assumptions(lm(mpg ~ wt + hp, mtcars))
#' @export
check_assumptions <- function(model, alpha = 0.05, ...) {
  UseMethod("check_assumptions", model)
}

#' @export
check_assumptions.default <- function(model, alpha = 0.05, ...) {
  stop("check_assumptions() supports lm, glm, coxph and aov objects.", call. = FALSE)
}

#' @export
check_assumptions.lm <- function(model, alpha = 0.05, ...) {
  if (inherits(model, "glm")) return(check_assumptions.glm(model, alpha, ...))
  lines <- character(0)
  r <- stats::residuals(model); n <- length(r)
  # normality of residuals
  if (n >= 3 && n <= 5000) {
    sw <- stats::shapiro.test(r)
    lines <- c(lines, .assum("Normality of residuals",
      paste0("Shapiro-Wilk W = ", fmt_num(sw$statistic, 3), ", ", fmt_p(sw$p.value)),
      sw$p.value >= alpha))
  }
  # homoscedasticity
  if (requireNamespace("car", quietly = TRUE)) {
    nc <- tryCatch(car::ncvTest(model), error = function(e) NULL)
    if (!is.null(nc)) lines <- c(lines, .assum("Homoscedasticity (constant variance)",
      paste0("Breusch-Pagan chi-squared = ", fmt_num(nc$ChiSquare), ", ", fmt_p(nc$p)),
      nc$p >= alpha))
    dw <- tryCatch(car::durbinWatsonTest(model), error = function(e) NULL)
    if (!is.null(dw)) lines <- c(lines, .assum("Independence (no autocorrelation)",
      paste0("Durbin-Watson = ", fmt_num(dw$dw, 2), ", ", fmt_p(dw$p)), dw$p >= alpha))
    if (length(stats::coef(model)) > 2) {
      v <- tryCatch(car::vif(model), error = function(e) NULL)
      if (!is.null(v)) { mv <- max(if (is.matrix(v)) v[, ncol(v)] else v)
        lines <- c(lines, .assum("No multicollinearity",
          paste0("maximum VIF = ", fmt_num(mv, 2)), mv < 5)) }
    }
  }
  # influential observations
  cd <- stats::cooks.distance(model); infl <- sum(cd > 4 / n, na.rm = TRUE)
  lines <- c(lines, .assum("No influential outliers",
    paste0("maximum Cook's distance = ", fmt_num(max(cd, na.rm = TRUE), 3), "; ", infl,
           " observation(s) exceed 4/n"), max(cd, na.rm = TRUE) < 1))
  new_interpretation("Assumption check: linear regression", lines,
    notes = "Diagnostic tests are sensitive to sample size; always inspect the residual and Q-Q plots as well.")
}

#' @export
check_assumptions.glm <- function(model, alpha = 0.05, ...) {
  fam <- tryCatch(stats::family(model)$family, error = function(e) "gaussian")
  link <- tryCatch(stats::family(model)$link, error = function(e) "identity")
  lines <- character(0); n <- length(stats::residuals(model))
  if (fam == "binomial") {
    if (requireNamespace("ResourceSelection", quietly = TRUE)) {
      y <- model$y; ph <- stats::fitted(model)
      hl <- tryCatch(ResourceSelection::hoslem.test(y, ph, g = 10), error = function(e) NULL)
      if (!is.null(hl)) lines <- c(lines, .assum("Calibration / goodness-of-fit",
        paste0("Hosmer-Lemeshow chi-squared = ", fmt_num(hl$statistic), ", ", fmt_p(hl$p.value)),
        hl$p.value >= alpha))
    }
    epv <- tryCatch(sum(model$y) / (length(stats::coef(model)) - 1), error = function(e) NA)
    if (!is.na(epv)) lines <- c(lines, .assum("Adequate events per variable (EPV)",
      paste0("EPV = ", fmt_num(epv, 1), " (rule of thumb >= 10)"), epv >= 10))
  }
  if (fam %in% c("poisson", "quasipoisson") || link == "log") {
    disp <- sum(stats::residuals(model, type = "pearson")^2) / stats::df.residual(model)
    lines <- c(lines, .assum("No overdispersion",
      paste0("dispersion (Pearson chi-squared / df) = ", fmt_num(disp, 2), " (~1 expected)"),
      disp <= 1.5))
  }
  if (requireNamespace("car", quietly = TRUE) && length(stats::coef(model)) > 2) {
    v <- tryCatch(car::vif(model), error = function(e) NULL)
    if (!is.null(v)) { mv <- max(if (is.matrix(v)) v[, ncol(v)] else v)
      lines <- c(lines, .assum("No multicollinearity",
        paste0("maximum VIF = ", fmt_num(mv, 2)), mv < 5)) }
  }
  if (!length(lines)) lines <- "No automated checks were available for this GLM family; inspect residuals and fit manually."
  note <- if (fam == "binomial")
    "For logistic models also check linearity of continuous predictors on the logit scale (e.g. Box-Tidwell) and discrimination (ROC/AUC)."
  else if (fam %in% c("poisson", "quasipoisson") || link == "log")
    "For count models, use an offset (log person-time) for differing exposure, and consider a negative binomial or zero-inflated model if overdispersion or excess zeros are present."
  else "Check the residual and fit diagnostics appropriate to the chosen family and link."
  new_interpretation("Assumption check: generalised linear model", lines, notes = note)
}

#' @export
check_assumptions.coxph <- function(model, alpha = 0.05, ...) {
  if (!requireNamespace("survival", quietly = TRUE))
    stop("Package 'survival' is required.", call. = FALSE)
  zph <- tryCatch(survival::cox.zph(model), error = function(e) NULL)
  lines <- character(0)
  if (!is.null(zph)) {
    tab <- zph$table
    gp <- tab["GLOBAL", "p"]
    lines <- c(lines, .assum("Proportional hazards (global)",
      paste0("global Schoenfeld test chi-squared = ", fmt_num(tab["GLOBAL", "chisq"]),
             ", ", fmt_p(gp)), gp >= alpha))
    covs <- setdiff(rownames(tab), "GLOBAL")
    viol <- covs[tab[covs, "p"] < alpha]
    if (length(viol)) lines <- c(lines,
      paste0("Per-covariate: proportional hazards may be violated for ", .join_and(viol), "."))
  }
  new_interpretation("Assumption check: Cox proportional hazards", lines,
    notes = "Complement cox.zph with scaled-Schoenfeld residual plots and log-minus-log plots. If PH is violated, consider a time-varying coefficient or stratification.")
}

#' @export
check_assumptions.aov <- function(model, alpha = 0.05, ...) {
  lines <- character(0); r <- stats::residuals(model); n <- length(r)
  if (n >= 3 && n <= 5000) {
    sw <- stats::shapiro.test(r)
    lines <- c(lines, .assum("Normality of residuals",
      paste0("Shapiro-Wilk W = ", fmt_num(sw$statistic, 3), ", ", fmt_p(sw$p.value)),
      sw$p.value >= alpha))
  }
  if (requireNamespace("car", quietly = TRUE)) {
    mf <- stats::model.frame(model)
    fac <- names(mf)[vapply(mf, function(z) is.factor(z) || is.character(z), logical(1))]
    if (length(fac)) {
      lt <- tryCatch(car::leveneTest(stats::residuals(model) ~ interaction(mf[fac])),
                     error = function(e) NULL)
      if (!is.null(lt)) lines <- c(lines, .assum("Homogeneity of variance",
        paste0("Levene F = ", fmt_num(lt[1, "F value"]), ", ", fmt_p(lt[1, "Pr(>F)"])),
        lt[1, "Pr(>F)"] >= alpha))
    }
  }
  new_interpretation("Assumption check: ANOVA", lines,
    notes = "If normality or homogeneity is violated, consider Welch's ANOVA or the Kruskal-Wallis test.")
}

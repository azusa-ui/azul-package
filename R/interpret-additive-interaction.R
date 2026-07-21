# ---------------------------------------------------------------------------
# Additive-scale interaction for public-health interpretation: RERI, AP and the
# synergy index S, with delta-method 95% CIs (Hosmer & Lemeshow 1992; Rothman).
# Computed from a logistic (glm binomial) or Cox model fitted with a 4-level
# composite exposure (00 reference, 10, 01, 11). No external package required.
# ---------------------------------------------------------------------------

#' Interpret additive interaction (RERI, AP, synergy index)
#'
#' Reports interaction on the additive scale from a multiplicative model
#' (logistic or Cox) fitted with a four-level composite exposure. The composite
#' must be coded with the doubly-unexposed group as the reference; the three
#' non-reference coefficients are the joint-exposure log effects.
#'
#' @param model A fitted \code{glm} (binomial) or \code{coxph} model.
#' @param exposures Length-3 character vector naming the coefficients for the
#'   "A only", "B only" and "A and B" groups, in that order (e.g.
#'   \code{c("comb10","comb01","comb11")}).
#' @param outcome Name of the outcome/event for the prose.
#' @param labels Optional length-2 labels for exposure A and B.
#' @param ... Styling args (alpha, markup).
#' @return An \code{azul_interpretation} object.
#' @examples
#' set.seed(1); n <- 800
#' d <- data.frame(a = rbinom(n,1,.5), b = rbinom(n,1,.5))
#' d$y <- rbinom(n, 1, plogis(-2 + 0.7*d$a + 0.6*d$b + 1.1*d$a*d$b))
#' d$comb <- factor(ifelse(d$a==0&d$b==0,"00",ifelse(d$a==1&d$b==0,"10",
#'            ifelse(d$a==0&d$b==1,"01","11"))), levels=c("00","10","01","11"))
#' fit <- glm(y ~ comb, data = d, family = binomial)
#' interpret_additive_interaction(fit, c("comb10","comb01","comb11"),
#'                                outcome = "disease")
#' @export
interpret_additive_interaction <- function(model, exposures, outcome = "the outcome",
                                           labels = c("exposure A", "exposure B"), ...) {
  a <- azul_args(...)
  stopifnot(length(exposures) == 3)
  ratio <- if (inherits(model, "coxph")) "HR" else "OR"
  b <- stats::coef(model)[exposures]
  if (any(is.na(b))) stop("One or more of the named exposure coefficients were not found in the model.", call. = FALSE)
  V <- stats::vcov(model)[exposures, exposures]
  th1 <- b[1]; th2 <- b[2]; th3 <- b[3]           # log effects: A only, B only, both
  RR10 <- exp(th1); RR01 <- exp(th2); RR11 <- exp(th3)
  z <- stats::qnorm(1 - a$alpha / 2)

  RERI <- RR11 - RR10 - RR01 + 1
  hR <- c(-RR10, -RR01, RR11)
  seRERI <- sqrt(as.numeric(t(hR) %*% V %*% hR))
  RERI_ci <- RERI + c(-1, 1) * z * seRERI

  AP <- RERI / RR11
  hA <- c(-exp(th1 - th3), -exp(th2 - th3), exp(th1 - th3) + exp(th2 - th3) - exp(-th3))
  seAP <- sqrt(as.numeric(t(hA) %*% V %*% hA))
  AP_ci <- AP + c(-1, 1) * z * seAP

  denom <- (RR10 - 1) + (RR01 - 1)
  S <- (RR11 - 1) / denom
  # delta method on ln(S)
  gS <- c(-RR10 / denom, -RR01 / denom, RR11 / (RR11 - 1))
  selnS <- sqrt(as.numeric(t(gS) %*% V %*% gS))
  S_ci <- exp(log(S) + c(-1, 1) * z * selnS)

  # joint effects (RR with model CIs) for the 2x4 table
  ci_lin <- suppressMessages(tryCatch(stats::confint(model)[exposures, , drop = FALSE],
              error = function(e) cbind(b - z * sqrt(diag(V)), b + z * sqrt(diag(V)))))
  jr <- function(i, lab) paste0(lab, ": ", ratio, " ",
          fmt_num(exp(b[i])), " (95% CI: ", fmt_num(exp(ci_lin[i, 1])), ", ", fmt_num(exp(ci_lin[i, 2])), ")")

  addsig <- !is.na(RERI_ci[1]) && (RERI_ci[1] > 0 || RERI_ci[2] < 0)
  paras <- c(
    paste0("Interaction between ", labels[1], " and ", labels[2], " on ", outcome,
           " was assessed on the additive scale, using the doubly-unexposed group as the reference."),
    paste0("Joint effects (each versus the doubly-unexposed reference): ",
           jr(1, paste0(labels[1], " only")), "; ", jr(2, paste0(labels[2], " only")), "; ",
           jr(3, "both exposures"), "."),
    paste0("The relative excess risk due to interaction (RERI) was ", fmt_num(RERI),
           " (95% CI: ", fmt_num(RERI_ci[1]), ", ", fmt_num(RERI_ci[2]), ")."),
    paste0("The attributable proportion due to interaction (AP) was ", fmt_num(AP),
           " (95% CI: ", fmt_num(AP_ci[1]), ", ", fmt_num(AP_ci[2]),
           "), i.e. about ", fmt_num(AP * 100, 1), "% of the risk in the jointly-exposed group."),
    paste0("The synergy index (S) was ", fmt_num(S), " (95% CI: ", fmt_num(S_ci[1]), ", ",
           fmt_num(S_ci[2]), ")."),
    paste0("This suggests ", if (addsig) "statistically significant" else "no statistically significant",
           " additive interaction (no additive interaction corresponds to RERI = 0, AP = 0 and S = 1)."))
  new_interpretation("Additive interaction (RERI, AP, synergy index)", paras,
    c("The composite exposure must use the lowest-risk (doubly-unexposed) group as the reference.",
      "RERI and AP are not invariant across covariate strata; S is preferred in adjusted models.",
      "Confidence intervals use the delta method; consider bootstrap CIs for small samples.",
      "Report the multiplicative-scale interaction (the product-term P value) as well."),
    estimates = data.frame(measure = c("RERI","AP","S"),
                           estimate = c(RERI, AP, S),
                           conf.low = c(RERI_ci[1], AP_ci[1], S_ci[1]),
                           conf.high = c(RERI_ci[2], AP_ci[2], S_ci[2])))
}

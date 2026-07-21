# ---------------------------------------------------------------------------
# Ordinal (MASS::polr, ordinal::clm) and multinomial (nnet::multinom) logistic.
# ---------------------------------------------------------------------------

# Compute two-sided Wald p-values from a summary coefficient table with a
# t/z column, matched to a tidy estimate table by term. Used for polr, whose
# summary reports t values but no p values.
.wald_p <- function(object) {
  ct <- tryCatch(stats::coef(summary(object)), error = function(e) NULL)
  if (is.null(ct)) return(NULL)
  tcol <- intersect(c("t value", "z value", "t", "z"), colnames(ct))
  if (!length(tcol)) return(NULL)
  p <- 2 * stats::pnorm(abs(ct[, tcol[1]]), lower.tail = FALSE)
  stats::setNames(p, rownames(ct))
}

.ordinal_body <- function(object, a, method, assum) {
  outcome <- a$outcome %||% "the outcome"
  tab <- coef_table(object, exponentiate = TRUE)
  tab <- tab[!grepl("\\|", tab$term), , drop = FALSE]  # drop threshold/intercept terms
  # backfill p-values if missing (polr)
  if (all(is.na(tab$p.value))) {
    pv <- .wald_p(object)
    if (!is.null(pv)) tab$p.value <- unname(pv[tab$term])
  }
  multiple <- nrow(tab) > 1
  eff_lab <- paste0(if (multiple) "Adjusted" else "Crude", " OR")
  sentences <- vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    paste0("A one-unit increase in ", r$term, " multiplied the odds of being in a higher category of ",
           outcome, " by ", fmt_num(r$estimate), " (", eff_lab, " ",
           fmt_ci(r$estimate, r$conf.low, r$conf.high), "; ", fmt_p(r$p.value, a$markup), ")")
  }, character(1))
  body <- paste0("An ordinal logistic (proportional odds) regression was used to model ",
                 outcome, ". The odds ratio has a single unified interpretation across all cut-points. ",
                 paste(sentences, collapse = ". "),
                 if (multiple) ", after adjusting for the other predictors." else ".",
                 " Categorical predictors are expressed relative to their reference level.")
  new_interpretation(method, body, assum, estimates = tab)
}

#' @rdname interpret
#' @export
interpret.polr <- function(object, ...) {
  a <- azul_args(...)
  .ordinal_body(object, a, "Ordinal logistic regression (proportional odds)",
    c("Proportional odds: the effect is constant across cut-points; test with the Brant test (P > 0.05 supports the assumption).",
      "If proportional odds fails for a predictor, allow a partial proportional odds / non-proportional term, or use multinomial logistic.",
      "Adequate cell counts across outcome categories."))
}

#' @rdname interpret
#' @export
interpret.clm <- function(object, ...) {
  a <- azul_args(...)
  .ordinal_body(object, a, "Ordinal logistic regression (cumulative link model)",
    c("Proportional odds (test with a nominal-effects likelihood-ratio test or the Brant test).",
      "Correct link function (logit gives proportional odds).",
      "Adequate cell counts across outcome categories."))
}

#' @rdname interpret
#' @param style For multinomial models, "expanded" (the default) gives a
#'   labelled subheading, a manuscript sentence and a plain-language sentence
#'   per predictor and outcome category; "brief" gives one compact sentence each.
#' @export
interpret.multinom <- function(object, style = c("expanded", "brief"), ...) {
  a <- azul_args(...)
  style <- match.arg(style)
  outref <- object$lev[1]
  xlev <- tryCatch(object$xlevels, error = function(e) list())
  labels <- tryCatch(attr(object$terms, "term.labels"), error = function(e) names(xlev))
  tt <- if (requireNamespace("broom", quietly = TRUE))
    tryCatch(broom::tidy(object, conf.int = TRUE, exponentiate = TRUE),
             error = function(e) NULL) else NULL

  assum <- c("Independence of irrelevant alternatives (IIA); assess with a Hausman-McFadden test if in doubt.",
             "Reference outcome category stated explicitly.",
             "Adequate sample size per outcome category.")

  if (!is.null(tt) && "y.level" %in% names(tt)) {
    tt <- tt[!grepl("Intercept", tt$term, ignore.case = TRUE), , drop = FALSE]
    est <- as.data.frame(tt[, c("y.level","term","estimate","conf.low","conf.high","p.value")])
    if (style == "expanded") {
      rows <- lapply(seq_len(nrow(tt)), function(i) {
        pr <- .parse_pred_term(tt$term[i], xlev)
        c(pr, list(term = tt$term[i], cat = tt$y.level[i], outref = outref,
                   rrr = tt$estimate[i], lo = tt$conf.low[i], hi = tt$conf.high[i],
                   p = tt$p.value[i], adjust = setdiff(labels, pr$var)))
      })
      lead <- paste0("A multinomial logistic regression was fitted with '", outref,
                     "' as the reference outcome category; exponentiated coefficients are ",
                     "relative risk ratios (RRR).")
      paras <- .multinom_rich(rows, lead, a$markup)
      return(new_interpretation("Multinomial logistic regression", paras, assum, estimates = est))
    }
    sentences <- vapply(seq_len(nrow(tt)), function(i) {
      r <- tt[i, ]
      paste0(r$term, " changed the relative risk of the '", r$y.level, "' category (versus '",
             outref, "') by a factor of ", fmt_num(r$estimate),
             " (RRR ", fmt_ci(r$estimate, r$conf.low, r$conf.high), "; ", fmt_p(r$p.value, a$markup), ")")
    }, character(1))
  } else {
    tab <- drop_intercept(coef_table(object, exponentiate = TRUE))
    est <- tab
    sentences <- vapply(seq_len(nrow(tab)), function(i) {
      r <- tab[i, ]
      paste0(r$term, " changed the relative risk (versus '", outref, "') by a factor of ",
             fmt_num(r$estimate), " (RRR ", fmt_ci(r$estimate, r$conf.low, r$conf.high),
             "; ", fmt_p(r$p.value, a$markup), ")")
    }, character(1))
  }
  body <- paste0("A multinomial logistic regression was fitted with '", outref,
                 "' as the reference outcome category. Exponentiated coefficients are relative risk ratios (RRR). ",
                 paste(sentences, collapse = ". "), ".")
  new_interpretation("Multinomial logistic regression", body, assum, estimates = est)
}

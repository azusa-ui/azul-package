# ---------------------------------------------------------------------------
# VGAM::vglm models (S4). Covers the common categorical families used in
# advanced categorical analysis: cumulative / proportional odds (ordinal, OR),
# multinomial (RRR), and Poisson / negative binomial (IRR).
# ---------------------------------------------------------------------------

#' @rdname interpret
#' @export
interpret.vglm <- function(object, style = c("expanded", "brief"), ...) {
  a <- azul_args(...)
  style <- match.arg(style)
  outcome <- a$outcome %||% "the outcome"
  fam <- tryCatch(object@family@vfamily, error = function(e) "unknown")
  # VGAM is S4; stats::coef() does not dispatch to its method. Read the
  # coefficient matrix from the summary object's coef3 slot, with fallbacks.
  sm <- tryCatch(summary(object), error = function(e) NULL)
  ct <- NULL
  if (!is.null(sm)) {
    ct <- tryCatch(methods::slot(sm, "coef3"), error = function(e) NULL)
    if (is.null(ct)) ct <- tryCatch(methods::slot(sm, "coefficients"), error = function(e) NULL)
  }
  if (is.null(ct) || !is.matrix(ct))
    stop("Could not extract the coefficient table from the vglm object.", call. = FALSE)

  est <- ct[, "Estimate"]; se <- ct[, "Std. Error"]
  pcol <- grep("Pr\\(", colnames(ct)); p <- if (length(pcol)) ct[, pcol[1]] else NA_real_
  terms <- rownames(ct)
  keep <- !grepl("Intercept", terms)
  terms <- terms[keep]; est <- est[keep]; se <- se[keep]; p <- p[keep]

  is_ord   <- any(grepl("cumulative|propodds|VGAMordinal", fam))
  is_multi <- any(grepl("multinomial", fam)) && !is_ord
  is_count <- any(grepl("poisson|negbin", fam))
  expo <- is_ord || is_multi || is_count
  z <- stats::qnorm(0.975)
  e  <- if (expo) exp(est) else est
  lo <- if (expo) exp(est - z * se) else est - z * se
  hi <- if (expo) exp(est + z * se) else est + z * se
  tab <- data.frame(term = terms, estimate = e, conf.low = lo, conf.high = hi,
                    p.value = p, stringsAsFactors = FALSE)

  if (is_multi) {
    # map each linear predictor j to its (category vs reference) via
    # predictors.names, e.g. "log(mu[,1]/mu[,3])" -> num=1, den=3
    ynames <- tryCatch(object@misc$ynames, error = function(e) NULL)
    pn <- tryCatch(object@misc$predictors.names, error = function(e) NULL)
    num <- den <- NULL
    if (!is.null(pn)) {
      num <- as.integer(sub(".*mu\\[,(\\d+)\\]/.*", "\\1", pn))
      den <- as.integer(sub(".*/mu\\[,(\\d+)\\].*", "\\1", pn))
    }
    base <- sub(":.*$", "", tab$term)
    idx  <- suppressWarnings(as.integer(ifelse(grepl(":", tab$term), sub("^.*:", "", tab$term), NA)))
    cat_of <- function(k) if (!is.null(ynames) && !is.na(k) && !is.null(num) && k <= length(num)) ynames[num[k]] else NA
    ref_of <- function(k) if (!is.null(ynames) && !is.na(k) && !is.null(den) && k <= length(den)) ynames[den[k]] else NA

    xlev <- tryCatch(object@xlevels, error = function(e) list())
    labels <- tryCatch(attr(object@terms$terms, "term.labels"),
                       error = function(e) unique(base))
    if (is.null(labels)) labels <- unique(base)

    if (style == "expanded" && !is.null(ynames) && !is.null(num)) {
      rows <- lapply(seq_len(nrow(tab)), function(i) {
        pr <- .parse_pred_term(base[i], xlev)
        c(pr, list(term = base[i], cat = cat_of(idx[i]), outref = ref_of(idx[i]),
                   rrr = tab$estimate[i], lo = tab$conf.low[i], hi = tab$conf.high[i],
                   p = tab$p.value[i], adjust = setdiff(labels, pr$var)))
      })
      lead <- paste0("A multinomial logistic regression (VGAM) was fitted; exponentiated ",
                     "coefficients are relative risk ratios (RRR), each contrasting an outcome ",
                     "category against its baseline.")
      paras <- .multinom_rich(rows, lead, a$markup)
      return(new_interpretation("Multinomial logistic regression (VGAM)", paras,
        c("Baseline (reference) outcome category identified from the model's linear predictors.",
          "Independence of irrelevant alternatives (IIA).",
          "Adequate sample size per outcome category."), estimates = tab))
    }

    sentences <- vapply(seq_len(nrow(tab)), function(i) {
      r <- tab[i, ]
      lp <- if (!is.na(cat_of(idx[i])))
        paste0(" (", cat_of(idx[i]), " versus ", ref_of(idx[i]), ")")
      else if (!is.na(idx[i])) paste0(" for the ", idx[i], ordinal_suffix(idx[i]), " logit") else ""
      paste0(base[i], " changed the relative risk of ", outcome, lp, " by a factor of ",
             fmt_num(r$estimate), " (RRR ", fmt_ci(r$estimate, r$conf.low, r$conf.high),
             "; ", fmt_p(r$p.value, a$markup), ")")
    }, character(1))
    body <- paste0("A multinomial logistic regression (VGAM) was fitted; exponentiated coefficients are ",
                   "relative risk ratios (RRR) for each category against its baseline. ",
                   paste(sentences, collapse = ". "), ".")
    return(new_interpretation("Multinomial logistic regression (VGAM)", body,
      c("Baseline (reference) outcome category identified from the model's linear predictors.",
        "Independence of irrelevant alternatives (IIA).",
        "Adequate sample size per outcome category."), estimates = tab))
  }

  if (is_ord) {
    multiple <- nrow(tab) > 1
    eff_lab <- paste0(if (multiple) "Adjusted" else "Crude", " OR")
    sentences <- vapply(seq_len(nrow(tab)), function(i) {
      r <- tab[i, ]
      paste0("A one-unit increase in ", r$term, " multiplied the odds of being in a higher category of ",
             outcome, " by ", fmt_num(r$estimate), " (", eff_lab, " ",
             fmt_ci(r$estimate, r$conf.low, r$conf.high), "; ", fmt_p(r$p.value, a$markup), ")")
    }, character(1))
    body <- paste0("An ordinal logistic (cumulative link / proportional odds) regression (VGAM) was used to model ",
                   outcome, ". ", paste(sentences, collapse = ". "),
                   if (multiple) ", after adjusting for the other predictors." else ".",
                   " Confirm the direction of the cumulative link (reverse = TRUE gives the odds of a higher category).")
    assum <- c("Proportional odds / parallel-lines assumption (fit with parallel = FALSE and compare by likelihood-ratio test, or use the Brant test).",
               "Direction of the cumulative logit (reverse argument) matches the intended interpretation.",
               "Adequate cell counts across outcome categories.")
    return(new_interpretation("Ordinal logistic regression, proportional odds (VGAM)", body, assum, estimates = tab))
  }

  if (is_count) {
    multiple <- nrow(tab) > 1
    eff_lab <- paste0(if (multiple) "Adjusted" else "Crude", " IRR")
    sentences <- vapply(seq_len(nrow(tab)), function(i) {
      r <- tab[i, ]
      paste0(r$term, " was associated with ", fmt_num(r$estimate), " times the expected count of ",
             outcome, " (", eff_lab, " ", fmt_ci(r$estimate, r$conf.low, r$conf.high),
             "; ", fmt_p(r$p.value, a$markup), ")")
    }, character(1))
    body <- paste0("A count regression (VGAM) was used to model ", outcome,
                   ". Exponentiated coefficients are incidence rate ratios (IRR). ",
                   paste(sentences, collapse = ". "), ".")
    return(new_interpretation("Count regression (VGAM)", body,
      c("Correct count distribution (check overdispersion; use negbinomial if variance exceeds the mean).",
        "Independent observations; use an offset for differing exposure."), estimates = tab))
  }

  # generic fallback ---------------------------------------------------------
  sentences <- vapply(seq_len(nrow(tab)), function(i) {
    r <- tab[i, ]
    paste0("For ", r$term, ", the estimate was ", fmt_ci(r$estimate, r$conf.low, r$conf.high),
           " (", fmt_p(r$p.value, a$markup), ")")
  }, character(1))
  new_interpretation(paste0("VGAM model (", paste(fam, collapse = "/"), ")"),
    paste0("A VGAM model was fitted. ", paste(sentences, collapse = ". "),
           ". Estimates are on the model's link scale; confirm whether exponentiation is appropriate for this family."),
    notes = "azul recognised a VGAM family it does not have tailored wording for; the estimates are reported generically.",
    estimates = tab)
}

# ordinal suffix helper: 1 -> st, 2 -> nd, 3 -> rd, else th
ordinal_suffix <- function(k) {
  k <- suppressWarnings(as.integer(k))
  if (is.na(k)) return("")
  if (k %% 100 %in% 11:13) return("th")
  switch(as.character(k %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
}

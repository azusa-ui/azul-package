#' Publication-ready parametric survival table (PH and AFT)
#'
#' Build an APA-styled table from a parametric survival model fitted with
#' [survival::survreg()]. Both the proportional-hazard (PH) metric and the
#' accelerated-failure-time (AFT) metric are presented side by side, with 95%
#' confidence intervals, standard errors, the Wald statistic and p-values.
#'
#' The fitted distribution is detected automatically from the model:
#'
#' * **exponential** and **Weibull** models have a proportional-hazards form, so
#'   the left block is labelled *Proportional Hazard (PH)* and the estimate is a
#'   hazard ratio (HR);
#' * the **log-logistic** model has no proportional-hazards form (it is a
#'   proportional-odds model), so the left block is labelled
#'   *Proportional Odds (PO)* and the estimate is a survival odds ratio (OR).
#'
#' The right block is always the *Accelerated Failure Time (AFT)* metric, whose
#' estimate is a time ratio (TR). The AFT estimates are taken directly from
#' `survreg`; the PH/PO estimates are obtained from the AFT coefficients by the
#' delta method (\eqn{-\beta/\sigma}), reproducing
#' `SurvRegCensCov::ConvertWeibull()` and `flexsurv` exactly.
#'
#' @param model A [survival::survreg()] object fitted with `dist` one of
#'   `"exponential"`, `"weibull"` or `"loglogistic"`.
#' @param exponentiate Logical. If `TRUE` (default) estimates, confidence
#'   intervals and standard errors are shown on the ratio scale (HR/OR, TR). If
#'   `FALSE` they are shown on the log scale (log(hazard)/log(odds), log(time)).
#' @param outcome Optional character label for the outcome, used in the table
#'   caption. If `NULL` (default) the response (time) variable name is parsed
#'   from the model formula.
#' @param digits Number of decimal places for **every** value column —
#'   estimates, confidence intervals, standard errors and the statistic.
#'   Default `2`. The p-value column is *not* affected: it is always shown to 3
#'   decimal places, and values below 0.001 are printed as `<0.001`.
#' @param font Font family for the whole table (caption, header, body and
#'   footer share it). Default `"Helvetica"`.
#' @param font_size Font size in points. Default `10`.
#'
#' @return A [flextable::flextable()] object that renders in both HTML and Word.
#'
#' @examples
#' library(survival)
#' m <- survreg(Surv(time, status) ~ sex + ph.ecog, data = lung, dist = "weibull")
#' azul_survtable(m)
#' azul_survtable(m, exponentiate = FALSE, outcome = "Death")
#'
#' @export
azul_survtable <- function(model,
                    exponentiate = TRUE,
                    digits = 2,
                    outcome = NULL,
                    font = "Helvetica",
                    font_size = 10) {

  if (!inherits(model, "survreg")) {
    stop("`model` must be a survreg object (survival::survreg()).", call. = FALSE)
  }
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer", quietly = TRUE)) {
    stop("azul_survtable() needs the 'flextable' and 'officer' packages; install them first.",
         call. = FALSE)
  }
  info  <- .azul_survtable_detect(model)
  est   <- .azul_survtable_estimates(model)
  body  <- .azul_survtable_body(model, est, exponentiate, info, digits)
  if (isTRUE(info$aft_only))
    .azul_survtable_flextable_aft(body, model, info, exponentiate, outcome, font, font_size)
  else
    .azul_survtable_flextable(body, model, info, exponentiate, outcome, font, font_size)
}

# AFT-only table (log-normal): a single time-ratio block, no PH/PO metric.
.azul_survtable_flextable_aft <- function(body, model, info, exponentiate, outcome,
                                          font, font_size) {
  est_r <- if (exponentiate) "TR" else "log(time)"
  dat <- body[, c("term", "aft_est", "aft_ci", "aft_se", "stat", "p")]
  ft <- flextable::flextable(dat)
  ft <- flextable::set_header_labels(ft, term = "Variables",
    aft_est = est_r, aft_ci = "95% CI", aft_se = "SE",
    stat = "Statistic", p = "p-value")
  ft <- flextable::add_header_row(ft, top = TRUE,
    values = c("Variables", "Accelerated Failure Time (AFT)", "Statistic", "p-value"),
    colwidths = c(1, 3, 1, 1))
  ft <- flextable::merge_v(ft, part = "header", j = c(1, 5, 6))
  outcome_lab <- if (is.null(outcome)) .azul_survtable_outcome(model) else outcome
  ft <- flextable::set_caption(ft, caption = sprintf(
    "Accelerated failure time survival model for %s, assuming %s distribution",
    outcome_lab, info$dist_label))
  ft <- flextable::add_footer_lines(ft, values = c(
    "The log-normal model has no proportional-hazards or proportional-odds form; only the time ratio (TR) is reported.",
    "Statistic and p-value are the Wald test on the log scale."))
  ft <- flextable::font(ft, fontname = font, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::fontsize(ft, size = font_size - 1, part = "footer")
  ft <- flextable::align(ft, part = "header", align = "center")
  ft <- flextable::align(ft, j = 1, part = "all", align = "left")
  ft <- flextable::align(ft, j = 2:6, part = "body", align = "center")
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  vbold <- which(body$rowtype %in% c("group", "numeric", "flat"))
  if (length(vbold)) ft <- flextable::bold(ft, i = vbold, j = 1, part = "body")
  ind <- which(body$indent == 1L)
  if (length(ind)) ft <- flextable::padding(ft, i = ind, j = 1, padding.left = 16)
  bd <- officer::fp_border(color = "black", width = 1)
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, part = "header", border = bd)
  ft <- flextable::hline(ft, i = 1, j = 2:4, part = "header", border = bd)
  ft <- flextable::hline_bottom(ft, part = "header", border = bd)
  ft <- flextable::hline_bottom(ft, part = "body", border = bd)
  flextable::autofit(ft)
}


# ---- distribution detection & metric labelling ---------------------------

.azul_survtable_detect <- function(model) {
  d <- model$dist
  if (identical(d, "exponential")) {
    list(dist = "exponential", dist_label = "exponential", aft_only = FALSE,
         left_metric = "PH", left_span = "Proportional Hazard (PH)",
         est_ratio = "HR", est_log = "log(hazard)",
         caption_metric = "Proportional hazard")
  } else if (identical(d, "weibull")) {
    list(dist = "weibull", dist_label = "Weibull", aft_only = FALSE,
         left_metric = "PH", left_span = "Proportional Hazard (PH)",
         est_ratio = "HR", est_log = "log(hazard)",
         caption_metric = "Proportional hazard")
  } else if (identical(d, "loglogistic")) {
    list(dist = "loglogistic", dist_label = "log-logistic", aft_only = FALSE,
         left_metric = "PO", left_span = "Proportional Odds (PO)",
         est_ratio = "OR", est_log = "log(odds)",
         caption_metric = "Proportional odds")
  } else if (identical(d, "lognormal")) {
    # log-normal has no proportional-hazards or proportional-odds form:
    # it is reported on the accelerated failure time (time ratio) metric only.
    list(dist = "lognormal", dist_label = "log-normal", aft_only = TRUE,
         left_metric = NA, left_span = NA, est_ratio = NA, est_log = NA,
         caption_metric = "Accelerated failure time")
  } else {
    stop("azul_survtable() supports dist = 'exponential', 'weibull', ",
         "'loglogistic' or 'lognormal'; got '", d, "'.", call. = FALSE)
  }
}


# ---- estimates: AFT (survreg) + left metric (delta method) ---------------
#
# AFT log coefficients come straight from survreg. The left metric (log HR for
# PH, or log survival-odds ratio for PO) is g_j = -beta_j / sigma, with sigma =
# exp(tau), tau = log(scale). The delta-method gradient of g_j is -1/sigma on
# beta_j and beta_j/sigma on tau, reproducing SurvRegCensCov::ConvertWeibull()
# and flexsurv's weibullPH / exp forms exactly.

.azul_survtable_estimates <- function(model) {
  cf  <- stats::coef(model)                 # AFT log-time coefs incl (Intercept)
  V   <- stats::vcov(model)                 # includes Log(scale) if estimated
  sig <- model$scale
  dn  <- rownames(V)
  tau_idx <- which(dn %in% c("Log(scale)", "log(scale)"))
  has_tau <- length(tau_idx) == 1L
  k <- length(cf)

  aft_est <- unname(cf)
  aft_se  <- sqrt(diag(V))[seq_len(k)]

  left_est <- left_se <- numeric(k)
  for (j in seq_len(k)) {
    left_est[j] <- -cf[j] / sig
    grad <- numeric(nrow(V))
    grad[j] <- -1 / sig
    if (has_tau) grad[tau_idx] <- cf[j] / sig
    left_se[j] <- sqrt(as.numeric(t(grad) %*% V %*% grad))
  }

  tab <- summary(model)$table            # Value, Std. Error, z, p
  m_z <- tab[match(names(cf), rownames(tab)), "z"]
  m_p <- tab[match(names(cf), rownames(tab)), "p"]

  data.frame(
    term     = names(cf),
    aft_est  = aft_est, aft_se = aft_se,
    left_est = left_est, left_se = left_se,
    z = as.numeric(m_z), p = as.numeric(m_p),
    row.names = NULL, stringsAsFactors = FALSE
  )
}


# ---- number formatting ---------------------------------------------------

.azul_survtable_fmt_num <- function(x, digits) formatC(x, format = "f", digits = digits)

# adaptive: very small / very large magnitudes (e.g. an exponentiated baseline
# hazard) print in scientific form instead of collapsing to "0.00"
.azul_survtable_fmt_val <- function(x, digits) {
  ax <- abs(x)
  if (is.finite(x) && ax != 0 && (ax < 0.01 || ax >= 1e4)) {
    formatC(x, format = "e", digits = 2)
  } else {
    formatC(x, format = "f", digits = digits)
  }
}

# p-values are always to 3 dp, with a "<0.001" floor, independent of `digits`
.azul_survtable_fmt_p <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
}

# one estimate/CI/SE triplet for a metric, honouring `exponentiate`
.azul_survtable_fmt_metric <- function(est, se, exponentiate, digits) {
  z <- stats::qnorm(0.975)
  lo <- est - z * se
  hi <- est + z * se
  if (exponentiate) {
    e  <- exp(est); l <- exp(lo); h <- exp(hi); s <- exp(est) * se  # delta SE
  } else {
    e <- est; l <- lo; h <- hi; s <- se
  }
  # estimate & CI use adaptive scientific for extreme magnitudes; SE always
  # respects `digits` (fixed decimals) so the column stays compact
  list(est = .azul_survtable_fmt_val(e, digits),
       ci  = paste0(.azul_survtable_fmt_val(l, digits), ", ", .azul_survtable_fmt_val(h, digits)),
       se  = .azul_survtable_fmt_num(s, digits))
}


# ---- body: grouped rows (variable header / reference / levels) -----------

.azul_survtable_body <- function(model, est, exponentiate, info, digits) {
  blank <- ""
  rows  <- list()

  # rowtype drives styling: intercept | group | numeric | ref | level | flat
  add_row <- function(label, indent, rowtype, coefname = NA) {
    if (rowtype == "group") {
      r <- data.frame(term = label, indent = indent, rowtype = rowtype,
                      ph_est = blank, ph_ci = blank, ph_se = blank,
                      aft_est = blank, aft_ci = blank, aft_se = blank,
                      stat = blank, p = blank, stringsAsFactors = FALSE)
    } else if (rowtype == "ref") {
      r <- data.frame(term = label, indent = indent, rowtype = rowtype,
                      ph_est = "Ref.", ph_ci = "Ref.", ph_se = "Ref.",
                      aft_est = "Ref.", aft_ci = "Ref.", aft_se = "Ref.",
                      stat = "Ref.", p = "Ref.", stringsAsFactors = FALSE)
    } else {
      i <- match(coefname, est$term)
      L <- .azul_survtable_fmt_metric(est$left_est[i], est$left_se[i], exponentiate, digits)
      A <- .azul_survtable_fmt_metric(est$aft_est[i],  est$aft_se[i],  exponentiate, digits)
      r <- data.frame(term = label, indent = indent, rowtype = rowtype,
                      ph_est = L$est, ph_ci = L$ci, ph_se = L$se,
                      aft_est = A$est, aft_ci = A$ci, aft_se = A$se,
                      stat = .azul_survtable_fmt_num(est$z[i], digits),
                      p = .azul_survtable_fmt_p(est$p[i]),
                      stringsAsFactors = FALSE)
    }
    rows[[length(rows) + 1L]] <<- r
  }

  # Intercept first
  if ("(Intercept)" %in% est$term) add_row("Intercept", 0L, "intercept", "(Intercept)")

  xlev  <- model$xlevels
  labs  <- attr(stats::terms(model), "term.labels")
  used  <- "(Intercept)"

  for (v in labs) {
    if (!is.null(xlev[[v]])) {                     # factor variable
      levs <- xlev[[v]]
      add_row(v, 0L, "group")
      add_row(levs[1], 1L, "ref")                  # reference level
      for (lv in levs[-1]) {
        cn <- paste0(v, lv)
        if (cn %in% est$term) { add_row(lv, 1L, "level", cn); used <- c(used, cn) }
      }
    } else if (v %in% est$term) {                  # numeric / plain term
      add_row(v, 0L, "numeric", v); used <- c(used, v)
    }
  }
  # any coefficients not yet placed (interactions, transforms) -> flat rows
  for (cn in setdiff(est$term, used)) add_row(cn, 0L, "flat", cn)

  do.call(rbind, rows)
}


# ---- flextable assembly (APA borders, spanners, caption, footnote) -------

.azul_survtable_flextable <- function(body, model, info, exponentiate, outcome,
                               font, font_size) {
  est_l <- if (exponentiate) info$est_ratio else info$est_log
  est_r <- if (exponentiate) "TR" else "log(time)"

  # a narrow empty "gap" column (j = 5) separates the PH and AFT blocks so their
  # spanner underlines do not run into each other
  dat <- body[, c("term", "ph_est", "ph_ci", "ph_se",
                  "aft_est", "aft_ci", "aft_se", "stat", "p")]
  dat$gap <- ""
  dat <- dat[, c("term", "ph_est", "ph_ci", "ph_se", "gap",
                 "aft_est", "aft_ci", "aft_se", "stat", "p")]

  ft <- flextable::flextable(dat)

  # column-label (lower header) row
  ft <- flextable::set_header_labels(
    ft,
    term = "Variables",
    ph_est = est_l, ph_ci = "95% CI", ph_se = "SE", gap = "",
    aft_est = est_r, aft_ci = "95% CI", aft_se = "SE",
    stat = "Statistic", p = "p-value")

  # spanner (upper header) row: PH(3) | gap(1) | AFT(3)
  ft <- flextable::add_header_row(
    ft, top = TRUE,
    values = c("Variables", info$left_span, "",
               "Accelerated Failure Time (AFT)", "Statistic", "p-value"),
    colwidths = c(1, 3, 1, 3, 1, 1))

  # merge the single-line columns across both header rows
  ft <- flextable::merge_v(ft, part = "header", j = c(1, 9, 10))

  # caption
  outcome_lab <- if (is.null(outcome)) .azul_survtable_outcome(model) else outcome
  caption <- sprintf(
    "%s and accelerated failure time survival model for %s, assuming %s distribution",
    info$caption_metric, outcome_lab, info$dist_label)
  ft <- flextable::set_caption(ft, caption = caption)

  # footnotes
  notes <- character(0)
  if (info$dist == "weibull") notes <- c(notes, .azul_survtable_weibull_note(model))
  notes <- c(notes,
    "Statistic and p-value are the Wald test on the log scale; identical for HR/OR and TR.")
  ft <- flextable::add_footer_lines(ft, values = notes)

  # ----- fonts (single family throughout, matching the caption) -----
  ft <- flextable::font(ft, fontname = font, part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::fontsize(ft, size = font_size - 1, part = "footer")

  # ----- alignment -----
  val_cols <- c(2, 3, 4, 6, 7, 8, 9, 10)
  ft <- flextable::align(ft, part = "header", align = "center")
  ft <- flextable::align(ft, j = 1, part = "all", align = "left")
  ft <- flextable::align(ft, j = val_cols, part = "body", align = "center")
  ft <- flextable::bold(ft, part = "header", bold = TRUE)
  ft <- flextable::valign(ft, part = "header", valign = "center")

  # ----- row spacing: blocks separated, levels within a block kept tight -----
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "body")
  starts <- which(body$rowtype %in% c("intercept", "group", "numeric", "flat"))
  if (length(starts)) ft <- flextable::padding(ft, i = starts, padding.top = 7, part = "body")

  # indent factor levels; bold the variable-name rows (factors + numerics)
  ind <- which(body$indent == 1L)
  if (length(ind)) ft <- flextable::padding(ft, i = ind, j = 1, padding.left = 16)
  vbold <- which(body$rowtype %in% c("group", "numeric", "flat"))
  if (length(vbold)) ft <- flextable::bold(ft, i = vbold, j = 1, part = "body")

  # ----- APA borders (top, under each spanner, under header, bottom only) ----
  bd <- officer::fp_border(color = "black", width = 1)
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_top(ft, part = "header", border = bd)             # top rule
  ft <- flextable::hline(ft, i = 1, j = 2:4, part = "header", border = bd)  # under PH spanner
  ft <- flextable::hline(ft, i = 1, j = 6:8, part = "header", border = bd)  # under AFT spanner
  ft <- flextable::hline_bottom(ft, part = "header", border = bd)          # under column labels
  ft <- flextable::hline_bottom(ft, part = "body", border = bd)            # bottom rule

  # ----- footer: tighter line spacing -----
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "footer")
  ft <- flextable::line_spacing(ft, space = 1, part = "footer")

  # ----- column widths (balanced) -----
  ft <- flextable::width(ft, j = 1, width = 1.40)
  ft <- flextable::width(ft, j = c(2, 6), width = 0.68)
  ft <- flextable::width(ft, j = c(3, 7), width = 1.35)
  ft <- flextable::width(ft, j = c(4, 8), width = 0.60)
  ft <- flextable::width(ft, j = 5, width = 0.18)   # gap
  ft <- flextable::width(ft, j = 9, width = 0.70)
  ft <- flextable::width(ft, j = 10, width = 0.65)

  ft
}


# ---- helpers: outcome name & Weibull footnote ----------------------------

.azul_survtable_outcome <- function(model) {
  resp <- attr(stats::terms(model), "variables")[[2]]  # e.g. Surv(time, status)
  if (is.call(resp) && length(resp) >= 2) {
    return(deparse(resp[[2]]))                          # first Surv() arg = time
  }
  deparse(resp)
}

# Weibull footnote: shape = 1/sigma with 95% CI (symmetric on log scale, as in
# flexsurv), and scale = sigma. Matches survreg's reported "Scale=" and flexsurv
# shape estimate/CI.
.azul_survtable_weibull_note <- function(model, digits = 2) {
  sig   <- model$scale
  shape <- 1 / sig
  V     <- stats::vcov(model)
  ti    <- which(rownames(V) %in% c("Log(scale)", "log(scale)"))
  se_t  <- sqrt(V[ti, ti])                       # SE of log(scale) = SE of -log(shape)
  z     <- stats::qnorm(0.975)
  lo    <- exp(log(shape) - z * se_t)
  hi    <- exp(log(shape) + z * se_t)
  f <- function(x) formatC(x, format = "f", digits = digits)
  sprintf("Weibull shape = %s (95%% CI: %s, %s); scale = %s.",
          f(shape), f(lo), f(hi), f(sig))
}

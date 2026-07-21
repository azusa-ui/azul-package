# ---------------------------------------------------------------------------
# Interpret results that are already in a table: a data.frame of estimates, a
# pasted coefficient table, or a gtsummary object.
# ---------------------------------------------------------------------------

# effect metadata: word = phrase after "times", noun = short noun for plain line
.effect_meta <- list(
  OR   = list(word = "the odds of",              noun = "odds",          lab = "OR"),
  HR   = list(word = "the hazard of",            noun = "hazard",        lab = "HR"),
  IRR  = list(word = "the expected rate of",     noun = "rate",          lab = "IRR"),
  RRR  = list(word = "the relative risk of",     noun = "relative risk", lab = "RRR"),
  beta = list(word = NULL,                        noun = NULL,            lab = "b"),
  mean_diff = list(word = NULL,                    noun = NULL,            lab = "mean difference")
)

#' Interpret a table of results
#'
#' Turns a data frame of estimates (or a \code{gtsummary} table) into a
#' manuscript-style interpretation. Use this when you only have the results
#' table, not the fitted model. If the table has an outcome-category column
#' (for multinomial results) the output is grouped and labelled per category,
#' with a plain-language sentence for each row, just like the model path.
#'
#' Best results come from passing the fitted model to \code{\link{interpret}}
#' directly, because azul can then read the true category and reference labels.
#'
#' @param x A data frame (or coercible) with a term/label column, a
#'   point-estimate column, confidence limits and a P value. Column names are
#'   matched flexibly (term/variable/label; estimate/OR/HR/RRR/b;
#'   conf.low/lower/LL; conf.high/upper/UL; p/p.value/P; and, optionally,
#'   category/y.level/comparison for multinomial tables).
#' @param type Effect measure: "OR", "HR", "IRR", "RRR", "beta", "mean_diff".
#' @param outcome Name of the outcome/event for the prose.
#' @param outcome_ref Optional label of the reference outcome category
#'   (for multinomial tables, e.g. "above_normal").
#' @param adjusted Logical; whether estimates are adjusted (labels Adjusted/Crude).
#' @param expanded Logical; if TRUE (default) each row gets a subheading and a
#'   plain-language sentence; if FALSE, one compact sentence per row.
#' @param ... Styling args (alpha, markup, ci_sep).
#' @return An \code{azul_interpretation} object.
#' @examples
#' tab <- data.frame(term = c("Male", "Age"),
#'                   OR = c(1.49, 1.03), LL = c(1.24, 1.01), UL = c(1.78, 1.05),
#'                   p = c(0.0004, 0.02))
#' interpret_table(tab, type = "OR", outcome = "coronary artery disease")
#' @export
interpret_table <- function(x, type = c("OR","HR","IRR","RRR","beta","mean_diff"),
                            outcome = "the outcome", outcome_ref = NULL,
                            adjusted = TRUE, expanded = TRUE, ...) {
  type <- match.arg(type)
  a <- azul_args(...)
  if (inherits(x, "gtsummary"))
    return(interpret(x, type = type, outcome = outcome, adjusted = adjusted, ...))
  df <- as.data.frame(x)
  pick <- function(cands) {
    hit <- which(tolower(names(df)) %in% tolower(cands))
    if (length(hit)) names(df)[hit[1]] else NA_character_
  }
  term_c <- pick(c("term","variable","label","factor","predictor","name"))
  est_c  <- pick(c("estimate","or","hr","irr","rrr","b","beta","coef","mean_diff","md"))
  lo_c   <- pick(c("conf.low","lower","ll","ci_low","lci","x2.5.."))
  hi_c   <- pick(c("conf.high","upper","ul","ci_high","uci","x97.5.."))
  p_c    <- pick(c("p.value","p","pval","p_value","sig"))
  cat_c  <- pick(c("y.level","category","outcome_level","response_level","comparison","logit"))
  ref_c  <- pick(c("reference","ref","ref_level","baseline"))
  if (is.na(est_c)) stop("Could not find an estimate column in the table.", call. = FALSE)

  meta <- .effect_meta[[type]]
  lab <- if (type %in% c("OR","HR","IRR","RRR"))
    paste0(if (adjusted) "Adjusted " else "Crude ", meta$lab) else meta$lab
  adjnote <- if (adjusted) ", with the other predictors adjusted for" else ""

  getv <- function(col, i, num = TRUE) {
    if (is.na(col)) return(if (num) NA_real_ else NA_character_)
    v <- df[[col]][i]
    if (num) suppressWarnings(as.numeric(v)) else as.character(v)
  }

  row_block <- function(i) {
    term <- if (!is.na(term_c)) as.character(df[[term_c]][i]) else paste("predictor", i)
    est <- getv(est_c, i); lo <- getv(lo_c, i); hi <- getv(hi_c, i); p <- getv(p_c, i)
    cat <- getv(cat_c, i, num = FALSE)
    pref <- getv(ref_c, i, num = FALSE)
    ci <- if (!is.na(lo) && !is.na(hi)) paste0("(", lab, " ", fmt_ci(est, lo, hi), "; ", fmt_p(p, a$markup), ")")
          else paste0("(", lab, " = ", fmt_num(est), "; ", fmt_p(p, a$markup), ")")
    # non-ratio effects (beta / mean difference)
    if (is.null(meta$word)) {
      main <- paste0(term, if (type == "mean_diff") " showed a mean difference of " else
                     " had a regression coefficient of ", fmt_num(est), " ", ci, adjnote, ".")
      plain <- paste0("This indicates that a one-unit change in ", term, " was associated with a ",
                      fmt_num(abs(est)), "-unit ", if (est >= 0) "increase" else "decrease", " in ", outcome, ".")
      sub <- paste0(term, ":")
      return(if (expanded) c(sub, main, plain) else main)
    }
    # ratio effects (OR/HR/IRR/RRR)
    tgt <- if (!is.na(cat)) paste0("being in the ", cat, " category",
              if (!is.null(outcome_ref)) paste0(" (relative to ", outcome_ref, ")") else "") else outcome
    relto <- if (!is.na(pref)) paste0(", relative to the ", pref, " group") else ", relative to the reference category"
    main <- paste0(term, " was associated with ", fmt_num(est), " times ", meta$word, " ", tgt, " ",
                   ci, relto, adjnote, ".")
    if (est >= 1) {
      pct <- (est - 1) * 100
      times <- if (est >= 2) paste0(" (approximately ", fmt_num(est, 1), " times the ", meta$noun, ")") else ""
      plain <- paste0("This indicates about ", fmt_num(pct, 1), "% higher ", meta$noun,
                      " of ", tgt, times, ".")
    } else {
      plain <- paste0("This indicates about ", fmt_num((1 - est) * 100, 1), "% lower ", meta$noun,
                      " of ", tgt, ".")
    }
    sub <- if (!is.na(cat)) paste0(term, ", ", cat,
             if (!is.null(outcome_ref)) paste0(" vs ", outcome_ref), ":") else paste0(term, ":")
    if (expanded) c(sub, main, plain) else main
  }

  if (expanded) {
    lead <- paste0("Interpretation of the supplied ", meta$lab, " table for ", outcome,
                   ". Each row contrasts the stated level against its reference.")
    paras <- c(lead, unlist(lapply(seq_len(nrow(df)), row_block), use.names = FALSE))
  } else {
    paras <- paste(vapply(seq_len(nrow(df)), row_block, character(1)), collapse = " ")
  }
  new_interpretation(paste0("Results table (", meta$lab, ")"), paras,
    notes = paste0("Interpreted from a supplied table as ", meta$lab,
      "; verify the effect measure, reference categories and adjustment set against the source. ",
      "For the richest, fully-labelled output, pass the fitted model to interpret() instead."),
    estimates = df)
}

#' @rdname interpret
#' @export
interpret.gtsummary <- function(object, type = "OR", outcome = "the outcome", adjusted = TRUE, ...) {
  tb <- tryCatch(object$table_body, error = function(e) NULL)
  if (is.null(tb)) stop("Could not read table_body from the gtsummary object.", call. = FALSE)
  # regression tables carry estimate/conf.low/conf.high/p.value columns
  if (all(c("estimate") %in% names(tb))) {
    keep <- tb[!is.na(tb$estimate), , drop = FALSE]
    df <- data.frame(term = keep$label,
                     estimate = keep$estimate,
                     conf.low = keep$conf.low %||% NA_real_,
                     conf.high = keep$conf.high %||% NA_real_,
                     p.value = keep$p.value %||% NA_real_,
                     stringsAsFactors = FALSE)
    # gtsummary already exponentiates when exponentiate=TRUE; assume estimates as-is
    return(interpret_table(df, type = type, outcome = outcome, adjusted = adjusted, ...))
  }
  new_interpretation("gtsummary table",
    "This appears to be a descriptive gtsummary table. Summarise the reported n (%), mean (SD) or median (IQR) directly; no inferential estimate column was found.",
    notes = "For regression tables built with tbl_regression(), azul extracts the estimates automatically.")
}

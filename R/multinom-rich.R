# ---------------------------------------------------------------------------
# Shared "expanded" multinomial writer. Produces, for each predictor x outcome
# category: a labelled subheading, a manuscript sentence with the actual
# category and reference labels, and a plain-language interpretation sentence.
# Used by interpret.multinom (nnet) and interpret.vglm (multinomial).
# ---------------------------------------------------------------------------

.cap1 <- function(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2))

.join_and <- function(x) {
  x <- x[nzchar(x)]
  if (!length(x)) return("")
  if (length(x) == 1) return(x[1])
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

# Parse a coefficient term like "raceBlack" into its factor and level.
.parse_pred_term <- function(term, xlevels) {
  for (v in names(xlevels)) {
    if (startsWith(term, v)) {
      lev <- substring(term, nchar(v) + 1)
      if (lev %in% xlevels[[v]])
        return(list(var = v, level = lev, ref = xlevels[[v]][1], is_factor = TRUE))
    }
  }
  list(var = term, level = NA, ref = NA, is_factor = FALSE)
}

.rrr_sub <- function(r) {
  if (r$is_factor)
    paste0(.cap1(r$var), " (", r$level, " vs ", r$ref, "), ", r$cat, " vs ", r$outref, ":")
  else
    paste0(r$term, ", ", r$cat, " vs ", r$outref, ":")
}

.rrr_main <- function(r, markup) {
  ci <- paste0("(95% CI: ", fmt_num(r$lo), ", ", fmt_num(r$hi), "; ", fmt_p(r$p, markup), ")")
  adj <- if (length(r$adjust)) paste0(", after adjusting for ", .join_and(r$adjust)) else ""
  if (r$is_factor)
    paste0("The ", r$level, " group had a relative risk ratio of ", fmt_num(r$rrr), " ", ci,
           " of being in the ", r$cat, " category compared with the ", r$outref,
           " category, relative to the ", r$ref, " group", adj, ".")
  else
    paste0("For each one-unit increase in ", r$term, ", the relative risk ratio of being in the ",
           r$cat, " category relative to ", r$outref, " was ", fmt_num(r$rrr), " ", ci, adj, ".")
}

.rrr_plain <- function(r) {
  if (is.na(r$rrr)) return("")
  if (r$rrr >= 1) {
    pct <- (r$rrr - 1) * 100
    times <- if (r$rrr >= 2) paste0(" (approximately ", fmt_num(r$rrr, 1), " times the relative risk)") else ""
    if (r$is_factor)
      paste0("This indicates that the ", r$level, " group had about ", fmt_num(pct, 0),
             "% higher relative risk of being in the ", r$cat, " category (relative to ", r$outref,
             ")", times, ", compared with the ", r$ref, " group.")
    else
      paste0("This indicates that a one-unit increase was associated with about ", fmt_num(pct, 0),
             "% higher relative risk of being in the ", r$cat, " category (relative to ", r$outref, ")", times, ".")
  } else {
    red <- (1 - r$rrr) * 100
    if (r$is_factor)
      paste0("This indicates that the ", r$level, " group had about ", fmt_num(red, 0),
             "% lower relative risk of being in the ", r$cat, " category (relative to ", r$outref,
             "), compared with the ", r$ref, " group.")
    else
      paste0("This indicates that each one-unit increase was associated with about ", fmt_num(red, 0),
             "% lower relative risk of being in the ", r$cat, " category (relative to ", r$outref, ").")
  }
}

# rows: list of row-lists with fields is_factor, var, level, ref, term, cat,
#       outref, rrr, lo, hi, p, adjust. Returns a character vector (blocks).
.multinom_rich <- function(rows, lead, markup) {
  blocks <- lead
  for (r in rows)
    blocks <- c(blocks, .rrr_sub(r), .rrr_main(r, markup), .rrr_plain(r))
  blocks
}

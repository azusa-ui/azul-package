# ---------------------------------------------------------------------------
# PRISMA flow interpretation for systematic / scoping reviews.
# Turns the identification -> screening -> eligibility -> included counts into
# the standard flow narrative, with an internal consistency check.
# Reference: Page et al. (2021) PRISMA 2020 (BMJ); Tricco et al. (2018)
# PRISMA-ScR (Ann Intern Med) for scoping reviews.
# ---------------------------------------------------------------------------

#' Interpret a PRISMA study-selection flow
#'
#' Produces the PRISMA flow narrative from the study-selection counts, with an
#' internal consistency check that the numbers reconcile across stages.
#'
#' @param x A named list with any of: \code{identified} (records from
#'   databases), \code{registers}, \code{other_sources} (grey literature /
#'   citation searching), \code{duplicates} (removed), \code{screened},
#'   \code{excluded_screen}, \code{sought} (reports sought for retrieval),
#'   \code{not_retrieved}, \code{assessed} (full text assessed),
#'   \code{excluded_fulltext}, \code{included} (studies included).
#' @param databases Optional character vector of database names.
#' @param reasons Optional named numeric vector of full-text exclusion reasons.
#' @param review "systematic" (default) or "scoping".
#' @param register Optional protocol registration (e.g. "PROSPERO CRD4202XXXXXX").
#' @param ... Styling args.
#' @return An \code{azul_interpretation} object.
#' @examples
#' interpret_prisma(list(identified = 2450, other_sources = 30, duplicates = 610,
#'   screened = 1870, excluded_screen = 1690, assessed = 180,
#'   excluded_fulltext = 158, included = 22),
#'   databases = c("PubMed", "Scopus", "Web of Science"),
#'   reasons = c("wrong outcome" = 70, "wrong design" = 58, "no full text" = 30))
#' @export
interpret_prisma <- function(x, databases = NULL, reasons = NULL,
                             review = c("systematic", "scoping"), register = NULL, ...) {
  a <- azul_args(...)
  review <- match.arg(review)
  g <- function(nm) if (!is.null(x[[nm]]) && !is.na(x[[nm]][1])) as.numeric(x[[nm]]) else NA_real_
  ff <- function(v) fmt_num(v, 0)
  db <- if (!is.null(databases)) paste0(" (", paste(databases, collapse = ", "), ")") else ""

  ident <- g("identified"); other <- g("other_sources"); reg <- g("registers")
  total_ident <- sum(c(ident, other, reg), na.rm = TRUE)
  s1 <- paste0("A total of ", ff(total_ident), " records were identified",
    if (!is.na(ident)) paste0(", ", ff(ident), " through database searching", db) else "",
    if (!is.na(reg)) paste0(", ", ff(reg), " from trial registers") else "",
    if (!is.na(other)) paste0(if (!is.na(ident) || !is.na(reg)) " and " else ", ",
      ff(other), " from other sources (grey literature or citation searching)") else "", ".")

  dup <- g("duplicates"); scr <- g("screened")
  s2 <- if (!is.na(dup))
    paste0("After ", ff(dup), " duplicate", if (dup != 1) "s were" else " was", " removed, ",
           if (!is.na(scr)) ff(scr) else ff(total_ident - dup), " records were screened by title and abstract.")
    else if (!is.na(scr)) paste0(ff(scr), " records were screened by title and abstract.") else NULL

  exs <- g("excluded_screen"); sought <- g("sought"); notret <- g("not_retrieved")
  s3 <- if (!is.na(exs))
    paste0("Of these, ", ff(exs), " were excluded, leaving ",
           if (!is.na(sought)) ff(sought) else ff(scr - exs), " reports sought for retrieval",
           if (!is.na(notret)) paste0(", of which ", ff(notret), " could not be retrieved") else "", ".")
    else NULL

  ass <- g("assessed"); exf <- g("excluded_fulltext"); inc <- g("included")
  reastxt <- if (!is.null(reasons))
    paste0(" (excluded for: ", paste0(names(reasons), " n = ", reasons, collapse = "; "), ")") else ""
  s4 <- if (!is.na(ass))
    paste0(ff(ass), " full-text reports were assessed for eligibility",
           if (!is.na(exf)) paste0(", of which ", ff(exf), " were excluded", reastxt) else "", ".")
    else NULL
  s5 <- if (!is.na(inc))
    paste0("A final ", ff(inc), " stud", if (inc != 1) "ies" else "y", " met the inclusion criteria and ",
           if (inc != 1) "were" else "was", " included in the ", review, " review.",
           if (!is.null(register)) paste0(" The protocol was registered (", register, ").") else "") else NULL

  paras <- stats::na.omit(c(s1, s2, s3, s4, s5)); paras <- paras[nzchar(paras)]

  # internal consistency checks
  notes <- character(0)
  if (!is.na(scr) && !is.na(exs) && !is.na(ass) && abs((scr - exs) - ass) > max(notret, 0, na.rm = TRUE) + 0.5 && is.na(sought))
    notes <- c(notes, "Records screened minus excluded does not equal full texts assessed; check the flow counts.")
  if (!is.na(ass) && !is.na(exf) && !is.na(inc) && (ass - exf) != inc)
    notes <- c(notes, paste0("Full texts assessed (", ff(ass), ") minus excluded (", ff(exf),
      ") does not equal included (", ff(inc), "); reconcile the numbers."))
  if (!length(notes)) notes <- "Study-selection counts reconcile across stages."

  new_interpretation(paste0("PRISMA flow (", review, " review)"), paras,
    c("Report the databases searched, the search dates, and the full search strategy.",
      "Give the reasons and counts for full-text exclusions.",
      "Register the protocol in advance (PROSPERO for systematic reviews; OSF for scoping reviews).",
      "Reference: Page et al. (2021) PRISMA 2020; Tricco et al. (2018) PRISMA-ScR."),
    notes = notes)
}

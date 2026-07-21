# ---------------------------------------------------------------------------
# Core engine: the interpret() generic and the azul_interpretation container.
# ---------------------------------------------------------------------------

#' Construct an azul_interpretation object
#'
#' Internal constructor used by all methods to return a consistent object.
#'
#' @param method Character name of the analysis/test.
#' @param paragraph Character vector: one or more manuscript-style paragraphs.
#' @param assumptions Character vector of assumption reminders (optional).
#' @param estimates Optional data frame of the tidy estimates used.
#' @param notes Optional character vector of caveats.
#' @return An object of class "azul_interpretation".
#' @keywords internal
new_interpretation <- function(method, paragraph, assumptions = character(),
                               estimates = NULL, notes = character()) {
  structure(
    list(method = method,
         paragraph = paragraph,
         assumptions = assumptions,
         estimates = estimates,
         notes = notes),
    class = "azul_interpretation"
  )
}

#' Interpret a statistical model, test or result table
#'
#' \code{interpret()} takes a fitted model object, an \code{htest}, or a table
#' of results and returns a manuscript-ready interpretation written in
#' third-person past-tense academic prose following the MJMS reporting style
#' (Arifin et al. 2016) and SAMPL guidelines. Every point estimate is reported
#' with its 95 percent confidence interval, the reference category is stated for
#' categorical predictors, the test is named, and the key assumptions to verify
#' are listed.
#'
#' Supported inputs include: \code{lm}, \code{glm} (binomial, Poisson,
#' quasi-Poisson), \code{MASS::glm.nb}, \code{survival::coxph} and
#' \code{clogit}, \code{survfit}, \code{survdiff}, \code{survreg},
#' \code{MASS::polr}, \code{ordinal::clm}, \code{nnet::multinom},
#' \code{lme4::lmerMod}/\code{glmerMod}, \code{nlme::lme}, \code{manova},
#' \code{lavaan}, and \code{htest} objects (t-test, chi-square, Fisher,
#' correlation, ANOVA via \code{aov}, Wilcoxon, Kruskal-Wallis, McNemar).
#' A results \code{data.frame} or a \code{gtsummary} table can be interpreted
#' with \code{\link{interpret_table}}.
#'
#' @param object The object to interpret.
#' @param ... Passed to methods. Common arguments: \code{outcome} (name of the
#'   dependent variable), \code{unit} (measurement unit of a numeric predictor),
#'   \code{alpha} (significance level, default 0.05), \code{markup}
#'   ("none" or "markdown"), \code{ci_sep} ("," or "to").
#' @return An object of class \code{azul_interpretation}. Print it to see the
#'   paragraph, or use \code{as.character()} to extract the text.
#' @examples
#' m <- lm(mpg ~ wt + hp, data = mtcars)
#' interpret(m, outcome = "fuel economy")
#' @export
interpret <- function(object, ...) {
  UseMethod("interpret")
}

#' @rdname interpret
#' @export
interpret.default <- function(object, ...) {
  cls <- paste(class(object), collapse = "/")
  stop("azul has no interpret() method for objects of class '", cls,
       "'. Supported: lm, glm, glm.nb, coxph, clogit, survfit, survdiff, ",
       "survreg, polr, clm, multinom, lmerMod, glmerMod, lme, manova, lavaan, ",
       "htest, and result tables via interpret_table().", call. = FALSE)
}

# Split prose into sentences without breaking decimals (e.g. 0.25) or the
# common abbreviations used in the interpretations (e.g., i.e., vs.).
.split_sentences <- function(txt) {
  txt <- paste(txt, collapse = " ")
  # protect abbreviations and decimals
  prot <- txt
  prot <- gsub("e\\.g\\.", "e<DOT>g<DOT>", prot)
  prot <- gsub("i\\.e\\.", "i<DOT>e<DOT>", prot)
  prot <- gsub("vs\\.",    "vs<DOT>",      prot)
  prot <- gsub("(\\d)\\.(\\d)", "\\1<DOT>\\2", prot)  # decimals
  # split after a period followed by a space and a letter or "(" (sentences may
  # start with a lower-case predictor name, e.g. "raceBlack", "wt", "smokeYes")
  parts <- strsplit(prot, "(?<=\\.)\\s+(?=[A-Za-z(])", perl = TRUE)[[1]]
  parts <- gsub("<DOT>", ".", parts)
  trimws(parts[nzchar(trimws(parts))])
}

# section header used in the console print
.hdr <- function(title) {
  cat("\n", title, "\n", sep = "")
  cat(strrep("-", nchar(title)), "\n", sep = "")
}

#' @export
print.azul_interpretation <- function(x, ...) {
  cat("=======================================================================\n")
  cat("  azul interpretation: ", x$method, "\n", sep = "")
  cat("=======================================================================\n")

  .hdr("INTERPRETATION")
  # split each stored block independently so labelled subheadings (which end
  # in ":" and contain no sentence break) stay on their own line
  sentences <- unlist(lapply(x$paragraph, .split_sentences), use.names = FALSE)
  cat(paste(sentences, collapse = "\n\n"), "\n", sep = "")

  if (length(x$assumptions)) {
    .hdr("ASSUMPTIONS TO CHECK")
    cat(paste0("  - ", x$assumptions), sep = "\n")
    cat("\n")
  }
  if (length(x$notes)) {
    .hdr("NOTES")
    cat(paste0("  - ", x$notes), sep = "\n")
    cat("\n")
  }
  cat("\n(Use as.character() or cat(format()) to get the single-paragraph",
      "version for pasting into a manuscript.)\n")
  invisible(x)
}

#' @export
format.azul_interpretation <- function(x, ...) {
  paste(x$paragraph, collapse = "\n\n")
}

#' @export
as.character.azul_interpretation <- function(x, ...) {
  format(x, ...)
}

# Small helper: pull common styling args with defaults ----------------------
azul_args <- function(...) {
  a <- list(...)
  list(
    outcome = a$outcome %||% NULL,
    unit    = a$unit %||% "unit",
    alpha   = a$alpha %||% 0.05,
    markup  = a$markup %||% "none",
    ci_sep  = a$ci_sep %||% ","
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

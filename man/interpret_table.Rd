\name{interpret_table}
\alias{interpret_table}
\alias{interpret.gtsummary}
\title{Interpret a table of results}
\usage{
interpret_table(x, type = c("OR", "HR", "IRR", "RRR", "beta", "mean_diff"),
  outcome = "the outcome", outcome_ref = NULL, adjusted = TRUE,
  expanded = TRUE, ...)
}
\arguments{
\item{x}{A data frame (or coercible), or a \code{gtsummary} table, with a
term/label column, an estimate column, confidence limits, a P value and,
optionally, an outcome-category column.}
\item{type}{Effect measure: one of \code{"OR"}, \code{"HR"}, \code{"IRR"},
\code{"RRR"}, \code{"beta"}, \code{"mean_diff"}.}
\item{outcome}{Name of the outcome/event for the prose.}
\item{outcome_ref}{Optional reference outcome category (multinomial tables).}
\item{adjusted}{Logical; whether estimates are adjusted (Adjusted/Crude).}
\item{expanded}{Logical; if \code{TRUE} each row gets a subheading and a
plain-language sentence.}
\item{...}{Styling args (\code{alpha}, \code{markup}, \code{ci_sep}).}
}
\value{An \code{azul_interpretation} object.}
\description{
Turns a data frame of estimates (or a \code{gtsummary} object) into a
manuscript-style interpretation. Column names are matched flexibly. Passing
the fitted model to \code{\link{interpret}} gives fully-labelled output.
}
\examples{
tab <- data.frame(term = c("Male", "Age"), OR = c(1.49, 1.03),
                  LL = c(1.24, 1.01), UL = c(1.78, 1.05), p = c(4e-4, 0.02))
interpret_table(tab, type = "OR", outcome = "coronary artery disease")
}

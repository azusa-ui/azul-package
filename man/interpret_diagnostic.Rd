\name{interpret_diagnostic}
\alias{interpret_diagnostic}
\title{Interpret a 2x2 diagnostic test table}
\usage{
interpret_diagnostic(x = NULL, tp = NULL, fp = NULL, fn = NULL, tn = NULL,
  layout = c("test_rows", "truth_rows"), test = "the index test", ...)
}
\arguments{
\item{x}{A 2x2 table or matrix of counts.}
\item{tp, fp, fn, tn}{Alternatively, the four cell counts directly.}
\item{layout}{Cell layout of \code{x}: \code{"test_rows"} (rows = test,
columns = truth, positive first) or \code{"truth_rows"}.}
\item{test}{Name of the index test for the prose.}
\item{...}{Styling args (\code{markup}, \code{ci_sep}).}
}
\value{An \code{azul_interpretation} object.}
\description{
Reports sensitivity, specificity, predictive values, accuracy and likelihood
ratios with Wilson 95\% confidence intervals.
}
\examples{
tb <- matrix(c(85, 15, 20, 180), 2, byrow = TRUE)
interpret_diagnostic(tb, test = "the rapid test")
}

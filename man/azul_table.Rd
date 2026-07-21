\name{azul_table}
\alias{azul_table}
\alias{azul_report}
\title{Publication-ready tables and reports}
\usage{
azul_table(x, measure = NULL, flextable = FALSE, ...)
azul_report(x, file = "azul_report.docx", assumptions = TRUE, ...)
}
\arguments{
\item{x}{An \code{azul_interpretation} or a fitted model.}
\item{measure}{Optional effect-measure label (e.g. "OR"); inferred if NULL.}
\item{flextable}{If TRUE and 'flextable' is installed, return a flextable.}
\item{file}{Output path for the report: \code{.docx} (needs 'officer' +
'flextable') or \code{.html} (no extra dependencies).}
\item{assumptions}{Append \code{\link{check_assumptions}} output for models.}
\item{...}{Passed to \code{\link{interpret}} when \code{x} is a model.}
}
\value{\code{azul_table} returns a data frame (class \code{azul_table}) or a
flextable; \code{azul_report} returns the output file path invisibly.}
\description{
\code{azul_table} turns the estimates from an interpretation into a tidy,
MJMS-style table (estimate with 95\% CI and P value). \code{azul_report} writes
a one-call Word or HTML document bundling the interpretation, the table and the
assumption checks.
}
\examples{
azul_table(glm(am ~ wt + hp, within(mtcars, am <- factor(am)), family = binomial))
\dontrun{
azul_report(lm(mpg ~ wt + hp, mtcars), "report.docx")
}
}

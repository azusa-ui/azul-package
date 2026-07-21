\name{fmt_ci}
\alias{fmt_ci}
\title{Format an estimate with its 95 percent confidence interval}
\usage{fmt_ci(est, lower, upper, dp = 2, label = NULL, ci_sep = ",")}
\arguments{
\item{est}{Point estimate.}
\item{lower, upper}{Confidence limits.}
\item{dp}{Decimal places.}
\item{label}{Optional label placed before the estimate.}
\item{ci_sep}{Separator inside the CI (\code{","} or \code{"to"}).}
}
\value{Character string of the form \code{estimate (95\% CI: lower, upper)}.}
\description{Format a point estimate with its 95 percent confidence interval.}
\examples{fmt_ci(1.49, 1.24, 1.78)}

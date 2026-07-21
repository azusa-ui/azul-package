\name{fmt_p}
\alias{fmt_p}
\title{Format a P value in MJMS style}
\usage{fmt_p(p, markup = c("none", "markdown"), label = "P")}
\arguments{
\item{p}{Numeric P value.}
\item{markup}{One of \code{"none"} or \code{"markdown"} (italicises the label).}
\item{label}{The letter to use (default \code{"P"}).}
}
\value{A character string such as \code{"P = 0.032"} or \code{"P < 0.001"}.}
\description{
Three decimal places; the smallest reported value is \code{P < 0.001}.
}
\examples{fmt_p(0.0324); fmt_p(0.00001)}

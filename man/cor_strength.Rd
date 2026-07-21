\name{cor_strength}
\alias{cor_strength}
\title{Verbal strength of a correlation coefficient}
\usage{cor_strength(r)}
\arguments{\item{r}{Correlation coefficient.}}
\value{Character string: weak, moderate, moderately strong, or strong.}
\description{
Classifies the absolute value of a correlation coefficient using common
epidemiology thresholds.
}
\examples{cor_strength(0.65)}

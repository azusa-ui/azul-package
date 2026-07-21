\name{interpret_smr}
\alias{interpret_smr}
\alias{interpret_lisa}
\alias{interpret_getis}
\title{Interpret spatial epidemiology results}
\usage{
interpret_smr(observed, expected, area = NULL, conf.level = 0.95, ...)
interpret_lisa(x, quadr = NULL, alpha = 0.05, ...)
interpret_getis(z, alpha = 0.05, ...)
}
\arguments{
\item{observed, expected}{Observed and expected event counts for the SMR.}
\item{area}{Optional area labels.}
\item{conf.level}{Confidence level for the SMR interval (default 0.95).}
\item{x}{A local Moran's I (LISA) result with an \code{Ii} column and a
p-value column.}
\item{quadr}{Optional per-location cluster types (High-High, Low-Low, etc.).}
\item{z}{Getis-Ord Gi* z-scores.}
\item{alpha}{Significance level.}
\item{...}{Styling args.}
}
\value{An \code{azul_interpretation} object.}
\description{
Interpret standardised morbidity/mortality ratios (SMR, with Byar's Poisson
CI), local Moran's I (LISA) cluster/outlier results, and Getis-Ord Gi*
hot-spot z-scores. Global Moran's I and Geary's C (\code{htest} objects) and
spatial lag/error regression (\code{Sarlm}) are handled by \code{\link{interpret}}.
Conventions follow Waller & Gotway (2004), Applied Spatial Statistics for
Public Health Data.
}
\examples{
interpret_smr(observed = 120, expected = 90, area = "District A")
}

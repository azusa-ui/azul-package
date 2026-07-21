\name{azul_survcompare}
\alias{azul_survcompare}
\title{Compare parametric survival distributions by AIC}
\usage{
azul_survcompare(formula, data,
  dists = c("exponential", "weibull", "loglogistic", "lognormal"),
  digits = 1, font = "Helvetica", font_size = 10)
}
\arguments{
\item{formula}{A survival formula, e.g. \code{Surv(time, status) ~ sex + age}.}
\item{data}{A data frame.}
\item{dists}{Distributions to compare (default exponential, Weibull,
log-logistic, log-normal).}
\item{digits}{Decimal places for logLik and AIC (default 1).}
\item{font, font_size}{Table font family and size.}
}
\value{A \code{flextable} ranked by AIC, with the preferred model (smallest AIC)
flagged; the tidy comparison data frame is attached as attribute \code{"table"}.}
\description{
Best practice for a parametric survival model is to fit several candidate
distributions and choose the best-fitting by the Akaike information criterion
(lower is better), confirmed with diagnostic plots. This fits each distribution
with \code{survival::survreg()} and returns a ranked comparison table.
}
\examples{
library(survival)
azul_survcompare(Surv(time, status) ~ sex + ph.ecog, data = lung)
}

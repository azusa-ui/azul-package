\name{interpret.Arima}
\alias{interpret.Arima}
\alias{interpret.arima}
\title{Interpret an ARIMA / seasonal ARIMA time series model}
\usage{
\method{interpret}{Arima}(object, ...)
}
\arguments{
\item{object}{A fitted \code{stats::arima} or \code{forecast::Arima} model.}
\item{...}{Styling args; \code{outcome} names the series.}
}
\value{An \code{azul_interpretation} object.}
\description{
Reports the ARIMA(p,d,q)(P,D,Q)[s] order, the differencing applied, the
estimated AR/MA (and seasonal) coefficients with confidence intervals, and the
AIC/BIC. Stationarity (ADF, KPSS) and residual white-noise (Ljung-Box) tests
are interpreted via \code{\link{interpret}} on the corresponding \code{htest}
objects. Reference: Hyndman & Athanasopoulos, Forecasting: Principles and
Practice; Box & Jenkins.
}
\examples{
set.seed(1); x <- ts(cumsum(rnorm(120)), frequency = 12)
m <- arima(x, order = c(1, 1, 1))
interpret(m, outcome = "monthly cases")
}

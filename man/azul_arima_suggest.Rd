\name{azul_arima_suggest}
\alias{azul_arima_suggest}
\title{Suggest ARIMA/SARIMA orders from the data}
\usage{
azul_arima_suggest(x, ...)
}
\arguments{
\item{x}{A time series (\code{ts}) or numeric vector.}
\item{...}{Styling args (markup, alpha).}
}
\value{An \code{azul_interpretation} object.}
\description{
Reports the data-driven building blocks for a Box-Jenkins model: whether a
variance-stabilising transformation is indicated (Box-Cox lambda), how many
regular and seasonal differences are needed (unit-root tests), and a starting
model chosen by \code{forecast::auto.arima}. Use alongside the manual ACF/PACF
reading from \code{\link{azul_plot}}.
}
\examples{
azul_arima_suggest(AirPassengers)
}

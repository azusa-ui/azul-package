\name{interpret_its}
\alias{interpret_its}
\title{Interpret an interrupted time series (segmented regression) model}
\usage{
interpret_its(model, level, trend = "time", slope = NULL, period = 1,
  unit = "time unit", period_label = NULL, outcome = "the outcome", ...)
}
\arguments{
\item{model}{A fitted \code{glm} (Poisson / quasi-Poisson, log link) or
\code{lm} segmented-regression model.}
\item{level}{Name of the intervention indicator coefficient (step / level
change), e.g. \code{"smokban"}.}
\item{trend}{Name of the continuous time coefficient (pre-intervention trend).}
\item{slope}{Name of the intervention-by-time interaction (slope change); if
\code{NULL}, \code{level:trend} / \code{trend:level} is detected automatically.}
\item{period}{Number of time units over which to express the trend (e.g. 12
for a per-year trend from monthly data).}
\item{unit}{Name of one time unit (e.g. "month").}
\item{period_label}{Label for the trend period (e.g. "year").}
\item{outcome}{Name of the outcome for the prose.}
\item{...}{Styling args (\code{alpha}, \code{markup}).}
}
\value{An \code{azul_interpretation} object.}
\description{
Reports the pre-intervention trend, the immediate level (step) change and, when
present, the change in slope, following the Lopez Bernal / Gasparrini
interrupted time series tutorial (Int J Epidemiol. 2017;46(1):348-355).
}
\examples{
set.seed(1); n <- 120
d <- data.frame(time = 1:n, smokban = as.integer(1:n > 60), pop = 1e5)
lp <- -6 + 0.002*d$time - 0.15*d$smokban - 0.004*d$smokban*d$time
d$aces <- rpois(n, exp(lp + log(d$pop)))
m <- glm(aces ~ offset(log(pop)) + smokban*time, family = quasipoisson, data = d)
interpret_its(m, level = "smokban", trend = "time", period = 12,
              unit = "month", period_label = "year", outcome = "acute coronary events")
}

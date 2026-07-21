\name{interpret_additive_interaction}
\alias{interpret_additive_interaction}
\title{Interpret additive interaction (RERI, AP, synergy index)}
\usage{
interpret_additive_interaction(model, exposures, outcome = "the outcome",
  labels = c("exposure A", "exposure B"), ...)
}
\arguments{
\item{model}{A fitted \code{glm} (binomial) or \code{coxph} model with a
four-level composite exposure (doubly-unexposed group as reference).}
\item{exposures}{Length-3 character vector naming the coefficients for the
"A only", "B only" and "A and B" groups, in that order.}
\item{outcome}{Name of the outcome/event for the prose.}
\item{labels}{Optional length-2 labels for exposure A and B.}
\item{...}{Styling args (\code{alpha}, \code{markup}).}
}
\value{An \code{azul_interpretation} object.}
\description{
Reports interaction on the additive scale from a multiplicative model: the
joint effects, the relative excess risk due to interaction (RERI), the
attributable proportion (AP) and the synergy index (S), each with delta-method
95\% confidence intervals (Hosmer & Lemeshow 1992; Rothman).
}
\examples{
set.seed(1); n <- 800
d <- data.frame(a = rbinom(n, 1, .5), b = rbinom(n, 1, .5))
d$y <- rbinom(n, 1, plogis(-2 + 0.7*d$a + 0.6*d$b + 1.1*d$a*d$b))
d$comb <- factor(ifelse(d$a==0 & d$b==0, "00",
            ifelse(d$a==1 & d$b==0, "10",
            ifelse(d$a==0 & d$b==1, "01", "11"))), levels = c("00","10","01","11"))
fit <- glm(y ~ comb, data = d, family = binomial)
interpret_additive_interaction(fit, c("comb10","comb01","comb11"), outcome = "disease")
}

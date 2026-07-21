\name{interpret.rma}
\alias{interpret.rma}
\alias{interpret.meta}
\title{Interpret a meta-analysis}
\usage{
\method{interpret}{rma}(object, ...)
\method{interpret}{meta}(object, ...)
}
\arguments{
\item{object}{A \code{metafor::rma} object or a \code{meta} object
(metabin/metacont/metagen/...).}
\item{...}{Styling args (\code{alpha}, \code{markup}).}
}
\value{An \code{azul_interpretation} object.}
\description{
Reports the pooled effect (exponentiated for ratio measures) with its 95\%
confidence interval, the between-study heterogeneity (I-squared with a verdict,
tau-squared, Cochran's Q), and the number of studies. Reference: Borenstein,
Hedges, Higgins & Rothstein, Introduction to Meta-Analysis; Cochrane Handbook.
}
\examples{
dat <- metafor::escalc(measure = "OR", ai = c(10,12,8), bi = c(40,38,42),
                       ci = c(5,7,6), di = c(45,43,44))
interpret(metafor::rma(yi, vi, data = dat))
}

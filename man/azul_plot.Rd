\name{azul_plot}
\alias{azul_plot}
\title{Draw and interpret the figure for a model}
\usage{
azul_plot(x, type = c("auto", "forest", "km", "roc", "residuals", "forest_meta",
  "ts", "tsdiag", "acf", "caterpillar", "schoenfeld", "funnel", "calibration"),
  main = NULL, ...)
}
\arguments{
\item{x}{A fitted model, an \code{azul_interpretation}, a \code{survfit},
\code{roc}, or meta-analysis object.}
\item{type}{"auto" (default), "forest", "km", "roc", "residuals" or
"forest_meta".}
\item{main}{Optional plot title.}
\item{...}{Passed to the underlying plot.}
}
\value{Invisibly, an \code{azul_interpretation} describing the figure; the plot
is drawn as a side effect.}
\description{
Produces the figure that best matches the analysis and returns a short
interpretation of it: a forest plot of the estimates (regression models), a
Kaplan-Meier curve (\code{survfit}), an ROC curve (\code{pROC}),
residual-diagnostic panels (\code{lm}/\code{glm}), or a meta-analysis forest
plot (\code{metafor}/\code{meta}). Uses base graphics only.
}
\examples{
azul_plot(glm(am ~ wt + hp, within(mtcars, am <- factor(am)), family = binomial))
}

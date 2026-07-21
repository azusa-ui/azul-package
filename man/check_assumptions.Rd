\name{check_assumptions}
\alias{check_assumptions}
\title{Check the assumptions of a fitted model}
\usage{
check_assumptions(model, alpha = 0.05, ...)
}
\arguments{
\item{model}{A fitted \code{lm}, \code{glm} (binomial or Poisson/quasi-Poisson),
\code{coxph}, or \code{aov} model.}
\item{alpha}{Significance level for the diagnostic tests (default 0.05).}
\item{...}{Unused.}
}
\value{An \code{azul_interpretation} object listing each assumption as met or
possibly violated, with the underlying test result.}
\description{
Runs the standard diagnostic tests for the model and reports each assumption as
met or possibly violated: normality (Shapiro-Wilk), homoscedasticity
(Breusch-Pagan), independence (Durbin-Watson), multicollinearity (VIF) and
influence (Cook's distance) for linear models; Hosmer-Lemeshow, events per
variable and VIF for logistic; overdispersion for counts; the global Schoenfeld
test for Cox models; and Shapiro-Wilk plus Levene for ANOVA.
}
\examples{
check_assumptions(lm(mpg ~ wt + hp, mtcars))
}

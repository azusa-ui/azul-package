\name{interpret}
\alias{interpret}
\alias{interpret.default}
\title{Interpret a statistical model, test or result table}
\usage{
interpret(object, ...)
}
\arguments{
\item{object}{The object to interpret: a fitted model, an \code{htest}, a
result table, a raw data frame, or one of the many supported classes.}
\item{...}{Passed to methods. Common arguments: \code{outcome} (name of the
dependent variable/event), \code{unit} (measurement unit of a numeric
predictor), \code{alpha} (significance level, default 0.05), \code{markup}
(\code{"none"} or \code{"markdown"}), \code{ci_sep} (\code{","} or \code{"to"}).}
}
\value{
An object of class \code{azul_interpretation}. Print it for a spaced,
sub-headed layout, or use \code{as.character()} for a single manuscript
paragraph.
}
\description{
\code{interpret()} is an S3 generic that turns a fitted model, hypothesis
test, result table or raw dataset into a manuscript-ready interpretation in
MJMS / DrPH style, always reporting 95\% confidence intervals, naming the
test, stating reference categories, and listing the assumptions to check.
}
\details{
Supported inputs include \code{lm}, \code{glm} (binomial, Poisson,
quasi-Poisson), \code{MASS::glm.nb}, \code{survival::coxph}/\code{clogit},
\code{survfit}, \code{survdiff}, \code{survreg}, \code{MASS::polr},
\code{ordinal::clm}, \code{nnet::multinom}, VGAM \code{vglm},
\code{lme4} and \code{nlme} mixed models, \code{aov}/\code{manova},
\code{lavaan}, \code{geepack::geeglm}, \code{pscl::zeroinfl}, \code{htest}
objects, factor-analysis and reliability objects (\code{psych}),
diagnostic/ROC objects, machine-learning models (\code{rpart},
\code{randomForest}, \code{e1071::svm}, \code{nnet}, \code{neuralnet},
\code{caret::train}), post-hoc objects (\code{TukeyHSD}, \code{emmeans}),
result tables, and raw data frames (descriptive summary).

When a model contains a statistically significant interaction, the marginal
main effects are not interpreted on their own; stratified simple effects are
reported instead.
}
\examples{
m <- lm(mpg ~ wt + hp, data = mtcars)
interpret(m, outcome = "fuel economy")

interpret(t.test(mpg ~ am, data = mtcars))

interpret(mtcars)
}

\name{azul-package}
\alias{azul-package}
\alias{azul}
\docType{package}
\title{azul: Thesis-Style Interpretation of Statistical Models and Tables}
\description{
azul converts fitted models, hypothesis tests, result tables and datasets into
ready-to-paste, manuscript-style interpretations following the Malaysian
Journal of Medical Sciences (MJMS) reporting style (Arifin et al. 2016) and
SAMPL guidelines, and builds publication-ready tables, reports and figures.
}
\section{Workflow}{
\itemize{
  \item \strong{Interpret}: \code{\link{interpret}} (the main generic),
    \code{\link{interpret_table}}, \code{\link{interpret_descriptive}},
    \code{\link{interpret_diagnostic}}, \code{\link{interpret_its}},
    \code{\link{interpret_smr}} and others.
  \item \strong{Check}: \code{\link{check_assumptions}} runs the model diagnostics.
  \item \strong{Tabulate / report}: \code{\link{azul_table}},
    \code{\link{azul_report}}, \code{\link{azul_survtable}},
    \code{\link{azul_survcompare}}.
  \item \strong{Time series}: \code{\link{azul_arima_suggest}}.
  \item \strong{Visualise}: \code{\link{azul_plot}} (forest, KM, ROC, ACF/PACF,
    caterpillar, Schoenfeld, funnel, calibration, effect, scree and more).
}
Run \code{\link{azul_help}()} for a one-line table of contents of every function.
}
\references{
Arifin WN, et al. Reporting statistical results in medical journals.
Malays J Med Sci. 2016;23(5):1-7.
}
\seealso{\code{\link{azul_help}}, \code{\link{interpret}}}
\author{AZul; original survtbl function by Fittri Fahmi (ctb).}

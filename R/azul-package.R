#' azul: Thesis-style interpretation of statistical models and tables
#'
#' The \pkg{azul} package converts fitted models, hypothesis tests and result
#' tables into ready-to-paste, manuscript-style interpretation paragraphs that
#' follow the Malaysian Journal of Medical Sciences (MJMS) reporting style
#' (Arifin et al. 2016) and SAMPL guidelines used in USM MPH/DrPH work.
#'
#' The single entry point is \code{\link{interpret}}, an S3 generic that
#' dispatches on the class of the supplied object. Tables of results are handled
#' by \code{\link{interpret_table}}.
#'
#' @section Conventions:
#' Output is third-person, past-tense academic prose. Every point estimate is
#' reported with its 95 percent confidence interval, the test is named,
#' reference categories are flagged, and the key assumptions to verify are
#' listed. P values follow MJMS style (three decimals; smallest \code{P < 0.001}).
#'
#' @references Arifin WN, et al. Reporting statistical results in medical
#'   journals. Malays J Med Sci. 2016;23(5):1-7.
#' @keywords internal
"_PACKAGE"

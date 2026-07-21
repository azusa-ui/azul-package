#' Overview of the azul package functions
#'
#' Prints a grouped list of the main azul functions with a one-line description
#' of each, as a quick table of contents. For full help on any function use
#' \code{?function_name}; to search, use \code{??azul}.
#'
#' @return Invisibly, \code{NULL} (called for its printed output).
#' @examples
#' azul_help()
#' @export
azul_help <- function() {
  line <- function(fn, desc) cat(sprintf("  %-28s %s\n", fn, desc))
  hd   <- function(t) cat("\n", t, "\n", strrep("-", nchar(t)), "\n", sep = "")
  cat("azul: thesis-style interpretation of statistical models, tests and tables\n")
  cat("Full help: ?function_name   |   Search: ??azul   |   Package help: ?azul\n")

  hd("Interpret (model/test -> MJMS prose)")
  line("interpret()", "the main generic; dispatches on ~50 model and test classes")
  line("interpret_table()", "interpret a results table (or gtsummary object)")
  line("interpret_descriptive()", "Table 1 descriptive narrative of a dataset")
  line("interpret_diagnostic()", "sensitivity/specificity/PPV/NPV/LR from a 2x2 table")
  line("interpret_additive_interaction()", "RERI, AP and synergy index")
  line("interpret_its()", "interrupted time series (level and slope change)")
  line("interpret_smr()", "standardised morbidity/mortality ratio")
  line("interpret_lisa() / interpret_getis()", "local spatial clusters / hot spots")

  hd("Check assumptions")
  line("check_assumptions()", "run the diagnostic tests for lm/glm/coxph/aov")

  hd("Tables and reports")
  line("azul_table()", "publication-ready estimates table (3-line APA)")
  line("azul_report()", "one-call Word/HTML report: prose + table + figure + checks")

  hd("Survival tables")
  line("azul_survtable()", "parametric survival table (PH/PO and AFT metrics)")
  line("azul_survcompare()", "rank parametric distributions by AIC")

  hd("Time series")
  line("azul_arima_suggest()", "data-driven differencing and a starting ARIMA order")

  hd("Figures (draw + interpret)")
  line("azul_plot()", "forest, KM, ROC, residuals, ACF/PACF, ARIMA, caterpillar,")
  cat(sprintf("  %-28s %s\n", "", "Schoenfeld, funnel, calibration, effect, qqrand, scree ..."))

  hd("Formatting helpers")
  line("fmt_num() / fmt_p() / fmt_ci()", "MJMS-style number, P-value and CI formatting")
  line("cor_strength()", "verbal strength of a correlation coefficient")
  cat("\n")
  invisible(NULL)
}

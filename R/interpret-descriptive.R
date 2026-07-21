# ---------------------------------------------------------------------------
# Descriptive interpretation of a raw dataset (Table 1 style narrative),
# following MJMS conventions: mean (SD) for normal numeric, median (IQR) for
# skewed, n (%) for categorical; no P values for baseline description.
# interpret.data.frame routes results tables to interpret_table() and raw
# datasets to interpret_descriptive().
# ---------------------------------------------------------------------------

.skewness <- function(x) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 3) return(0)
  m <- mean(x); s <- stats::sd(x)
  if (is.na(s) || s == 0) return(0)
  (sum((x - m)^3) / n) / (s^3)
}

# describe one numeric vector -> sentence
.desc_numeric <- function(v, x, N, normal, a) {
  miss <- sum(is.na(x)); nn <- N - miss
  xx <- x[!is.na(x)]
  skewed <- switch(normal,
    normal = FALSE, skewed = TRUE,
    auto = abs(.skewness(xx)) > 1)
  misstxt <- if (miss > 0) paste0(" Data were missing for ", miss, " (", fmt_num(100 * miss / N, 1), "%).") else ""
  if (skewed) {
    q <- stats::quantile(xx, c(.25, .5, .75), na.rm = TRUE)
    paste0("The median ", v, " was ", fmt_num(q[2]), " (IQR ", fmt_num(q[1]), " to ",
           fmt_num(q[3]), "; range ", fmt_num(min(xx)), " to ", fmt_num(max(xx)), ").", misstxt)
  } else {
    paste0("The mean ", v, " was ", fmt_num(mean(xx)), " (SD ", fmt_num(stats::sd(xx)),
           "; range ", fmt_num(min(xx)), " to ", fmt_num(max(xx)), ").", misstxt)
  }
}

# describe one categorical vector -> sentence
.desc_categorical <- function(v, x, N, a) {
  miss <- sum(is.na(x)); nn <- N - miss
  ordinal <- is.ordered(x)
  tb <- if (ordinal) table(x) else sort(table(x), decreasing = TRUE)
  parts <- vapply(names(tb), function(l)
    paste0(l, ", ", tb[[l]], " (", fmt_num(100 * tb[[l]] / nn, 1), "%)"), character(1))
  misstxt <- if (miss > 0) paste0(" (missing for ", miss, ", ", fmt_num(100 * miss / N, 1), "%)") else ""
  medtxt <- ""
  if (ordinal) {
    codes <- as.integer(x)
    medlev <- levels(x)[stats::median(codes, na.rm = TRUE)]
    if (!is.na(medlev)) medtxt <- paste0(" The median category was '", medlev, "'.")
  }
  paste0("For ", v, if (ordinal) " (ordinal)" else "", ", the distribution was: ",
         paste(parts, collapse = "; "), misstxt, ".", medtxt)
}

# describe a Date / date-time vector -> sentence
.desc_date <- function(v, x, N, a) {
  miss <- sum(is.na(x)); nn <- N - miss
  xx <- x[!is.na(x)]
  misstxt <- if (miss > 0) paste0(" Data were missing for ", miss, " (", fmt_num(100 * miss / N, 1), "%).") else ""
  paste0("The variable ", v, " (date/time) had ", nn, " non-missing values ranging from ",
         format(min(xx)), " to ", format(max(xx)), ".", misstxt)
}

.is_date <- function(x) inherits(x, c("Date", "POSIXct", "POSIXt"))
.is_binary_num <- function(x) is.numeric(x) && !.is_date(x) && length(unique(x[!is.na(x)])) <= 2

#' Interpret descriptive statistics of a dataset
#'
#' Produces a Table 1 style narrative: mean (SD) for approximately normal
#' numeric variables, median (IQR) for skewed ones, and n (%) for categorical
#' variables, with missing-data counts. No P values are reported for baseline
#' description (per MJMS). Optionally stratify by a grouping variable.
#'
#' @param data A data frame or tibble.
#' @param by Optional name of a grouping variable to stratify by.
#' @param normal How to summarise numeric variables: "auto" (default; skewness
#'   decides mean vs median), "normal", or "skewed".
#' @param max_levels Categorical variables with more levels than this are
#'   summarised by their number of distinct values only.
#' @param ... Styling args (markup, ci_sep).
#' @return An \code{azul_interpretation} object.
#' @export
interpret_descriptive <- function(data, by = NULL, normal = c("auto","normal","skewed"),
                                  max_levels = 12, ...) {
  a <- azul_args(...)
  normal <- match.arg(normal)
  df <- as.data.frame(data)
  N <- nrow(df); vars <- setdiff(names(df), by)

  describe_block <- function(sub, label = NULL) {
    n <- nrow(sub)
    head <- if (is.null(label)) paste0("The dataset comprised ", n, " observations on ", length(vars), " variables.")
            else paste0("Among ", label, " (n = ", n, "):")
    lines <- vapply(vars, function(v) {
      x <- sub[[v]]
      if (.is_date(x)) {
        .desc_date(v, x, n, a)
      } else if (is.factor(x) || is.character(x) || is.logical(x) || .is_binary_num(x)) {
        if (!.is_binary_num(x) && length(unique(x[!is.na(x)])) > max_levels)
          paste0("Variable ", v, " had ", length(unique(x[!is.na(x)])), " distinct categories (too many to list).")
        else .desc_categorical(v, x, n, a)
      } else if (is.numeric(x)) {
        .desc_numeric(v, x, n, normal, a)
      } else {
        paste0("Variable ", v, " was of an unsupported type and was not summarised.")
      }
    }, character(1))
    c(head, lines)
  }

  if (is.null(by)) {
    paras <- describe_block(df)
  } else {
    g <- df[[by]]
    lev <- if (is.factor(g)) levels(g) else sort(unique(g[!is.na(g)]))
    paras <- c(paste0("The dataset comprised ", N, " observations, described below overall and stratified by ", by, "."),
               describe_block(df),
               unlist(lapply(lev, function(l) describe_block(df[!is.na(g) & g == l, , drop = FALSE],
                                                            label = paste0(by, " = ", l))), use.names = FALSE))
  }
  new_interpretation("Descriptive statistics (baseline characteristics)", paras,
    c("Normal numeric variables are summarised as mean (SD); skewed as median (IQR); categorical as n (%).",
      "Baseline descriptive tables should not report P values for group comparisons (the concern is comparability, not populations).",
      "Confirm the normality decision (histogram / Q-Q plot) for key variables."),
    notes = "Descriptive summary generated by azul; verify variable types (coded categoricals may appear as numeric).")
}

#' @rdname interpret
#' @export
interpret.data.frame <- function(object, ...) {
  df <- as.data.frame(object)
  nms <- tolower(names(df))
  est_like <- any(nms %in% c("estimate","or","hr","irr","rrr"))
  info_like <- any(nms %in% c("conf.low","conf.high","lower","upper","ll","ul",
                              "p.value","p","pval","p_value"))
  if (est_like && info_like) {
    type <- if ("or" %in% nms) "OR" else if ("hr" %in% nms) "HR" else if ("rrr" %in% nms) "RRR"
            else if ("irr" %in% nms) "IRR" else "OR"
    return(interpret_table(df, type = type, ...))
  }
  interpret_descriptive(df, ...)
}

#' @rdname interpret
#' @export
interpret.table <- function(object, ...) {
  a <- azul_args(...)
  d <- dim(object)
  # 1-D frequency table -> categorical distribution (+ optional goodness-of-fit)
  if (length(d) == 1 || is.null(d)) {
    nn <- sum(object)
    parts <- vapply(names(object), function(l)
      paste0(l, ", ", object[[l]], " (", fmt_num(100 * object[[l]] / nn, 1), "%)"), character(1))
    body <- paste0("The frequency distribution (n = ", nn, ") was: ", paste(parts, collapse = "; "), ".")
    return(new_interpretation("Frequency distribution", body,
      notes = "Use interpret(chisq.test(x, p = ...)) to test the counts against expected proportions."))
  }
  # 2-D contingency table -> association (chi-square, or Fisher if sparse & 2x2)
  if (length(d) == 2) {
    exp_ok <- tryCatch(all(suppressWarnings(stats::chisq.test(object)$expected) >= 5),
                       error = function(e) TRUE)
    test <- if (all(d == 2) && !exp_ok) stats::fisher.test(object)
            else suppressWarnings(stats::chisq.test(object))
    return(interpret(test, ...))
  }
  new_interpretation("Contingency table",
    "A table with more than two dimensions was supplied; analyse it with a log-linear model (glm Poisson) and interpret that instead.")
}

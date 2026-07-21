# ---------------------------------------------------------------------------
# Machine-learning models (predictive). Interpretation emphasises performance,
# variable importance and the appropriate caveats rather than coefficients/CIs:
#   interpret.rpart, interpret.randomForest, interpret.svm, interpret.nnet,
#   interpret.train (caret), interpret.nn (neuralnet).
# ---------------------------------------------------------------------------

.topn <- function(x, n = 5) {
  x <- sort(x, decreasing = TRUE)
  names(x)[seq_len(min(n, length(x)))]
}

#' @rdname interpret
#' @export
interpret.rpart <- function(object, ...) {
  a <- azul_args(...)
  outcome <- a$outcome %||% tryCatch(all.vars(stats::formula(object))[1], error = function(e) "the outcome")
  cls <- object$method == "class"
  nterm <- sum(object$frame$var == "<leaf>")
  nsplit <- if (!is.null(object$splits)) nrow(object$splits) else NA
  imp <- object$variable.importance
  top <- if (!is.null(imp)) .topn(imp) else character(0)
  body <- c(
    paste0("A classification and regression tree (CART) was grown to predict ", outcome,
           " (", if (cls) "classification" else "regression", " tree)."),
    paste0("The tree had ", nterm, " terminal node", if (nterm != 1) "s" else "",
           if (!is.na(nsplit)) paste0(" derived from candidate splits on the predictors") else "", ". ",
           if (cls) "Each case is assigned the majority class of its terminal node."
           else "Each case is assigned the mean outcome of its terminal node."),
    if (length(top)) paste0("The most important predictors (by total reduction in node impurity) were ",
           .join_and(top), ".") else NULL)
  new_interpretation("Decision tree (CART / rpart)", stats::na.omit(body),
    c("Prune with the complexity parameter (cp), e.g. the 1-SE rule, to avoid overfitting.",
      "Single trees are unstable: small data changes can give a very different tree.",
      if (cls) "Report accuracy, sensitivity/specificity and a confusion matrix on an independent or cross-validated test set."
      else "Report RMSE and R-squared on an independent or cross-validated test set.",
      "Consider ensembles (random forest, boosting) for better predictive performance."))
}

#' @rdname interpret
#' @export
interpret.randomForest <- function(object, ...) {
  a <- azul_args(...)
  reg <- object$type == "regression"
  ntree <- object$ntree
  imp <- tryCatch(object$importance, error = function(e) NULL)
  if (reg) {
    rsq <- object$rsq[length(object$rsq)]; mse <- object$mse[length(object$mse)]
    body <- c(
      paste0("A random forest of ", ntree, " regression trees was trained."),
      paste0("The out-of-bag (OOB) performance was an R-squared of ", fmt_num(rsq, 3),
             " (", fmt_num(rsq * 100, 1), "% of variance explained), with an OOB mean squared error of ",
             fmt_num(mse), " (RMSE ", fmt_num(sqrt(mse)), ")."))
    impcol <- if (!is.null(imp)) intersect(c("%IncMSE","IncNodePurity"), colnames(imp))[1] else NA
  } else {
    oob <- object$err.rate[nrow(object$err.rate), "OOB"]
    cm <- object$confusion
    perclass <- if (!is.null(cm) && "class.error" %in% colnames(cm))
      paste0("Per-class OOB error ranged from ", fmt_num(100 * min(cm[, "class.error"]), 1), "% to ",
             fmt_num(100 * max(cm[, "class.error"]), 1), "%. ") else ""
    body <- c(
      paste0("A random forest of ", ntree, " classification trees was trained."),
      paste0("The out-of-bag (OOB) error rate was ", fmt_num(100 * oob, 1), "% (OOB accuracy ",
             fmt_num(100 * (1 - oob), 1), "%). ", perclass))
    impcol <- if (!is.null(imp)) intersect(c("MeanDecreaseAccuracy","MeanDecreaseGini"), colnames(imp))[1] else NA
  }
  if (!is.na(impcol)) {
    iv <- imp[, impcol]; names(iv) <- rownames(imp)
    body <- c(body, paste0("The most important predictors (by ", impcol, ") were ", .join_and(.topn(iv)), "."))
  }
  new_interpretation("Random forest", body,
    c("The OOB estimate is an internal validation; still confirm on a held-out test set.",
      "Variable importance can be biased toward correlated or high-cardinality predictors (consider conditional/permutation importance).",
      "Tune mtry and ntree; check that the error has stabilised with the number of trees."))
}

#' @rdname interpret
#' @export
interpret.svm <- function(object, ...) {
  a <- azul_args(...)
  types <- c("C-classification","nu-classification","one-classification",
             "eps-regression","nu-regression")
  kernels <- c("linear","polynomial","radial basis","sigmoid")
  ty <- types[object$type + 1]; ke <- kernels[object$kernel + 1]
  acc <- if (!is.null(object$tot.accuracy))
    paste0(" Cross-validated accuracy was ", fmt_num(object$tot.accuracy, 1), "%.") else ""
  body <- c(
    paste0("A support vector machine (", ty, ", ", ke, " kernel) was fitted."),
    paste0("The model used cost = ", fmt_num(object$cost, 3),
           if (!is.null(object$gamma)) paste0(" and gamma = ", fmt_num(object$gamma, 4)) else "",
           ", with ", object$tot.nSV, " support vectors.", acc))
  new_interpretation("Support vector machine", body,
    c("Scale/standardise the features before fitting an SVM.",
      "Tune cost and gamma with a grid search and cross-validation.",
      "The SVM is not directly interpretable; report test-set accuracy (and a confusion matrix / AUC) and, if needed, permutation importance."))
}

#' @rdname interpret
#' @export
interpret.nnet <- function(object, ...) {
  a <- azul_args(...)
  cls <- !is.null(object$lev)
  n <- object$n
  body <- c(
    paste0("A single-hidden-layer feed-forward neural network was trained to ",
           if (cls) "classify " else "predict ", a$outcome %||% "the outcome", "."),
    paste0("The architecture had ", n[1], " input, ", n[2], " hidden and ", n[3],
           " output unit", if (n[3] != 1) "s" else "", " (", length(object$wts), " weights).",
           if (!is.null(object$decay) && object$decay > 0) paste0(" Weight decay was ", fmt_num(object$decay, 4), ".") else ""))
  new_interpretation("Artificial neural network (nnet)", body,
    c("Normalise/scale inputs; results depend on the random starting weights (set a seed, try several starts).",
      "Guard against overfitting with weight decay and by tuning the number of hidden units via cross-validation.",
      "Interpretability is limited; report test-set performance and variable importance (e.g. Garson or Olden methods)."))
}

#' @rdname interpret
#' @export
interpret.nn <- function(object, ...) {
  a <- azul_args(...)
  rm <- tryCatch(object$result.matrix, error = function(e) NULL)
  err <- if (!is.null(rm) && "error" %in% rownames(rm)) rm["error", 1] else NA
  steps <- if (!is.null(rm) && "steps" %in% rownames(rm)) rm["steps", 1] else NA
  hidden <- tryCatch(object$model.list$response, error = function(e) NULL)
  body <- c(
    "A feed-forward neural network (neuralnet) was trained.",
    paste0("Training converged", if (!is.na(steps)) paste0(" in ", fmt_num(steps, 0), " steps") else "",
           if (!is.na(err)) paste0(" to a final error of ", fmt_num(err, 4)) else "", "."))
  new_interpretation("Neural network (neuralnet)", body,
    c("Scale inputs and outputs to a common range before training.",
      "The error is the training error; evaluate predictive performance on a held-out test set.",
      "Tune the hidden-layer structure by cross-validation; beware overfitting."))
}

#' @rdname interpret
#' @export
interpret.train <- function(object, ...) {
  a <- azul_args(...)
  res <- object$results
  metric <- object$metric
  best <- object$bestTune
  # locate the results row matching bestTune
  idx <- seq_len(nrow(res))
  for (p in names(best)) idx <- intersect(idx, which(res[[p]] == best[[p]]))
  row <- res[idx[1], , drop = FALSE]
  tune_txt <- paste(vapply(names(best), function(p) paste0(p, " = ", best[[p]]), character(1)), collapse = ", ")
  method <- object$method
  resamp <- tryCatch(object$control$method, error = function(e) "resampling")
  perf <- character(0)
  for (mn in intersect(c("Accuracy","Kappa","ROC","RMSE","Rsquared","MAE"), names(row))) {
    sdn <- paste0(mn, "SD")
    perf <- c(perf, paste0(mn, " = ", fmt_num(row[[mn]], 3),
              if (sdn %in% names(row)) paste0(" (SD ", fmt_num(row[[sdn]], 3), ")") else ""))
  }
  body <- c(
    paste0("A ", method, " model was tuned by ", resamp, " and selected on ", metric, "."),
    paste0("The best tuning parameters were ", tune_txt, "."),
    paste0("Resampled performance was: ", .join_and(perf), "."))
  new_interpretation(paste0("Tuned model (caret: ", method, ")"), body,
    c("These are resampled (cross-validated) estimates; confirm on an independent held-out test set.",
      "Report the full tuning grid and the resampling scheme.",
      "For classification, also report a confusion matrix and, where relevant, the ROC/AUC."))
}

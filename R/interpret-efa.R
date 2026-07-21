# ---------------------------------------------------------------------------
# Exploratory factor analysis and reliability:
#   interpret.fa (psych), interpret.factanal (stats), interpret.KMO (psych),
#   interpret.alpha (psych, Cronbach's alpha). Bartlett's test of sphericity
#   (a bare list) is routed from interpret.list.
# ---------------------------------------------------------------------------

# summary of a loadings matrix at a threshold
.loadings_summary <- function(L, thr = 0.40) {
  L <- unclass(L); L <- as.matrix(L)
  nf <- ncol(L)
  per <- vapply(seq_len(nf), function(j) {
    it <- rownames(L)[abs(L[, j]) >= thr]
    paste0("Factor ", j, " was defined by ", if (length(it)) paste(it, collapse = ", ") else "no item at the threshold")
  }, character(1))
  cross <- rownames(L)[rowSums(abs(L) >= thr) > 1]
  none  <- rownames(L)[rowSums(abs(L) >= thr) == 0]
  list(per = per, cross = cross, none = none, rng = range(abs(L)))
}

.msa_verdict <- function(k) {
  if (is.na(k)) return("undetermined")
  if (k >= 0.90) "marvelous" else if (k >= 0.80) "meritorious" else if (k >= 0.70) "middling"
  else if (k >= 0.60) "mediocre" else if (k >= 0.50) "miserable" else "unacceptable"
}

.alpha_verdict <- function(al) {
  if (is.na(al)) return("undetermined")
  if (al >= 0.90) "excellent (though very high values may signal item redundancy)"
  else if (al >= 0.80) "good" else if (al >= 0.70) "acceptable"
  else if (al >= 0.60) "questionable" else if (al >= 0.50) "poor" else "unacceptable"
}

#' @rdname interpret
#' @export
interpret.fa <- function(object, ...) {
  a <- azul_args(...)
  nf <- object$factors
  cumvar <- tryCatch(object$Vaccounted["Cumulative Var", nf], error = function(e) NA_real_)
  propvar <- tryCatch(object$Vaccounted["Proportion Var", ], error = function(e) NULL)
  ls <- .loadings_summary(object$loadings)
  rmsea <- tryCatch(object$RMSEA[["RMSEA"]], error = function(e) NA_real_)
  tli <- object$TLI %||% NA_real_
  lowc <- tryCatch(names(object$communality)[object$communality < 0.30], error = function(e) character(0))

  paras <- c(
    paste0("An exploratory factor analysis (", object$fm %||% "minres", " extraction) retained ", nf,
           " factor", if (nf > 1) "s" else "",
           if (!is.na(cumvar)) paste0(", together explaining ", fmt_num(cumvar * 100, 1), "% of the total variance") else "", "."),
    if (!is.null(propvar))
      paste0("The factors accounted for ",
             paste0(fmt_num(propvar * 100, 1), "%", collapse = ", "), " of the variance respectively."),
    paste0("Using a loading cut-off of 0.40: ", paste(ls$per, collapse = "; "), "."),
    if (length(ls$cross)) paste0("Item(s) ", paste(ls$cross, collapse = ", "),
           " cross-loaded on more than one factor and may warrant review or removal.") else NULL,
    if (length(ls$none)) paste0("Item(s) ", paste(ls$none, collapse = ", "),
           " did not load on any factor above 0.40.") else NULL,
    if (length(lowc)) paste0("Item(s) ", paste(lowc, collapse = ", "),
           " had communalities below 0.30, indicating weak shared variance.") else NULL,
    paste0("Model fit: TLI = ", fmt_num(tli, 3), ", RMSEA = ", fmt_num(rmsea, 3),
           " (RMSEA < 0.08 and TLI >= 0.90 are commonly used as acceptable thresholds)."))

  new_interpretation("Exploratory factor analysis (psych)", stats::na.omit(unlist(paras)),
    c("Confirm factorability first: KMO >= 0.60 and a significant Bartlett's test of sphericity.",
      "Adequate sample size (rules of thumb: N >= 200, or 5 to 10 respondents per item).",
      "Justify the number of factors (eigenvalue > 1, scree plot, and parallel analysis).",
      "State the rotation (orthogonal e.g. varimax, or oblique e.g. oblimin if factors are correlated)."))
}

#' @rdname interpret
#' @export
interpret.factanal <- function(object, ...) {
  a <- azul_args(...)
  nf <- object$factors
  L <- object$loadings
  SS <- colSums(unclass(L)^2); nv <- nrow(unclass(L))
  cumvar <- sum(SS) / nv
  ls <- .loadings_summary(L)
  p <- object$PVAL; stat <- object$STATISTIC; dof <- object$dof
  suff <- if (is.na(p)) "could not be tested" else if (p >= a$alpha)
    "adequate (the hypothesis that this many factors is sufficient was not rejected)"
    else "insufficient (more factors may be needed)"
  paras <- c(
    paste0("A maximum-likelihood exploratory factor analysis retained ", nf, " factor",
           if (nf > 1) "s" else "", ", explaining ", fmt_num(cumvar * 100, 1), "% of the total variance."),
    paste0("Using a loading cut-off of 0.40: ", paste(ls$per, collapse = "; "), "."),
    if (length(ls$cross)) paste0("Item(s) ", paste(ls$cross, collapse = ", "), " cross-loaded on more than one factor.") else NULL,
    if (!is.na(stat)) paste0("The goodness-of-fit test indicated the ", nf, "-factor solution was ", suff,
           " (chi-squared = ", fmt_num(stat), ", df = ", fmt_num(dof, 0), ", ", fmt_p(p, a$markup), ")."))
  new_interpretation("Exploratory factor analysis (maximum likelihood)", stats::na.omit(unlist(paras)),
    c("The chi-square sufficiency test is sensitive to sample size; use it alongside RMSEA/scree/parallel analysis.",
      "Confirm factorability (KMO, Bartlett) and adequate sample size.",
      "State the rotation used."))
}

#' @rdname interpret
#' @export
interpret.KMO <- function(object, ...) {
  a <- azul_args(...)
  msa <- object$MSA
  low <- tryCatch(names(object$MSAi)[object$MSAi < 0.50], error = function(e) character(0))
  para <- paste0("The Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy was ", fmt_num(msa, 3),
    ", which is ", .msa_verdict(msa), " (values >= 0.60 are generally required for factor analysis). ",
    if (length(low)) paste0("Item(s) ", paste(low, collapse = ", "),
        " had individual MSA below 0.50 and could be considered for removal.")
    else "All individual item MSA values were at or above 0.50.")
  new_interpretation("KMO sampling adequacy", para,
    c("KMO assesses whether the correlations are compact enough for factor analysis.",
      "Pair with a significant Bartlett's test of sphericity."))
}

#' @rdname interpret
#' @export
interpret.alpha <- function(object, ...) {
  a <- azul_args(...)
  raw <- tryCatch(object$total$raw_alpha, error = function(e) NA_real_)
  std <- tryCatch(object$total$std.alpha, error = function(e) NA_real_)
  nitems <- tryCatch(object$nvar, error = function(e) NA_integer_)
  drop_hint <- ""
  ad <- tryCatch(object$alpha.drop$raw_alpha, error = function(e) NULL)
  if (!is.null(ad) && any(ad > raw + 0.01, na.rm = TRUE)) {
    it <- rownames(object$alpha.drop)[which.max(ad)]
    drop_hint <- paste0(" Reliability would increase if item '", it,
                        "' were dropped (alpha rising to ", fmt_num(max(ad, na.rm = TRUE), 3), ").")
  }
  para <- paste0("Cronbach's alpha for the ", if (!is.na(nitems)) paste0(nitems, "-item ") else "",
    "scale was ", fmt_num(raw, 3), " (standardised alpha ", fmt_num(std, 3), "), indicating ",
    .alpha_verdict(raw), " internal consistency.", drop_hint)
  new_interpretation("Reliability (Cronbach's alpha)", para,
    c("Alpha assumes a unidimensional scale and tau-equivalence; check dimensionality first (e.g. with factor analysis).",
      "Alpha increases with the number of items, so interpret high values alongside item redundancy.",
      "Reverse-score negatively worded items before computing alpha (psych: check.keys)."))
}

#' @rdname interpret
#' @export
interpret.scree <- function(object, ...) {
  a <- azul_args(...)
  pcv <- object$pcv; fv <- object$fv
  ncomp_k <- sum(pcv > 1, na.rm = TRUE)             # Kaiser (PCA)
  nfact_k <- sum(fv > 1, na.rm = TRUE)              # eigenvalue > 1 on factors
  # scree elbow: component after the largest drop in successive eigenvalues
  elbow <- if (length(pcv) > 2) which.max(-diff(pcv)) else length(pcv)
  para <- paste0(
    "The scree plot and eigenvalues were used to decide how many factors/components to retain. ",
    "By the Kaiser criterion (eigenvalue > 1), ", ncomp_k, " component",
    if (ncomp_k != 1) "s" else "", " were suggested for a principal components solution",
    if (nfact_k != ncomp_k) paste0(", and ", nfact_k, " factor", if (nfact_k != 1) "s" else "",
      " on the (reduced) factor eigenvalues") else "", ". ",
    "The scree plot showed its largest drop after component ", elbow,
    ", pointing to about ", elbow, " retained dimension", if (elbow != 1) "s" else "", ".")
  new_interpretation("Scree plot / eigenvalue retention", para,
    c("The Kaiser (eigenvalue > 1) rule tends to over-extract; treat it as an upper bound.",
      "Prefer parallel analysis (fa.parallel) or the scree elbow, cross-checked with interpretability.",
      "Report the criterion actually used to fix the number of factors."))
}

#' @rdname interpret
#' @export
interpret.parallel <- function(object, ...) {
  a <- azul_args(...)
  nf <- object$nfact; nc <- object$ncomp
  para <- paste0(
    "Horn's parallel analysis compared the observed eigenvalues with those from random data. ",
    "It suggested retaining ", nf, " factor", if (nf != 1) "s" else "",
    " and ", nc, " component", if (nc != 1) "s" else "",
    " (dimensions whose observed eigenvalues exceeded the random/resampled eigenvalues).")
  new_interpretation("Parallel analysis (number of factors)", para,
    c("Parallel analysis is generally more accurate than the Kaiser rule or a subjective scree elbow.",
      "The factor and component counts can differ; choose based on your intended model (EFA vs PCA).",
      "Confirm that the retained solution is interpretable and theoretically sensible."))
}

# ---------------------------------------------------------------------------
# Interaction handling. When a model contains an interaction term that is
# statistically significant, the marginal main effects are misleading and must
# be interpreted STRATIFIED (simple effects / simple slopes) by the moderator.
# This module detects interactions, tests their joint significance, and builds
# stratified interpretation paragraphs via emmeans / emtrends.
# ---------------------------------------------------------------------------

# effect measure/scale for a model
.effect_scale <- function(object) {
  if (inherits(object, "coxph"))
    return(list(ratio = TRUE, measure = "HR", noun = "hazard"))
  if (inherits(object, "glm")) {
    fam <- tryCatch(stats::family(object)$family, error = function(e) "gaussian")
    link <- tryCatch(stats::family(object)$link, error = function(e) "identity")
    if (fam == "binomial") return(list(ratio = TRUE, measure = "OR", noun = "odds"))
    if (fam == "poisson" || fam == "quasipoisson" || link == "log")
      return(list(ratio = TRUE, measure = "IRR", noun = "rate"))
  }
  list(ratio = FALSE, measure = "mean difference", noun = NULL)
}

# term labels that are interactions (contain ":")
.interaction_terms <- function(object) {
  tl <- tryCatch(attr(stats::terms(object), "term.labels"), error = function(e) character(0))
  tl[grepl(":", tl)]
}

# joint p-values for each interaction term (car::Anova type III; fallbacks)
.interaction_pvals <- function(object, iterms) {
  if (!length(iterms)) return(setNames(numeric(0), character(0)))
  av <- NULL
  if (requireNamespace("car", quietly = TRUE))
    av <- tryCatch(car::Anova(object, type = 3), error = function(e)
          tryCatch(car::Anova(object), error = function(e2) NULL))
  if (is.null(av))
    av <- tryCatch(stats::drop1(object, scope = ~., test = "Chisq"), error = function(e) NULL)
  if (is.null(av)) return(setNames(rep(NA_real_, length(iterms)), iterms))
  ad <- as.data.frame(av)
  pcol <- grep("Pr\\(|p.value|P\\.value", colnames(ad), value = TRUE)[1]
  if (is.na(pcol)) return(setNames(rep(NA_real_, length(iterms)), iterms))
  out <- setNames(rep(NA_real_, length(iterms)), iterms)
  for (t in iterms) {
    # match term ignoring order of the colon-separated parts
    parts <- sort(strsplit(t, ":")[[1]])
    hit <- which(vapply(rownames(ad), function(rn)
      setequal(strsplit(rn, ":")[[1]], parts), logical(1)))
    if (length(hit)) out[t] <- ad[[pcol]][hit[1]]
  }
  out
}

# variable type in the model frame
.is_factor_var <- function(mf, v) is.factor(mf[[v]]) || is.character(mf[[v]])

# extract estimate/CI/p from an emmeans summary data frame, robustly
.emm_row <- function(sm, i) {
  nm <- names(sm)
  est_c <- intersect(c("estimate","odds.ratio","ratio","risk.ratio","hazard.ratio",
                       nm[grepl("\\.trend$", nm)]), nm)[1]
  lo_c  <- intersect(c("lower.CL","asymp.LCL","LCL"), nm)[1]
  hi_c  <- intersect(c("upper.CL","asymp.UCL","UCL"), nm)[1]
  p_c   <- intersect(c("p.value"), nm)[1]
  list(est = if (!is.na(est_c)) sm[[est_c]][i] else NA_real_,
       lo  = if (!is.na(lo_c)) sm[[lo_c]][i] else NA_real_,
       hi  = if (!is.na(hi_c)) sm[[hi_c]][i] else NA_real_,
       p   = if (!is.na(p_c)) sm[[p_c]][i] else NA_real_)
}

# stratified sentences for a single two-way interaction term "A:B"
.stratified_paras <- function(object, term, a) {
  if (!requireNamespace("emmeans", quietly = TRUE))
    return("A significant interaction was detected but the 'emmeans' package is not installed; install it to obtain the stratified simple effects.")
  parts <- strsplit(term, ":")[[1]]
  if (length(parts) != 2)
    return(paste0("A significant higher-order interaction (", term,
                  ") was detected; interpret the highest-order simple effects with emmeans and report them stratified."))
  mf <- tryCatch(stats::model.frame(object), error = function(e) NULL)
  if (is.null(mf)) return("A significant interaction was detected; refit with emmeans to obtain stratified effects.")
  f1 <- .is_factor_var(mf, parts[1]); f2 <- .is_factor_var(mf, parts[2])
  sc <- .effect_scale(object)
  rtype <- if (sc$ratio) "response" else "link"

  out <- tryCatch({
    if (f1 && f2) {
      # factor x factor: moderator = factor with fewer levels
      nl1 <- nlevels(factor(mf[[parts[1]]])); nl2 <- nlevels(factor(mf[[parts[2]]]))
      mod <- if (nl1 <= nl2) parts[1] else parts[2]
      foc <- setdiff(parts, mod)
      emm <- emmeans::emmeans(object, specs = foc, by = mod)
      ctr <- summary(emmeans::contrast(emm, "pairwise", type = rtype), infer = c(TRUE, TRUE))
      cd <- as.data.frame(ctr)
      vapply(seq_len(nrow(cd)), function(i) {
        r <- .emm_row(cd, i)
        eff <- if (sc$ratio) paste0(sc$measure, " ", fmt_num(r$est), " (95% CI: ", fmt_num(r$lo), ", ", fmt_num(r$hi), ")")
               else paste0("mean difference ", fmt_ci(r$est, r$lo, r$hi))
        paste0("Among ", mod, " = ", cd[[mod]][i], ", the contrast ", cd$contrast[i],
               " gave ", eff, " (", fmt_p(r$p, a$markup), ").")
      }, character(1))
    } else if (xor(f1, f2)) {
      # numeric x factor: simple slope of the numeric focal within factor levels
      foc <- if (f1) parts[2] else parts[1]      # numeric
      mod <- if (f1) parts[1] else parts[2]      # factor
      et <- emmeans::emtrends(object, specs = mod, var = foc)
      sm <- as.data.frame(summary(et, infer = c(TRUE, TRUE)))
      vapply(seq_len(nrow(sm)), function(i) {
        r <- .emm_row(sm, i)
        if (sc$ratio) {
          rr <- exp(r$est); lo <- exp(r$lo); hi <- exp(r$hi)
          paste0("Among ", mod, " = ", sm[[mod]][i], ", each one-unit increase in ", foc,
                 " was associated with ", sc$measure, " ", fmt_num(rr), " (95% CI: ",
                 fmt_num(lo), ", ", fmt_num(hi), "; ", fmt_p(r$p, a$markup), ").")
        } else {
          paste0("Among ", mod, " = ", sm[[mod]][i], ", the slope of ", foc,
                 " was ", fmt_ci(r$est, r$lo, r$hi), " (", fmt_p(r$p, a$markup), ").")
        }
      }, character(1))
    } else {
      # numeric x numeric: slopes of one variable at moderator mean and mean +/- 1 SD
      foc <- parts[1]; mod <- parts[2]
      mv <- mean(mf[[mod]], na.rm = TRUE); sv <- stats::sd(mf[[mod]], na.rm = TRUE)
      atl <- setNames(list(c(mv - sv, mv, mv + sv)), mod)
      et <- emmeans::emtrends(object, specs = mod, var = foc, at = atl)
      sm <- as.data.frame(summary(et, infer = c(TRUE, TRUE)))
      lab <- c("1 SD below the mean", "at the mean", "1 SD above the mean")
      vapply(seq_len(nrow(sm)), function(i) {
        r <- .emm_row(sm, i)
        val <- if (sc$ratio) exp(r$est) else r$est
        lo  <- if (sc$ratio) exp(r$lo) else r$lo
        hi  <- if (sc$ratio) exp(r$hi) else r$hi
        meas <- if (sc$ratio) sc$measure else "slope"
        paste0("When ", mod, " was ", lab[i], " (", fmt_num(sm[[mod]][i]), "), the ", foc,
               " effect was ", meas, " ", fmt_num(val), " (95% CI: ", fmt_num(lo), ", ",
               fmt_num(hi), "; ", fmt_p(r$p, a$markup), ").")
      }, character(1))
    }
  }, error = function(e)
    paste0("A significant interaction (", term, ") was detected; azul could not compute the ",
           "simple effects automatically (", conditionMessage(e),
           "). Obtain them with emmeans::emmeans(model, ~ focal | moderator) or ",
           "emmeans::emtrends(model, ~ moderator, var = focal) and report stratified."))
  out
}

# Master: returns NULL if no interaction, else a list describing significance
# and the stratified paragraphs.
.interaction_analysis <- function(object, a) {
  iterms <- .interaction_terms(object)
  if (!length(iterms)) return(NULL)
  pvals <- .interaction_pvals(object, iterms)
  sig <- iterms[!is.na(pvals) & pvals < a$alpha]
  list(iterms = iterms, pvals = pvals, sig = sig, any_sig = length(sig) > 0)
}

# a one-line note describing the interaction test result(s)
.interaction_note <- function(ia, a) {
  vapply(ia$iterms, function(t) {
    p <- ia$pvals[[t]]
    paste0("The ", gsub(":", " x ", t), " interaction was ",
           if (!is.na(p) && p < a$alpha) "statistically significant" else "not statistically significant",
           " (", fmt_p(p, a$markup), ").")
  }, character(1))
}

# Compose a model interpretation, inserting stratified simple effects when an
# interaction is significant, or a marginal note when it is present but not.
.compose_with_interaction <- function(object, a, lead, marginal_sentences, tail = NULL) {
  ia <- .interaction_analysis(object, a)
  if (is.null(ia))
    return(list(body = c(lead, marginal_sentences, tail), stratified = FALSE, extra_assum = NULL))
  notes <- .interaction_note(ia, a)
  if (ia$any_sig) {
    caveat <- paste0("Because a statistically significant interaction was present, the effect of ",
      "each involved variable depends on the level of the other; the marginal main effects are ",
      "therefore not interpreted on their own, and the stratified (simple) effects are reported below.")
    strat <- unlist(lapply(ia$sig, function(t)
      c(paste0("Stratified effects for the ", gsub(":", " x ", t), " interaction:"),
        .stratified_paras(object, t, a))), use.names = FALSE)
    return(list(body = c(lead, caveat, notes, strat, tail), stratified = TRUE,
                extra_assum = "Interaction present and significant: report simple effects stratified by the moderator, not the marginal main effects."))
  }
  list(body = c(lead, marginal_sentences, notes,
                "As the interaction was not statistically significant, the main effects are interpreted marginally.",
                tail),
       stratified = FALSE,
       extra_assum = "An interaction term was tested and was not statistically significant.")
}

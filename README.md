# azul

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/azusa-ui/azul-package/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/azusa-ui/azul-package/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Thesis-style interpretation of statistical models, tests and tables — in Malaysian Journal of Medical Sciences (MJMS) / DrPH prose.**

`azul` turns a fitted model, a hypothesis test, a result table, or even a raw
dataset into a ready-to-paste interpretation written in third-person,
past-tense academic prose. Every point estimate is reported with its 95%
confidence interval, the test is named, reference categories are stated, and
the key assumptions to check are listed. Formatting follows the MJMS reporting
style (Arifin et al. 2016) and SAMPL guidelines.

There is one entry point — `interpret()` — which dispatches on the class of the
object you give it. Tables and datasets are also handled by `interpret_table()`,
`interpret_descriptive()` and `interpret_diagnostic()`.

## Installation

```r
# from a local source tarball
install.packages("azul_0.1.0.tar.gz", repos = NULL, type = "source")

# or from GitHub (once pushed)
# remotes::install_github("azusa-ui/azul-package")
library(azul)
```

`azul` is pure R (no compilation, no Rtools needed). It depends only on
`stats` + `methods`; every model-specific package is a *Suggests* and is used
only if you have it.

## Quick start

```r
library(azul)

# a fitted model
m <- glm(am ~ wt + hp, data = within(mtcars, am <- factor(am)), family = binomial)
interpret(m, outcome = "manual transmission")

# a hypothesis test
interpret(t.test(mpg ~ am, data = mtcars))

# a raw dataset -> descriptive Table 1 narrative
interpret(mtcars)

# a results table you typed up
tab <- data.frame(term = c("Male","Age"), OR = c(1.49,1.03),
                  LL = c(1.24,1.01), UL = c(1.78,1.05), p = c(4e-4,0.02))
interpret_table(tab, type = "OR", outcome = "coronary artery disease")
```

`print()` shows a spaced, sub-headed layout for reading on screen;
`as.character()` (or `cat(format(x))`) returns the single flowing paragraph for
pasting into a manuscript.

## What it interprets

**Descriptive**
- Raw data frame / tibble -> `interpret(data)` or `interpret_descriptive(data, by = "group")`

**Hypothesis tests (`htest`)**
- One-sample / paired / independent / Welch t-tests
- Pearson / Spearman / Kendall correlation
- Chi-square, Fisher's exact, McNemar
- Mann-Whitney, Wilcoxon signed-rank, Kruskal-Wallis, Friedman, Kolmogorov-Smirnov, sign test
- Hosmer-Lemeshow goodness-of-fit

**Comparison of means**
- One-way ANOVA, ANCOVA / factorial ANOVA (`aov`)
- MANOVA / MANCOVA (`manova`)
- Post-hoc: `TukeyHSD`, `emmeans` contrasts

**Regression**
- Linear: simple & multiple (`lm`) — separates continuous vs factor-level effects
- Logistic (`glm` binomial) -> OR
- Poisson / quasi-Poisson / negative binomial -> IRR
- Ordinal proportional odds (`MASS::polr`, `ordinal::clm`, VGAM `cumulative`)
- Multinomial (`nnet::multinom`, VGAM `multinomial`) -> expanded, labelled RRR per category
- Conditional logistic / matched (`survival::clogit`)
- GEE (`geepack::geeglm`), zero-inflated (`pscl::zeroinfl`)
- Linear & generalised mixed models (`lme4`, `nlme`)

**Survival**
- Kaplan-Meier (`survfit`), log-rank (`survdiff`)
- Cox proportional hazards (`coxph`)
- Parametric AFT (`survreg`, and `summary.survreg`) with TR and equivalent HR

**Scale / factor analysis**
- KMO, Bartlett's sphericity, scree, parallel analysis
- EFA (`psych::fa`, `factanal`), CFA / SEM (`lavaan`)
- Reliability: Cronbach's alpha (`psych::alpha`)

**Diagnostic accuracy / model performance**
- 2x2 table -> `interpret_diagnostic()` (sens/spec/PPV/NPV/LR with Wilson CIs)
- `caret::confusionMatrix`, `pROC` ROC/AUC

**Machine learning**
- CART (`rpart`), random forest (`randomForest`), SVM (`e1071`),
  neural nets (`nnet`, `neuralnet`), tuned models (`caret::train`)

**Assumption / helper outputs**
- `ordinal::nominal_test` / `scale_test`, `epiDisplay::poisgof`, gtsummary tables

## Interaction stratification

If a model contains a **statistically significant interaction**, `azul` does
not report the marginal main effects on their own. It flags the interaction and
reports the **stratified simple effects** (via `emmeans` / `emtrends`): pairwise
contrasts of the focal factor within each moderator level, simple slopes within
factor levels, or slopes at the moderator mean ± 1 SD — on the model's scale
(mean difference / OR / IRR / HR). If the interaction is present but not
significant, it interprets marginally and says so.

## Styling options

`interpret(x, alpha = 0.05, markup = "markdown", ci_sep = "to",
           outcome = "...", unit = "mmHg")`

## Reference

Arifin WN, et al. Reporting statistical results in medical journals.
*Malays J Med Sci.* 2016;23(5):1-7.

# azul

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/azusa-ui/azul-package/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/azusa-ui/azul-package/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Thesis-style interpretation of statistical models, tests and tables — in Malaysian Journal of Medical Sciences (MJMS) / DrPH prose.**

`azul` is a full **fit → interpret → check → tabulate → visualise → report**
workflow for statistical analysis, all in third-person, past-tense academic
prose. Every point estimate is reported with its 95% confidence interval, the
test is named, reference categories are stated, and assumptions are checked.
Formatting follows the MJMS reporting style (Arifin et al. 2016) and SAMPL
guidelines.

The core entry point is `interpret()`, which dispatches on the class of the
object you give it (~50 model and test types). Around it sit companion
functions to check assumptions, build publication-ready tables and reports, and
draw-and-interpret figures. Run `azul_help()` for a one-line index of every
function, or `?azul` for the package overview.

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

## The full workflow

From one fitted model you can produce a complete results section:

```r
m <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = survival::lung)

interpret(m, outcome = "death")     # 1. manuscript prose (with AT A GLANCE summary)
check_assumptions(m)                # 2. run the diagnostics (PH, etc.)
azul_table(m)                       # 3. publication-ready 3-line APA table
azul_plot(m)                        # 4. the right figure (here a HR forest plot) + its reading

# 5. one call -> a Word (or HTML) document with prose + table + figure + checks
azul_report(m, file = "results.docx", outcome = "death")
```

## Workflow functions

| Function | Purpose |
|---|---|
| `interpret()` | model / test / table / dataset → MJMS prose |
| `check_assumptions()` | run diagnostics (normality, homoscedasticity, VIF, PH, overdispersion, Hosmer-Lemeshow, Levene) |
| `azul_table()` | publication-ready estimates table (three-line APA, bold predictors, adjustment note; `digits =` control) |
| `azul_report()` | one-call Word/HTML report: prose + table + figure + assumption checks |
| `azul_plot()` | draw **and interpret** the right figure (16 types, see below) |
| `azul_survtable()` | parametric survival table (PH/PO and AFT side by side; exp/Weibull/log-logistic/log-normal) |
| `azul_survcompare()` | rank parametric survival distributions by AIC |
| `azul_arima_suggest()` | data-driven differencing + a starting ARIMA/SARIMA order |
| `azul_help()` | one-line index of every function |

### `azul_plot()` figure types

`forest`, `km`, `roc`, `residuals`, `forest_meta`, `ts` (series+ACF+PACF),
`tsdiag` (ARIMA residuals), `acf`, `caterpillar` (mixed-model random effects),
`schoenfeld` (Cox PH), `funnel` (meta-analysis), `calibration` (logistic),
`effect` (interaction / simple slopes), `qqrand`, `scree`. Each is auto-selected
by object class and returns a plain-language interpretation.

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

**Epidemiology & specialised**
- Additive interaction: RERI, AP, synergy index (`interpret_additive_interaction()`)
- Interrupted time series: level & slope change (`interpret_its()`)
- Spatial: Moran's I / Geary's C, LISA, Getis-Ord Gi*, SMR, spatial lag/error regression
- Time series: ARIMA/SARIMA, ADF / KPSS / Ljung-Box
- Meta-analysis: `metafor::rma`, `meta` (pooled effect, I², Q)

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

## Citation

If you use azul in your work, please cite it:

```r
citation("azul")
```

> AZul (2026). *azul: Thesis-Style Interpretation of Statistical Models and
> Tables.* https://github.com/azusa-ui/azul-package

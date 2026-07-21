# azul 0.1.0

First release. `interpret()` produces MJMS / DrPH-style interpretation prose for:

- Descriptive statistics of a data frame (`interpret_descriptive`, `by =` stratification).
- Hypothesis tests: t-tests, correlation, chi-square/Fisher/McNemar, Mann-Whitney,
  Wilcoxon, Kruskal-Wallis, Friedman, Kolmogorov-Smirnov, sign test, Hosmer-Lemeshow.
- ANOVA / ANCOVA / MANOVA and post-hoc (TukeyHSD, emmeans contrasts).
- Regression: linear, logistic, Poisson / quasi-Poisson / negative binomial,
  ordinal, multinomial (expanded labelled RRR), conditional logistic, GEE,
  zero-inflated, linear/generalised mixed models.
- Survival: Kaplan-Meier, log-rank, Cox, parametric AFT (with equivalent HR).
- Factor analysis / reliability: KMO, Bartlett, scree, parallel analysis, EFA,
  CFA/SEM (lavaan), Cronbach's alpha.
- Diagnostic accuracy and model performance: 2x2 tables, confusion matrices,
  ROC/AUC.
- Machine learning: CART, random forest, SVM, neural nets, caret-tuned models.
- Result tables (`interpret_table`) and gtsummary objects.

Automatic interaction handling: when an interaction is statistically significant,
stratified simple effects are reported instead of marginal main effects.

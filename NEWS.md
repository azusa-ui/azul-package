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

## azul 0.1.0 (workflow additions)

Beyond interpretation, azul now provides a full analysis workflow:

- `check_assumptions()` runs model diagnostics and reports each assumption as met/violated.
- `azul_table()` and `azul_report()` build publication-ready three-line APA tables and one-call Word/HTML reports (prose + table + figure + assumption checks).
- `azul_survtable()` (parametric survival PH/AFT, incl. log-normal) and `azul_survcompare()` (AIC distribution ranking).
- `azul_plot()` draws and interprets 16 figure types (forest, KM, ROC, residuals, meta forest, ACF/PACF, ARIMA diagnostics, caterpillar, Schoenfeld, funnel, calibration, effect, qqrand, scree).
- `azul_arima_suggest()` for data-driven ARIMA/SARIMA order selection.
- `azul_help()` prints a table of contents; `?azul` gives the package overview.
- Additive interaction (RERI/AP/S), interrupted time series, spatial (Moran/Geary/LISA/Getis/SMR), time series (ARIMA + ADF/KPSS/Ljung-Box) and meta-analysis interpreters.

Number formatting follows the MJMS/Arifin convention (percentages 1 dp, P-values 3 dp, estimates/CIs/statistics 2 dp).

# azul cookbook

`interpret()` converts a fitted model, a hypothesis test, a result table, or a
raw dataset into a manuscript-ready interpretation in MJMS / DrPH style.

```r
library(azul)

## Descriptive Table 1 narrative
interpret(mtcars)

## A hypothesis test
interpret(t.test(mpg ~ am, data = mtcars))

## Linear regression (continuous vs factor levels worded differently)
d <- transform(mtcars, gear = factor(gear))
interpret(lm(mpg ~ wt + gear, d), outcome = "fuel economy", unit = "mpg")

## Logistic regression -> odds ratios
d <- transform(mtcars, am = factor(am))
interpret(glm(am ~ wt + hp, d, family = binomial), outcome = "manual transmission")

## Survival (Cox proportional hazards)
library(survival)
interpret(coxph(Surv(time, status) ~ age + sex, lung), outcome = "death")

## Interaction stratification (significant interaction -> simple effects)
set.seed(1); n <- 300
d <- data.frame(sex = factor(sample(c("F","M"), n, TRUE)),
                trt = factor(sample(c("placebo","drug"), n, TRUE)))
d$y <- 10 + 3*(d$trt=="drug") + 8*(d$trt=="drug")*(d$sex=="M") + rnorm(n, 0, 4)
interpret(lm(y ~ sex * trt, d), outcome = "symptom score")

## A results table
tab <- data.frame(term = c("Male","Age"), OR = c(1.49, 1.03),
                  LL = c(1.24, 1.01), UL = c(1.78, 1.05), p = c(4e-4, 0.02))
interpret_table(tab, type = "OR", outcome = "coronary artery disease")

## Diagnostic accuracy
interpret_diagnostic(matrix(c(85, 15, 20, 180), 2, byrow = TRUE), test = "the rapid test")

## Manuscript paragraph vs console layout
x <- interpret(lm(mpg ~ wt, mtcars), outcome = "fuel economy")
cat(as.character(x))   # single flowing paragraph for pasting
```

# Shared fixtures for the advanced-categorical tests
make_cat_data <- function(n = 300, seed = 42) {
  set.seed(seed)
  data.frame(
    y_bin = rbinom(n, 1, 0.4),
    cnt   = rpois(n, 2),
    grp3  = factor(sample(c("A", "B", "C"), n, TRUE)),
    sev   = ordered(sample(c("mild", "mod", "sev"), n, TRUE),
                    levels = c("mild", "mod", "sev")),
    x     = rnorm(n, 10, 2),
    ptime = runif(n, 0.5, 2),
    clust = factor(sample(1:40, n, TRUE))
  )
}
txt <- function(x) as.character(x)

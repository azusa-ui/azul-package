\name{interpret_descriptive}
\alias{interpret_descriptive}
\title{Interpret descriptive statistics of a dataset}
\usage{
interpret_descriptive(data, by = NULL,
  normal = c("auto", "normal", "skewed"), max_levels = 12, ...)
}
\arguments{
\item{data}{A data frame or tibble.}
\item{by}{Optional name of a grouping variable to stratify by.}
\item{normal}{How to summarise numeric variables: \code{"auto"} (skewness
decides mean vs median), \code{"normal"}, or \code{"skewed"}.}
\item{max_levels}{Categorical variables with more levels than this are
summarised by their number of distinct values only.}
\item{...}{Styling args (\code{markup}, \code{ci_sep}).}
}
\value{An \code{azul_interpretation} object.}
\description{
Produces a Table 1 style narrative: mean (SD) for approximately normal numeric
variables, median (IQR) for skewed ones, n (\%) for categorical variables, with
missing-data counts. No P values are reported for baseline description (MJMS).
}
\examples{
interpret_descriptive(mtcars)
}

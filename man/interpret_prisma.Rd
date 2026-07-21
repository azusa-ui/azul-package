\name{interpret_prisma}
\alias{interpret_prisma}
\title{Interpret a PRISMA study-selection flow}
\usage{
interpret_prisma(x, databases = NULL, reasons = NULL,
  review = c("systematic", "scoping"), register = NULL, ...)
}
\arguments{
\item{x}{A named list with any of: \code{identified}, \code{registers},
\code{other_sources}, \code{duplicates}, \code{screened},
\code{excluded_screen}, \code{sought}, \code{not_retrieved}, \code{assessed},
\code{excluded_fulltext}, \code{included}.}
\item{databases}{Optional character vector of database names.}
\item{reasons}{Optional named numeric vector of full-text exclusion reasons.}
\item{review}{"systematic" (default) or "scoping".}
\item{register}{Optional protocol registration (e.g. "PROSPERO CRD...").}
\item{...}{Styling args.}
}
\value{An \code{azul_interpretation} object.}
\description{
Produces the PRISMA flow narrative from the study-selection counts, with an
internal consistency check that the numbers reconcile across stages.
Reference: Page et al. (2021) PRISMA 2020; Tricco et al. (2018) PRISMA-ScR.
}
\examples{
interpret_prisma(list(identified = 2450, duplicates = 610, screened = 1840,
  excluded_screen = 1660, assessed = 180, excluded_fulltext = 158, included = 22))
}

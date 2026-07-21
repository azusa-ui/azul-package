\name{interpret_bibliometrix}
\alias{interpret_bibliometrix}
\title{Interpret a bibliometric analysis}
\usage{
interpret_bibliometrix(x, field = "the field", ...)
}
\arguments{
\item{x}{A \code{bibliometrix} \code{biblioAnalysis} object, or a named list of
"Main Information" metrics (any subset): \code{documents}, \code{sources},
\code{timespan}, \code{annual_growth} (percent), \code{avg_citations_per_doc},
\code{references}, \code{author_keywords}, \code{keywords_plus}, \code{authors},
\code{authors_single}, \code{single_authored_docs}, \code{coauthors_per_doc},
\code{international_coauthorship} (percent), \code{collaboration_index}.}
\item{field}{Optional research field/topic label for the prose.}
\item{...}{Styling args (markup, alpha).}
}
\value{An \code{azul_interpretation} object.}
\description{
Turns the key figures of a bibliometric analysis (corpus size, growth, impact,
authorship, collaboration and keywords) into a manuscript-style paragraph, with
reminders on performance analysis vs science mapping and the bibliometric laws.
Reference: Aria & Cuccurullo (2017); Donthu et al. (2021).
}
\examples{
interpret_bibliometrix(list(documents = 1240, sources = 210,
  timespan = "2010:2023", annual_growth = 12.4, avg_citations_per_doc = 18.6),
  field = "digital health")
}

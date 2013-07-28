\name{renderDOI}
\alias{renderDOI}
\title{Render a formatted citation for a DOI}
\usage{
  renderDOI(x, csl = "apa", verbose = TRUE, ...)
}
\arguments{
  \item{x}{character; vector of one or more DOIs}

  \item{csl}{characer; the name of a valid CSL style}

  \item{verbose}{logical; report on progress?}

  \item{...}{additional arguments. Not currently used}
}
\value{
  A list whose components are the rendered citations
  corresponding to the supplied DOIs.
}
\description{
  Renders a fully formatted citation from a DOI via a
  lookup to \url{http://data.crossref.org}. CSL styles are
  used to render the reference.
}
\examples{
writeLines(renderDOI("10.1007/s10933-013-9697-7",
                     verbose = FALSE)[[1]])
}
\author{
  Gavin L. Simpson
}


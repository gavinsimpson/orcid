\name{fromDOI}
\alias{fromDOI}
\title{Lookup Crossref metadata for DOIs}
\usage{
  fromDOI(x,
    format = c("bibtex", "ris", "rdf", "turtle", "json", "crossref", "datacite"),
    verbose = TRUE, ...)
}
\arguments{
  \item{x}{character; vector of one or more DOIs}

  \item{format}{character; one of the stated choices.
  Controls the format of the metadata returned}

  \item{verbose}{logical; report on progress?}

  \item{...}{additional arguments. Not currently used}
}
\value{
  A list whose components contain the requested metadata
  for the supplied DOIs.
}
\description{
  Queries \url{http://data.crossref.org} for metadata
  associated with a DOI. Metadata can be returned in a
  number of formats.
}
\examples{
writeLines(fromDOI("10.4319/lo.2009.54.6_part_2.2529",
                   verbose = FALSE)[[1]])
}
\author{
  Gavin L. Simpson
}


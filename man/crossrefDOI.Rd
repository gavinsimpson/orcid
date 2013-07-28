\name{crossrefDOI}
\alias{crossrefDOI}
\title{Look up a Crossref metadata for a DOI}
\usage{
  crossrefDOI(doi, raw = FALSE, verbose = TRUE, ...)
}
\arguments{
  \item{doi}{character; a vector of one or more DOIs to
  lookup.}

  \item{raw}{logical; return the raw, unparsed metadata?}

  \item{verbose}{logical; should progress be reported?}

  \item{...}{Additional arguments, currently not used.}
}
\value{
  A list of class \code{"crossrefList"}, each component of
  which is a list of class \code{"crossrefEntry"}
  containing the parsed metadata for a single DOI. If
  \code{raw = TRUE}, an additional component, \code{"raw"},
  a data frame containing the raw results of the metadata
  search.
}
\description{
  Retrieve metadata for a DOI via a lookup to Crossref
  metadata search. The resulting COinS metadata is then
  parsed into an into a convenient format.
}
\details{
  Currently, only articles are parsed
}
\examples{
crossrefDOI("10.1007/s10933-013-9697-7")
}
\author{
  Gavin L. Simpson
}


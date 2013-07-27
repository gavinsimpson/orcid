\name{orcidWorks}
\alias{orcidWorks}
\title{Retrieve Orcid Works metadata}
\usage{
  orcidWorks(orcid)
}
\arguments{
  \item{orcid}{character; an Orcid.}
}
\value{
  a list with components

  \describe{ \item{identifiers}{data frame; Orcid Work
  Identifier types and their values, typically DOIs.}
  \item{works}{list; raw metadata from the parsed JSON.} }
}
\description{
  Returns the metadata for author works associated with an
  Orcid. The quality of the metadata depends on that
  provided when authors claim works in Orcid.
}
\examples{
orcidWorks(0000-0002-9084-8413)$identifiers
}
\author{
  Gavin L. Simpson
}


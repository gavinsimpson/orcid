##' @title Lookup Crossref metadata for DOIs
##'
##' @description Queries \url{http://data.crossref.org} for metadata
##' associated with a DOI. Metadata can be returned in a number of
##' formats.
##'
##' @param x character; vector of one or more DOIs
##' @param format character; one of the stated choices. Controls the
##' format of the metadata returned
##' @param verbose logical; report on progress?
##' @param ... additional arguments. Not currently used
##'
##' @return A list whose components contain the requested metadata for
##' the supplied DOIs.
##'
##' @author Gavin L. Simpson
##'
##' @examples
##' writeLines(fromDOI("10.4319/lo.2009.54.6_part_2.2529",
##'                    verbose = FALSE)[[1]])
##'
##' @export
##'
##' @importFrom RCurl getURL
##'
`fromDOI` <- function(x,
                      format = c("bibtex","ris","rdf","turtle","json",
                      "crossref","datacite"),
                      verbose = TRUE, ...) {
    format <- match.arg(format)
    if (isTRUE(all.equal(format, "datacite"))) {
        base <- "http://data.datacite.org/"
    } else {
        base <- "http://data.crossref.org/"
    }

    ## process format to Accept
    accept <- switch(format,
                     bibtex   = "application/x-bibtex",
                     ris      = "application/x-research-info-systems",
                     rdf      = "application/rdf+xml",
                     turtle   = "text/turtle",
                     json     = "application/vnd.citationstyles.csl+json",
                     crossref = "application/vnd.crossref.unixref+xml",
                     datacite = "application/vnd.datacite.datacite+xml"
                     )

    refs <- vector(mode = "list", length = length(x))

    if (verbose) {
        pb <- txtProgressBar(min = 0, max = length(x), style = 3, file = "")
        on.exit(close(pb))
    }
    for (i in seq_along(x)) {
        URL <- paste0(base, x[i])
        refs[[i]] <- try(getURL(URL,
                                HTTPHeader = c(Accept = accept)))
        if (verbose) {
            setTxtProgressBar(pb, i)
        }
    }
    refs
}

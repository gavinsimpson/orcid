##' @title Retrieve Orcid Works metadata
##'
##' @description Returns the metadata for author works associated with an
##' Orcid. The quality of the metadata depends on that provided when authors
##' claim works in Orcid.
##'
##' @param orcid character; an Orcid.
##'
##' @return a list with components
##'
##' \describe{
##'   \item{orcid}{character; the orcid.}
##'   \item{identifiers}{data frame; Orcid Work Identifier types and their
##'      identifiers, typically DOIs.}
##'   \item{works}{list; raw metadata from the parsed JSON.}
##'   \item{retrieved}{time and date of metadata retrieval.}
##' }
##'
##' @author Gavin L. Simpson
##'
##' @examples
##' ids <- orcidWorks("0000-0002-9084-8413")
##' ids
##'
##' @importFrom RCurl getURL
##' @importFrom RJSONIO fromJSON
##'
##' @export
##'
`orcidWorks` <- function(orcid) {
    URI <- "http://pub.orcid.org/"
    req <- paste0(URI, orcid, "/orcid-works")
    res <- getURL(req, HTTPHeader = c(Accept = "application/orcid+json"))
    tstamp <- Sys.time()
    js <- fromJSON(res)
    works <- js[[2]][["orcid-activities"]][["orcid-works"]][["orcid-work"]]
    idents <- lapply(works, function(x) x[["work-external-identifiers"]])
    miss <- sapply(idents, is.null)
    idents[miss] <- list(NA, NA)
    idents <- lapply(idents, unlist)
    idents <- do.call(rbind.data.frame, idents)
    names(idents) <- c("Type", "Identifier")
    idents$Identifier <- sub("http://dx.doi.org/", "", idents$Identifier)
    out <- list(orcid = orcid, identifiers = idents, works = works,
                retrieved = tstamp)
    class(out) <- "orcidWorks"
    out
}

##' @S3method print orcidWorks
`print.orcidWorks` <- function(x, ...) {
    cat("\n")
    writeLines(strwrap(paste("Orcid Works identifiers for", x$orcid),
                       prefix = "\t"))
    cat("\n")
    print(x$identifiers, ...)
}

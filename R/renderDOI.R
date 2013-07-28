##' @title Render a formatted citation for a DOI
##'
##' @description Renders a fully formatted citation from a DOI via a
##' lookup to \url{http://data.crossref.org}. CSL styles are used to
##' render the reference.
##'
##' @param x character; vector of one or more DOIs
##' @param csl characer; the name of a valid CSL style
##' @param verbose logical; report on progress?
##' @param ... additional arguments. Not currently used
##'
##' @return A list whose components are the rendered citations
##' corresponding to the supplied DOIs.
##'
##' @author Gavin L. Simpson
##'
##' @examples
##' writeLines(renderDOI("10.1007/s10933-013-9697-7",
##'                      verbose = FALSE)[[1]])
##'
##' @export
##'
##' @importFrom RCurl getURL
##'
`renderDOI` <- function(x, csl = "apa", verbose = TRUE, ...) {
    base <- "http://data.crossref.org/"

    refs <- vector(mode = "list", length = length(x))

    ## process the header
    accept = paste0("text/x-bibliography; style=", csl)

    if (verbose) {
        pb <- txtProgressBar(min = 0, max = length(x), style = 3)
        on.exit(close(pb))
    }
    for (i in seq_along(x)) {
        URL <- paste0(base, x[i])
        refs[[i]] <- try(getURL(URL,
                                HTTPHeader = c(Accept = accept)),
                         silent = TRUE)
        if (verbose) {
            setTxtProgressBar(pb, i)
        }
    }

    refs
}

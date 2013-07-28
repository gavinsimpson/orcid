##' @title Look up a Crossref metadata for a DOI
##'
##' @description Retrieve metadata for a DOI via a lookup to Crossref
##' metadata search. The resulting COinS metadata is then parsed into
##' an into a convenient format.
##'
##' @details Currently, only articles are parsed
##'
##' @param doi character; a vector of one or more DOIs to lookup.
##' @param raw logical; return the raw, unparsed metadata?
##' @param verbose logical; should progress be reported?
##' @param ... Additional arguments, currently not used.
##'
##' @return A list of class \code{"crossrefList"}, each component of
##' which is a list of class \code{"crossrefEntry"} containing the
##' parsed metadata for a single DOI. If \code{raw = TRUE}, an
##' additional component, \code{"raw"}, a data frame containing the
##' raw results of the metadata search.
##'
##' @author Gavin L. Simpson
##'
##' @importFrom RCurl getURL
##'
##' @export
##'
##' @examples
##' crossrefDOI("10.1007/s10933-013-9697-7")
##'
`crossrefDOI` <- function(doi, raw = FALSE, verbose = TRUE, ...) {
    base <- "http://search.crossref.org/dois?q="

    js <- vector(mode = "list", length = length(doi))
    if (verbose) {
        pb <- txtProgressBar(min = 0, max = length(doi), style = 3)
        on.exit(close(pb))
    }
    for (i in seq_along(doi)) {
        if (is.na(doi[i])) {
            js[[i]] <- NA
        } else {
            URL <- paste0(base, doi[i])
            res <- try(getURL(URL), silent = TRUE)
            js[[i]] <- unlist(fromJSON(res))
        }
        if (verbose) {
            setTxtProgressBar(pb, i)
        }
    }

    ## stick them all together.
    js <- do.call(rbind.data.frame, js)
    names(js) <- c("doi", "score", "normalizedScore", "title",
                   "fullCitation", "coins", "year")

    ## split up the coins for the meta data
    scoin <- strsplit(as.character(js$coins), "&amp;")

    ## parse each COinS element
    out <- lapply(scoin, parseCoinsArticle)
    class(out) <- "crossrefList"
    if (raw) {
        out$raw <- js
    } else {
        out$raw <- NULL
    }
    out
}

parseCoinsArticle <- function(text, ...) {
        stopifnot(any(grepl("genre=article", text)))
        out <- list()
        ## article title
        f <- "^rft.atitle="
        out$title <- gsub("+", " ",
                          URLdecode(sub(f, "", text[grep(f, text)])),
                          fixed = TRUE)
        f <- "^rft.date="
        out$date <- as.numeric(sub(f, "", text[grep(f, text)]))
        f <- "^rft.jtitle="
        out$journal <- gsub("+", " ", sub(f, "", text[grep(f, text)]),
                            fixed = TRUE)
        f <- "^rft.volume="
        out$volume <- as.numeric(sub(f, "", text[grep(f, text)]))
        f <- "^rft.issue="
        out$issue <- as.numeric(sub(f, "", text[grep(f, text)]))
        f <- "^rft.spage="
        out$spage <- as.numeric(sub(f, "", text[grep(f, text)]))
        f <- "^rft.epage="
        out$epage <- as.numeric(sub(f, "", text[grep(f, text)]))
        ## authors - can be multiple
        want <- grep("^rft.au=", text)
        out$authors <- lapply(text[want], parseCoinsAuthor)
        ## DOI
        re <- "^[[:alnum:]:]*/{1}([[:print:]]*)"
        want <- grep("^rft_id=", text)
        out$doi <- if(grep("doi", text[want])) {
            doi <- sub("^rft_id=", "", URLdecode(text[want]))
            doi <- sub(re, "\\1", doi)
            sub("http://dx.doi.org/", "", doi, fixed = TRUE)
        } else {
            NA
        }
        class(out) <- c("crossrefArticle", "crossrefEntry")
        out
    }


`parseCoinsAuthor` <- function(text) {
    text <- sub("^rft.au=", "", text)
    text <- gsub("+", " ", text, fixed = TRUE)[[1]]
    sub("^ ", "", text)
    text
}

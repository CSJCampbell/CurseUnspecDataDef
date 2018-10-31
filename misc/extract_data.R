
library(polite)
library(rvest)
library(dplyr)
library(stringr)

#' @title Get Data from Paranormal Database
#' @description Pulls page then searches for body text in fixed format.
#' @param session polite session object
#' @param path page within page tree
#' @param params page query info (default NULL)
#' @return data.frame
#' @author ccampbell@@mango-solutions.com
#' @examples
#' session <- bow(url, force = TRUE)
#' extract_data(session, path = "cheshire/chesdata.php")

extract_data <- function(session, path, params = NULL) {
    resi <- get_data(session = session, path = path, params = params)
    if (!is(resi, "try-error")) {
        nodes <- xml_find_all(resi, xpath = ".//p")
        res <- bind_rows(lapply(
            X = nodes, 
            FUN = function(x) {
                # get bodytext
                x <- xml_text(x)
                # some paragraphs are blank
                blank <- tbl_df(
                    matrix(NA, 
                        nrow = 1, ncol = 4, 
                        dimnames = list(NULL, c("Location", "Type", "Date", "Comments"))))
                # for non-blank records
                if (grepl(pattern = "^Location:", x = x)) {
                    # seems to be quite clean structure
                    x <- str_split(
                        pattern = "(^Location: )|(Type: )|(Date / Time: )|(Further Comments: )", 
                        string = x)[[1L]][-1L]
                    tbl_df(
                        matrix(x, 
                            nrow = 1, 
                            dimnames = list(NULL, c("Location", "Type", "Date", "Comments"))))
                } else {
                    # if this is start of multipage blob, get rest of pages
                    if (grepl(pattern = "^Records 1 - [0-9]{1,2} of [0-9]+$", x = x)) {
                        # pagination
                        pn <- 25L
                        nrecs <- as.integer(str_extract(string = x, pattern = "[0-9]+$"))
                        npages <- nrecs %/% pn + if_else(nrecs %% pn > 0, true = 1L, false = 0L)
                        outp <- vector(mode = "list", length = npages - 1L)
                        for (pg in seq_len(npages - 1L)) {
                            # TODO probably need to bind list here
                            outp[[pg]] <- extract_data(
                                session = session, 
                                path = path, 
                                params = paste0(
                                    "pageNum_paradata=", pg, 
                                    "&totalRows_paradata=", nrecs))
                        }
                        if (length(outp) > 1L) {
                            bind_rows(outp)
                        } else {
                            blank
                        }
                    } else {
                        # else blank
                        blank
                    }
                }
            }))
        na.omit(res)
    } else {
        NULL
    }
}

#' @title Get Data from Website
#' @description Request permission, then scrape from town/county page
#' @inheritParams extract_data
#' @return XML definition or die trying

get_data <- function(session, path, params = NULL) {
    res <- try(nod(session, path) %>% 
            scrape(params))
    if (is(res, "try-error")) {
        res <- try(nod(session, path) %>% 
                scrape(params = params, 
                    content = "text/html; charset=latin1"))
    }
    res
}


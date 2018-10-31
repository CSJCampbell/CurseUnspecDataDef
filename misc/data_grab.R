
library(polite)
library(rvest)
library(dplyr)
library(stringr)

source("extract_data.R")

url <- "https://www.paranormaldatabase.com"
# register client
session <- bow(url, force = TRUE)
session

# index info
result <- scrape(session)
# class(result)
pages <- xml_attr(xml_find_all(result, xpath = ".//a"), attr = "href")
pages <- pages[!grepl(pattern = "index|^(/){0,1}regions|^search|^(/){0,1}forms|^(/){0,1}calendar|^reports|^https|^zenphoto|^(/){0,1}furtherread|^faq|^links|^legal|^aviation", x = pages)]
head(pages)
pages <- gsub(pattern = "^/", replacement = "", x = pages)
pages <- pages[!grepl("(wales|ireland)/.*htm(l){0,1}", x = pages)]

pagesp <- pages[!grepl("htm(l){0,1}$", pages)]
pages <- pages[grepl("htm(l){0,1}$", pages)]

# custom handling
pages <- gsub(pattern = "pages/somerset.htm$", replacement = "somedata.php", x = pages)
pages <- gsub(pattern = "essex.htm$", replacement = "essedata.php", x = pages)
pages <- gsub(pattern = "northants.htm$", replacement = "nantdata.php", x = pages)
pages <- gsub(pattern = "devon.htm$", replacement = "devodata.php", x = pages)
pages <- gsub(pattern = "westmidlands.htm$", replacement = "westdata.php", x = pages)
pages <- gsub(pattern = "cheshire.htm$", replacement = "chesdata.php", x = pages)

pages <- gsub(pattern = "hertfordshire.htm$", replacement = "hertdata.php", x = pages)
pages <- gsub(pattern = "bedfordshire.htm$", replacement = "bedforddata.php", x = pages)
pages <- gsub(pattern = "northumber.htm$", replacement = "nhumdata.php", x = pages)
pages <- gsub(pattern = "kent.htm$", replacement = "kentdata.php", x = pages)
pages <- gsub(pattern = "isleofwight.htm$", replacement = "wighdata.php", x = pages)

# rest of name transformations
pages <- gsub(pattern = "((r(i){0,1}d(ge){0,1}){0,1}(ick|f|p|y|ester|oln){0,1}s(hire){0,1}|wall|(o){0,1}n|e(t|x|y)|olk|ford|ria|am).htm$", replacement = "data.php", x = pages)

out <- vector(mode = "list", length = length(pages))

for (i in seq_along(pages)) {
    # html pages are background; do not query
    if (grepl("htm(l){0,1}$", pages[i])) {
       message(paste("  skipping", pages[i]))
       next
    } else {
        message(paste("reading", pages[i])) 
        # create data.frame
        res <- try(extract_data(session = session, path = pages[i]))
        if (!is(res, "try-error") && nrow(res) > 0L) {
            message(paste("content found on", pages[i]))
            out[[i]] <- cbind(path = pages[i], res)
        } else {
            message(paste("  could not parse content on", pages[i]))
        }
    }
}

#sum(sapply(out, is.null))
# remove NULL
out <- plyr::compact(out)
all_out <- bind_rows(out)
dim(all_out)

save(all_out_en, file = "all_out_en.RData")

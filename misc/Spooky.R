
library(R6)
library(dplyr)
library(lubridate)

#' @title An R6 Class for Paranormaldatabase.com Data
#' @description Spooky data has the following variables:\itemize{
#'   \item Location: Description of Site
#'   \item Type: Sighting Category
#'   \item Comments: Description of Sighting
#'   \item Date: Date of Sighting
#'   \item lon: Site position East or West of the Greenwich Meridian
#'   \item lat: Site position North or South of the Equator
#' }
#' When \code{initialize} is called on a data.frame, business rules
#' are applied to the dataset.
#' @examples
#' ob1 <- Spooky$new()
#' ob1
#' ob1$colnames()
#' 
#' ob2 <- Spooky$new(example07)
#' ob2$data() %>% head

Spooky <- R6Class("Spooky", 
    public = list(
        Location = NA_character_,
        Type = factor(NA_character_), 
        Date = as.Date(NA_character_),
        Comments = NA_character_,
        lat = NA_real_,
        lon = NA_real_,
        initialize = function(data) {
            # permit empty object
            if (!missing(data)) {
                # verify data.frame
                stopifnot(is.data.frame(data))
                is_miss <- self$colnames() %in% data
                if (any(is_miss)) {
                    stop(paste("data is missing columns:", 
                        paste(self$colnames()[is_miss], collapse = ", ")))
                }
                # drop out of range Latitude/Longitude
                nr <- nrow(data)
                data <- filter(data, 
                    !is.na(lat), !is.na(lon), lat >= 44, lon >= -10)
                if (nr > nrow(data)) {
                    warning(paste("dropped", nr - nrow(data), 
                        "records with missing or out of range coordinates"))
                }
                # permit character Type
                # coerce to factor with pre-defined levels
                if (is.character(data$Type)) {
                    # report new levels
                    dlevs <- unique(data$Type)
                    droplevs <- !dlevs %in% self$levels()
                    if (any(droplevs)) {
                        warning(paste("dropping", sum(droplevs, na.rm = TRUE), 
                            "levels from Type:", 
                            paste(dlevs[droplevs], collapse = ", ")))
                    }
                    # new levels dropped
                    data$Type <- factor(data$Type, levels = self$levels())
                }
                # parse ad hoc date formats
                if (is.character(data$Date)) {
                    data$Date <- self$get_date(data$Date)
                }
                # check data types
                types <- vapply(
                    X = data[self$colnames()],
                    FUN = typeof,
                    FUN.VALUE = character(1L))
                is_wrong_type <- types != self$typeof()
                if (any(is_wrong_type)) {
                    stop(paste("data is wrong type:", 
                        paste(self$colnames()[is_wrong_type], collapse = ", ")))
                }
                # remove latin1 encodings
                self$Location = iconv(
                    x = data$Location, 
                    from = "latin1", 
                    to = "ASCII", 
                    sub = "byte")
                # enforce known levels if already factor
                self$Type = factor(
                    x = data$Type, 
                    levels = self$levels())
                self$Date = as.Date(data$Date)
                # remove latin1 encodings
                self$Comments = iconv(
                    x = data$Comments, 
                    from = "latin1", 
                    to = "ASCII", 
                    sub = "byte")
                self$lat = data$lat
                self$lon = data$lon
            }
        },
        data = function() {
            data.frame(
                "Location" = self$Location, 
                "Type" = self$Type, 
                "Date" = self$Date, 
                "Comments" = self$Comments, 
                "lat" = self$lat, 
                "lon" = self$lon, 
                stringsAsFactors = FALSE)
        },
        # self definitions for easier testing
        # explicit self documentation
        # simpler to update spec
        colnames = function() {
            c("Location", "Type", "Date", "Comments", "lat", "lon")
        },
        typeof = function() {
            c("character", "integer",
            "double", "character",
            "double", "double")
        },
        levels = function() {
            c(
                "ABC", "Crisis Manifestation", "Cryptozoology", 
                "Curse", "Dragon", "Environmental Manifestation", 
                "Fairy", "Haunting Manifestation", 
                "Legend", "Legend - Old Nick", 
                "Manifestation of the Living", 
                "Other", "Poltergeist", 
                "Post-Mortem Manifestation", "SHC", "Shuck", 
                "UFO", "Unknown Ghost Type")
        },
        # attempt to parse date variable
        get_date = function(d) { 
            out <- as.Date(d, format = "%B %Y")
            out[is.na(out)] <- dmy(d[is.na(out)])
            out[is.na(out)] <- dmy(paste("01", d[is.na(out)]))
            out[is.na(out)] <- dmy(paste("01 01", d[is.na(out)]))
            # custom text (semi-regularised)
            out[grepl("Nineteenth century", x = d)] <- dmy("01 01 1850")
            out[grepl("(Pre twentieth|Late nineteenth) century", x = d)] <- dmy("01 01 1875")
            out[grepl("Early twentieth century", x = d)] <- dmy("01 01 1925")
            out[grepl("(Mid t|T)wentieth century", x = d)] <- dmy("01 01 1950")
            out[grepl("Late twentieth century", x = d)] <- dmy("01 01 1975")
            out[grepl("Twenty-first century", x = d)] <- dmy("01 01 2010")
            out[grepl("19 May \\(reoccurring\\)", x = d)] <- dmy("19 05 2016")
            out[grepl("31 October \\(reoccurring\\)", x = d)] <- dmy("31 10 2016")
            out[grepl("24 December \\(reoccurring\\)", x = d)] <- dmy("24 12 2016")
            out[grepl("25 December \\(reoccurring\\)", x = d)] <- dmy("25 12 2016")
            out[grepl("31 December \\(reoccurring\\)", x = d)] <- dmy("31 12 2016")
            # current - could be happening now (not going to check weather...)
            out[grepl("([Ss]till present|Midnight \\(reoccurring\\|Weather Dependent: Stormy nights))", x = d)] <- Sys.Date()
            out
        }
        
    )
)


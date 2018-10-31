setwd("C:/Users/ccampbell/Documents/Training/A Presentation - NW Universities Data")

library(dplyr)
library(lubridate)
# install latest GitHub version
library(ggmap)
library(leaflet)

# register for a free google API key
gkey <- NULL
# script contains API key
source("personal_key.R")
stopifnot(!is.null(gkey))
register_google(key = gkey)

load(file = "all_out.RData")
#load(file = "all_out_en.RData")
#head(all_out)
#dim(all_out)
# [1] 2303  5
#all_out <- rbind(all_out, all_out_en)
#save(all_out, file = "all_out.RData")
dim(all_out)
# [1] 9324    5

table(all_out$Type)
#                        ABC        Crisis Manifestation 
#                        134                         141 
#              Cryptozoology                       Curse 
#                        345                          69 
#                     Dragon Environmental Manifestation 
#                         77                          54 
# Experimental Manifestation                       Fairy 
#                          2                         212 
#     Haunting Manifestation     Haunting Manifestation? 
#                       6170                           1 
#                     legend                      Legend 
#                          2                         564 
#          Legend - Old Nick Manifestation of the Living 
#                        145                          21 
#                      Other                 Poltergeist 
#                        259                         330 
#  Post-Mortem Manifestation                         SHC 
#                         82                          11 
#                      Shuck                         UFO 
#                        250                         130 
#         Unknown Ghost Type                     Vampire 
#                        296                          16 
#                   Werewolf 
#                         13 
#sample(x = all_out$Date, size = 10)

# parse dates where provided
get_date1 <- function(d) { 
    out <- as.Date(d, format = "%B %Y")
    out[is.na(out)] <- dmy(d[is.na(out)])
    out[is.na(out)] <- dmy(paste("01", d[is.na(out)]))
    out[is.na(out)] <- dmy(paste("01 01", d[is.na(out)]))
    out[grepl("Nineteenth century", x = out)] <- dmy("01 01 1850")
    out[grepl("(Pre twentieth|Late nineteenth) century", x = out)] <- dmy("01 01 1875")
    out[grepl("Early twentieth century.*", x = out)] <- dmy("01 01 1925")
    out[grepl("(Mid t|T)wentieth century.*", x = out)] <- dmy("01 01 1950")
    
    out[grepl("Late twentieth century", x = out)] <- dmy("01 01 1975")
    out[grepl("Twenty-first century", x = out)] <- dmy("01 01 2010")
    out[grepl("19 May \\(reoccurring\\)", x = out)] <- dmy("19 05 2016")
    out[grepl("31 October \\(reoccurring\\)", x = out)] <- dmy("31 10 2016")
    out[grepl("24 December \\(reoccurring\\)", x = out)] <- dmy("24 12 2016")
    out[grepl("25 December \\(reoccurring\\)", x = out)] <- dmy("25 12 2016")
    out[grepl("31 December \\(reoccurring\\)", x = out)] <- dmy("31 12 2016")
    out[grepl("([Ss]till present|Midnight \\(reoccurring\\|Weather Dependent: Stormy nights))", x = out)] <- dmy("01 01 2015")
    out
}

all_out <- mutate(all_out, 
    Date_ = Date,
    Date = get_date1(Date))

# clean0 <- filter(
#     mutate(all_out, Date = get_date1(Date)), 
#     !is.na(Date))
# dim(clean0)
# # [1] 3421    5
# clean0 <- mutate(clean0, 
#     Country = case_when(
#         grepl(pattern = "ireland", x = path) ~ "Ireland", 
#         grepl(pattern = "wales", x = path) ~ "Wales",
#         grepl(pattern = "^highlands|^lowlands", x = path) ~ "Scotland",
#         TRUE ~ "UK"))

load("clean2.RData")

m <- leaflet(clean2_en) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(popup = ~Location_)
m 

###########################################################
# 01 initial example

set.seed(754420)
example01 <- filter(clean2, 
    lat > 39, lon > -7) %>% 
    slice(sample(seq_len(nrow(.)), size = 100))

# initially permitted levels
levs <- c(
    "ABC", "Crisis Manifestation", "Cryptozoology", 
    "Curse", "Dragon", "Environmental Manifestation", 
    "Fairy", "Haunting Manifestation", 
    "Legend", "Legend - Old Nick", 
    "Manifestation of the Living", 
    "Other", "Poltergeist", 
    "Post-Mortem Manifestation", "SHC", "Shuck", 
    "UFO", "Unknown Ghost Type")

example01 <- mutate(example01, 
    Type = factor(Type, levels = levs)) %>% 
    select(-Country)

m <- leaflet(example01) %>%
    # Add default OpenStreetMap map tiles
    addTiles() %>%  
    addMarkers(popup = ~Location)
m 

# working case
save(example01, file = "paranormal01/example01.RData")

###########################################################
# 02 types

set.seed(108593)
example02 <- filter(clean2, 
    lat > 39, lon > -7) %>% 
    slice(sample(x = seq_len(nrow(.)), size = 100))

example02[5, "lat"] <- "51,43446"
m <- leaflet(example02) %>%
    addTiles() %>%  
    # raises informative error message
    addMarkers(popup = ~Location)
m

example02 <- mutate(example02, 
    Type = factor(Type, levels = levs)) %>% 
    select(-Country)

save(example02, file = "paranormal02/example02.RData")

###########################################################
# 03 missing values

set.seed(5664103)
# missing
c2m <- filter(clean2, is.na(lat))
example03 <- rbind(filter(clean2, 
    !is.na(lat) |
    (lat > 39 & lon > -7)) %>% 
    slice(sample(x = seq_len(nrow(.)), size = 100 - nrow(c2m))), 
    c2m)
dim(example03)
example03 <- arrange(example03, sample(seq_len(100)))
head(example03)
m <- leaflet(example03) %>%
    addTiles() %>%  
    addMarkers(popup = ~Location)
m

example03 <- mutate(example03, 
    Type = factor(Type, levels = levs)) %>% 
    select(-Country)

# missing values removed
save(example03, file = "paranormal03/example03.RData")


###########################################################
# 04 types

scale_unity <- function(x) {
    (x - min(x, na.rm = TRUE)) /
        diff(range(x, na.rm = TRUE)) }
set.seed(214401)
example04 <- filter(clean2, 
    !is.na(lon),
    !grepl(pattern = "<", 
        x = iconv(Location, 
            from = "latin1", to = "ASCII", sub = "byte"))) %>% 
    slice(sample(x = seq_len(nrow(.)), size = 100, prob = 1 - scale_unity(lon)))

m <- leaflet(example04) %>%
    addTiles() %>%  
    # raises informative error message
    addMarkers(popup = ~Location)
m

example04 <- mutate(example04, 
    Type = factor(Type, levels = levs)) %>% 
    select(-Country)

# mis-located points change plot scale
save(example04, file = "paranormal04/example04.RData")

###########################################################
# 05 encoding

clean2_en <- mutate(clean2, Location_ = gsub("<.+>", "", iconv(Location, from = "latin1", to = "ASCII", sub = "byte")))
dim(clean2_en)
# [1] 2219    9
#save(clean2_en, file = "clean2_en.RData")
#load("clean2_en.RData")

with(clean2_en, which(Location != Location_))
# [1]  38 531 769
enc_rows <- c(38, 531, 769)

clean2_en[enc_rows, c("lat", "lon")] <- 
    matrix(c(
        55.964, -4.6569, 
        53.284, -6.1567, 
        49.245, -2.2297), 
        nrow = 3, ncol = 2,
        byrow = TRUE)

clean3_en <- filter(clean2_en, 
    lat > 39, lon > -7)

with(clean3_en, which(Location != Location_))
enc_rows <- c(37, 459, 648)

set.seed(236490)
rows <- sample(x = seq_len(nrow(clean3_en))[-enc_rows], 
    size = 97)
rows <- sample(x = c(rows, enc_rows))

example05 <- slice(clean3_en, rows)
example05 <- select(example05, -Location_)

m <- leaflet(example05) %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(popup = ~Location_)
m 


example05 <- mutate(example05, 
    Type = factor(Type, levels = levs)) %>% 
    select(-Country)

# raises error when plotting Location due to encoding
save(example05, file = "paranormal05/example05.RData")

###########################################################
# 06 Dates & Timestamps

set.seed(593471)
example06 <- filter(clean2, 
    lat > 39, lon > -10) %>% 
    slice(sample(x = seq_len(nrow(.)), size = 100))
example06 <- left_join(
    x = select(example06, -Date), 
    y = select(all_out, Comments, Date), 
    by = "Comments")
m <- leaflet(example06) %>%
    addTiles() %>%  
    # raises informative error message
    addMarkers(popup = ~Location)
m
example06$Type[example06$Type == "Haunting Manifestation?"] <- "Haunting Manifestation"
example06 <- mutate(example06, 
    Type = factor(Type, levels = levs)) %>% 
    select(-Country)

# raises error when plotting Location due to encoding
save(example06, file = "paranormal06/example06.RData")

###########################################################
# 07 Data Volume

example07 <- left_join(
    x = all_out, 
    y = transmute(clean2, 
        Location, 
        lat, lon))
head(example07)

need_geo_rows <- which(is.na(example07$lat))
need_geo_rows <- sample(need_geo_rows, size = 2400)

georows7a <- ggmap::geocode(location = example07[need_geo_rows, "Location"])
head(example07[need_geo_rows, ])
example07[need_geo_rows, c("lon", "lat")] <- georows7a[, c("lon", "lat")]

clean2a <- rbind(
    select(clean2, -Country), 
    transmute(
        left_join(
            x = georows7a, 
            y = all_out, 
            by = "Location"),
        path, Location, Type, Date = get_date1(Date), Comments, lon, lat))

example07 %>% 
    mutate( 
        Location = gsub(
            pattern = "<.+>", 
            replacement = "", 
            x = iconv(
                Location, 
                from = "latin1", 
                to = "ASCII", 
                sub = "byte"))) %>% 
    leaflet %>%
    addTiles() %>%  
    # raises informative error message
    addMarkers(popup = ~Location)


save(example07, file = "paranormal07/example07.RData")

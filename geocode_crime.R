# Geocode the Cville Crime dataset
# nathancday@gmail.com
# October 22, 2017

# Globals ----
library(geojsonio)
library(ggmap)
register_google(key = "AIzaSyC5c6J680q-SMx6V789FrYpOLXaqz0HY88", account_type = "premium")
library(raster)
library(sf)
library(sp)

library(magrittr)
library(tidyverse)

theme_set(theme_bw())
setwd("~/future/cville_crime/")
# Inputs ----
crime_json <- geojson_read("https://opendata.arcgis.com/datasets/d1877e350fad45d192d233d2b2600156_7.geojson",
                           parse = TRUE) # ~ 1-5 minutes...
saveRDS("crime_20171202.RDS")
crime_json <- readRDS("crime_20171202.RDS")

# extract table of interest
crime_df <- crime_json[["features"]]
crime_df <-  crime_df[["properties"]]

# Geocode ----
# build 'address' var for GoogleMaps query

crime_df %<>% mutate_at(vars(BlockNumber, StreetName), funs(trimws(.))) # always trim whitespace

# adjust block numbers
crime_df$BlockNumber %<>% as.numeric()
table(crime_df$BlockNumber, useNA = "always")
# convert 0s and NAs to 100s
crime_df$BlockNumber %<>% ifelse(is.na(.), 100, .) %>% ifelse(. == 0, 100, .)
table(crime_df$BlockNumber, useNA = "always") # fixed

ggplot(crime_df, aes(BlockNumber)) +
    geom_bar() +
    scale_x_continuous(limits = c(0,3000))

# * estimate the average street in Charlottesville ----
pois_glm <- crime_df %>% count(BlockNumber) %>%
    glm("n ~ BlockNumber", poisson, .)

summary(pois_glm)

preds_glm <- tibble(BlockNumber = seq(100,3000, by = 100))
preds_glm$pn <- predict(pois_glm, preds_glm) %>% exp()

ggplot(crime_df, aes(BlockNumber)) +
    geom_col(data = preds_glm, aes(y = pn)) +
    geom_bar(alpha = .5, fill = "red") +
    scale_x_continuous(limits = c(0,3000))


table(crime_df$StreetName) %>% sort(decreasing = TRUE)

crime_df %<>% mutate(address =  paste(BlockNumber, StreetName, "Charlottesville VA"))

# check for unique addresses
address_df <- tibble(address = unique(crime_df$address))
nrow(address_df) # 3144
saveRDS(address_df, "address_df.RDS")

slice(address_df, 2) %>% unlist() %>% geocode


## * Google Query -----
# limited by 2500 Google Maps API queries per day so 2 batches
address_list <- split(address_df, rep(c(T,F), length.out = nrow(address_df)))
saveRDS(address_list, "address_list.RDS")

res1a <- geocode(address_list[[1]]$address, source = "google", output = "all")
res2a <- geocode(address_list[[2]]$address, source = "google", output = "all")
names(res1a) <- address_list[[1]]$address
names(res2a) <- address_list[[2]]$address

# check for successes
map_lgl(res1a, ~.["status"] == "OK") %>% sum(na.rm = T) # 1550 / 1572
map_lgl(res2a, ~.["status"] == "OK") %>% sum(na.rm = T) # 1551 / 1572

# big lists need an extractor function to pull out relevant data
extractor <- function(le) {
    if (length(le) == 2) {
        if (le$status == "OK") { # so we ignore error: ZERO_RESULTS
            res <- le$results %>% unlist() %>% bind_rows() %>%
                select(lat = geometry.location.lat,
                       lon = geometry.location.lng,
                       formatted_address,
                       geometry_loc_type = geometry.location_type)
        }
    }
    else { res <- tibble(formatted_address = NA) } # leave a place holder for any misses
    return(unique(res))
}


res1a_parsed <- map_df(res1a, extractor, .id = "query")
res2a_parsed <- map_df(res2a, extractor, .id = "query")

res <- bind_rows(res1a_parsed, res2a_parsed) %>% full_join(address_df, ., by = c("address" = "query"))

saveRDS(res, "addresses_geocoded.RDS")

geocodeQueryCheck(userType = "free") # useful

## Merge Back to Crime_df ----

crime <- inner_join(crime_df, res)

saveRDS(crime, "crime_geocoded.RDS")



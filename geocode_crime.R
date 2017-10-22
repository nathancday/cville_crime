# Geocode the Cville Crime dataset
# nathancday@gmail.com
# October 22, 2017

# Globals ----
library(geojsonio)
library(ggmap)
library(tidyverse)
library(magrittr)

theme_set(theme_bw())

# Inputs ----
crime_json <- geojson_read("https://opendata.arcgis.com/datasets/d1877e350fad45d192d233d2b2600156_7.geojson",
                           parse = TRUE) # ~ 1-5 minutes...

# extract table of interest
crime_df <- crime_json[["features"]]
crime_df <-  crime_df[[2]]

# Geocode ----
# build 'address' var for GoogleMaps query
crime_df %<>% mutate(address =  paste(BlockNumber, StreetName, "Charlottesville VA"))

# check for unique addresses
address_df <- tibble(address = unique(crime_df$address))
nrow(address_df)

# read in prior caches lat-lon coords
most_latlon <- readRDS("~/future/CvilleTowing/most_latlon.RDS")

# find those that are missing and geocode only those
missing <- anti_join(address_df, most_latlon)
gcodes <- geocode(missing$address)
missing %<>% bind_cols(gcodes)
# 1 missing, this varies depending on query, 

# so I try to re-run them to double check
miss2 <- filter(missing, is.na(lat))
gcodes2 <- geocode(miss2$address) # it worked
miss2 %<>% select(-(lat:lon)) %>%
    bind_cols(gcodes2)

missing %<>% filter(!is.na(lat)) %>%
    bind_rows(miss2)

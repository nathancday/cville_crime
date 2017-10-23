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
crime_df2 <- crime_json[["features"]]
crime_df <-  crime_df[[2]]

# Geocode ----
# build 'address' var for GoogleMaps query
crime_df %<>% mutate_at(vars(BlockNumber, StreetName), funs(trimws(.)))
crime_df %<>% mutate(address =  paste(BlockNumber, StreetName, "Charlottesville VA"))

# check for unique addresses
address_df <- tibble(address = unique(crime_df$address))
nrow(address_df)

# read in prior caches lat-lon coords
most_latlon <- readRDS("most_latlon.RDS")
table(is.na(most_latlon$lat))
# 29 missing due to bad addresses
# looks like errors in reports (typos, intersections, etc)

# find those addresses that have not yet been geocoded
missing <- filter(address_df, !(address %in% most_latlon$address))
# gcodes <- geocode(missing$address)
missing %<>% bind_cols(gcodes)
# 1 missing, this varies depending on query, 

# so I try to re-run them to double check
miss2 <- filter(missing, is.na(lat)) %>%
    mutate(address = gsub("UNIVERSITY AVE ", "", address))
# gcodes2 <- geocode(miss2$address) # it worked
miss2 %<>% select(-(lat:lon)) %>%
    bind_cols(gcodes2)

# put them all back together again
missing %<>% filter(!is.na(lat)) %>%
    bind_rows(miss2)

# make all_latlon for use with inner_join()
all_latlon <- bind_rows(most_latlon, missing)

#check
anti_join(address_df, all_latlon) %>% nrow()
# fix
address_df %<>% mutate(address = gsub("(0 14TH ST) UNIVERSITY AVE", "\\1", address))

anti_join(crime_df, all_latlon) %>% nrow()
# fix again
# fix
crime_df %<>% mutate(address = gsub("(0 14TH ST) UNIVERSITY AVE", "\\1", address))

coded_df <- inner_join(crime_df, all_latlon)
# not all address are coded
# in fact 101 are not, due to misformatted addresses
# most of these seem like recording er

# see them...
retry <- filter(coded_df, is.na(lat))
# leaving these as is, looks like addresses where incorrected reported in the original data

# Export ----
# make names match convention, ie CapitalCamel
coded_df %<>% rename(Address = address, Lon = lon, Lat = lat)


# rebuild addresses df to match crime dataset from API
select(coded_df, Address, BlockNumber, StreetName, Lon, Lat) %>%
    unique() %>%
    write_csv("Crime_Data_Geocoded_UniqueAddressesOnly.csv")

# write out full set too
write_csv(coded_df, "Crime_Data_Geocoded.csv")


# Tests -----
test <- read_csv("Crime_Data_Geocode_UniqueAddressesOnly.csv", col_types = "ccccc") # works

test2 <- inner_join(crime_df, test)

anti_join(crime_df, test2)

test3 <- read_csv("Crime_Data_Geocoded.csv")





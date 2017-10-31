# Geocoder for street addresses
# Cville Crime Data
# nathancday@gmail.com
# 2017/09/16

rm(list = ls())

#### globals ------------------------------------------------------------------

setwd("~/future/CvilleTowing/")

library(forecast) # time-series modeling
library(ggsci) # sick color pallettes
library(forcats) # fct tools
library(lubridate) # date tools
library(magrittr) # piping
library(tidyverse) # data wranglin'

theme_set(theme_minimal())
ang <- theme(axis.text.x = element_text(angle = 45, vjust = .75, hjust = .75))

#### import -----------------------------------------------------------------

# csv downloaded from http://opendata.charlottesville.org/datasets/crime-data
tib <- read_csv("Crime_Data.csv")
names(tib) %<>% tolower()

str(tib)

tib %>% map(~ table(is.na(.)))

tib %<>% filter(complete.cases(tib))

# clean out "-"s and "/"s as the represent subgroups and aliases
tib$offense %<>% gsub("-.*", "", .) %>%
    gsub("/.*", "", .)

tib %<>% unite(address, blocknumber, streetname)

tib %<>% rename_at(vars(contains("reported")), # save the chars
                        funs(gsub("reported", "", .)))

tib %<>% mutate(hour = gsub("2400", "0000", hour), # miscoded?
                hour = parse_time(hour, format = "%H%M"),
                date = as.Date(date),
                year = year(date),
                month = month(date),
                offense = fct_infreq(offense), # see tow forcast md
                address = gsub("_", " ", address), # "_" messes up geocoding
                address = paste0(address, ", Charlottesville VA")) # make full w/ city, state

# pick up with the top 16 offenses
tib %<>% filter(offense %in% levels(offense)[1:16]) %>%
    droplevels()
# make better (shorter) names
decode <- c("Larceny", "Assault", "Towing", "Traffic",
            "Vandalism", "Property", "Drugs", "Assist Citizen",
            "Suspicious Activity", "Fraud", "Burglary", "Animal",
            "Runaway", "DUI", "Disorderly", "Missing Person") %>%
    set_names(levels(tib$offense))

tib$offense %<>% decode[.] %>% fct_infreq()
## geocode ----------------------------------

# library(ggmap)
# latlon <- tib$address %>% geocode(output = "latlon", source = "dsk",
#                                   client = "123", signature = "abc")

# Warning messages:
# 1: geocode failed with status ZERO_RESULTS, location = "200 BYPASS & MCINTIRE RD, Charlottesville VA" 
# 2: geocode failed with status ZERO_RESULTS, location = "0 MAYWOOD & JEFFERSON PARK AVE, Charlottesville VA"

# saveRDS(latlon, "latlon.RDS")
latlon <- readRDS("latlon.RDS")

tib %>% filter(address %in% c("200 BYPASS & MCINTIRE RD, Charlottesville VA",
                              "0 MAYWOOD & JEFFERSON PARK AVE, Charlottesville VA"))
# Traffic

tib %<>% bind_cols(latlon) %>%
    filter(!is.na(lat))

# filter down to 2016
tib %<>% filter(year == 2016) # 6495 points

ggplot(tib, aes(x =lon, y = lat)) +
    geom_point(size = 3, shape = 1) +
    labs(title = "CPD taking work trips to the eastern hemisphere?")


eastern_hemi <- filter(tib, lon > 0)
# eastern_hemi[1,c("lon", "lat")] %>% unlist() %>%
#     get_map(zoom = 6) %>%
#     ggmap() +
#     geom_text(data = eastern_hemi[1,],
#                aes(lon, lat, label = gsub(",.*", "", address)))
# 
# eastern_hemi[2,c("lon", "lat")] %>% unlist() %>%
#     get_map(zoom = 6) %>%
#     ggmap() +
#     geom_text(data = eastern_hemi[2,],
#               aes(lon, lat, label = gsub(",.*", "", address)))

# drop easter_hemi
tib %<>% filter(lon < 0)

# check for redundant geocodes
tib %>% unite(loc, lon, lat) %>% with(table(loc)) %>% sort()

# whoa there 345 reports at the same location I doubt it
sames <- filter(tib, lon == -78.483032)
View(sames) # all same area of E Market St, but different blocks

# see if google can do better than GSK
# glatlon <- sames$address %>% geocode()
# glatlon %>% unite(loc, lon, lat) %>% with(table(loc)) %>% sort() # it does

# save for future scraping
# saveRDS(glatlon, "glatlon.RDS")
glatlon <- readRDS("glatlon.RDS")

# merge in new lat lon coordinates
sames %<>% select(-lat, -lon) %>% bind_cols(glatlon)
tib %<>% filter(lon != -78.483032) %>% bind_rows(sames)

# recheck for hi frequency locations
tib %>% unite(loc, lon, lat) %>% with(table(loc)) %>% sort()
sames2 <- filter(tib, lon == "-78.4774586") # all 600 E Market, the police station!

zoomed_map <- get_map(unique(sames2$address)[1], zoom = 19)
ggmap(zoomed_map) +
    geom_point(x = unique(sames2$lon), y = unique(sames2$lat), shape = 13, size = 5) +
    theme_void() +
    labs(title = "Most frequent address: '600 E Market St'",
         subtitle = "Is that the police station?")

ggplot(sames2, aes(x = fct_infreq(offense))) +
    geom_bar() +
    coord_flip() +
    labs(title = "Offenses reported at '600 E Market St",
        subtitle = "Due to drug testing at the station?",
        x = NULL, y = NULL)

# check second highest
tib %>% unite(loc, lon, lat) %>% with(table(loc)) %>% sort()
sames3 <- filter(tib, lon == "-78.469152")
table(sames3$address)
# ok the geo-encoding is pretty good

# but what's going on at that address
ggplot(sames3, aes(x = fct_infreq(offense))) +
    geom_bar() +
    coord_flip() +
    labs(title = "1400 HAMPTON ST, Charlottesville VA",
         subtitle = "Assited living facility nearby?",
         x = NULL, y = NULL)

# check third highest
tib %>% unite(loc, lon, lat) %>% with(table(loc)) %>% sort()
sames4 <- filter(tib, lon == "-78.49477")
table(sames4$address)
table(sames4$offense) %>% sort()


# add some jitter to handle the same addresses
tib2 <- mutate_at(tib, vars(lat, lon), funs(jitter(., amount = .0005)))

tib2 %>% unite(loc, lon, lat) %>% with(table(loc)) %>% sort()

# make a custom color var to collapse similar offenses
decode <- c("theft", "assault", "towing", "vehicular", "vandalism", "vandalism", "drugs", "assist",
            "sketchiness", "theft", "theft", "assit", "assist", "vehicular", "sketchiness", "assist") %>%
    set_names(levels(tib2$offense))

tib2$color <- decode[tib2$offense] %>% fct_inorder()


d3_10 <- pal_d3()(10) %>% gsub("FF$","",.)
b8 <- scales::brewer_pal(palette = "Dark2")(8)


offense_pal <- colorFactor(palette = b8,
                           domain = unique(tib2$color))

library(leaflet)
library(htmltools)
leaflet(tib2, options = leafletOptions(minZoom = 12, maxZoom = 15)) %>%
    setView("-78.4767", "38.0293", 12) %>%
    addProviderTiles("CartoDB.Positron") %>% 
    addCircleMarkers(color = ~offense_pal(color), weight = 1, radius = 5,
                     opacity = 1, fillOpacity = .15,
                     label = ~paste0(offense, "<br>", date)) %>%
    addLegend("bottomright", pal = offense_pal, values = ~color,
              title = "Reported offense")

saveRDS(tib2, "CrimeCast/tib2.RDS")

## geojson ---------------------------------------------------------------------

# https://rstudio.github.io/leaflet/json.html
library(geojsonio)
cville_json <- geojsonio::geojson_read("https://opendata.arcgis.com/datasets/ceaf5bd3321d4ae8a0c1a2b21991e6f8_9.geojson",
                                       what = "sp")

pal <- colorNumeric("viridis", NULL)

leaflet(cville_json,
        options = leafletOptions(minZoom = 12, maxZoom = 17)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "#a5a597", smoothFactor = 0.3, fillOpacity = .5,
                fillColor = ~pal(POPULATION),
                label = ~paste0(NAME, ": ", formatC(POPULATION, big.mark = ",")))

# determine which beats the gps coordinates are in
# https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile
library(rgeos)
library(rgdal)

dat <- select(tib2, lon, lat, offense)
coordinates(dat) <-  ~ lon + lat
proj4string(dat) <- proj4string(cville_json)

res <- over(dat, cville_json)
# returns the match in cville_son for each row in dat
table(complete.cases(res)) # 210 didn't match, those outside of the city like foxfield

tib3 <- bind_cols(tib2, select(res,NAME)) %>% filter(!is.na(NAME))
saveRDS(tib3, "CrimeCast/tib3.RDS")

tib_sum <- group_by(tib3, NAME) %>%
    count()

# can't use mutate() with class 'SpatialPolygon'
cville_json$n <- tib_sum$n
cville_json$density <- cville_json$n / (cville_json$POPULATION+1)

# if rates above 1 turn to NA to save the scale
cville_json$density %<>% ifelse(. > 1, NA, .)
saveRDS(cville_json, "cville_json.RDS")

cville_json$label <- paste0(cville_json$NAME, "<br>",
                            "Pop: ", formatC(cville_json$POPULATION, big.mark = ","), "<br>",
                            "Reports: ", cville_json$n, "<br>",
                            "Rate: ", round(cville_json$density,2))

leaflet(cville_json,
    options = leafletOptions(minZoom = 12, maxZoom = 17)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "#a5a597", weight = 1, smoothFactor = 0.3, fillOpacity = .5,
                fillColor = ~pal(density),
                label = ~map(label, HTML))



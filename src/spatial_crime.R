# Spatial Point Patterns
# Cville Crime data
# nathancday@gmail.com 
# December 2, 2017

library(tidycensus)
census_api_key("72cfeef68568abee0cb7bcd33d284748d4dc0e37", install = TRUE)
library(geojsonio)
library(sf)
library(raster)
library(viridis)
library(lubridate)
library(magrittr)
library(tidyverse)

theme_set(theme_grey() + theme(legend.position = "top"))

# get geo-coded crime data from GitHub
crime <- read_csv("https://raw.githubusercontent.com/NathanCDay/cville_crime/master/crime_geocode.csv")
crime %<>% filter(complete.cases(crime))
names(crime) %<>% tolower()
crime %<>% mutate(drug_flag = ifelse(grepl("drug", offense, ignore.case = TRUE),
                                     "drugs", "not_drugs"))

# look for historical trends
crime$datereported %<>% as.Date()
# drop 2012 since mostly not these
crime %<>% filter(datereported > as.Date("2013-01-01"))

crime %<>% mutate(year_reported = year(datereported))

ggplot(crime, aes(as.factor(year_reported))) +
    geom_bar() +
    facet_wrap(~drug_flag, scales = "free")

# look for monthly trends
crime %<>% mutate(month_reported = month(datereported, label = TRUE))

ggplot(crime, aes(as.factor(month_reported))) +
    geom_bar()  +
    facet_wrap(~drug_flag, scales = "free")

### Look for Spatial Trends ----------------------------------------
# get census blocks from ODP
census <- geojson_read("https://opendata.arcgis.com/datasets/e60c072dbb734454a849d21d3814cc5a_14.geojson",
                       what = "sp")
names(census) %<>% tolower()
class(census)
census %<>% st_as_sf()
st_crs(census)

head(census)
census %<>% select(geometry, starts_with("h"), objectid:other)

# project crime onto census CRS
crs_val <- st_crs(census, asText = TRUE)
crime %<>% st_as_sf(coords = c("lon", "lat"), crs = crs_val)

# filter for only points within census shapes
crime %<>% mutate(within = st_within(crime, census) %>% as.numeric()) # returns NA for thos outside
crime %<>% filter(!is.na(within))

ggplot(census) +
    geom_sf() +
    geom_sf(data = crime,aes(color = drug_flag), shape = 1)
# takes a minutes, 30,000 points is a lot for ggplot

# better to group by address and use size to show volume
crime_sum2 <- group_by(crime, address) %>%
    summarise(total_crime = n(),
              drug_crime = sum(drug_flag == "drugs"))

ggplot(census) +
    geom_sf() +
    geom_sf(data = crime_sum2,
            aes(size = total_crime,
                alpha = total_crime)) +
    scale_size_area(max_size = 10) +
    scale_alpha_continuous(range = c(.1, .8)) # whats that huge cluster downtown

crime_sum2 %<>% arrange(-total_crime)
head(crime_sum2) # 600 E Market St
# the police station is 606 E Market St, hmmm

# fraction total crime at 600 E Market
crime_sum2$total_crime[1] / sum(crime_sum2$total_crime) # 3.4%
# fraction drug crime at 600 E Market
crime_sum2$drug_crime[1] / sum(crime_sum2$drug_crime) # 22.3%
# something is happening here

# drop the police station to look at spatial trends in the community
crime %<>% filter(address != "600 E MARKET ST Charlottesville VA")


# summarise counts by census block
crime_sum <- st_set_geometry(crime, NULL) %>% # need to remove geometry property
    group_by(within, drug_flag) %>%
    count() %>%
    spread(drug_flag, n) %>% rowwise() %>%
    mutate(frac_drugs = drugs / sum(drugs + not_drugs))

# average rate of drug / other in Cville
mean(crime_sum$frac_drugs) # 4.1%
range(crime_sum$frac_drugs) # <1% - 8.5%

# join rate info into census
census %<>% inner_join(crime_sum, by = c("objectid" = "within"))

ggplot(census) +
    geom_sf(aes(fill = drugs)) +
    scale_fill_viridis()

ggplot(census) +
    geom_sf(aes(fill = drugs + not_drugs)) +
    scale_fill_viridis()

ggplot(census) +
    geom_sf(aes(fill = frac_drugs)) +
    scale_fill_viridis()

# using a grid overlay instead of census block groups

grd <- st_make_grid(census, n = 50) %>% st_sf() # 50x50 evenly sized squares

grd %<>% st_intersection(st_geometry(st_union(census))) # filter to those in census


# filter into grid
crime$grd <- st_within(crime, grd) %>% as.numeric()
crime %<>% filter(!is.na(grd))

crime_sum3 <- st_set_geometry(crime, NULL) %>% # need to remove geometry property
    group_by(grd, drug_flag) %>%
    count() %>%
    spread(drug_flag, n) %>% rowwise() %>%
    mutate(frac_drugs = drugs / sum(drugs + not_drugs))


# join rate info into census
grd %<>% set_names("geometry") %>%
    mutate(grd = 1:nrow(.)) %>%
    full_join(crime_sum3)

# some areas don't have drug related crime, recode from NA to 0
grd$frac_drugs %<>% ifelse(is.na(.), 0, .)


ggplot(grd) +
    geom_sf(aes(fill = frac_drugs), lwd = 0) +
    geom_sf(data = st_boundary(census)) +
    scale_fill_viridis()

# look at environmental variables

# fraction of houses vacant
census %<>% mutate(frac_vacant = hu_vacant / housing_units)

ggplot(census) +
    geom_sf(aes(fill = frac_vacant)) +
    scale_fill_viridis()

# faction of population demographics
census %<>% mutate(frac_black = black / population,
                   frac_not_white = (population - white) / population)

ggplot(census) +
    geom_sf(aes(fill = frac_black)) +
    scale_fill_viridis()

ggplot(census) +
    geom_sf(aes(fill = frac_not_white)) +
    scale_fill_viridis()

library(cowplot)
ggarrange(ggplot(census) +
            geom_sf(aes(fill = frac_not_white)) +
            scale_fill_viridis(),
        ggplot(census) +
            geom_sf(aes(fill = frac_drugs)) +
            scale_fill_viridis())

# median income
library(tidycensus)
census_api_key("72cfeef68568abee0cb7bcd33d284748d4dc0e37", install = TRUE)

income <- get_acs(geography = "block group", county = "Charlottesville", state = "VA",
               variables = "B19013_001") # income
head(income)
income %<>% select(GEOID, median_income = estimate)
# join with census
census %<>% inner_join(income, by = c("blockgroup" = "GEOID"))

ggplot(census) +
    geom_sf(aes(fill = median_income)) +
    scale_fill_viridis()

ggplot(census) +
    stat_sf(aes(fill = median_income))

library(leaflet)
leaflet(census) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons()


# v15 <- load_variables(2015, "acs5", cache = TRUE)

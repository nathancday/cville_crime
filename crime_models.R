# Spatial Point Patterns
# Cville Crime data
# nathancday@gmail.com 
# December 2, 2017

library(tidycensus)
census_api_key("72cfeef68568abee0cb7bcd33d284748d4dc0e37", install = TRUE)
library(geojsonio)
library(spatstat)
library(sf)
library(raster)
library(viridis)
library(lubridate)
library(magrittr)
library(tidyverse)

theme_set(theme_grey() + theme(legend.position = "top"))

### Ins ----------------------------------------------------------------------------
# get geo-coded crime data from GitHub
crime <- read_csv("https://raw.githubusercontent.com/NathanCDay/cville_crime/master/crime_geocode.csv")
crime %<>% filter(complete.cases(crime))
names(crime) %<>% tolower()
crime %<>% mutate(drug_flag = ifelse(grepl("drug", offense, ignore.case = TRUE),
                                     "drugs", "not_drugs"))
# cleanups
filter(crime, drug_flag == "drugs") %>% with(table(offense))
crime %<>% filter(offense != "DRUG AWARENESS PRESENTATION")

table(crime$address) %>% sort(decreasing = TRUE) %>% head()
crime %<>% filter(address != "600 E MARKET ST Charlottesville VA")

# get census data for shapes and housing data
census <- geojson_read("https://opendata.arcgis.com/datasets/e60c072dbb734454a849d21d3814cc5a_14.geojson",
                       what = "sp")
names(census) %<>% tolower()
census %<>% st_as_sf()

census %<>% select(geometry, starts_with("hu"), pop = population, objectid)


# project crime onto census CRS
crime %<>% st_as_sf(coords = c("lon", "lat"), crs = st_crs(census))

# filter for only points within census shapes
crime %<>% mutate(objectid = st_within(crime, census) %>% as.numeric()) # returns NA for those outside
crime %<>% filter(!is.na(objectid))

table(crime$drug_flag) 
1436/28552 # 5% of total crime

### Spatial clustering (abondoned) ------------------------------------------
# spatstat
library(spatstat)

bbox <- st_bbox(census)

window <- owin(xrange = c(bbox[c(1,3)]),
               yrange = c(bbox[c(2,4)]))

coords <- st_coordinates(crime) %>% as.tibble()

crime_pp <- ppp(x = coords$X,
                y = coords$Y,
                window = window,
                marks = as.factor(crime$drug_flag)) # factor is key

split(crime_pp) %>% plot()

crime_densities <- density(split(crime_pp))
plot(crime_densities)

# Calc the drugs density divided by the sum of both
frac_drug_crime_density <- crime_densities[[1]] / 
    (crime_densities[[1]] + crime_densities[[2]])
frac_drug_crime_density

# Plot the density of the fraction of drug crime
plot(frac_drug_crime_density)
# ehh

## * bandwidth -----
library(spatialkernel)
# Scan from 500m to 1000m in steps of 50m
bw_choice <- spseg(
    crime_pp, 
    h = seq(500, 1000, by = 50),
    opt = 1)

# Plot the results and highlight the best bandwidth
plotcv(bw_choice); abline(v = bw_choice$hcv, lty = 2, col = "red")

# Print the best bandwidth
print(bw_choice$hcv)


# Set the correct bandwidth and run for 10 simulations only
seg10 <- spseg(
    pts = preston_crime, 
    h = 800,
    opt = 3,
    ntest = 10, 
    proc = FALSE)
# Plot the segregation map for violent crime
plotmc(seg10, "Violent crime")

# Plot seg, the result of running 1000 simulations
plotmc(seg, "Violent crime")



### Areal stats -------------------------------------------------------------

# sumamrise by census blocks

# remove point geometry
st_geometry(crime) <- NULL

crime %<>% group_by(objectid, drug_flag) %>%
    count() %>%
    spread(drug_flag, n) %>% 
    mutate(frac_drugs = drugs / not_drugs)

hist(crime$frac_drugs)

# merge into census
census %<>% inner_join(crime)

# hackerman 
scale_fill_continuous <- function(...) { viridis::scale_fill_viridis(...) }

ggplot(census) +
    geom_sf(aes(fill = frac_drugs))

# compare to pop
ggplot(census) +
    geom_sf(aes(fill = pop))

# compare to fraction of housing units vacant
census %<>% mutate(frac_vacant = hu_vacant / hu_occupied)

ggplot(census) +
    geom_sf(aes(fill = frac_vacant))


# * spatial correlation -----
library(spdep) # it's an older package sir

census_sp <- as(census, "Spatial") # so we need to revert to Spatial
block_nb <- poly2nb(census_sp) # build a neighbor network

plot(census_sp); plot(block_nb, coordinates(census_sp), add = TRUE) # see the network

moran.mc(census_sp$frac_drugs, nb2listw(block_nb), nsim = 999) # sptial correlated
moran.mc(census_sp$pop, nb2listw(block_nb), nsim = 999) # not
moran.mc(census_sp$hu_vacant, nb2listw(block_nb), nsim = 999) # correlated


# * smr ------
# standardized morbidity rate from epidemioloy, areas > 1 are high

# using frac_drugs, compute avg rate for city
rate <- mean(census$frac_drugs)

census %<>% mutate(total = drugs + not_drugs,
                   exp_drugs = total * rate, # use to calculate expected
                   sr_drugs = drugs / exp_drugs) # standardize, like frac_drugs

# make a label df
labels <- st_centroid(census) %>%
    st_geometry() %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as.tibble() %>%
    mutate(objectid = census$objectid)


ggplot(census) +
    geom_sf(aes(fill = sr_drugs)) +
    geom_label(data = labels, aes(x = V1, y = V2, label = objectid))

# For the binomial statistics function
library(epitools)

# Get CI from binomial distribution
crime_ci <- with(census, binom.exact(drugs, total)) %>%
    as.tibble()

# add relvant data
crime_ci %<>% mutate(objectid = census$objectid,
                     sr = proportion / rate)

# Subset the high SMR data
crime_high <- filter(crime_ci, sr > 1) %>%
    arrange(-sr) %>%
    mutate(objectid = fct_inorder(as.factor(objectid)))



# Plot estimates with CIs
ggplot(crime_high, aes(x = objectid, y = sr,
                     ymin = lower / rate, ymax = upper / rate,
                     color = sr)) +
    geom_pointrange(size = 2) +
    scale_color_viridis()


?pbinom

# compute probability that sr is greater than 1.5x
census %<>% mutate(gt = 1 - pbinom(1.5 * exp_drugs, size = total, prob = frac_drugs))

# use scale to cut off those prob(gt) > 75%
ggplot(census) +
    geom_sf(aes(fill = gt)) +
    scale_fill_viridis(limit = c(.75, NA)) +
    geom_text(data = labels, aes(x = V1, y = V2, label = objectid))





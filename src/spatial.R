#### Globals --------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(geojsonio)

#### Location Data ---------------------------------------------------------

# read in geo-coded data from github
cville <- read_csv("https://raw.githubusercontent.com/NathanCDay/cville_crime/master/Crime_Data_Geocoded.csv")
names(cville) %<>% tolower()
# needs the raw subdomain to work on my link
cville$offense %<>% as.factor() %>% fct_infreq()

levels(cville$offense)[1:10]

steals <- filter(cville, grepl("LARCENY", offense)) %>%
    mutate(offense = "larceny")

beats <- filter(cville, grepl("ASSAULT", offense)) %>%
    mutate(offense = "assult")

both <- bind_rows(steals, beats)
both %<>% filter(complete.cases(.))

# check hi-freq addresses
table(both$address) %>% sort(decreasing = TRUE) %>% head(20)

### Spatial Segmentation ---------------------------------------------------
library(spatstat); library(spatialkernel)

# map <- get_map(location = "Charlottesville", zoom = 13)

ggmap(map) +
    geom_point(data = both, aes(lon, lat, color = offense), alpha = .1)

# lots of redundant coordinates
both %>% group_by(lat, lon) %>% tally() %>% nrow() # only 1175 unique
# add some jitter
both %<>% mutate_at(vars(lat, lon), funs(jitter(., amount = .0005)))

both %>% group_by(lat, lon) %>% tally() %>% nrow() # now all 5157 are unique

ggplot(both, aes(lon, lat, col = offense)) +
    geom_point()

# build a ppp object
xr <- range(both$lon, na.rm = TRUE)
yr <- range(both$lat, na.rm = TRUE)

cntr <- c(-78.487, 38.035)
p_both <- ppp(both$lon, both$lat, disc(.045, cntr),
               marks = as.factor(both$offense))

plot(p_both, cex = .1)

spseg(p_both)

# get an ideal bin-width
bw <- spseg(p_both, 
    h = seq(25, 500, by = 25), opt = 1)
plotcv(bw)
bw$hcv

# use the ideal bw to plot segregation
seg10 <- spseg(
    pts = p_both, 
    h = bw$hcv,
    opt = 3,
    ntest = 500, 
    proc = FALSE)

# Plot the segregation map for violent crime
plotmc(seg10, "drug")


library(geojsonio)
census <- geojson_read("https://opendata.arcgis.com/datasets/e60c072dbb734454a849d21d3814cc5a_14.geojson",
                             what = "sp")

census@data %<>% .[c(1,5:6,12:19)]
names(census@data) %<>% tolower()

# Use the cartogram and rgeos packages
library(cartogram); library(rgeos)
# show population is not correlated with area, so the map will be visually misleading
plot(census$population, gArea(census, byid = TRUE))

# Make a cartogram, to scale the areas to populations, to show the people better
carto_ref <- cartogram(census, "population")
plot(carto_ref)
plot(census)

# Check the linearity of the pop-area plot
plot(carto_ref$population, gArea(carto_ref, byid = TRUE))
# better

# Look at where black and white people live
spplot(carto_ref, "black") # lattice
spplot(carto_ref, "white")

# now do it in ggplot
library(sf); library(viridis)
# https://cran.r-project.org/web/packages/sf/vignettes/sf2.html#conversion-to-and-from-sp
carto_sf <- st_as_sf(carto_ref)
# http://strimas.com/r/tidy-sf/
carto_geom <- st_geometry(carto_sf)
ggplot(carto_sf) +
    geom_sf(aes(fill = population)) +
    scale_fill_viridis(option = "plasma")

# no do it with leaflet
library(leaflet)
pal <- colorNumeric("plasma", NULL)
leaflet(carto_ref,
        options = leafletOptions(minZoom = 13, maxZoom = 17)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "black", smoothFactor = 0.3, fillOpacity = .5, stroke = F,
                fillColor = ~pal(population),
                label = ~paste0(formatC(population, big.mark = ",")))

# determine which beats the gps coordinates are in
# https://gis.stackexchange.com/questions/133625/checking-if-points-fall-within-polygon-shapefile
library(rgeos);library(rgdal)

dat <- select(both, lon, lat)
coordinates(dat) <-  ~ lon + lat
proj4string(dat) <- proj4string(census)

res <- over(dat, census)
# returns the match in cville_son for each row in dat
table(complete.cases(res)) #112 didn't match

# bind together then filter
both %<>% bind_cols(select(res, objectid)) %>% filter(complete.cases(.))

# summarise by objectid (Census Tract) and move values into carto_ref

both_sum <- both %>% group_by(objectid) %>%
    count(offense) %>%
    spread(offense, n)

carto_sf %<>% inner_join(both_sum)
carto_ref@data %<>% inner_join(both_sum)

ggplot(carto_sf) +
    geom_sf(aes(fill = larceny)) +
    scale_fill_viridis()

leaflet(carto_ref,
        options = leafletOptions(minZoom = 13, maxZoom = 17)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "black", smoothFactor = 0.3, fillOpacity = .5, stroke = F,
                fillColor = ~pal(larceny),
                label = ~paste0(objectid))

#### Autocorrelation Tests -------------------------------------------------
library(spdep)

# Make neighbor list
borough_nb <- poly2nb(census)

# Get center points of each borough
borough_centers <- coordinates(census)

# Show the connections
plot(census); plot(borough_nb, borough_centers, add = TRUE)

census@data %<>% mutate(pct_black = black / population,
                        pct_white = white / population,
                        pct_asian = asian / population)

# Map the total pop'n
spplot(census, zcol = "pct_asian")

leaflet(census,
        options = leafletOptions(minZoom = 13, maxZoom = 17)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "black", smoothFactor = 0.3, fillOpacity = .5, stroke = F,
                fillColor = ~pal(pct_asian),
                label = ~paste0(pct_asian))

# Run a Moran I test on total pop'n
moran.test(census$population, nb2listw(borough_nb)) # not significant

moran.test(census$pct_asian, nb2listw(borough_nb)) # significant


# Run a Moran I MonteCarloSim test on pct_black
moran.mc(census$pct_asian, nb2listw(borough_nb), nsim = 999) # significant

leaflet(census,
        options = leafletOptions(minZoom = 13, maxZoom = 17)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "black", smoothFactor = 0.3, fillOpacity = .5, stroke = F,
                fillColor = ~pal(pct_asian),
                label = ~paste0(pct_asian))

# bring is crime area summary
census@data %<>% inner_join(both_sum)

leaflet(census,
        options = leafletOptions(minZoom = 13, maxZoom = 17)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "black", smoothFactor = 0.3, fillOpacity = .5, stroke = F,
                fillColor = ~pal(pct_white),
                label = ~paste0(pct_black,"<br>",assult))

#### Tidycensus ------------------------------------------------------------
# a detour
library(tidycensus)
census_api_key("72cfeef68568abee0cb7bcd33d284748d4dc0e37", install = TRUE)

cvl <- get_acs(geography = "block group", county = "Charlottesville", state = "VA",
               variables = "B19013_001") # income
# cvl$GEOID = census$blockgroup
cvl %<>% rename(blockgroup = GEOID)
census@data %<>% inner_join(cvl)

leaflet(census,
        options = leafletOptions(minZoom = 13, maxZoom = 17)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "black", smoothFactor = 0.3, fillOpacity = .5, stroke = F,
                fillColor = ~pal(estimate),
                label = ~paste(estimate))

# check for other variables
all_vars <- load_variables(2015, "acs5", cache = TRUE)
filter(all_vars, grepl("income", concept, ignore.case = TRUE)) %>% View()

#### GLMs in Spatial serttings --------------------------------------------
## * spatial  correlation of residuals -----

model_assault <- glm(
    assult ~ pct_black, 
    offset = log(population), 
    data = census@data, 
    family = "poisson")

# Is HealthDeprivation significant?
summary(model_assault)

# Put residuals into the spatial data.
census@data$resid <- residuals(model_assault)

# Map the residuals using spplot
spplot(census, "resid")

leaflet(census, options = leafletOptions(minZoom = 12, maxZoom = 12)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = "black", smoothFactor = 0.3, fillOpacity = .5,
                fillColor = ~pal(resid),
                label = ~paste(resid))

# compute neighbor structure of census polygons
census_nb <- poly2nb(census)
centers <- coordinates(census)
plot(census); plot(census_nb, centers, add = TRUE)
# Get center points of each borough

# use Moran I test to check
moran.mc(census$resid, listw = nb2listw(census_nb), nsim = 999)
# not spatially correlated

# fitting the same model using Bayesian Inference
library(R2BayesX)
# need to run \/ this \/ to get it to work 
# install.packages("BayesXsrc", type = "source") # commented bc it takes a few minutes

# basic glm model from earlier                    
summary(model_assault)
confint(model_assault)

# Use 
bayes_assault <- bayesx(assult ~ pct_black, offset = log(census$population), 
                    family = "poisson", data = as.data.frame(census), 
                    control = bayesx.control(seed = 17610407))

# Summarize it                    
summary(bayes_assault)

# Look at the samples from the Bayesian model
plot(samples(bayes_assault))

## * Bayesian GLM with spatial component --------------------
# Compute adjacency objects
census@data$i <- census@data$objectid
census_nb <- poly2nb(census)
census_gra <- nb2gra(census_nb)
rownames(census_gra) <- 1:37
colnames(census_gra) <- 1:37
# Fit spatial model
assault_spatial <- bayesx(
    assult ~ pct_black + sx(objectid, bs = "spatial", map = census_gra),
    offset = log(census$population),
    family = "poisson", data = as.data.frame(census@data), 
    control = bayesx.control(seed = 17610407)
)

# Summarize the model
summary(assault_spatial)
# with the spatial term in the model the pct_black is no longer significant

# Map the fitted spatial term only
census$spatial <- fitted(assault_spatial, term = "sx(objectid):mrf")[, "Mean"]
spplot(census, zcol = "spatial")

# Map the residuals
census$spatial_resid <- residuals(assault_spatial)[, "mu"]
spplot(census, zcol = "spatial_resid")

# Test residuals for spatial correlation
moran.mc(census$spatial_resid, nb2listw(borough_nb), 999)

# Spatial Trends
# from: https://assets.datacamp.com/production/course_3070/datasets/ca_geo.rds
ca_geo <- readRDS("~/Downloads/ca_geo.rds")
# Are they called lat-long, up-down, or what?
coordnames(ca_geo)

# Complete the formula
m_trend <- lm(pH ~ x + y, as.data.frame(ca_geo))

# Check the coefficients
summary(m_trend)

# predict from the model for missing data
miss <- is.na(ca_geo$pH)

# Create a data frame of missing data
ca_geo_miss <- as.data.frame(ca_geo)[miss, ]

# Predict pH for the missing data
predictions <- predict(m_trend, newdata = ca_geo_miss, se.fit = TRUE)

# Compute the exceedence probability
pAlkaline <- 1 - pnorm(7, mean = predictions$fit, sd = predictions$se.fit)
hist(pAlkaline)

# make a variogram
library(gstat)
# cloud version, not as good
plot(variogram(pH ~ 1, ca_geo[!miss, ], cloud = TRUE, cutoff = 10 * 1000))
# 10km cutoff needs to be in meters

# Make a variogram of the non-missing data
plot(variogram(pH ~ 1, ca_geo[!miss, ]))

# vario with a spatial trend
ph_vgm <- variogram(pH ~ x + y, ca_geo[!miss, ])
plot(ph_vgm)
# since the trend flattens out around 20,000 m (20 km) indicating spatial cor is not present
# beyond 20 km

# Eyeball the parameter based on previous plot
nugget <- .15
psill <- .27
range <- 20000

# Fit the variogram
v_model <- fit.variogram(
    ph_vgm, 
    model = vgm(
        model = "Ste",
        nugget = nugget,
        psill = psill,
        range = range,
        kappa = 0.5
    )
)

# Show the fitted variogram on top of the binned variogram
plot(ph_vgm, model = v_model)
print(v_model)

## Krigging
# the process of interpolating data from geo-spatial data and variograms
# apply a variogram and other data

# predict the missing values via model
km <- krige(pH ~ x + y, ca_geo[!miss, ], newdata = ca_geo[miss, ], model = v_model)
names(km)

# Plot the predicted values
spplot(km, "var1.pred")

# Compute the probability of alkaline samples, and map
km$pAlkaline <- 1 - pnorm(7, mean = km$var1.pred, sd = sqrt(km$var1.var))
spplot(km, "pAlkaline")

## * Making a prediction grid
# need to figure out how to make this jump comes from sp::SpatialPolygons
# Plot the polygon and points
plot(geo_bounds); points(ca_geo)

# Find the corners of the boundary
bs <- bbox(geo_bounds)

# Define a 2.5km square grid over the polygon extent. The first parameter is
# the bottom left corner.
grid <- GridTopology(round(bs[1:2]), c(2500, 2500), c(72, 48))

# Create points with the same coordinate system as the boundary
gridpoints <- SpatialPoints(grid, proj4string = CRS(projection(geo_bounds)))
plot(gridpoints)

# Crop out the points outside the boundary
cropped_gridpoints <- crop(gridpoints, geo_bounds)
plot(cropped_gridpoints)

# Convert to SpatialPixels ( ie clean up for spplot())
spgrid <- SpatialPixels(cropped_gridpoints)
coordnames(spgrid) <- c("x", "y")
plot(spgrid)

# Do kriging predictions over the grid
ph_grid <- krige(pH ~ x + y, ca_geo[!miss, ], newdata = spgrid, model = v_model)

# Calc the probability of pH exceeding 7
ph_grid$pAlkaline <- 1 - pnorm(7, mean = ph_grid$var1.pred, sd = sqrt(ph_grid$var1.var))

# Map the probability of alkaline samples
spplot(ph_grid, zcol = "pAlkaline")

# Kriging with linear trend, predicting over the missing points
ph_auto <- autoKrige(
    pH ~ x + y, 
    input_data = ca_geo[!miss, ], 
    new_data = ca_geo[miss, ], 
    model = "Mat"
)

# Plot the variogram, predictions, and standard error
plot(ph_auto)

# Auto-run the kriging
ph_auto_grid <- autoKrige(pH ~ x + y, input_data = ca_geo[!miss,], new_data = spgrid)

# Remember predictions from manual kriging
plot(ph_grid)

# Plot predictions and variogram fit
plot(ph_auto_grid)
plot(ph_auto_grid$krige_output)

# Compare the variogram model to the earlier one
v_model
ph_auto_grid$var_model


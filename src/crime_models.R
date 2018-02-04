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

# hackerman 
theme_set(theme_grey() + theme(legend.position = "top"))
scale_fill_continuous <- function(...) { viridis::scale_fill_viridis(...) }
geom_sf <- function(data, ...) {geom_sf(data = data, ...) +
                                  coord_sf(crs = st_crs(data), datum = NA) }


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

census %<>% select(geometry, starts_with("hu"), white, black, pop = population, objectid, blockgroup)

# project crime onto census CRS
crime %<>% st_as_sf(coords = c("lon", "lat"), crs = st_crs(census))

# filter for only points within census shapes
crime %<>% mutate(objectid = st_within(crime, census) %>% as.numeric()) # returns NA for those outside
crime %<>% filter(!is.na(objectid))

table(crime$drug_flag)
1436 / (1436 + 28552) # 4.7% of crime

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

ggplot(census) +
    geom_sf(aes(fill = frac_drugs))

# compare to pop
ggplot(census) +
    geom_sf(aes(fill = pop))

# compare to fraction of housing units vacant
census %<>% mutate(frac_vacant = hu_vacant / (hu_occupied + hu_vacant))

ggplot(census) +
    geom_sf(aes(fill = frac_vacant))


# * auto correlation -----
library(spdep) # it's an older package sir, but it checks out

census_sp <- as(census, "Spatial") # so we need to revert to Spatial
block_nb <- poly2nb(census_sp) # build a neighbor network

plot(census_sp); plot(block_nb, coordinates(census_sp), add = TRUE) # see the network

moran.mc(census_sp$frac_drugs, nb2listw(block_nb), nsim = 999) # sptial correlated
ggplot(census) + geom_sf(aes(fill = frac_drugs))

moran.mc(census_sp$pop, nb2listw(block_nb), nsim = 999) # not
ggplot(census) + geom_sf(aes(fill = pop))

moran.mc(census_sp$frac_vacant, nb2listw(block_nb), nsim = 999) # correlated
ggplot(census) + geom_sf(aes(fill = frac_vacant))


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
    scale_fill_gradient2(midpoint = 1) +
    geom_label(data = labels, aes(x = V1, y = V2, label = objectid))

# * excedence probabilities -----

# For the binomial statistics function
library(epitools)

# Get CI from binomial distribution
crime_ci <- with(census, binom.exact(drugs, total)) %>%
    as.tibble()

# add relvant data
crime_ci %<>% mutate(objectid = census$objectid)

# Subset the high SMR data
crime_high <- arrange(crime_ci, -proportion) %>%
    mutate(objectid = fct_inorder(as.factor(objectid)))



# Plot estimates with CIs
ggplot(crime_high, aes(x = objectid,
                       y = proportion / rate, # SR
                       ymin = lower / rate, ymax = upper / rate,
                     color = proportion / rate)) +
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

# * glms ------

# loop up variables
# v15 <- load_variables(2015, "acs5", cache = TRUE)
# View(v15)

# get median income & age from ACS via library(tidycensus)
cvl <- get_acs(geography = "block group", county = "Charlottesville", state = "VA",
               variables = c("B19013_001", "B01002_001") )

decode <- c("income", "age") %>% set_names(c("B19013_001", "B01002_001"))
cvl$variable %<>% decode[.]

cvl %<>% select(GEOID, variable, estimate) %>%
    spread(variable, estimate)

ggplot(cvl, aes(age, income)) +
    geom_point() # missing values
# let's impute them by neighbors

cvl %<>% rename(blockgroup = GEOID)

census %<>% full_join(cvl)

# sequester the missing values value
miss <- census %>% filter(is.na(income))

# calculate the mean its neightbors
miss$income <- st_touches(miss, census) %>% # return the row_ids for adjacent polygons
    map_dbl(~ census[., ] %>% with(mean(income))) # calculate the means per missing block

# builder decoder
dc <- miss$income %>% set_names(miss$objectid) 
    
# back together again
census$income %<>% ifelse(is.na(.), dc["19"], .)

# bc drug laws are racist
census %<>% mutate(frac_black = black / pop)

# pred column positions for ggpairs()
pred_cols <- match(c("frac_drugs", "frac_vacant", "age", "income", "frac_black"), names(census))

scaled_preds <- census[pred_cols] %>% st_set_geometry(NULL) %>%
    mutate_at(vars(-frac_drugs), scale) %>%
    map_dfc(as.numeric)

ggpairs(census, columns = pred_cols)
ggpairs(scaled_preds)


# model
mod <- glm(drugs ~  frac_black + frac_vacant, data = census,
           offset = log(not_drugs+drugs), family = "poisson")
summary(mod)
resid(mod) %>% hist()
resid(mod) %>% qqnorm()
qqline(resid(mod))
shapiro.test(resid(mod))

census$resid <-  resid(mod)
census$fitted <- fitted(mod)
ggplot(census, aes(resid, fitted)) +
    geom_point()

census$predicted <- exp(1)^predict(mod) # de-link
ggplot(census, aes(predicted, drugs)) +
    geom_point() +
    geom_abline()

ggplot(census) + geom_sf(aes(fill = resid))
# check if resids are auto-correlated
mod_sp <- as(census, "Spatial")
mod_nb <- poly2nb(mod_sp)

moran.mc(census$resid, nb2listw(block_nb), nsim = 999) # its not

# * Bayesian glms ----
library(R2BayesX)
bayes_mod <- bayesx(drugs ~ frac_black + frac_vacant,
                    offset = log(census$not_drugs + census$drugs), 
                    family = "poisson", data = census)

confint(mod)
confint(bayes_mod)

# * Now with Spatial random effects
# special new graph type of neighbor representation
mod_gra <- nb2gra(block_nb)

sp_re <- sx(i, bs = "spatial", map = mod_gra)
bayes_mod_sp <- bayesx(drugs ~ frac_black + frac_vacant + sx(objectid, bs = "spatial", map = mod_gra),
                    offset = log(census$not_drugs + census$drugs), 
                    family = "poisson", data = census)

summary(bayes_mod_sp)

confint(bayes_mod)
confint(bayes_mod_sp)

census$spatial <- fitted(bayes_mod_sp)$mu
ggplot(census) + geom_sf(aes(fill = spatial))

# Map the residuals
census$spatial_resid <- residuals(bayes_mod_sp)[,2]
ggplot(census) + geom_sf(aes(fill = spatial_resid))

moran.mc(census$spatial_resid, nb2listw(block_nb), nsim = 999)


Cville Crime Data Dive
========================================================
author: Nate Day
date: 2018-02-07
autosize: true

Why drug crime?
========================================================

- Community impact
- Cooler than parking tickets
- Easy access to ~31,000 observations

Goals of this talk
========================================================

- Show the spatial distribution of crime in the city
- Test if the distribution of drug crime is random
- Provide a R template for analyzing other CPD data


Raw Data --->>> Maps
========================================================

1. Ask Google to GPS map a mailing address.
2. Repeat a bunch.
3. Save.
4. `ggplot(crime_data) + geom_sf()`


Maps --->>> Meaning
========================================================





```
Error in crime %<>% mutate(drug_flag = ifelse(grepl("drug", Offense, ignore.case = TRUE),  : 
  could not find function "%<>%"
```

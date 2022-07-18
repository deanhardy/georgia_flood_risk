rm(list=ls())

library(tidyverse) ## load tidyverse package
library(lubridate)
library(dataRetrieval)

## USGS tutorial: https://owi.usgs.gov/R/dataRetrieval.html#1

## set data directory
datadir <- "/Users/dhardy/Dropbox/r_data/georgia_flood_risk"

# Hudson River, Meridian, GA
siteNo <- "022035975"
pCode <- "00065"
start.date <- "2007-10-01"
end.date <- "2022-07-17"

df <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

## station datum NAVD88 = +1.1 ft
## VDATUM says Hudson Creek entrance NAVD88 0 ft is 4.177 ft in MLLW, so 5.277 ft for station
##  convert datum to mllw elevation datum 
df2 <- mutate(df, navd88 = X_00065_00000 + 4.18) %>% 
  mutate(year = year(dateTime)) %>%
  rename(datetime = dateTime) %>%
  select(agency_cd, site_no, datetime, year, tz_cd, navd88)

ggplot(df2, aes(datetime, navd88)) + 
  geom_line()

write_csv(df, paste0(datadir, '/data/meridian_071031-220718.csv'))


rm(list=ls())

library(tidyverse) ## load tidyverse package
library(lubridate)
library(dataRetrieval)

## USGS tutorial: https://owi.usgs.gov/R/dataRetrieval.html#1

## set data directory
datadir <- "/Users/dhardy/Dropbox/r_data/georgia_flood_risk"

## list of codes
codes <- parameterCdFile

# Hudson River, Meridian, GA
siteNo <- "022035975"
pCode <- "00065"
statCode <- "00021"
start.date <- "2000-10-06" ## earliest available date
end.date <- "2022-12-25"

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

# write_csv(df, paste0(datadir, '/data/meridian_071001-220718.csv'))


##############################################
## SINERR SWMP Data for Marsh Landing
##############################################

## read in nerr data
nerr <- read_csv(paste0(datadir, '/data/original/220719-sinerr-all/sapldwq2021.csv')) %>%
  mutate(DateTimeStamp = as.POSIXct(DateTimeStamp, format = "%m/%d/%Y %H:%M")) %>%
  mutate(date = as.Date(DateTimeStamp),
         navd88 = Depth - 4.345) %>%
  filter(Depth > 0)

yr <- year(nerr$date)
  
nerr_dl <- nerr %>%
  group_by(date) %>%
  summarise(daily_level = mean(navd88))

ggplot(nerr_dl, aes(date, daily_level)) + 
  geom_line() + 
  scale_x_date(name = yr, breaks = '1 month', date_labels = '%m/%d') + 
  scale_y_continuous(name = 'Mean Water Level (m NAVD88)') + 
  ggtitle("Marsh landing SINERR Station")



rm(list=ls())

library(tidyverse)
library(sf)

## set data directory
datadir <- "/Users/dhardy/Dropbox/r_data/georgia_flood_risk"

## import map data
world <- map_data("world")
usa <- map_data("usa")
states <- map_data("state")
counties <- map_data("county")

AL <- read.csv2(paste0(datadir, '/data/original/hurdat2-1851-2022-050423.txt'), sep = ',', header = FALSE)


ALpt <- 
  AL %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, agr = "constant") %>%
  rename(ts_diam = tropicalstorm_force_diameter, hu_diam = hurricane_force_diameter) %>%
  mutate(date = ymd(paste(year, month, day)))

ALpt_hu <- ALpt %>%
  filter(status == 'hurricane')


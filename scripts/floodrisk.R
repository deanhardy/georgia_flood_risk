rm(list=ls())

library(tidyverse) 
library(lubridate)

## set data directory
datadir <- ("/Users/dhardy/Dropbox/r_data/georgia_flood_risk")


## read in high/low tidal data from gauge station 
## https://waterdata.usgs.gov/ga/nwis/nwismap/?site_no=022035975&agency_cd=USGS
## gage datum is 1.10 feet above NAVD88
df <- read.delim(file.path(datadir, "data/original/20210708_height_allobserved_ml.txt"), header=TRUE, sep = '\t', dec = '.',
                 skip = 30) %>%
  slice(2:n()) %>%
  rename(high = X35070_00065_00021, low = X35071_00065_00024,
         hquality = X35070_00065_00021_cd, lquality = X35071_00065_00024_cd) %>%
  # select(-hquality, -lquality) %>%
  unite('high', high, hquality) %>%
  unite('low', low, lquality) %>%
  gather(key = type, value = height, 4:5) %>%
  separate(height, c('height', 'quality'), sep = '_') %>%
  mutate(height = as.numeric(height)) %>%
  filter(type == "high") %>%
  mutate(height = height + 4.18, year = year(datetime))

df$datetime <- as.POSIXct(df$datetime) ## convert datetime column to correct format

## revising data by subtracting 1.1 post Matthew
df2 <- df %>%
  mutate(height = ifelse(datetime > '2016-10-07', height - 1.1, height))

## filter highest annual high tides and rank order them
df3 <- df2 %>% 
  group_by(year) %>%
  slice(which.max(height)) %>%
  ungroup() %>%
  arrange(desc(height))

## calc recurrence interval (T) and exceedance probability (P)
df4 <- df3 %>%
  mutate(T = ((1+n()/row_number())),
         P = 1/T)

fnt <- 16
A <- 1## convert to meters

xbreaks <- rev(c(1,2,5,10,20,30,40,50,60,70,80,90,95,98,99,99.5,99.9)/100)

ggplot(df4, aes(height, rev(P))) + 
  geom_point() + 
  scale_y_continuous(trans='probit', breaks = xbreaks, minor_breaks=qnorm(xbreaks)) +
  scale_x_log10(breaks=seq(0,12,1)) + 
  geom_smooth(method = lm, col = 'black', size = 0.5) + 
  labs(y="Exceedance Probability", x="Height (feet)")

## https://stackoverflow.com/questions/17823474/my-probability-plot-and-log-scales-do-not-line-up-with-my-gridlines-using-ggplo



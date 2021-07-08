rm(list=ls())

library(tidyverse) 
library(lubridate)

## set data directory
datadir <- ("/Users/dhardy/Dropbox/r_data/georgia_hurricanes")

## read in high/low tidal data from gauge station 
## https://waterdata.usgs.gov/ga/nwis/nwismap/?site_no=022035975&agency_cd=USGS
## station datum NAVD88 = +1.1 ft
## VDATUM says Hudson Creek entrance NAVD88 0 ft is 4.177 ft in MLLW, so 5.277 ft for station
##  convert datum to mllw elevation datum 
df <- read.delim(file.path(datadir, "data/original/20210708_height_allobserved_ml.txt"), header=TRUE, sep = '\t', dec = '.',
                 skip = 30) %>%
  slice(2:n()) %>%
  rename(high = X35070_00065_00021, low = X35071_00065_00024,
         quality = X35070_00065_00021_cd, quality2 = X35071_00065_00024_cd) %>%
  select(-quality, -quality2) %>%
  gather(key = type, value = height, 4:5) %>%
  mutate(height = as.numeric(height)) %>%
  filter(type == "high") %>%
  mutate(height = height + 4.177-1.1, year = year(datetime))

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
  scale_x_log10(breaks=seq(0,10, 1))

  labs(x="Exceedance Probability", y="Height (feet)")

## https://stackoverflow.com/questions/17823474/my-probability-plot-and-log-scales-do-not-line-up-with-my-gridlines-using-ggplo


fig <- ggplot(filter(df2, type == 'high'), aes(datetime, height*A)) +
  geom_point(pch=19, size = 1, color = 'grey') + 
  geom_point(mapping = aes(datetime, height*A, col = 'red'),
             data = filter(df, type == 'high' & height >= 9.5),
             size = 5, pch = 17, inherit.aes = TRUE) +
  geom_smooth(method = lm, col = 'black', size = 0.5) + 
  geom_hline(yintercept = 9.5*A, color = 'red', linetype = 'dashed') + ## 9.5 feet is flood stage at Fort Pulaski
  scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", 
                   limits = c(df$datetime[1], last(df$datetime)), expand = c(0.01, 0.0)) + 
  scale_y_continuous(breaks = seq(0,12,1),
                     labels = seq(0,12,1),
                     limits = c(0,12), expand = c(0,0)) +
  xlab("Year") +
  ylab("High Tide (feet)") +
  labs(caption = "subtracted 1.1 ft following Hurricane Matthew peak on 10/07/2016") +
  #ggtitle("Meridian Landing Gage") + 
  theme(axis.title = element_text(size = fnt),
        axis.text = element_text(size = fnt),
        axis.text.x = element_text(margin = margin(0.5, 0, 0, 0, unit = 'cm')),
        axis.text.y = element_text(margin = margin(0, 0.5, 0, 0, unit = 'cm')),
        axis.ticks.length=unit(-0.1, "cm"),
        axis.ticks = element_line(size = 0.4),
        title = element_text(size = fnt),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = 'white', color = 'black', size = 0.5),
        legend.position = "none") + 
  ggtitle("Revised by Dean, High Tide Data (downloaded 07/08/2021)") + 
  annotate(geom="text", y = 9.5*A, x = df$datetime[3700], label = "Fort Pulaski\nFlood Stage", col = 'red')
fig

tiff(file.path(datadir, 'figures/meridian_hightides_alltime-July2021revised-by-dean.tiff'), res=300, unit='in', width = 13.33, height = 7.5, 
               compression = 'lzw')
fig
dev.off()

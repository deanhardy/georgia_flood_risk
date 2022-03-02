rm(list=ls())

library(tidyverse) ## load tidyverse package
library(lubridate)
##library(timeSeries)

## set data directory
datadir <- "/Users/dhardy/Dropbox/r_data/georgia_flood_risk"

## read in high/low tidal data from gauge station 
## https://waterdata.usgs.gov/ga/nwis/nwismap/?site_no=022035975&agency_cd=USGS
## gage datum is 1.10 feet above NAVD88
df <- read.delim(file.path(datadir, "data/original/20220301_height_allobserved_ml.txt"), header=TRUE, sep = '\t', dec = '.',
                 skip = 30) %>%
  slice(2:n()) %>%
  rename(high = X35070_00065_00021, low = X35071_00065_00024,
         hquality = X35070_00065_00021_cd, lquality = X35071_00065_00024_cd) %>%
  # select(-hquality, -lquality) %>%
  unite('high', high, hquality) %>%
  unite('low', low, lquality) %>%
  gather(key = type, value = height, 4:5) %>%
  separate(height, c('height', 'quality'), sep = '_') %>%
  mutate(height = as.numeric(height))

df$datetime <- as.POSIXct(df$datetime) ## convert datetime column to correct format

## station datum NAVD88 = +1.1 ft
## VDATUM says Hudson Creek entrance NAVD88 0 ft is 4.177 ft in MLLW, so 5.277 ft for station
##  convert datum to mllw elevation datum 
df <- mutate(df, height = height + 4.18) %>% 
  mutate(year = year(datetime))

## playing around with revised data by revising post Irma
## appears corrected sometime between July 2021 and March 2022
# df2 <- df %>%
#   mutate(height = ifelse(datetime > '2016-10-07', height - 1.1, height))

## select out highest high tides and low tides
hi10 <- top_n(df, 10, height)
lo10 <- 
  df %>%
  filter(type == "low") %>%
  top_n(10, height)

# lims <- as.POSIXct(strptime(c("2011-01-01 03:00","2011-01-01 16:00"), format = "%Y-%m-%d %H:%M"))    
fnt <- 10
A <- 1## convert to meters

fig <- ggplot(filter(df, type == 'high'), aes(datetime, height*A)) +
  geom_point(pch=19, size = 0.1, color = 'grey') + 
  geom_point(mapping = aes(datetime, height*A, col = 'red'),
             data = filter(df, type == 'high' & height >= 10.7),
             size = 2, pch = 17, inherit.aes = TRUE) +
  geom_smooth(method = lm, col = 'black', size = 0.1) + 
  # geom_vline(xintercept = as.POSIXct('2016-10-07 12:00', format = "%Y-%m-%d %H:%M"), linetype = 'dashed') + 
  geom_hline(yintercept = 9.7*A, color = 'black', linetype = 'dashed') + ## 9.7 feet is action stage at Meridian
  geom_hline(yintercept = 10.2*A, color = 'black', linetype = 'dashed') + ## 10.2 feet is flood stage at Meridian
  geom_hline(yintercept = 10.7*A, color = 'red', linetype = 'dashed') + ## 10.7 feet is moderate flood stage at Meridian
  geom_hline(yintercept = 11.2*A, color = 'black', linetype = 'dashed') + ## 11.2 feet is major flood stage at Meridian
  scale_x_datetime(date_breaks = "2 years", date_minor_breaks = '1 year', date_labels = "%Y", 
                   limits = c(df$datetime[1], last(df$datetime)), expand = c(0.01, 0.0)) + 
  scale_y_continuous(breaks = seq(2,14,1),
                     labels = seq(2,14,1),
                     limits = c(2,14), expand = c(0,0)) +
  xlab("Year") +
  ylab("High Tide (feet)") +
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
  # ggtitle("Meridian Landing High Tide Data (downloaded 07/08/2021)") + 
  # annotate(geom="text", y = 3, x = as.POSIXct('2016-10-07 12:00', format = "%Y-%m-%d %H:%M"), 
  #          label = "Hurricane Matthew", col = 'black') + 
  # labs(caption = "subtracted 1.1 ft following Hurricane Matthew peak on 10/07/2016") + 
  annotate(geom="text", y = 11.2*A, x = df$datetime[3700], label = "Major", col = 'red', vjust = 0.2) +
  annotate(geom="text", y = 10.7*A, x = df$datetime[3700], label = "Moderate", col = 'red', vjust = 0.2) +
  annotate(geom="text", y = 10.2*A, x = df$datetime[3700], label = "Flood", col = 'red', vjust = 0.2) +
  annotate(geom="text", y = 9.7*A, x = df$datetime[3700], label = "Nusiance", col = 'red', vjust = 0.2) + 
  annotate(geom="text", y = 11.24*A, x = df$datetime[1800], label = "TS Tammy", col = 'red', hjust = -0.1, vjust = 0.1) + 
  annotate(geom="text", y = 13.05*A, x = df$datetime[6185], label = "Irma", col = 'red', hjust = -0.1, vjust = 0.1) + 
  annotate(geom="text", y = 11.27*A, x = df$datetime[5846], label = "Matthew", col = 'red', hjust = -0.1, vjust = 0.1)
fig

# tiff(file.path(datadir, 'figures/legacy-vulnerability-fig.tiff'),res=300, unit='in', 
#      width = 13.33, height = 7.5, compression = 'lzw')
# fig
# dev.off()

tiff(file.path(datadir, 'figures/legacy-vulnerability-fig.tiff'),res=300, unit='in',
     width = 6, height = 4, compression = 'lzw')
fig
dev.off()

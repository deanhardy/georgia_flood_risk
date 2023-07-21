rm(list=ls())

library(tidyverse) ## load tidyverse package
library(lubridate)
library(dataRetrieval) ## https://cran.r-project.org/web/packages/dataRetrieval/vignettes/dataRetrieval.html
##library(timeSeries)

## set data directory
datadir <- "/Users/dhardy/Dropbox/r_data/georgia_flood_risk"

####################################################
## adding automated download process for NERR data 
####################################################

# Hudson River, Meridian, GA
siteNo <- "022035975"
pCode <- "00065" ## gage height data
statCode <- "00021" ## tidal high-high values
start.date <- "2007-10-01" ## earliest available date
end.date <- "2023-06-30"

df <- readNWISdv(siteNumbers = siteNo,
                 parameterCd = pCode,
                 startDate = start.date,
                 endDate = end.date,
                 statCd = statCode) %>%
  rename(height = X_00065_00021,
         quality = X_00065_00021_cd,
         datetime = Date) %>%
  mutate(type = 'high')

df$datetime <- as.POSIXct(df$datetime) ## convert datetime column to correct format

## read in high/low tidal data from gauge station 
## https://waterdata.usgs.gov/ga/nwis/nwismap/?site_no=022035975&agency_cd=USGS
## gage datum is 1.10 feet above NAVD88
# df <- read.delim(file.path(datadir, "data/original/20220301_height_allobserved_ml.txt"), header=TRUE, sep = '\t', dec = '.',
#                  skip = 30) %>%
#   slice(2:n()) %>%
#   rename(high = X35070_00065_00021, low = X35071_00065_00024,
#          hquality = X35070_00065_00021_cd, lquality = X35071_00065_00024_cd) %>%
#   # select(-hquality, -lquality) %>%
#   unite('high', high, hquality) %>%
#   unite('low', low, lquality) %>%
#   gather(key = type, value = height, 4:5) %>%
#   separate(height, c('height', 'quality'), sep = '_') %>%
#   mutate(height = as.numeric(height))


## station datum NAVD88 = +1.1 ft... no longer???
## VDATUM says Hudson Creek entrance NAVD88 0 ft is 4.177 ft in MLLW, so 5.277 ft for station
##  convert datum to mllw elevation datum 
df <- mutate(df, height = height + 4.18) %>% 
  mutate(year = year(datetime))

## playing around with revised data by revising post Irma
## appears corrected sometime between July 2021 and March 2022
# df2 <- df %>%
#   mutate(height = ifelse(datetime > '2016-10-07', height - 1.1, height))

## select out highest high tides and low tides
# hi10 <- top_n(df, 10, height)
# lo10 <- 
#   df %>%
#   filter(type == "low") %>%
#   top_n(10, height)

# lims <- as.POSIXct(strptime(c("2011-01-01 03:00","2011-01-01 16:00"), format = "%Y-%m-%d %H:%M"))    
fnt <- 18 ## figure text size
ant.fnt <- 6 ## annotation text size
A <- 1## convert to meters/feet

## working on counting number floods since 2015 and calculating percentage
fld <- df %>% filter(height >= 10.2) %>%
  mutate(group = ifelse(datetime > '2015-10-01', 'recent', 'older'))

## calc percent floods
pct <- fld %>% group_by(group) %>% summarise(n = n()) %>%
  mutate(percent = n/sum(n))


fig <- ggplot(filter(df, type == 'high'), aes(datetime, height*A)) +
  geom_point(pch=19, size = 0.1, color = 'grey') + 
  geom_point(mapping = aes(datetime, height*A, col = 'red'),
             data = filter(df, type == 'high' & height >= 11.2),
             size = 4, pch = 17, inherit.aes = TRUE) +
  geom_point(mapping = aes(datetime, height*A, col = 'red'),
             data = filter(df, type == 'high' & height >= 10.2),
             size = 0.9, pch = 10, inherit.aes = TRUE) +
  geom_smooth(method = lm, col = 'black', size = 0.5) + 
  # geom_vline(xintercept = as.POSIXct('2016-10-07 12:00', format = "%Y-%m-%d %H:%M"), linetype = 'dashed') + 
  geom_hline(yintercept = 9.7*A, color = 'black', linetype = 'dashed') + ## 9.7 feet is action stage at Meridian
  geom_hline(yintercept = 10.2*A, color = 'black', linetype = 'dashed', lwd = 0.5) + ## 10.2 feet is flood stage at Meridian
  # geom_hline(yintercept = 10.7*A, color = 'black', linetype = 'dashed') + ## 10.7 feet is moderate flood stage at Meridian
  geom_hline(yintercept = 11.2*A, color = 'black', linetype = 'dashed', lwd = 0.5) + ## 11.2 feet is major flood stage at Meridian
  scale_x_datetime(date_breaks = "1 year", date_minor_breaks = '1 year', date_labels = "%Y", 
                   limits = c(df$datetime[1], last(df$datetime)), expand = c(0.01, 0.0)) + 
  scale_y_continuous(breaks = seq(4,14,1),
                     labels = seq(4,14,1),
                     limits = c(4,14),
                     sec.axis = dup_axis(name = '', labels = NULL)) +
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
  # ggtitle("Meridian Landing High Tide Trend") +
  # annotate(geom="text", y = 3, x = as.POSIXct('2016-10-07 12:00', format = "%Y-%m-%d %H:%M"), 
  #          label = "Hurricane Matthew", col = 'black') + 
  # labs(caption = "subtracted 1.1 ft following Hurricane Matthew peak on 10/07/2016") + 
  annotate(geom="text", y = 11.2*A, x = df$datetime[2200], label = "Major Flood", col = 'black', vjust = -0.5, size = ant.fnt) +
  # annotate(geom="text", y = 10.7*A, x = df$datetime[3700], label = "Moderate", col = 'red', vjust = 0.2) +
  annotate(geom="text", y = 10.2*A, x = df$datetime[2200], label = "Flood Stage", col = 'black', vjust = -0.5, size = ant.fnt) +
  annotate(geom="text", y = 9.7*A, x = df$datetime[2200], label = "Action Stage", col = 'black', vjust = -0.5, size = ant.fnt) +
  # annotate(geom="text", y = 11.78*A, x = as.POSIXct('2002-12-03 19:00:00'), label = "Nor'easter + Spring Tide", col = 'red', 
  #          hjust = -0.04, vjust = -0.1, size = ant.fnt) + 
  # annotate(geom="text", y = 11.24*A, x = as.POSIXct('2005-10-04 20:00:00'), label = "TS Tammy", col = 'red', 
  #          hjust = -0.1, vjust = -0.1, size = ant.fnt) + 
  annotate(geom="text", y = 13.05*A, x = as.POSIXct('2017-09-10 20:00:00'), label = "Irma", col = 'red', 
           hjust = -0.1, vjust = -0.1, size = ant.fnt) + 
  annotate(geom="text", y = 11.36*A, x = as.POSIXct('2022-11-09 19:00:00'), label = "Nicole", col = 'red', 
           hjust = 1.2, vjust = -0.1, size = ant.fnt) + 
  annotate(geom="text", y = 11.27*A, x = as.POSIXct('2016-10-06 20:00:00'), label = "Matthew", col = 'red', 
           hjust = -0.1, vjust = -0.1, size = ant.fnt)
fig

# tiff(file.path(datadir, 'figures/legacy-vulnerability-fig.tiff'),res=300, unit='in', 
#      width = 13.33, height = 7.5, compression = 'lzw')
# fig
# dev.off()

tiff(file.path(datadir, 'figures/meridian_landing_hightide_trend.tiff'), res=300, unit='cm',
     width = 19, height = 13.3, compression = 'lzw')
fig
dev.off()

png(file.path(datadir, 'figures/meridian_landing_hightide_trend.png'), res = 150, unit = 'in',
    width = 13.33, height = 7)
fig
dev.off()

jpeg(file.path(datadir, 'figures/meridian_landing_hightide_trend.jpg'), res=300, unit='in',
     width = 13, height = 7)
fig
dev.off()

##########################
## averages by unit time
##########################
df2 <- df %>%
  mutate(prd = floor_date(datetime, "month")) %>%
  group_by(prd) %>%
  summarize(avg = mean(height))

ggplot(df2, aes(prd, avg)) + 
  geom_point() + 
  geom_smooth(method = lm, se = F)


#####################################
## count number of events above MSL+1.7m
#####################################
df3 <- df %>%
  filter(height > (3.784 + 5.577)) %>% ## MSL for station plus 1.7 meters
  mutate(x = floor_date(datetime, "year")) %>%
  mutate(x = year(x)) %>%
  group_by(x) %>%
  summarise(y = n())

## https://stackoverflow.com/questions/37329074/geom-smooth-and-exponential-fits
linear.model <-lm(y ~ x, df3)
log.model <-lm(log(y) ~ x, df3)
# exp.model <-lm(y ~ exp(x), df3)

log.model.df <- data.frame(x = df3$x,
                           y = exp(fitted(log.model)))

ext <- ggplot(df3, aes(x, y, label = y)) + 
  geom_line(color = 'blue') +
  geom_text(size = 8) + 
  # geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ exp(x)), se=FALSE, linetype = 1) +
  geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 1, show.legend = F) + 
  # guides(color = guide_legend("Model Type")) + 
  scale_x_continuous(breaks = seq(2000, 2022, 5), minor_breaks = seq(2000,2022,1)) + 
  theme_minimal(base_size = 18) + 
  labs(x = 'Year', y = '# of Events', title = "Meridian Landing\n# of Events >= 1.7m (MSL)")
ext

png(file.path(datadir, 'figures/meridian_landing_extreme_events.png'), res = 150, unit = 'in',
    width = 13.33, height = 7)
ext
dev.off()


#####################################
## count number of events above flood stage
#####################################
df4 <- df %>%
  filter(height > (9.7)) %>%
  mutate(x = floor_date(datetime, "year")) %>%
  mutate(x = year(x)) %>%
  group_by(x) %>%
  summarise(y = n())

## https://stackoverflow.com/questions/37329074/geom-smooth-and-exponential-fits
linear.model <-lm(y ~ x, df4)
log.model <-lm(log(y) ~ x, df4)
# exp.model <-lm(y ~ exp(x), df4)

log.model.df <- data.frame(x = df4$x,
                           y = exp(fitted(log.model)))

ext <- ggplot(df4, aes(x, y, label = y)) + 
  geom_line(color = 'blue') +
  geom_text(size = 8) + 
  # geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ exp(x)), se=FALSE, linetype = 1) +
  geom_line(data = log.model.df, aes(x, y, color = "Log Model"), size = 1, linetype = 1, show.legend = F) + 
  # guides(color = guide_legend("Model Type")) + 
  scale_x_continuous(breaks = seq(2007, 2023, 2), minor_breaks = seq(2007,2023,1), limits = c(2007, 2023)) + 
  theme_bw(base_size = 18) + 
  labs(x = 'Year', y = 'Events (#)', title = "Events >= 9.7 ft (Action Stage)")
ext

png(file.path(datadir, 'figures/meridian_landing_action_stage.png'), res = 150, unit = 'in',
    width = 13.33, height = 7)
ext
dev.off()

# set working directory on Windows
setwd("C:/Users/dhardy/Dropbox/sesync/outreach/DarienNews/hurricanes/R")

library(tidyverse) ## load tidyverse package
##library(timeSeries)
library(gridExtra)

## read in tidal data 
df <- read.csv("data/stormtides.csv", header=TRUE)
df$datetime <- as.POSIXct(df$datetime) ## convert datetime column to correct format

## NAVD88 adjustment of Meridian Landing USGS gage data to MLLW datum is to add 4.178 ft based on 
## VDATUM at Hudson Creek entrace
df <- mutate(df, height = ifelse(site == "Meridian Landing", height + 4.178, height))
head(filter(df, site == "Meridian Landing"))     

temp <- df %>% filter(site == "Meridian Landing") %>% filter(storm =="Matthew")
max(temp$height)


###########################
###########################
## create prediction for Meridian Landing site based on NOAA's Hudson Creek entrance subordinate station (8675245)
## High and Low tides are *1.05 of Fort Pulaski (reference station) and high: 37 min later; low: 31 min later (34 as mean)
## do iteratively for each storm

## run for Irma
## first filter out observed
mlob <- 
  df %>%
  filter(site == "Meridian Landing") %>%
  filter(storm == "Irma") %>%
  filter(type == "observed")

## now filter our predicted for FP and convert to ML numbers
mlpr <- 
  df %>% 
  filter(site =="Fort Pulaski") %>% 
  filter(type == "predicted") %>%
  filter(storm == "Irma") %>%
  mutate(height = height*1.05, site = "Meridian Landing", datetime = datetime + 34*60)

## create function to interpolate values
f <- splinefun(mlpr$datetime, mlpr$height)

##interpolate values from time 1 to 2 by defined interval
mlpr2 <- f(seq(from = mlpr$datetime[1]+120, to = mlpr$datetime[1199]-60, by = 15*60))
mlpr3 <- mutate(mlob, type = "predicted", height = mlpr2)

surge <- mlob$height - mlpr3$height  ## calculate surge as observed minus predicted
mlsr <- mutate(mlob, type = "surge", height = surge)

df1 <- rbind(df, mlpr3) ## join predicted to main dataframe
df2 <- rbind(df1, mlsr) ## join surge to main dataframe

## run for Matthew
## first filter out observed
mlob <- 
  df %>%
  filter(site == "Meridian Landing") %>%
  filter(storm == "Matthew") %>%
  filter(type == "observed")

mlpr <- 
  df %>% 
  filter(site =="Fort Pulaski") %>% 
  filter(type == "predicted") %>%
  filter(storm == "Matthew") %>%
  mutate(height = height*1.05, site = "Meridian Landing", datetime = datetime + 34*60)

## create function to interpolate values
f <- splinefun(mlpr$datetime, mlpr$height)

##interpolate values from time 1 to 2 by defined interval
mlpr2 <- f(seq(from = mlpr$datetime[1]+120, to = mlpr$datetime[1199]-60, by = 15*60))
mlpr3 <- mutate(mlob, type = "predicted", height = mlpr2)

surge <- mlob$height - mlpr3$height  ## calculate surge as observed minus predicted
mlsr <- mutate(mlob, type = "surge", height = surge)

df3 <- rbind(df2, mlpr3) ## join predicted to main dataframe
df4 <- rbind(df3, mlsr) ## join surge to main dataframe




################################
################################
## Matthew plot - compare tidal heights by gauge

matthew <- ggplot(filter(df4, storm == "Matthew", type != "surge"), aes(datetime, height)) +
  geom_line(aes(color = site, linetype = type)) +
  xlab("Date") + 
  ylab("Tidal Height (feet)") +
  scale_y_continuous(limits = c(-1,12.5), breaks = c(0,3,6,9,12), minor_breaks = c(-1,1,2,4,5,7,8,10,11)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d/%y") +
  scale_color_manual(values = c("#7fc97f", "#fdc086", "#386cb0"),
                     breaks = c("Fernandina Beach", "Meridian Landing", "Fort Pulaski")) +
  ggtitle("Hurricane Matthew - 2016") +
  labs(color = "Location", linetype = "Water Level", caption = "(All data for Fort Pulaski & Fernandina Beach from NOAA; Observed data for Meridian Landing from\nUSGS/DNR/SINERR and predicted data estimated based on Fort Pulaski)") +
  theme(plot.caption = element_text(size = 8, color = "white"), legend.position = c(0.89, 0.91)) +
  guides(color = FALSE, linetype =  guide_legend(keyheight = 0.2, default.unit = "inch"))

## Irma plot - compare gauges
irma <- ggplot(filter(df4, storm == "Irma", type != "surge"), aes(datetime, height)) +
  geom_line(aes(color = site, linetype = type)) +
  xlab("Date") + 
  ylab("Tidal Height (feet)") + 
  scale_y_continuous(limits = c(-1,12.5), breaks = c(0,3,6,9,12), minor_breaks = c(-1,1,2,4,5,7,8,10,11)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d/%y") +
  scale_color_manual(name = c("Location"), values = c("#7fc97f", "#fdc086", "#386cb0"),
                     breaks = c("Fernandina", "Meridian Landing", "Fort Pulaski")) +
  ggtitle("Hurricane Irma - 2017") + 
  labs(color = "Location", linetype = "Water Level", caption = "(All data for Fort Pulaski & Fernandina from NOAA; Observed data for Meridian Landing from USGS/DNR/SINERR gauge and\nadjusted to tidal height; predicted data estimated from NOAA's Hudson Creek Entrance subordinate station)") +
  theme(plot.caption = element_text(size = 8), legend.position = c(0.86, 0.89), 
        axis.title.y = element_text(color = "white")) +
  guides(color = guide_legend(keyheight = 0.2, default.unit = "inch"),
         linetype = FALSE)

## export as pngs and tiffs
tiff("figures/stormtides_compare_color.tif", width = 12, height = 6, units = 'in', res = 300)
grid.arrange(matthew, irma, ncol = 2)
dev.off()

png("figures/stormtides_compare_color.png", width = 12, height = 6, units = 'in', res = 150)
grid.arrange(matthew, irma, ncol = 2)
dev.off()

tiff("figures/stormtides_matthew.tif", width=6, height=5, unit = 'in', res = 300)
matthew
dev.off()

tiff("figures/stormtides_irma.tif", width=6, height=5, unit = 'in', res = 300)
irma
dev.off()




################################
################################
## Matthew plot - compare storm surge heights by gauge

matthew.surge <- ggplot(filter(df4, storm == "Matthew", type == "surge"), aes(datetime, height)) +
  geom_line(aes(color = site)) +
  xlab("Date") + 
  ylab("Storm Surge (feet)") +
  scale_y_continuous(limits = c(-3,9), breaks = c(-3,0,3,6,9), minor_breaks = c(-2,-1,1,2,4,5,7,8)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d/%y") +
  scale_color_manual(values = c("#7fc97f", "#fdc086", "#386cb0"),
                     breaks = c("Fernandina Beach", "Meridian Landing", "Fort Pulaski")) +
  ggtitle("Hurricane Matthew - 2016") +
  labs(caption = "(All data for Fort Pulaski & Fernandina Beach from NOAA; Observed data for Meridian Landing from\nUSGS/DNR/SINERR and predicted data estimated based on Fort Pulaski)") +
  theme(plot.caption = element_text(size = 8, color = "white"), legend.position = c(0.89, 0.91)) +
  guides(color = FALSE, linetype =  guide_legend(keyheight = 0.2, default.unit = "inch"))

## Irma plot - compare surges
irma.surge <- ggplot(filter(df4, storm == "Irma", type == "surge"), aes(datetime, height)) +
  geom_line(aes(color = site)) +
  xlab("Date") + 
  ylab("Tidal Height (feet)") + 
  scale_y_continuous(limits = c(-3,9), breaks = c(-3,0,3,6,9), minor_breaks = c(-2,-1,1,2,4,5,7,8)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d/%y") +
  scale_color_manual(name = c("Location"), values = c("#7fc97f", "#fdc086", "#386cb0"),
                     breaks = c("Fernandina", "Meridian Landing", "Fort Pulaski")) +
  ggtitle("Hurricane Irma - 2017") + 
  labs(color = "Location", caption = "(All data for Fort Pulaski & Fernandina from NOAA; Observed data for Meridian Landing from USGS/DNR/SINERR gauge and\nadjusted to tidal height; predicted data estimated from NOAA's Hudson Creek Entrance subordinate station)") +
  theme(plot.caption = element_text(size = 8), legend.position = c(0.86, 0.89), 
        axis.title.y = element_text(color = "white")) +
  guides(color = guide_legend(keyheight = 0.2, default.unit = "inch"),
         linetype = FALSE)

## export as pngs and tiffs
tiff("figures/stormsurge_compare_color.tif", width = 12, height = 6, units = 'in', res = 300)
grid.arrange(matthew.surge, irma.surge, ncol = 2)
dev.off()

png("figures/stormsurge_compare_color.png", width = 12, height = 6, units = 'in', res = 150)
grid.arrange(matthew.surge, irma.surge, ncol = 2)
dev.off()




#################################
## Matthew - compare storm surges by gauge
tiff("figures/surge_matthew.tif", width=6, height=5, unit = 'in', res = 150)
ggplot(filter(df4, storm == "Matthew", type == "surge"), aes(datetime, height)) +
  geom_line(aes(color = site)) +
  xlab("Date") + 
  ylab("Surge Height (feet)") +
  scale_y_continuous(limits = c(-3,8), breaks = c(-3,0,2,4,6,8)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
  ggtitle("Hurricane Matthew - 2016")
dev.off()

## Irma - compare storm surges
tiff("figures/surge_irma.tif", width=6, height=5, unit = 'in', res = 150)
ggplot(filter(df4, storm == "Irma", type == "surge"), aes(datetime, height)) +
  geom_line(aes(color = site)) +
  xlab("Date") + 
  ylab("Surge Height (feet)") + 
  scale_y_continuous(limits = c(-3,8), breaks = c(-3,0,2,4,6,8)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
  ggtitle("Hurricane Irma - 2017")
dev.off()




################################PNG
################################PNG
## Matthew plot - compare tidal heights by gauge
png("figures/stormtides_matthew.png", width=6, height=5, unit = 'in', res = 150)
ggplot(filter(df4, storm == "Matthew", type != "surge"), aes(datetime, height)) +
  geom_line(aes(color = site, linetype = type)) +
  xlab("Date") + 
  ylab("Tidal Height (feet)") +
  scale_y_continuous(limits = c(-1,12.5), breaks = c(0,3,6,9,12), minor_breaks = c(-1,1,2,4,5,7,8,10,11)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
  ggtitle("Hurricane Matthew - 2016")
dev.off()

## Irma plot - compare gauges
png("figures/stormtides_irma.png", width=6, height=5, unit = 'in', res = 150)
ggplot(filter(df4, storm == "Irma", type != "surge"), aes(datetime, height)) +
  geom_line(aes(color = site, linetype = type)) +
  xlab("Date") + 
  ylab("Tidal Height (feet)") + 
  scale_y_continuous(limits = c(-1,12.5), breaks = c(0,3,6,9,12), minor_breaks = c(-1,1,2,4,5,7,8,10,11)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
  ggtitle("Hurricane Irma - 2017")
dev.off()

## Irma plot - Meridian
png("figures/stormtides_irma_ml.png", width=6, height=5, unit = 'in', res = 150)
ggplot(filter(df4, site == "Meridian Landing", storm == "Irma", type != "surge"), aes(datetime, height)) +
  geom_line(aes(color = site, linetype = type)) +
  xlab("Date") + 
  ylab("Tidal Height (feet)") + 
  scale_y_continuous(limits = c(-1,12.5), breaks = c(0,3,6,9,12), minor_breaks = c(-1,1,2,4,5,7,8,10,11)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
  ggtitle("Hurricane Irma - 2017")
dev.off()


#################################
## Matthew - compare storm surges by gauge
png("figures/surge_matthew.png", width=6, height=5, unit = 'in', res = 150)
ggplot(filter(df4, storm == "Matthew", type == "surge"), aes(datetime, height)) +
  geom_line(aes(color = site)) +
  xlab("Date") + 
  ylab("Surge Height (feet)") +
  scale_y_continuous(limits = c(-3,8), breaks = c(-3,0,2,4,6,8)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
  ggtitle("Hurricane Matthew - 2016")
dev.off()

## Irma - compare storm surges
png("figures/surge_irma.png", width=6, height=5, unit = 'in', res = 150)
ggplot(filter(df4, storm == "Irma", type == "surge"), aes(datetime, height)) +
  geom_line(aes(color = site)) +
  xlab("Date") + 
  ylab("Surge Height (feet)") + 
  scale_y_continuous(limits = c(-3,8), breaks = c(-3,0,2,4,6,8)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%d") +
  ggtitle("Hurricane Irma - 2017")
dev.off()




#######################
#######################

ml.irma <- 
  df4 %>%
  filter(site == "Meridian Landing") %>%
  filter(storm == "Irma")


filter(ml.irma, type == "predicted", datetime == "2017-09-11 13:30") 
#%>% filter(height == max(height))


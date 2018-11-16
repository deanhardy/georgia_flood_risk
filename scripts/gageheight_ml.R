# set working directory on Windows
setwd("C:/Users/dhardy/Dropbox/sesync/outreach/DarienNews/hurricanes/R")

library(tidyverse) ## load tidyverse package
tidyverse_update() ## check for updates 
##library(timeSeries)

## read in tidal data 
df <- read.csv("data/height_allobserved_ml.csv", header=TRUE)
df$datetime <- as.POSIXct(df$datetime) ## convert datetime column to correct format

df <- mutate(df, height = height +4.178) ## convert to mllw elevation datum

## select out highest high tides and low tides
hi10 <- top_n(df, 10, height)
lo10 <- 
  df %>%
  filter(type == "low") %>%
  top_n(10, height)

png("figures/meridian_tides_alltime.png", res=150, unit='in', width = 7, height = 5)
ggplot(df, aes(datetime, height, color = type)) +
  geom_point(pch=19) + 
  scale_x_datetime(date_breaks = "1 years", date_labels = "%y") + 
  scale_y_continuous(minor_breaks = c(-4,-3,-2,-1,1,2,3,4,6,7,8,9,11,12)) +
  xlab("Year") +
  ylab("Tidal Height (feet)") +
  ggtitle("Meridian Landing Gage")
dev.off()

rm(list=ls())

library(tidyverse) ## load tidyverse package
##library(timeSeries)

## set data directory
datadir <- ("/Users/dhardy/Dropbox/r_data/georgia_hurricanes")

## read in high/low tidal data from gauge station 
# df <- read.csv(file.path(datadir, "datsa/height_allobserved_ml.csv"), header=TRUE) ##meridian landing
df <- read.delim(file.path(datadir, "data/original/20201104_height_allobserved_ml.txt"), header=TRUE, sep = '\t', dec = '.',
                 skip = 29) %>%
  slice(2:n()) %>%
  rename(high = X35070_00065_00021, low = X35071_00065_00024,
         quality = X35070_00065_00021_cd, quality2 = X35071_00065_00024_cd) %>%
  select(-quality, -quality2) %>%
  gather(key = type, value = height, 4:5) %>%
  mutate(height = as.numeric(height))

df$datetime <- as.POSIXct(df$datetime) ## convert datetime column to correct format

df <- mutate(df, height = height + 4.178) ## convert to mllw elevation datum

## select out highest high tides and low tides
hi10 <- top_n(df, 10, height)
lo10 <- 
  df %>%
  filter(type == "low") %>%
  top_n(10, height)

# lims <- as.POSIXct(strptime(c("2011-01-01 03:00","2011-01-01 16:00"), format = "%Y-%m-%d %H:%M"))    
fnt <- 16

fig <- ggplot(filter(df, type == 'high'), aes(datetime, height*0.3048)) +
  geom_point(pch=19, size = 1, color = 'grey') + 
  geom_point(mapping = aes(datetime, height*0.3048, col = 'red'),
             data = filter(df, type == 'high' & height >= 9.5),
             size = 5, pch = 17, inherit.aes = TRUE) +
  geom_smooth(method = lm, col = 'black', size = 0.5) + 
  geom_hline(yintercept = 9.5*0.3048, color = 'red', linetype = 'dashed') + ## 9.5 feet is flood stage at Fort Pulaski
  scale_x_datetime(date_breaks = "2 years", date_labels = "%Y", 
                   limits = c(df$datetime[1], df$datetime[14668]), expand = c(0.01, 0.0)) + 
  scale_y_continuous(breaks = c(0,1,2,3,4),
                     labels = c(0,1,2,3,4),
                     limits = c(0,4), expand = c(0,0)) +
  xlab("Year") +
  ylab("High Tide (meters)") +
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
  annotate(geom="text", y = 9.5*0.3048, x = df$datetime[3700], label = "Fort Pulaski\nFlood Stage", col = 'red')
fig

tiff(file.path(datadir, 'figures/meridian_tides_alltime.tiff'), res=300, unit='in', width = 13.33, height = 7.5, 
               compression = 'lzw')
fig
dev.off()

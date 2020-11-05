rm(list=ls())

library(tidyverse) ## load tidyverse package
##library(timeSeries)

## set data directory
datadir <- ("/Users/dhardy/Dropbox/r_data/georgia_hurricanes")

## read in high/low tidal data from gauge station 
# df <- read.csv(file.path(datadir, "datsa/height_allobserved_ml.csv"), header=TRUE)
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

fig <- ggplot(filter(df, type == 'high'), aes(datetime, height)) +
  geom_point(pch=19, size = 0.1, color = 'grey') + 
  geom_point(aes(datetime, height, col = 'red'), filter(df, type == 'high' & height >= 9.5), size = 1, pch = 17) + 
  geom_smooth(method = lm, col = 'black', size = 0.5) + 
  geom_hline(yintercept = 9.5, color = 'red', linetype = 'dashed') + ## 9.5 feet is flood stage at Fort Pulaski
  scale_x_datetime(date_breaks = "2 years", date_labels = "%y", date_minor_breaks = '1 year',
                   limits = c(df$datetime[1], df$datetime[14668]), expand = c(0,0)) + 
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12)) +
  xlab("Year") +
  ylab("High Tide (feet)") +
  #ggtitle("Meridian Landing Gage") + 
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        title = element_text(size = 10),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = 'white', color = 'black'),
        legend.position = "none") + 
  annotate(geom="text", y = 9.5, x = df$datetime[3700], label = "Fort Pulaski\nFlood Stage", col = 'red')
fig

tiff(file.path(datadir, 'figures/meridian_tides_alltime.tiff'), res=300, unit='in', width = 6, height = 4, 
               compression = 'lzw')
fig
dev.off()

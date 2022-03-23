# set working directory on Windows
setwd("//Users/dhardy/Dropbox/sesync/outreach/DarienNews/hurricanes/R")

## Tidy spatial data in R
## http://strimas.com/r/tidy-sf/
## https://github.com/SESYNC-ci/geospatial-packages-in-R-lesson

## The motiviation for this is both Hurricane Matthew and Irma as well as Bossak et al's
## (2014) analysis of Georgia landfalling hurricanes in SE Geographer; My analysis is currently
## missing those in their database:
# Sep 1-13, 1878
# Aug 21-27, 1885
# Aug 30-Sep 1, 1898
# But I have and they don't:
# AL061874; Sep 25-Oct 1, 1874
# AL041894; Sep 18-Oct 1, 1894
# AL021898


library(tidyverse) ## load tidyverse package
library(HURDAT) ## load hurdat package
library(sf)
library(gridExtra)
library(maps)
#library(ggmap)
library(RColorBrewer)
library(stringr)
library(gtable)
library(grid)

## import Atlantic storm data from HURDAT2
AL <- get_hurdat(basin = "AL")

## rearrange Key as YYYYBBSS (year, basin, storm number)
AL <- 
  AL %>%
  mutate(Key2 = paste(substr(Key, 5, 8), substr(Key, 1, 4), sep="")) %>%
  mutate(Name = ifelse(Name == "UNNAMED", "Unnamed", Name))

## convert hurdat data to spatial object
ALpt <- 
  AL %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, agr = "constant")

## import map data
world <- map_data("world")
usa <- map_data("usa")
states <- map_data("state")
counties <- map_data("county")
ccbuf <- st_read("data/ccbuf20nm.shp") ## import coastal county buffer

## bbox for ga coastal counties
c <- st_bbox(ccbuf)

## filter coastal counties in GA
coast <- c("chatham", "liberty", "bryan", "mcintosh", "glynn", "camden")
ga.county <- subset(counties, region == "georgia")
ga.coast <- 
  ga.county %>%
  filter(subregion %in% coast)

## convert coastal counties to spatial objects, then to polygons
ga.cc <- st_as_sf(ga.coast, coords = c("long", "lat"), crs = 4326, agr = "constant")

## filter to storms making landfall in GA coastal counties as hurricanes
## first as the points associated within counties
ga.storms <- st_intersection(ccbuf, ALpt)
ga <- 
  ga.storms %>%
  filter(Status == "HU") #%>%
  #filter(Record == "L")

## next as full paths
ga.hu <- 
  AL %>%
  filter(Key %in% unique(ga$Key))
ga.hu <- arrange(ga.hu, DateTime) ## order by storm key

## add unique group id by Key2
ga.hu <- 
  ga.hu %>%
  mutate(ID = group_indices_(ga.hu, .dots = ("Key2")))

colourCount = length(unique(ga.hu$Key2))
getPalette = colorRampPalette(brewer.pal(8, "Accent"))

## extract first observations for each storm
ga.fst <-
  ga.hu %>%
  group_by(Key2) %>%
  arrange(DateTime) %>%
  filter(row_number()==1)
  
## extract last observations for each storm
ga.lst <-
  ga.hu %>%
  group_by(Key2) %>%
  arrange(DateTime) %>%
  filter(row_number()==n())

## create string column for date range for each storm
Dates <- cbind(as.character(format(ga.fst$DateTime, "%Y, %b %d")), 
               as.character(format(ga.lst$DateTime, "%b %d")))

Dates <- str_c(Dates[,1], Dates[,2], sep = " - ")
  
## extract first and last observations for each storm
ga.fl <-
  ga.hu %>%
  group_by(Key2) %>%
  arrange(DateTime) %>%
  filter(row_number()==1 | row_number()==n())

## denote with an * those storms that made landfall in GA
names.lf <- ifelse(ga.fl$Key == "AL031854" | ga.fl$Key == "AL051881" | ga.fl$Key == "AL061893" | 
                     ga.fl$Key == "AL071898" | ga.fl$Key == "AL091947" |
                     ga.fl$Key == "AL091979", str_c(ga.fl$Name, "*", sep = ""), ga.fl$Name)

## extract paths of just GA landfalling storms
ga.landfall <- 
  ga.hu %>%
  filter(Key == "AL031854" | Key == "AL051881" | Key == "AL061893" | Key == "AL071898" | 
           Key == "AL091947" | Key == "AL091979")
  
ga.fl <-
  ga.fl %>%
  ungroup() %>%
  mutate(Name = names.lf) %>%
  group_by(Key2)

## make legend table with ID, name, and date range
lgd <-  
  ga.fl %>%
  ungroup() %>%
  select(ID, Name) %>%
  mutate(Dates = rep(Dates, each = 2)) %>%
  group_by(ID) %>%
  filter(row_number()==1)
#lgd <- tableGrob(lgd, rows = NULL, theme = mytheme)
  
## set tableGrob text size, see more at 
# browseVignettes("gridExtra")
mytheme <- gridExtra::ttheme_minimal(
  core = list(fg_params=list(cex =0.5, hjust = 0, x=0)),
  colhead = list(fg_params=list(cex = 0.5, hjust = 0, x = 0)),
  rowhead = list(fg_params=list(cex = 0.5)))

## convert georgia hurricanes to spatial points data frame and export
## convert storms to spatial objects, then group into lines by storm key
ga.st <- 
  ga.hu %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, agr = "constant") %>%
  st_cast("MULTIPOINT")

st.ln <- ga.st %>%
  arrange(DateTime) %>%
  group_by(Key) %>%
  summarise() %>%
  st_cast("LINESTRING")
#plot(st_geometry(st.ln))

st_write(ga.st, "data/ga_hurricanes.shp", driver = "ESRI Shapefile")

b <- st_bbox(ga.st)

## plot based on tips from
## http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
hurricanes <- ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "grey") +
  geom_polygon(data = states, aes(long, lat, group = group), fill = "white", color = "darkgrey") +
  coord_map(projection = "mollweide") +
  geom_path(data = ga.hu, aes(Lon, Lat, group = Key2, colour = Key2), lwd = 0.3) +
  geom_point(data = ga.fst, aes(Lon, Lat), pch = 21, fill = "white", color = "black", size = 5) +
  geom_text(data = ga.fst, aes(Lon, Lat, label = ID), size = 3, color = "black") +
  coord_fixed(xlim = c(b[1],b[3]), ylim = c(b[2],b[4]), ratio = 1.3) +
  #coord_fixed(xlim = c(-78, -83), ylim = c(28, 33), ratio = 1.3) +
  #coord_fixed(xlim = c(c[1],c[3]+0.3), ylim = c(c[2],c[4]), ratio = 1.3) +
  scale_x_continuous(breaks = seq(-90, 0, 10)) +
  scale_y_continuous(breaks = seq(10, 60, 10)) +
  #scale_color_manual(values = rep("black", 18), labels = "ID") +
  xlab("Longitude") + 
  ylab("Latitude") +
  labs(caption = "(The six storm names marked with an asterisk(*) mean the storm's eye passed directly over\nGeorgia's coastline; Storm tracks and dates based on HURDAT2 data released April 2017; Brian\nBossak and others' research published in the Southeastern Geographer influenced map design.)") + 
  annotation_custom(tableGrob(lgd, rows = NULL, theme = mytheme), 
                   xmin = 0, xmax = 17, ymin = 4, ymax = 55) +
  #annotation_custom(gtable_add_grob(lgd, grobs = rectGrob(gp = gpar(fill = NA, lwd = 1)), 
  #                                  t = 1, b = nrow(lgd), l = 1, r = ncol(lgd)),
  #                  xmin = 0, xmax = 17, ymin = 4, ymax = 55) +
  #annotation_custom(textGrob("(Based on research by Brian Bossak and others (2014) and HURDAT2 data as of August 2017)"), 
  #                  xmin = -100, xmax = -10, ymin = -10, ymax = 0) +
  theme(plot.caption = element_text(size=8), legend.position = "none", plot.margin = unit(c(0.2,0,0.2,-1.8), "in")) + 
  ggtitle("Coastal Georgia Hurricanes (1851-2016)", subtitle = "with tracks that passed within 20 Nautical Miles")
hurricanes

tiff("figures/ga.hurricanes.tif", units = 'in', res = 300, width = 7.5, height = 5)
hurricanes
dev.off()

png("figures/ga.hurricanes.png", units = 'in', res = 150, width = 7.5, height = 5)
hurricanes
dev.off()


#####################################################################
## plot Matthew 2016 and other storms I'm missing from Bossak et al
#####################################################################
MA <- 
  AL %>%
  filter(Key == "AL021952")

## ones I missed from Bossak et al 2014
AL.m <- 
  AL %>% 
  filter(Key == "AL051878" | Key == "AL021885" |  Key =="AL021898") 
AL.m <- rbind(AL.m, MA)

## ones that are new to those in Bossak et al 2014
AL.n <- 
  AL %>% 
  filter(Key == "AL061874" | Key == "AL041894" | Key == "AL061893")

## plot missing and new storms relatuve to Bossak et al 2014
ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "grey") +
  geom_polygon(data = states, aes(long, lat, group = group), fill = "white", color = "darkgrey") +
  geom_path(data = AL.n, aes(Lon, Lat, group = Key, color = Key)) +
  #geom_point(data = AL.n, aes(Lon, Lat, group = Key, color = Status)) +
  geom_path(data = AL.m, aes(Lon, Lat, group = Key, color = Key)) +
  geom_point(data = MA, aes(Lon, Lat, group = Key, color = Status)) +
  #coord_fixed(xlim = c(b[1],b[3]), ylim = c(b[2],b[4]), ratio = 1.3) +
  coord_fixed(xlim = c(-78, -83), ylim = c(28, 33), ratio = 1.3) +
  #coord_fixed(xlim = c(c[1],c[3]+0.3), ylim = c(c[2],c[4]), ratio = 1.3) +
  scale_x_continuous(breaks = seq(-90, 0, 10)) +
  scale_y_continuous(breaks = seq(10, 60, 10)) +
  guides(fill = FALSE)



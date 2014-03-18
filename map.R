# descriptive maps
library(ggplot2)
library(sp)
library(rgeos)
library(spatial)
library(plyr)
library(binom)
library(xtable)
library(gstat)
library(reshape2)
library(gridExtra)
library(SpatialEpi)
library(maptools)
library(grid)
library(mapproj)

setwd("/home/simon/Documents/MSc_modules/spatial/")
df <-read.csv("/home/simon/Documents/MSc_modules/spatial/Assessment/assessment_data.csv", 
              header = TRUE, quote = "\"")
#df <- read.csv("F:\\Assessment\\assessment_data.csv", header = TRUE, 
#               quote = "\"")
head(df)

# load scale bar functions
source("scale_bars.R")

coast <- readShapePoly("south_coast.shp")
head(coast@data)
coast.points <- fortify(coast, region = "ID_4")
coast@data$id <- coast@data$ID_4
coast.df <- join(coast.points, coast@data, by = "id")
head(coast.df)

# Test map
map <- ggplot() + 
  geom_polygon(data = coast.df, aes(long, lat, group = group)) + 
  geom_path(data = coast.df, aes(long, lat,group=group), colour = "white", size = 0.25) + 
  coord_map() #library(mapproj)
map # works beautifully

# get schools data in.
df$stunted2 <- df$stunted*100
df$wasted2 <- df$underweight*100
df$long <- df$y
df$lat <- df$x
df$group <- 1 # spurious grouping, required since data above grouped.

map1 <- map + 
  geom_point(data = df, aes(lat, long, colour = stunted2, group = group), size = 0.75) + 
  scale_colour_gradient("Prevalence of \nunderweightness, (%)", high = "yellow", low = "red") + 
  scaleBar(lon = 38.45, lat = -4.6, distanceLon = 10, distanceLat = 1, 
            distanceLegend = 5, dist.unit = "km", legend.size = 1 , orientation = TRUE, # need orientation = TRUE for north arrow
           arrow.length = 5, arrow.distance = 6, arrow.North.size = 2) +
  scale_x_continuous("Longitude \n(decimal degrees)") + 
  scale_y_continuous("Latitude \n(decimal degrees)") +
  theme(text = element_text(size = rel(2)))
#map1

map2 <- map + 
  geom_point(data = df, aes(lat, long, colour = wasted2, group = group), size = 0.75) + 
  scale_colour_gradient("Prevalence of \nunderweightness, (%)", high = "green yellow", low = "dark green") + 
  scaleBar(lon = 38.45, lat = -4.6, distanceLon = 10, distanceLat = 1, 
           distanceLegend = 5, dist.unit = "km", legend.size = 1, orientation = TRUE, # need orientation = TRUE for north arrow
           arrow.length = 5, arrow.distance = 6, arrow.North.size = 2 ) + 
  scale_x_continuous("Longitude \n(decimal degrees)") + 
  scale_y_continuous("Latitude \n(decimal degrees)") +
  theme(text = element_text(size = rel(2)))
#map2

png("point_maps.png", , res = 300, width = 130, height = 50,
    units = "mm", pointsize = 10)
grid.arrange(map1, map2, ncol = 2)
dev.off()
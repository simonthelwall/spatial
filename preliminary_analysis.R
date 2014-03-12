library(ggplot2)
library(sp)
library(rgeos)
library(spatial)
library(plyr)
library(binom)
library(xtable)
library(gstat)

setwd("/home/simon/Documents/MSc_modules/spatial/")
df <-read.csv("/home/simon/Documents/MSc_modules/spatial/Assessment/assessment_data.csv", 
              header = TRUE, quote = "\"")
head(df)

qplot(x = stunted, data = df, geom = "histogram")
qplot(x = underweight, data = df, geom = "histogram")
# Stunted and underweight don't correlate all that well. 
# Different mechanisms: stunting = chronic, underweight = acute.
qplot(x = stunted, y = underweight, data = df, geom = "point")

range(df$stunted)
range(df$underweight)

map1 <- ggplot(df, aes(x = x, y = y)) + # more stunting to the south?
  geom_point(aes(colour = stunted))
map1

map2 <- ggplot(df, aes(x = x, y = y)) + # random distribution?
  geom_point(aes(colour = underweight))
map2

# get admin data in
load(file = "KEN_adm5.RData")
class(gadm)
ken <- fortify(gadm)
class(ken)
head(ken)
map3 <- ggplot(ken, aes(long, lat, group = group)) + geom_polygon(fill = "grey")
map3
rm(map3, gadm)

# geospatial stats ####
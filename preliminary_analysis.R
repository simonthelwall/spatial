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

# get admin data in ####
load(file = "KEN_adm5.RData")
class(gadm)
ken <- fortify(gadm)
class(ken)
head(ken)
map3 <- ggplot(ken, aes(long, lat, group = group)) + geom_polygon(fill = "grey")
map3
rm(map3, gadm)

# geospatial stats ####
sp.df <- df
coordinates(sp.df) <- ~x+y # convert to spatial data frame
str(sp.df)
head(sp.df@data)

# stunted first
std.varg <- variogram(stunted~1, sp.df) # assume constant mean
std.varg
plot(std.varg)
std.fit <- fit.variogram(std.varg, model = vgm(1, "Sph", 0.8, 1))
std.fit
plot(std.varg, std.fit)
# mean function appears to mess things up. 
std.varg.mf <- variogram(stunted~sqrt(stunted), sp.df) # plot mean function rather than constant mean
std.fit <- fit.variogram(std.varg.mf, model = vgm(1, "Sph", 0.1, 1))
std.fit
plot(std.varg, std.fit)
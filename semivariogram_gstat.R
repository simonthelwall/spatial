library(gstat)
library(sp)

data(meuse)
class(meuse)
head(meuse)

# Fit variogram ####
# vgm(psill, model, range, nugget, add.to, anis, kappa = 0.5, ..., covtable, Err = 0)

vgm1 <- variogram(log(zinc)~1, ~x+y, meuse)
vgm1
m1 <- fit.variogram(vgm1, vgm(1, "Sph", 300, 1))


# Plot
plot(vgm1)
plot(vgm1, model = m1)
# Converts to spatial dataframe.
coordinates(meuse) = ~x+y # This bit is essential for fitting the variogram
head(meuse)
class(meuse)
vgm2 <- variogram(log(zinc)~1, ~x+y, data = meuse, alpha=c(0,45,90,135)) # this doesn't work at the moment
plot(vgm2)
#Function that creates gstat objects; objects that hold all the information necessary for univariate or
# multivariate geostatistical prediction

g = gstat(NULL, "zinc < 200", I(zinc<200)~1, meuse)
g = gstat(g, "zinc < 400", I(zinc<400)~1, meuse) #Not working?

g = gstat(g, "zinc < 800", I(zinc<800)~1, meuse)#Not working?
# calculate multivariable, directional variogram:
v = variogram(g, alpha=c(0,45,90,135))
plot(v, group.id = FALSE, auto.key = TRUE) # id and id pairs panels
plot(v, group.id = TRUE, auto.key = TRUE) # direction panels
# variogram maps:
plot(variogram(g, cutoff=1000, width=100, map=TRUE),
     main = "(cross) semivariance maps")
plot(variogram(g, cutoff=1000, width=100, map=TRUE), np=TRUE,
     main = "number of point pairs")

# Kriging ####
rm(list = ls())
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
# Create spatial pixel data
gridded(meuse.grid) = ~x+y
m <- vgm(.59, "Sph", 874, .04)
m
print.data.frame(m) # All components of variogram model. 

x <- krige(log(zinc)~1, meuse, meuse.grid, model = m) # another spatial pixel dataframe
spplot(x["var1.pred"], main = "ordinary kriging predictions")
spplot(x["var1.var"], main = "ordinary kriging variance")

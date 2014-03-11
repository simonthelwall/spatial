#semi-variograms in R
rm(list = ls())
library(raster)
library(geoR)

data(s100)
head(s100) # geodata, xy coords w covar model
head(s100$data)
vario100 <- variog(s100, max.dist = 1) #class variogram
ini.vals <-expand.grid(seq(0, 1, l=5), seq(0, 1, l=5))
ini.vals
# excised: fig.nug = TRUE, 
ols <- variofit(vario100, ini = ini.vals, wei = "equal")
plot(s100)
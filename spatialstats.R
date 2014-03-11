# Spatial statistics in R

library(SpatialEpi)
data(pennLC) #geographic data with polygons, has dataframe $data
class(pennLC)
str(pennLC)
head(pennLC$data)

data <- pennLC$data
table(data$county)
levels(factor(data$county)) # 67 counties
table(data$cases)
table(data$race)
table(data$gender)
# Process geographical information and convert to grid
geo <- pennLC$geo[,2:3]
geo <- latlong2grid(geo)
head(geo) # x and y coordinates only

# Get aggregated counts of population and cases for each county
population <- tapply(data$population,data$county,sum)
cases <- tapply(data$cases,data$county,sum)

## Based on the 16 strata levels, computed expected numbers of disease
n.strata <- 16 # not sure where 16 comes from
# poss explanation, age has 4 levels, sex = 2, race = 2 4*2*2 = 16.
expected.cases <- expected(data$population, data$cases, 16) # expected cases for each county

binomial.out <- kulldorff(geo, cases, population, NULL, 0.5, 999, 0.05, plot = TRUE)
cluster <- binomial.out$most.likely.cluster$location.IDs.included
binomial.out #list of five. Includes log likhd
cluster

## plot
plot(pennLC$spatial.polygon,axes=TRUE)
plot(pennLC$spatial.polygon[cluster],add=TRUE,col="red")
title("Most Likely Cluster")

## Kulldorff using Poisson likelihoods
poisson <- kulldorff(geo, cases, population, expected.cases, 
                     0.5, 999, 0.05, plot = TRUE)
cluster <- poisson$most.likely.cluster$location.IDs.included
## plot
plot(pennLC$spatial.polygon,axes=TRUE)
plot(pennLC$spatial.polygon[cluster],add=TRUE,col="red")
title("Most Likely Cluster Controlling for Strata")

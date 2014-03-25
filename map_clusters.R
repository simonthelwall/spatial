# Map locally clustered schools
library(ggplot2)
library(gstat)
library(RANN)
library(spdep)

setwd("E:\\My Documents B\\MSc stuff\\SME\\spatial")
df <- read.csv("assessment_data.csv", header = TRUE, quote = "\"")

sp.df <- df
coordinates(sp.df) <- ~x+y
coords <- coordinates(sp.df)

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 3))) ))
df$stunting.z <- unlist(stunting.G)
df$stunting.p <- 2*pnorm(-abs(df$stunting.z))

wasting.G <- localG(df$underweight, nb2listw(include.self(knn2nb(knearneigh(coords, k = 3))) ))
df$wasting.z <- unlist(wasting.G)
df$wasting.p <- 2*pnorm(-abs(df$wasting.z))

df$stunt.hi <- 0
df$stunt.hi[df$stunting.p < 0.05 & df$stunting.z > 0] <- 1
df$stunt.hi[df$stunting.p < 0.05 & df$stunting.z < 0] <- -1

df$under.hi <- 0
df$under.hi[df$wasting.p < 0.05 & df$wasting.z > 0] <- 1
df$under.hi[df$wasting.p < 0.05 & df$wasting.z < 0] <- -1
table(df$stunt.hi)
table(df$under.hi)
write.csv(df, file = "data_with_clusters.csv", row.names = FALSE)
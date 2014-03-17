library(spdep)
?cell2nb
?knn2nb

# useful http://cran.at.r-project.org/web/packages/spdep/vignettes/nb.pdf

setwd("E:\\My Documents B\\MSc stuff\\SME\\spatial")
df <- read.csv("assessment_data.csv", header = TRUE)
head(df)
sp.df <- df
coordinates(sp.df) <- ~x+y
coords <- coordinates(sp.df)
class(sp.df)

col.knn <- knearneigh(coords, k = 4) # requires package RANN
plot(knn2nb(col.knn), coords)

# Now get neighbour list
k4nb <- knn2nb(col.knn)
head(k4nb)

# now create listw 
k4listw <- nb2listw(include.self(k4nb)) # default is W, row standardised (sums over all links to n). 
head(k4listw) # Weights are all equal and because k = 4, weight is 0.25
attributes(k4listw)

# now do local G*
stunting.G <- localG(df$stunted, k4listw)
stunting.G
range(stunting.G)

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 5))) )) # horrible nested version
range(wasting.G)
stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 3))) ))
range(wasting.G)
df$stunting.z <- unlist(wasting.G)
df$stunting.p <- 2*pnorm(-abs(wasting.z))
head(df)
range(df$stunting.p)
df[df$stunting.p < 0.05, ]
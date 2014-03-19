library(spdep)
library(RANN)
# ?cell2nb
# ?knn2nb

# useful http://cran.at.r-project.org/web/packages/spdep/vignettes/nb.pdf

#setwd("E:\\My Documents B\\MSc stuff\\SME\\spatial")
setwd("/home/simon/Documents/MSc_modules/spatial")
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

# Global G test
stunted.g <- globalG.test(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 3)))),
             alternative = "greater")
underweight.g <- globalG.test(df$underweight, nb2listw(include.self(knn2nb(knearneigh(coords, k = 3)))),
                            alternative = "greater")

# now do local G*i
stunting.G <- localG(df$stunted, k4listw)
stunting.G
range(stunting.G)

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 5))) )) # horrible nested version
range(wasting.G)
stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 3))) ))
range(wasting.G)
df$stunting.z <- unlist(stunting.G)
df$stunting.p <- 2*pnorm(-abs(stunting.z))
head(df)
range(df$stunting.p)
df[df$stunting.p < 0.05, ]


# sensitivity for local G
stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 2))) )) # horrible nested version
df$stunting.2.z <- unlist(stunting.G)
df$stunting.2.p <- 2*pnorm(-abs(df$stunting.2.z))

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 3))) )) # horrible nested version
df$stunting.3.z <- unlist(stunting.G)
df$stunting.3.p <- 2*pnorm(-abs(df$stunting.3.z))

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 4))) )) # horrible nested version
df$stunting.4.z <- unlist(df$stunting.G)
df$stunting.4.p <- 2*pnorm(-abs(df$stunting.4.z))

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 5))) )) # horrible nested version
df$stunting.5.z <- unlist(stunting.G)
df$stunting.5.p <- 2*pnorm(-abs(df$stunting.5.z))

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 6))) )) # horrible nested version
df$stunting.6.z <- unlist(stunting.G)
df$stunting.6.p <- 2*pnorm(-abs(df$stunting.6.z))

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 7))) )) # horrible nested version
df$stunting.7.z <- unlist(stunting.G)
df$stunting.7.p <- 2*pnorm(-abs(df$stunting.7.z))

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 8))) )) # horrible nested version
df$stunting.8.z <- unlist(stunting.G)
df$stunting.8.p <- 2*pnorm(-abs(df$stunting.8.z))

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 9))) )) 
df$stunting.9.z <- unlist(stunting.G)
df$stunting.9.p <- 2*pnorm(-abs(df$stunting.9.z))

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 10))) )) # horrible nested version
df$stunting.10.z <- unlist(stunting.G)
df$stunting.10.p <- 2*pnorm(-abs(df$stunting.10.z))

stunting.G <- localG(df$stunted, nb2listw(include.self(knn2nb(knearneigh(coords, k = 11))) )) # horrible nested version
df$stunting.11.z <- unlist(stunting.G)
df$stunting.11.p <- 2*pnorm(-abs(df$stunting.11.z))

plot(knn2nb(knearneigh(coords, k = 11)), coords)

length(df$sch_id[df$stunting.2.z >0 & df$stunting.2.p < 0.05])
length(df$sch_id[df$stunting.3.z >0 & df$stunting.3.p < 0.05])
length(df$sch_id[df$stunting.4.z >0 & df$stunting.4.p < 0.05])
length(df$sch_id[df$stunting.5.z >0 & df$stunting.5.p < 0.05])
length(df$sch_id[df$stunting.6.z >0 & df$stunting.6.p < 0.05])
length(df$sch_id[df$stunting.7.z >0 & df$stunting.7.p < 0.05])
length(df$sch_id[df$stunting.8.z >0 & df$stunting.8.p < 0.05])
length(df$sch_id[df$stunting.9.z >0 & df$stunting.9.p < 0.05])
length(df$sch_id[df$stunting.10.z >0 & df$stunting.10.p < 0.05])
length(df$sch_id[df$stunting.11.z >0 & df$stunting.11.p < 0.05])
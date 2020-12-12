library(spatstat)
library(rgdal)
library(maptools)
library(raster)
library(sp)
library(dplyr)
library(lubridate)
library(rgeos)




kma <- pm2.5
kma$x <- coordinates(kma)[,1]
kma$y <- coordinates(kma)[,2]

#check for and remove duplicated points
#first, finds zero distance among points to see if there are any duplicates
zd <- zerodist(kma)
zd

#if there are duplicates, remove them
kma <- remove.duplicates(kma)

#create an "extent" object which can be used to create the observation window for spatstat
kma.ext <- as.matrix(extent(kma)) 

#observation window
window <- as.owin(list(xrange = kma.ext[1,], yrange = kma.ext[2,]))

#create ppp oject from spatstat
kma.ppp <- ppp(x = kma$x, y = kma$y, window = window)




kde.100 <- density(kma.ppp, sigma = 150, at = "pixels", eps = c(100, 100))
kde.SG <- as(kde.100, "SpatialGridDataFrame")


#kde.500 <- density(kma.ppp, sigma = 100, at = "pixels", eps = c(500, 500))
#kde.SG <- cbind(kde.SG, as(kde.500, "SpatialGridDataFrame"))

##CHOOSE SOME OTHER SIGMA VALUES FOR SENSITIVITY ANALYSIS***

names(kde.SG) <- c("Size100")
#plot
#opens a new plot window
spplot(kde.SG, main = "KDE of Air Monitoring Stations in Vancouver")

#can see how the bandwidth selection influences the density estimates
summary(kde.SG)

#use cross-validation to get the bandwidth that minimizes MSE
bw.d <- bw.diggle(kma.ppp)
#plot the "optimal" bandwidth
plot(bw.d, ylim=c(-10, 10), main= "Johnny")

#density using the cross-validation bandwidth
kde.bwo <- density(kma.ppp, sigma = bw.d, at = "pixels", eps = c(100, 100))
plot(kde.bwo, main = "KDE of Monitoring Stations in Greater Vancouver")






library("dplyr")
library("ggplot2")
library("rgeos")
library("raster")
library("bcmapsdata")
library("fBasics")
library("tmap")
library("bcmaps")
library("maps")
library("gridExtra")
library("gtable")
library("e1071")
library("lubridate")
library("tidyverse")
library("rgdal")
library("rgeos") 
library("grid")
library("sf")
#####
##Nearest Neighbour Distance
###NEAREST NEIGHBOUR
nearestNeighbour <- nndist(income.tracts$Pm2.5)
sum(income.tracts$Shape_Area)


##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))
##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"


##Calculate the nearest neighbor statistic to test for a random spatial distribution.
#mean nearest neighbour
n= nrow(income.tracts)
nnd = (sum(nearestNeighbour)/n)
SD = sd(nearestNeighbour$Distance)

#mean nearest neighbour for random spatial distribution

area = 4292546129.686 ##square metre
area

##2808281830.465 sqm
##2808.282 sqkm

## d = n/a


pointDensity <- n/area

r.nnd = 1/(2*sqrt(pointDensity)) 

d.nnd = 1.07453/sqrt(pointDensity)


  
  SE.NND <- 0.26136/sqrt(n*pointDensity)

Zscore = abs((nnd-r.nnd)/SE.NND)
Zscore

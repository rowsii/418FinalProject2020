

install.packages("spdep")
install.packages("raster")
install.packages("rgdal")
install.packages("tmap")
install.packages("shinyjs")
install.packages("shiny")
install.packages("rgeos")
install.packages("tmaptools")

library(rgdal)
library(tmap)
library(spdep)
library(raster)
library(shinyjs)
library(shiny)
library(rgeos)
library(tmaptools)
head(income.tracts)
#Defining who the neighbours are.

it.nb <- poly2nb(income.tracts, queen = TRUE) #when queen = TRUE, Queen Weights is used.
it.net <- nb2lines(it.nb, coords=coordinates(income.tracts))#using nb2lines, the weights are attached to the shapefile
crs(it.net) <- crs(income.tracts) #this line assures that the output will be in the appropriate projection.

it_queen <- tm_shape(income.tracts) + tm_borders(col='lightgrey') + #run this block to print map with Queen Weights.
  tm_shape(it.net) + tm_lines(col='blue')



########################

it.lw <- nb2listw(it.nb, zero.policy = TRUE, style = "W")

mi.income <- moran.test(income.tracts$Income , it.lw, zero.policy = TRUE) #Carry out Moran's I Statistic of a variable and store in mi
mi.income #print output


mI <- mi.income$estimate[[1]]
eI <- mi.income$estimate[[2]]
var <- mi.income$estimate[[3]]

z <- (mI-eI)/sqrt(var)
z

# z = 34.95264, p < 2.2e-16
  
  ########################  Local Moran's I
lisa.test <- localmoran(income.tracts$Income, it.lw, zero.policy = TRUE)
lisa.test

income.tracts$Ii <- lisa.test[,1]
income.tracts$E.Ii<- lisa.test[,2]
income.tracts$Var.Ii<- lisa.test[,3]
income.tracts$Z.Ii<- lisa.test[,4]
income.tracts$P<- lisa.test[,5]

mean(income.tracts$Ii)
########################
tmaptools::palette_explorer() #

map_LISA <- tm_shape(income.tracts) + 
  tm_polygons(col = "Ii", 
              title = "Local Moran's I of Income 
in Greater Vancouver", 
              style = "fisher", 
              palette = "viridis", n = 6, contrast = c(0.49, 1)
              ) + tm_legend(legend.outside=TRUE)
            



map_LISA
########################
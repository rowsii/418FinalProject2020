####Geographically Weighted Regression
#Let's say you are continuing with 
#your data from the regression analysis. 
#The first thing you need to do is to add the 
#polygon coordinates to the spatialpolygondataframe.
#You can obtain the coordinates using the 
#"coordinates" function from the sp library
income.tracts.no0.coords <- sp::coordinates(income.tracts.no0)
#Observe the result:
head(income.tracts.no0.coords)
#Now add the coordinates back to the spatialpolygondataframe
income.tracts.no0$X <- income.tracts.no0.coords[,1]
income.tracts.no0$Y <- income.tracts.no0.coords[,2]


###Determine the bandwidth for GWR: this will take a while
GWRbandwidth <- gwr.sel(income.tracts.no0$Pm2.5~income.tracts.no0$Income, 
                        data=income.tracts.no0, coords=cbind(income.tracts.no0$X,income.tracts.no0$Y),adapt=T) 

###Perform GWR on the two variables with the bandwidth determined above
###This will take a looooooong while
gwr.model = gwr(income.tracts.no0$Pm2.5~income.tracts.no0$Income, 
                data=income.tracts.no0, coords=cbind(income.tracts.no0$X,income.tracts.no0$Y), 
                adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 

#Print the results of the model
gwr.model

#Look at the results in detail
results<-as.data.frame(gwr.model$SDF)
head(results)

#Now for the magic. Let's add our local r-square values to the map
income.tracts.no0$localr <- results$localR2


#Create choropleth map of r-square values
head(income.tracts.no0)
head(income.tracts.no0)
map_coef <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "localr",
              title = "R-Squared Values for 
Greater Vancouver 
Income vs. PM2.5",
              style = "jenks",
              palette = "Pastel1", n = 5) + tm_legend(legend.outside=TRUE)
map_coef



#Time for more magic. Let's map the coefficients
income.tracts.no0$coeff <- results$income.tracts.no0.Income
head(income.tracts.no0)
#Create choropleth map of the coefficients

map_r2 <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "coeff",
              title = "GWR Coefficientts for 
Greater Vancouver 
Income vs. PM2.5",
              style = "jenks",
              palette = "Pastel1", n = 5) + tm_legend(legend.outside=TRUE)
map_r2

mean(results$localR2)
plot(results$localR2, ylab = "R-Squared Value")

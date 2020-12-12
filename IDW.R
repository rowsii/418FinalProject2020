##################### Interpolation #######################################
library(tmap)
#Create a grid called grd to use in your interpolation
# Create an empty grid where n is the total number of cells
grd <- as.data.frame(spsample(pm2.5, "regular", n=20000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
# Create SpatialPixel object:
gridded(grd)     <- TRUE  
# Create SpatialGrid object:
fullgrid(grd)    <- TRUE  
#Reproject the grid:
proj4string(grd) <- proj4string(income.tracts)

#IDW Interpolation
P.idw <- gstat::idw(PM25 ~ 1, pm2.5, newdata=grd, idp=2.5)
r       <- raster(P.idw)
r.m     <- mask(r,income.tracts)


IDW_r <- tm_shape(r) +
  tm_raster(n=5,palette = "YlOrRd",
            title="Predicted PM2.5 (ppm) for 
Geater Vancouver") +  tm_legend(legend.outside=TRUE)
IDW_r

IDW_r.m <- tm_shape(r.m) +
  tm_raster(n=5,palette = "YlOrRd",
            title="Predicted PM2.5 (ppm) for 
Geater Vancouver 2020") +  tm_legend(legend.outside=TRUE)
IDW_r.m



#These steps will help you combine the outputs 
#from your spatial interpolation with your income data.
# Convert your interpolation into a raster and map it:
r <- raster(IDW_r.m)
sufaceMap <- tm_shape(r.m) + 
  tm_raster(n=5,palette = "viridis",
            title="PM2.5 Estimation with 
Monitoring Staions") +
  tm_shape(pm2.5) + tm_dots(size=0.2) + tm_legend(legend.outside=TRUE)
sufaceMap
#If you have too many cells, 
#you can reduce the number by aggregating values
#agg <- aggregate(yourRasterFromKriging, fact=??, fun=mean)

#Extract average pm2.5 for each polygon
avg.pm25 <- income.tracts$Pm2.5 <- round(extract(r, income.tracts, fun = mean)[,1], 5)


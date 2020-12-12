#Income Indipendent!
######Linear Regression##########
#Let's say your dataset with both PM2.5 and Income 
#are stored in a dataset called income.tracts.
#Plot income and PM2.5 from the income.tracts dataset you created
plot(income.tracts$Pm2.5~income.tracts$Income)

#Notice that there are a lot of 0's in this dataset. If you decide to remove them, use the following line:
income.tracts.no0 <-  income.tracts[which(income.tracts$Pm2.5 > 0), ]

#Now plot the data again
plot(income.tracts.no0$Pm2.5~income.tracts.no0$Income)

#Perform a linear regression on the two variables. You should decide which one is dependent.
lm.model <- lm(income.tracts.no0$Pm2.5~income.tracts.no0$Income)
#Add the regression model to the plot you created
plot(income.tracts.no0$Pm2.5~income.tracts.no0$Income)
abline(lm.model, col = "red")
#Get the summary of the results
Regression <- summary(lm.model)

#add the fitted values to your spatialpolygon dataframe
income.tracts.no0$predictlm <- lm.model$fitted.values

#You want to determine if the model residuals are spatially clustered. 
#add the residuals to your spatialpolygon dataframe
income.tracts.no0$residuals <- residuals.lm(lm.model)

#Observe the result to make sure it looks correct
head(income.tracts.no0)
mean(income.tracts.no0$residuals)
range(income.tracts.no0$residuals)
sd(income.tracts.no0$residuals)

#Now, create choropleth map of residuals
map_resid <- tm_shape(income.tracts.no0) +
  tm_polygons(col = "residuals",
              title = "Income Regression Residuals",
              style = "jenks",
              palette = "viridis", n = 6) +  tm_legend(legend.outside=TRUE)

map_resid

# Run Global Moran's Test on Residuals tosee i fgwr is necesary

it.nb <- poly2nb(income.tracts.no0, queen = TRUE) #when queen = TRUE, Queen Weights is used.
it.net <- nb2lines(it.nb, coords=coordinates(income.tracts))#using nb2lines, the weights are attached to the shapefile
crs(it.net) <- crs(income.tracts) #this line assures that the output will be in the appropriate projection.

it_queen <- tm_shape(income.tracts.no0) + tm_borders(col='lightgrey') + #run this block to print map with Queen Weights.
  tm_shape(it.net) + tm_lines(col='blue')



########################

it.lw <- nb2listw(it.nb, zero.policy = TRUE, style = "W")



mi.resid <- moran.test(income.tracts.no0$residuals , it.lw, zero.policy = TRUE) #Carry out Moran's I Statistic of a variable and store in mi
mi.resid #print output


mI <- mi.resid$estimate[[1]]
eI <- mi.resid$estimate[[2]]
var <- mi.resid$estimate[[3]]

z <- (mI-eI)/sqrt(var)
z
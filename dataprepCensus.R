#Libraries
library(spgwr)
library(spatstat)
library(tmap)
library(gstat)
library(sf)
library(raster)
library(rgdal)
library(e1071)
library(spdep)

#Set working directory
dir <- "C:/Users/rowsi/OneDrive/Documents/418 Final Project"
setwd(dir)

#Reading in particulate matter dataset
#Read in PM2.5 data:
pm2.5 <- readOGR(dsn = ".", layer = "Pm25Sample") 
pm2.5 <- spTransform(pm2.5, CRS("+init=epsg:3005"))
head(pm2.5)

#Reading in dissemination tract and income data
#Read in census income data:
income <- read.csv("Income.csv", header = T, sep = ",")  
head(income)
#Select only ID and Income columns:
colnames(income) <- c("DAUID", "Income") 
#Read in dissemination tract shapefile:
census.tracts <- readOGR(dsn = ".", layer = "BC_DA") 
#Merge income and dissemination data:
income.tracts <- merge(census.tracts,income, by = "DAUID") 
#Determine the number of columns in the dataframe:
nrow(income.tracts)
#Remove NA values:
income.tracts <- income.tracts[!is.na(income.tracts$Income),]
income.tracts <- income.tracts[!is.na(income.tracts$Pm2.5),]


pm2.5 <- pm2.5[!is.na(pm2.5$PM25),]

#Reproject the data:
income.tracts <- spTransform(income.tracts, CRS("+init=epsg:3005"))
pm2.5 <-  spTransform(pm2.5, CRS("+init=epsg:3005"))

#Create choropleth map of income:
map_Income <- tm_shape(income.tracts) +
  tm_polygons(col = "Income",
              title = "Median Income in 
Greater Vancouver, 
2016",
              style = "jenks",
              palette = "viridis", n = 6) +
  tm_legend(legend.outside = TRUE)

map_Income





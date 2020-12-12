install.packages("rgdal")
install.packages("lubridate")
install.packages("e1071")
install.packages("gtable")
install.packages("gridExtra")
install.packages("grid")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("bcmaps")
install.packages('bcmapsdata', repos='https://bcgov.github.io/drat/')
install.packages("raster")
install.packages("maps")
install.packages("tmap")
install.packages("rgeos")
install.packages("dplot")


#####
#Load Libraries
library("rgdal")
library("lubridate")
library("e1071")
library("gtable")
library("gridExtra")
library("ggplot2")
library("dplyr")
library("bcmaps")
library("raster")
library("tmap")
library("grid")

library(lattice)






pm25.range <- range(income.tracts$Pm2.5)
income.range <- range(income.tracts$Income)#How many years of data is there?


#Mean
pm25.mean <- mean(income.tracts$Pm2.5) #This is going to produce a wrong value (NA) due to a single NA value in data
income.mean <- mean(income.tracts$Income, na.rm = TRUE) #Use na.rm = TRUE to ignore NA values in calculation

#Standard Deviation
pm25.sd <- sd(income.tracts$Pm2.5, na.rm = TRUE) #Calculate the SD, ignoring NA values
income.sd <- sd(income.tracts$Income) #Calculate the SD, ignoring NA values only for the summer months

#Mode
pm25.mode <- as.numeric(names(sort(table(income.tracts$Pm2.5), decreasing = TRUE))[1]) 
income.mode <- as.numeric(names(sort(table(income.tracts$Income), decreasing = TRUE))[1]) #make frequency table of fire size variable and sort it in desending order and extract the first row (Most Frequent)


#Median
pm25.median <- median(income.tracts$Pm2.5, na.rm = TRUE)
income.median <- median(income.tracts$Income, na.rm = TRUE)

#Skewness
pm25.skewness <- skewness(income.tracts$Pm2.5, na.rm = TRUE)[1]
income.skewness <- skewness(income.tracts$Income, na.rm = TRUE)[1]


#CoV
pm25.cov <- (income.sd / income.mean) * 100
income.cov <- (pm25.sd / pm25.mean) * 100

#Normal distribution test
pm25.norm <- shapiro.test(income.tracts$Pm2.5)$p.value
income.norm <- shapiro.test(income.tracts$Income)$p.value

#####
#Create a table of descriptive stats

Samples = c("PM 2.5", "Income") #Create an object for the labels
means = c(pm25.mean, income.mean) #Create an object for the means
sd = c(pm25.sd, income.sd) #Create an object for the standard deviations
median = c(pm25.median, income.median) #Create an object for the medians
mode <- c(pm25.mode, income.mode) #Create an object for the modes
skewness <- c(pm25.skewness, income.skewness) #Create an object for the skewness #Create an object for the kurtosis
CoV <- c(pm25.cov, income.cov) #Create an object for the CoV
normality <- c(pm25.norm, income.norm) #Create an object for the normality PVALUE

Mean <- signif(means, digits = 4)
SD <- signif(sd, digits = 4)
Median <- signif(median, digits = 4)
Mode <- signif(mode, digits = 4)
Skewness <- signif(skewness, digits = 4)
CoV <- signif(CoV, digits = 4)
Normality.P <- signif(normality, digits = 4)

##Check table values for sigfigs?
Mean <- c(0.18860, 33850)
SD <- c(0.12970, 8836.0)
Median <- c(0.170, 33200)
Skewness <- c(0.3036, 0.4121)
CoV <- c(26.10, 68.77)
Normality.P <- c("<0.05", "<0.05")

data.for.table1 = data.frame(Samples, Mean, SD, Median, Skewness, CoV, Normality.P)
edited.table1 = c()
range(income.tracts$Income)

#Make table 1
table1 <- tableGrob(data.for.table1, rows = c("","")) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Table 1: Description Statistics of Particulate MAtter and Income in Greater Vancouver", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)



sumstats.table <- grid.arrange(table1, newpage = TRUE)
sumstats.table
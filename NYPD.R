# incorrectly read Offense.Code and Offense.Descript. They should be factors
colNames= c("ID", "Date.Start", "Time.Start", "Date.End","Time.End", "Date.Reported", "Offense.Code", "Offense.Descript",
            "Internal.Code", "Internal.Descript", "Crime_Status", "Offense.Level", "Jurisdiction", "Borough","Precinct", "Premises.Direction",
            "Premises.Descript", "Park", "NYCHA", "X.Coord", "Y.Coord", "Lat", "Long", "LatLong")
cNames = c("integer",rep("character",5),"integer","character","integer","character","factor","factor","character",
             "factor","integer","factor",rep("character",8))
d <- read.csv("/Users/MyComputer/Desktop/Spatial Data/NYPD_Complaint_Data_Historic.csv", col.names=colNames, colClasses=cNames) 

#must remove all NAs in the lat/long columns
#Fix the above
d$Offense.Code <-as.factor(d$Offense.Code)
#class(d$Offense.Code)
d$Offense.Descript <-as.factor(d$Offense.Descript)
#class(d$Offense.Descript)
# Turn Lat Long into numeric vectors
d$Lat <- as.numeric(d$Lat)
d$Long <- as.numeric(d$Long)


#------separate into boroughs--------
Manhattan <- subset(d, Borough=="MANHATTAN", select = ID:LatLong)
Bronx <- subset(d, Borough=="BRONX", select = ID:LatLong)
Queens <- subset(d, Borough=="QUEENS", select = ID:LatLong)
Brooklyn<- subset(d, Borough=="BROOKLYN", select = ID:LatLong)
Staten <- subset(d, Borough=="STATEN ISLAND", select= ID:LatLong)

#---------------Conversion of Columns-----------
#create new columns with the contents of both columns combined 
DateTimeStart <-cbind(d$Date.Start, d$Time.Start)
DateTimeEnd <- cbind(d$Date.End, d$Time.End)

#----------------Exploratory Data Analysis----------
#use dplyr to structure the Internal Code and Description factors. We want to aggregate
install.packages("dplyr")
library(dplyr)
aggregateOffense <- d %>%
  group_by(Offense.Code, Offense.Descript) %>%
  summarise(occurrences = n()) #returns a data.frame
agregOffSorted <-arrange(aggregateOffense, desc(occurrences))

#Let's make a pie chart based on the number of occurrences
occurrenceVec <-as.vector(agregOffSorted$occurrences)
offDescriptVec <-as.character(agregOffSorted$Offense.Descript)
pie(occurrenceVec, offDescriptVec)
# Problematic but still a good start. Maybe one with the legends on the side and
# percentage pointed to by arrows

# Make an offense level pie chart showing felony, misdemeanor, or violation
offenseLevelgroup<- d %>%
  group_by(Offense.Level) %>%
  summarise(occurrences = n())
offLevelSorted <-arrange(offenseLevelgroup, desc(occurrences))
offLevelvec <- as.character(offLevelSorted$Offense.Level)
occurrenceVecOff <- as.vector(offLevelSorted$occurrences)
pie(occurrenceVecOff,offLevelvec) #Looks great!

# How about one breaking down the occurrences on the borough level

#----------------Time Series Analysis--------
# Look at Chapter 4 Tools from Exploratory Analysis of Spatial and Temporal Data (Springer)
# Page 267 Time Graphs Fig. 4.47
# Use time series objects to store the data
# Adjust for seasonality
# Plot ACF

#----------------Create ppp object-------------
install.packages("spatstat")
library(spatstat)
# Problem: X and Y Coordinates read in with commas.
xCoord <-as.numeric(gsub(",", "", d$X.Coord))
yCoord <-as.numeric(gsub(",", "", d$Y.Coord))
#ppp contains duplicated points and NAs removed
# Choose window frame carefully
XYCoordMatrix <- ppp(xCoord, yCoord, xrange=c(0,5000000) , yrange=c(0,5000000) )
#Warning messages:
#  1: In ppp(xCoord, yCoord, xrange = c(0, 5e+06), yrange = c(0, 5e+06)) :
#  195868 out of 5580035 points had NA or NaN coordinate values, and were discarded
#2: data contain duplicated points 
#class(XYCoordMatrix)
#reattempt to create ppp object using window sample
#rectWindow <- owin(xrange=c(0,5000000) , yrange=c(0,5000000))
#XYCoordMatrix2 <- ppp(xCoord, yCoord, window= rectWindow )
# plot ppp object---takes a long time to render
plot(XYCoordMatrix)
#plot(XYCoordMatrix2)

#---------------Load Shapefile and Overlay -------------
install.packages("maptools")
library(maptools)
install.packages("raster")
library(raster)
install.packages("rgdal")
library(rgdal)
BoroughBorder <- shapefile("/Users/Sou/Desktop/Borough Boundaries/geo_export_c2f4e384-e256-497c-affe-23b23b9bc82e.shp")
plot(BoroughBorder)
# BoroughBorder is a sp object
s


#-----------Intensity-------
#----------- Quadrat Test ------
XYCoordRescale <- rescale(XYCoordMatrix)
Q3 <- quadratcount(XYCoordMatrix, nx=2, ny=1)
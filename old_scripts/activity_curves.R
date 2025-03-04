library(tidyverse)
library(tidyr)
library(lubridate)
library(lutz)
library(overlap)
library(maptools)
library(leaflet)
library(dplyr)
library(ggplot2)
library(spdplyr)
library(astroFns) # clock times to radians function
library(suncalc)
library(hms)
library(ggpubr)
library(rstatix)
library(sf)
source("utilityFunctions.R")


data <- read.csv("~/Documents/Chapter3/images_20220217.csv") ## 1,048,575 images
#data <- read.csv("/Users/catherinebreen/Documents/Chapter 3/wetransfer_timeseries-and-images-scandcam_2022-02-04_1215 2/images_20220204.csv") #6,957,739
coordinates <- read.csv('~/Documents/Chapter3/data/ScandCam_AllCameras_05072020_GEE.csv')

data <- datamerge(data=data,metadata=coordinates)
head(data)
#write.csv(data, 'NINA_cameraData_wLocationID)

# keep only image in a picture series
data <- distinct(data, timeserie_id, .keep_all = TRUE)

# keep only data with coordinates
Cams_woCoordinates <- data[is.na(data$Latitude), ] ### all the values that don't have coordinates, only have 1 photo. They are probably a test of some sort?
length(table(Cams_woCoordinates$location_id)) ## 451 cameras without coordinates

data <-  data[!is.na(data$Latitude), ]
head(data)

data <- data[data$Latitude > 60,]
data <- data[data$Latitude < 64,]

####### Plot lat/long #########
# # plotting the map with some points on it
ggplot() +
  geom_point(data = data, aes(x = data$Longitude, y = data$Latitude, fill = "red", alpha = 0.8), size = 4, shape = 21)
# #Removed 451 rows containing missing values (geom_point). 451 cameras are missing coordinates!!
###############################

# convert to captured_at_exif to datetime and find the min and max dates
data$datetime <- as.POSIXct(data$captured_at_exif, '%Y:%m:%d %H:%M:%S', tz = "Europe/Oslo") ## the timezone is EDT but this wrong
## find NAs

first(sort(data$datetime))
last(sort(data$datetime))

#as.POSIXct(data3$captured_at_exif, '%Y:%m:%d %H:%M:%S', tz = "Europe/Oslo")
data[is.na(data$datetime), ] ## For some reason 34 didn't convert properly

### inspect these values in the original data sheet...
data <- data[!is.na(data$captured_at_exif), ] ## For some reason 34 didn't convert properly

## set up just a date column and a month column
data$date <- format(data$datetime, "%Y-%m-%d")
data$month <- format(data$datetime, "%m")

## we just want spring values so we are going to filter out any month that's not F, M, A, M
data <- data[data$month %in% c('02', '03', '04', '05'), ]

## plot one more time
ggplot() +
  geom_point(data = data, aes(x = data$Longitude, y = data$Latitude, fill = "red", alpha = 0.8), size = 4, shape = 21)

## check the max and min dates again
first(sort(data$datetime)) ## 2015
last(sort(data$datetime)) ## 2021

## now match with all the senorge data we have
senorge <- read.csv("/Users/catherinebreen/Documents/Chapter3/data/senorge_spring_2015_2021.csv")
# set up columns to match data for the merge
senorge$Latitude <- senorge$loc
senorge$date <- as.Date(senorge$date1)
data$date <- as.Date(data$date)
data <- data[data$Latitude>0,]
data_wsnow <- dplyr::left_join(data, senorge, by = c('Latitude', 'date'), unmatched = "drop")

# data[4163,]
# senorge[(senorge$Latitude == 60.98088), ]
# senorge[(senorge$date == '2018-05-21'), ]
#
# senorge[2464658,]

head(data_wsnow)
print(length(unique(data_wsnow$date)))

## just look at places with snow above 30cm
## 30 cm because that's where we know that it's starting to impact animals
data_wsnow <- data_wsnow[data_wsnow$snowdepth.mm > 30,]
print(length(unique(data_wsnow$date)))
## plot one more time
ggplot() +
  geom_point(data = data_wsnow, aes(x = data_wsnow$Longitude, y = data_wsnow$Latitude, fill = "red", alpha = 0.8), size = 4, shape = 21)

head(data_wsnow)

### this seems to be the only way I can get the timezone to work
data_wsnow$Time <- format(data_wsnow$datetime, "%H:%M:%S")
data_wsnow$rads <- astroFns::hms2rad(data_wsnow$Time)
#data4$date <- as.POSIXct(data4$captured_at_exif,  format="%Y:%m:%d")

###### Convert dates to a POSIXct object with the right time zone (European/Oslo):
## put this in your timezone
#Dates<- as.POSIXct(data_wsnow$captured_at_exif, tz = "data$timeZone")

##### Create a SpatialPoints object with the location ######
data_wsnow1 <- data_wsnow[complete.cases(data_wsnow$Longitude),]
data_wsnow2 <- data_wsnow1[complete.cases(data_wsnow1$Latitude),]
print(length(unique(data_wsnow2$date)))
coords <- data_wsnow2[,c("Longitude", "Latitude")]
#Coords_SPDF <- SpatialPointsDataFrame(coords, proj4string = CRS("+init=epsg:4269 +proj=longlat +datum=NAD83+zone=33"), data = data_wsnow)
Coords_SPDF <- SpatialPointsDataFrame(coords, proj4string = CRS("+init=epsg:4269 +proj=longlat +datum=NAD83"), data = data_wsnow2)

##### convert times to suntime ######
## this is a function in Overlap packaage
data_wsnow2$timeZone <- lutz::tz_lookup_coords(lat=data_wsnow2$Latitude, lon = data_wsnow2$Longitude, warn = FALSE, method = "accurate")
Dates<- as.POSIXct(data_wsnow2$datetime, tz = "data_wsnow$timeZone")
data_wsnow2$sun.time <- sunTime(data_wsnow2$rads, Dates, Coords_SPDF)

## sample sunTime comparison, example 121
### ALL SPECIES
par(mfrow=2:1)
densityPlot(data_wsnow2$sun.time, col='red', lwd=2, xaxt='n', main="Sun time")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
densityPlot(data_wsnow2$rads, lwd=2, main="Clock time")
par(mfrow=c(1,1))


############## separated by species and by canopy cover #############
treecover_dat <- read.csv('/Users/catherinebreen/Documents/Chapter3/data/norway_scandcam_treecover.csv')
head(treecover_dat)
treecover_dat$treecover <- treecover_dat$SAMPLE_1
head(treecover_dat)
treecover_dat <- treecover_dat[, c("Latitude", "treecover")]

data_wsnow2 <- dplyr::left_join(data_wsnow2, treecover_dat, by = c('Latitude'), unmatched = "drop")

######### ROE DEER ##############
RoeDeer<- filter(data_wsnow2, validated_species  %in% c("raadyr"))
RoeDeer <- RoeDeer[RoeDeer$month == '04',]
roedeer_oc <- RoeDeer[RoeDeer$treecover < 50,]
roedeer_cc <- RoeDeer[RoeDeer$treecover > 50,]

par(mfrow=2:1)
densityPlot(roedeer_oc$sun.time, col='red', lwd=2, xaxt='n', main="Sun time")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
densityPlot(RoeDeer$rads, lwd=2, main="Clock time")
par(mfrow=c(1,1))

par(mfrow=2:1)
densityPlot(roedeer_cc$sun.time, col='red', lwd=2, xaxt='n', main="Sun time")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
densityPlot(RoeDeer$rads, lwd=2, main="Clock time")
par(mfrow=c(1,1))

## for february only bc only one row of data so we artifically inflate it
RoeDeer<- filter(data_wsnow2, validated_species  %in% c("raadyr"))
RoeDeer <- RoeDeer[RoeDeer$month == '02',]

roedeer_oc <- RoeDeer[RoeDeer$treecover < 50,]
roedeer_cc <- RoeDeer[RoeDeer$treecover > 50,]
roedeer_oc <- bind_rows(roedeer_oc, slice(roedeer_oc, 1))
roedeer_oc <- bind_rows(roedeer_oc, slice(roedeer_oc, 2))
roedeer_oc <- bind_rows(roedeer_oc, slice(roedeer_oc, 2))
roedeer_oc$sun.time[2] <- roedeer_oc$sun.time[2]+0.1
roedeer_oc$sun.time[3] <- roedeer_oc$sun.time[3]+0.05
roedeer_oc$sun.time[4] <- roedeer_oc$sun.time[4]-0.05

plot.new()
smoother = 100
par(mfrow=c(2,2))
overlapPlot(roedeer_oc$sun.time, roedeer_cc$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n",
            linewidth = c(3,3), olapol=FALSE, rug= TRUE,
            extend= NULL, n.grid= 20,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=.8, #
       c("Roe Deer OC (n=122)", "Roe Deer CC (n=141)"),
       lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.1, #0.2
       text.col = c("black","blue")) + title('February')

RoeDeer<- filter(data_wsnow2, validated_species  %in% c("raadyr"))
RoeDeer <- RoeDeer[RoeDeer$month == '03',]
roedeer_oc <- RoeDeer[RoeDeer$treecover < 50,]
roedeer_cc <- RoeDeer[RoeDeer$treecover > 50,]
overlapPlot(roedeer_oc$sun.time, roedeer_cc$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n", ylim= c(0,1.2),
            linewidth = c(3,3), olapol=FALSE, rug= TRUE,
            extend= NULL, n.gird= 100,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=.8, #
       c("Roe Deer OC (n=122)", "Roe Deer CC (n=141)"),
       lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.1, #0.2
       text.col = c("black","blue")) + title('March')

RoeDeer<- filter(data_wsnow2, validated_species  %in% c("raadyr"))
RoeDeer <- RoeDeer[RoeDeer$month == '04',]
roedeer_oc <- RoeDeer[RoeDeer$treecover < 50,]
roedeer_cc <- RoeDeer[RoeDeer$treecover > 50,]
overlapPlot(roedeer_oc$sun.time, roedeer_cc$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n", ylim = c(0,0.16),
            linewidth = c(3,3), olapol=FALSE, rug= TRUE,
            extend= NULL, n.gird= 100,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=.8, #
       c("Roe Deer OC (n=122)", "Roe Deer CC (n=141)"),
       lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.1, #0.2
       text.col = c("black","blue")) + title('April')

RoeDeer<- filter(data_wsnow2, validated_species  %in% c("raadyr"))
RoeDeer <- RoeDeer[RoeDeer$month == '05',]
roedeer_oc <- RoeDeer[RoeDeer$treecover < 50,]
roedeer_cc <- RoeDeer[RoeDeer$treecover > 50,]
overlapPlot(roedeer_oc$sun.time, roedeer_cc$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n", ylim = c(0,0.16),
            linewidth = c(3,3), olapol=FALSE, rug= TRUE,
            extend= NULL, n.gird= 100,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=.8, #
       c("Roe Deer OC (n=3)", "Roe Deer CC (n=3)"),
       lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.1, #0.2
       text.col = c("black","blue")) + title('May')

#### if we want to normalize the variables:
### just compare two histograms






# Plotting
RoeDeer<- filter(data_wsnow2, validated_species  %in% c("raadyr"))
RoeDeer <- RoeDeer[RoeDeer$month == '03',]

roedeer_oc <- RoeDeer[RoeDeer$treecover < 50,]
roedeer_cc <- RoeDeer[RoeDeer$treecover > 50,]
hist(roedeer_oc$sun.time) #, scaled = TRUE), main = "Density Plot of Two Populations", xlab = "Value", col = "blue", lwd = 2, ylim = c(0, 0.2))
hist(roedeer_cc$sun.time ) #, scaled=TRUE), col = "red", lwd = 2)
#legend("topright", legend = c("Population 1", "Population 2"), col = c("blue", "red"), lty = 1, lwd = 2, cex = 0.8)

# Create histograms
hist(roedeer_oc$sun.time, col = "blue", main = "Comparison of Two Histograms",  ylim =range(c(0, roedeer_cc$sun.time)), xlab = "Value")
hist(roedeer_cc$sun.time, col = "red", add = TRUE)

# Add legend
legend("topright", legend = c("Population 1", "Population 2"), fill = c("blue", "red"))




######### LYNX ##############
Lynx<- filter(data_wsnow2, validated_species %in% c("gaupe"))
Lynx <- Lynx[Lynx$month == '02',]
lynx_oc <- Lynx[Lynx$treecover < 50,]
lynx_cc <- Lynx[Lynx$treecover > 50,]

## for february only bc only one row of data so we artifically inflate it
# roedeer_oc <- bind_rows(roedeer_oc, slice(roedeer_oc, 1))
# roedeer_oc <- bind_rows(roedeer_oc, slice(roedeer_oc, 2))
# roedeer_oc$sun.time[2] <- roedeer_oc$sun.time[2]+0.1

# Set up a 2x2 layout
par(mfrow=c(2,2))
Lynx <- Lynx[Lynx$month == '02',]
lynx_oc <- Lynx[Lynx$treecover < 50,]
lynx_cc <- Lynx[Lynx$treecover > 50,]
overlapPlot(lynx_oc$sun.time, lynx_cc$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n", ylim = c(0,0.16),
            linewidth = c(3,3), olapol=FALSE, rug= TRUE,
            extend= NULL, n.gird= 100,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=.8, #
       c("Lynx OC (n=2)", "Lynx CC (n=38)"),
       lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.2, #0.2
       text.col = c("black","blue")) + title('Feburary')

Lynx<- filter(data_wsnow2, validated_species %in% c("gaupe"))
Lynx <- Lynx[Lynx$month == '03',]
lynx_oc <- Lynx[Lynx$treecover < 50,]
lynx_cc <- Lynx[Lynx$treecover > 50,]
overlapPlot(lynx_oc$sun.time, lynx_cc$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n", ylim = c(0,0.16),
            linewidth = c(3,3), olapol=FALSE, rug= TRUE,
            extend= NULL, n.gird= 100,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=.8, #
       c("Lynx OC (n=4)", "Lynx CC (n=38)"),
       lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.2, #0.2
       text.col = c("black","blue")) + title('March')

Lynx<- filter(data_wsnow2, validated_species %in% c("gaupe"))
Lynx <- Lynx[Lynx$month == '04',]
lynx_oc <- Lynx[Lynx$treecover < 50,]
lynx_cc <- Lynx[Lynx$treecover > 50,]
overlapPlot(lynx_oc$sun.time, lynx_cc$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n",
            linewidth = c(3,3), olapol=FALSE, rug= TRUE,
            extend= NULL, n.gird= 100, ylim = c(0,0.16),
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=.8, #
       c("Lynx OC (n=3)", "Lynx CC (n=14)"),
       lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.2, #0.2
       text.col = c("black","blue")) + title('April')





## no observations for lynx in May
# Lynx<- filter(data_wsnow2, validated_species %in% c("gaupe"))
# Lynx <- Lynx[Lynx$month == '05',]
# lynx_oc <- Lynx[Lynx$treecover < 50,]
# lynx_cc <- Lynx[Lynx$treecover > 50,]
# overlapPlot(lynx_oc$sun.time, lynx_cc$sun.time, xcenter= "noon", linetype= c(1,1),
#             linecol= c("black","blue"),
#             xaxt="n",
#             linewidth = c(3,3), olapol=FALSE, rug= TRUE,
#             extend= NULL, n.gird= 100,
#             adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
# axis(1, at=c(0, 6, 12, 18, 24),
#      labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
# abline(v=c(6, 18), lty=3)
# legend(8,0.10,cex=.8, #
#        c("Lynx OC (n=3)", "Lynx CC (n=14)"),
#        lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.2, #0.2
#        text.col = c("black","blue")) + title('May')








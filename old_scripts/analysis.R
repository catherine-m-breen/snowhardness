## titled as moreScandcamData.R in R directory 




library(tidyverse)
library(tidyr)
library(lubridate)
library(lutz)
library(overlap)
# library(maptools)
library(leaflet)
library(dplyr)
library(ggplot2)
# library(spdplyr)
library(astroFns) # clock times to radians function
library(suncalc)
library(hms)
library(ggpubr)
library(rstatix)
library(sf)
library(sp)
source("/Users/catherinebreen/Dropbox/Chapter3/utilityFunctions.R")





datamerge <- function(data, metadata){
  metadata$LokalitetID <- as.factor(metadata$LokalitetID)
  joined_tibble <- left_join(data, metadata, by = c("location_id" = "LokalitetID"))
  return(joined_tibble)
}

##
dat1 <- readRDS("/Users/catherinebreen/Dropbox/Chapter3/Neri_data/DataKatie.rds")
head(dat1)

dat2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/Neri_data/LocationsKatie.rds')
head(dat2)
unique(dat2$LokalitetID)
dat2$Latitude <-  st_coordinates(dat2$geometry)[, "Y"]
dat2$Longitude <- st_coordinates(dat2$geometry)[, "X"]
head(dat2)
dat2 <- dat2 %>% st_drop_geometry()
class(dat2)

## put in string format so that we don't have issues with rounding errors
dat2$Latitude <- as.character(dat2$Latitude)
dat2$Longitude <- as.character(dat2$Longitude)

#write.csv(dat2, 'LocationsKatie2.csv')

data3 <- datamerge(data=dat1,metadata=dat2)
head(data3)

dat2[dat2$Latitude == '59.7521741752405',]
data3[data3$Latitude == '59.7521741752405',]

data3$LatitudeNum <- as.numeric(data3$Latitude)
data3$LongitudeNum <- as.numeric(data3$Longitude)


data3 <- data3[c('location_id','captured_at','validated_species','num_animals','datetime','Latitude',"Longitude", "LatitudeNum", 'LongitudeNum')]
head(data3)

###
# write.csv(data3, 'NINA_cameraData_wLocationID_mar11up.csv')
# saveRDS(data3, file = "NINA_cameraData_wLocationID_mar11upRDS.rds")

# keep only image in a picture series
# data3 <- distinct(data3, timeserie_id, .keep_all = TRUE)

# keep only data with coordinates
Cams_woCoordinates <- data3[is.na(data3$Latitude), ] ### all the values that don't have coordinates, only have 1 photo. They are probably a test of some sort?
length(table(Cams_woCoordinates$location_id)) ## 451 cameras without coordinates

data3 <-  data3[!is.na(data3$Latitude), ]
head(data3)
data <- data3
## if just want a small section
#data <- data[data$Latitude > 60,]
data <- data[data$LatitudeNum < 64,]


####### Plot lat/long #########
# # plotting the map with some points on it

cameras <- distinct(data, Latitude, .keep_all = TRUE)

ggplot() +
  geom_point(data = cameras, aes(x = LongitudeNum, y = LatitudeNum, fill = "red", alpha = 0.8), size = 4, shape = 21)

# #Removed 451 rows containing missing values (geom_point). 451 cameras are missing coordinates!!
###############################

# convert to captured_at_exif to datetime and find the min and max dates
data$datetime <- as.POSIXct(data$captured_at, '%Y:%m:%d %H:%M:%S', tz = "Europe/Oslo") ## the timezone is EDT but this wrong

## find NAs

first(sort(data$datetime)) ## 2017-01-01
last(sort(data$datetime)) ## 2024-01-31

#as.POSIXct(data3$captured_at_exif, '%Y:%m:%d %H:%M:%S', tz = "Europe/Oslo")
rownames(data) <- NULL
data[is.na(data$datetime), ] ## For some reason 34 didn't convert properly

###
data$captured_at[15753]
as.POSIXct(data$captured_at[15753], '%Y:%m:%d %H:%M:%S', tz = "Europe/Oslo") #
### we will skip for now because they are mostly nothing and grevling
### inspect these values in the original data sheet...]
data <- data[!is.na(data$captured_at), ] ## For some reason 34 didn't convert properly

## set up just a date column and a month column
data$date <- format(data$datetime, "%Y-%m-%d")
data$month <- format(data$datetime, "%m")
data$year <- format(data$datetime, "%Y")
data$hour <- format(data$datetime, '%H')

## we just want spring values so we are going to filter out any month that's not F, M, A, M
data <- data[data$month %in% c('02', '03', '04', '05'), ]
data <- data[data$validated_species != 'nothing',]


## plot one more time
# ggplot() +
#   geom_point(data = data, aes(x = data$Longitude, y = data$Latitude, fill = "red", alpha = 0.8), size = 4, shape = 21)

## check the max and min dates again
first(sort(data$datetime)) ## 2015 # 2017
last(sort(data$datetime)) ## 2021 #2023


############## COVARIATES ##############
## snow depth
senorge <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/senorge_spring_2015_2024_updloc.csv')
## temperature
cross0df <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/minmaxavg_temp_senorge_CAMERAS.csv')
cross0df

# senorge_avg_temp <-
# senorge_avgdensity <-
##  human density
# ask inger maren about and john

## now match with all the senorge data we have
# senorge <- read.csv("/Users/catherinebreen/Documents/Chapter3/data/senorge_spring_2015_2024_upd.csv") #/senorge_spring_2015_2021.csv
head(senorge)
# set up columns to match data for the merge
senorge$location_id <- senorge$loc
senorge$date <- as.Date(senorge$date1)
data$date <- as.Date(data$date)
data$location_id <- as.numeric(data$location_id)

#data1 <- data[!is.na(data$date),]

head(senorge)
head(data)

## drop na values
data <- data[!is.na(data$location_id),]

## testing
senorge[senorge$location_id == 22,]
test <- data[data$location_id == 22,]

head(data)
head(senorge)
data_wsnow <- dplyr::left_join(data, senorge, by = c('location_id', 'date'), unmatched = "drop", relationship = "many-to-many")
head(data_wsnow)

####### avg temperature & and swe (for density) ######
swe <- read.csv("/Users/catherinebreen/Dropbox/Chapter3/data/swe_senorge_spring_2015_2024_updloc.csv")
swe$location_id <- swe$loc
swe$date <- as.Date(swe$date1)
swe <- swe[c('location_id','swe','date')]
head(swe)

data_wsnowswe <- dplyr::left_join(data_wsnow, swe, by = c('location_id','date'), unmatched = "drop")
head(data_wsnowswe)

avgtemp <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/avg_temp_senorge_CAMERAS.csv')
avgtemp$location_id <- avgtemp$loc
avgtemp$date <- as.Date(avgtemp$date1)
avgtemp <- avgtemp[c('location_id','avg.temp','date')]
head(avgtemp)

data_wsnowswetemp <- dplyr::left_join(data_wsnowswe, avgtemp, by = c('location_id','date'), unmatched = "drop")
head(data_wsnowswetemp)

######### now do the tree canopy cover ### ** could update this
############## separated by species and by canopy cover #############
# head(treecanopy)
# treecanopy$treecover <- treecanopy$SAMPLE_treecanopycover1
# treecanopy$location_id <- treecanopy$LokalitetID
# head(treecanopy)
# treecanopy <- treecanopy[, c("location_id", "treecover")] ## this is 2018!!!
# head(treecanopy)

#data_wsnowtree <- dplyr::left_join(data_wsnow, treecanopy, by = c('location_id'), unmatched = "drop")
## updated tree info:
treec <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/treecovariates.rds')
head(treec)
# data_wsnow <- as_tibble(data_wsnow)
# head(data_wsnow)
head(data_wsnowswetemp)
head(treec)
data_wsnowtree <- dplyr::left_join(data_wsnowswetemp, treec, by = c('location_id','year'), unmatched = "drop")
head(data_wsnowtree)


###### add cross0 #####
head(cross0df)
cross0df$location_id <- cross0df$loc
cross0df$date <- as.Date(cross0df$date1)
head(cross0df)
cross0df <- cross0df[, c("location_id", 'date', "cross_0")] ##
data_wsnowtreecross0 <- dplyr::left_join(data_wsnowtree, cross0df, by = c('location_id', 'date'), unmatched = "drop")
head(data_wsnowtreecross0)

data_main <- data_wsnowtreecross0[!is.na(data_wsnowtreecross0$validated_species),]

############ INSERT HUMAN POPULATION DENSITY FOR NORWAY ################
## units are number of persons per sq km ######
humanDensity <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/humanDensityNorway_GEE.csv')
humanDensity$humandens <- humanDensity$SAMPLE_humanPop1
humanDensity$location_id <- humanDensity$LokalitetID
humanDensity <- humanDensity[, c("location_id", "humandens")] ## this is 2018!!!
head(humanDensity)
mean(na.omit(humanDensity$humandens))

data_main <- dplyr::left_join(data_main, humanDensity, by = c('location_id'), unmatched = "drop")

data_main$density <- data_main$swe / data_main$snowdepth.mm *100
hist(data_main$density)

saveRDS(data_main, file = "data_main.rds")
head(data_main)

#########################################
## predator density covariate
# Assuming data_main is your data frame with columns location_id, date, and validated_species


# Initialize an empty list to store results
pred_per_day_density <- list()

# Get unique dates
dates <- unique(as.character(data_main$date))

# Loop through each date
# for (i in seq(1, length(dates), 1)) {
for (d in dates) {
  #d <- as.character(dates[i])
  # Filter data for the current date and specific species
  df <- data_main %>%
    filter(date == d & validated_species %in% c('ulv', 'gaupe', 'bjorn'))

  # Get unique location IDs for the current date
  cams <- unique(df$location_id)

  # If there are cameras for the current date
  if (length(cams) != 0) {
    for (c in cams) {
      # Append the results to the list
      pred_per_day_density <- append(pred_per_day_density, list(c(d, c, nrow(df))))
    }
  } else {
    # Append the results to the list with zero density
    pred_per_day_density <- append(pred_per_day_density, list(c(d, NA, 0)))
  }
}

# Convert the list to a data frame
pred_per_day_density <- do.call(rbind, pred_per_day_density)
pred_per_day_density <- as.data.frame(pred_per_day_density, stringsAsFactors = FALSE)
colnames(pred_per_day_density) <- c("date", "location_id", "density")

# Convert appropriate columns to numeric
pred_per_day_density$density <- as.numeric(pred_per_day_density$density)


print(pred_per_day_density)



# Display the pred_density vector
print(pred_density)


####################################################################################################

############ SUNTIME #######################################################################################

### this seems to be the only way I can get the timezone to work
data_main$Time <- format(data_main$datetime, "%H:%M:%S")
data_main$rads <- astroFns::hms2rad(data_main$Time)

#data4$date <- as.POSIXct(data4$captured_at_exif,  format="%Y:%m:%d")

###### Convert dates to a POSIXct object with the right time zone (European/Oslo):
## put this in your timezone
#Dates<- as.POSIXct(data_wsnow$captured_at_exif, tz = "data$timeZone")

##### Create a SpatialPoints object with the location ######
data_main <- data_main[complete.cases(data_main$Longitude),]
data_main <- data_main[complete.cases(data_main$Latitude),]
print(length(unique(data_main$date)))
coords <- data_main[,c("LongitudeNum", "LatitudeNum")]
#Coords_SPDF <- SpatialPointsDataFrame(coords, proj4string = CRS("+init=epsg:4269 +proj=longlat +datum=NAD83+zone=33"), data = data_wsnow)
Coords_SPDF <- SpatialPointsDataFrame(coords, proj4string = CRS("+init=epsg:4269 +proj=longlat +datum=NAD83"), data = data_main)

##### convert times to suntime ######
## this is a function in Overlap packaage
data_main$timeZone <- lutz::tz_lookup_coords(lat=data_main$LatitudeNum, lon = data_main$LongitudeNum, warn = FALSE, method = "accurate")
Dates<- as.POSIXct(data_main$datetime, tz = "data_main$timeZone")
data_main$sun.time <- sunTime(data_main$rads, Dates, Coords_SPDF)

data_main <- data_main[complete.cases(data_main$sun.time),]
data_main_test <- data_main %>% drop_na()#data_main[!is.na(data_main$sun.time),]
head(data_main_test)
## sample sunTime comparison, example 121
### ALL SPECIES
par(mfrow=2:1)
densityPlot(data_main_test$sun.time, col='red', lwd=2,  main="Sun time")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
densityPlot(data_main_test$rads, lwd=2, main="Clock time")
par(mfrow=c(1,1))




######### ROE DEER ##############
RoeDeer<- filter(data_main_test, validated_species  %in% c("raadyr"))

## cross 0
RoeDeer<- filter(RoeDeer, cross_0  %in% c(1))

roedeer_s <- RoeDeer[RoeDeer$snowdepth.mm > 40,]
roedeer_ns <- RoeDeer[RoeDeer$snowdepth.mm < 40,]

roedeer_oc <- RoeDeer[RoeDeer$treecover < 50,]
roedeer_cc <- RoeDeer[RoeDeer$treecover > 50,]

roedeer_oc_es <- roedeer_oc[(roedeer_oc$month == '02') | (roedeer_oc$month == '03'),]
roedeer_cc_es <- roedeer_cc[(roedeer_cc$month == '02') | (roedeer_cc$month == '03'),]

roedeer_oc_ls <- roedeer_oc[(roedeer_oc$month == '04') | (roedeer_oc$month == '05'),]
roedeer_cc_ls <- roedeer_cc[(roedeer_cc$month == '04') | (roedeer_cc$month == '05'),]

roedeer_oc_es_s <- roedeer_oc_es[roedeer_oc_es$snowdepth.mm > 30,]
roedeer_cc_es_s <- roedeer_cc_es[roedeer_cc_es$snowdepth.mm > 30,]

roedeer_oc_es_ns <- roedeer_oc_es[roedeer_oc_es$snowdepth.mm < 30,]
roedeer_cc_es_ns <- roedeer_cc_es[roedeer_cc_es$snowdepth.mm < 30,]

roedeer_oc_ls_s <- roedeer_oc_ls[roedeer_oc_ls$snowdepth.mm > 30,]
roedeer_cc_ls_s <- roedeer_cc_ls[roedeer_cc_ls$snowdepth.mm > 30,]

roedeer_oc_ls_ns <- roedeer_oc_ls[roedeer_oc_ls$snowdepth.mm < 30,]
roedeer_cc_ls_ns <- roedeer_cc_ls[roedeer_cc_ls$snowdepth.mm < 30,]

roedeer_oc_es_s_hh <- roedeer_oc_es_s[roedeer_oc_es_s$humandens > 100,]
roedeer_oc_es_ns_hh <- roedeer_oc_es_ns[roedeer_oc_es_ns$humandens > 100,]
roedeer_cc_es_s_hh <- roedeer_cc_es_s[roedeer_cc_es_s$humandens > 100,]
roedeer_cc_es_ns_hh <- roedeer_cc_es_ns[roedeer_cc_es_ns$humandens > 100,]
roedeer_oc_ls_ns_hh <- roedeer_oc_ls_s[roedeer_oc_ls_s$humandens > 100,]
roedeer_cc_ls_ns_hh <- roedeer_cc_ls_s[roedeer_cc_ls_s$humandens > 100,]

roedeer_oc_es_s_lh <- roedeer_oc_es_s[roedeer_oc_es_s$humandens < 100,]
roedeer_oc_es_ns_lh <- roedeer_oc_es_ns[roedeer_oc_es_ns$humandens < 100,]
roedeer_cc_es_s_lh <- roedeer_cc_es_s[roedeer_cc_es_s$humandens < 100,]
roedeer_oc_ls_ns_lh <- roedeer_oc_ls_s[roedeer_oc_ls_s$humandens < 100,]
roedeer_cc_ls_ns_lh <- roedeer_cc_ls_s[roedeer_cc_ls_s$humandens < 100,]

plot.new()
smoother = 100
#par(mfrow=c(1,2))
############# NOT WORKING
RoeDeer<- filter(RoeDeer, cross_0  %in% c(1))
roedeer_ns <- RoeDeer %>%
  filter(snowdepth.mm < 50 & month %in% c('02', '03', '04', '05') )
roedeer_s <- RoeDeer %>%
  filter(snowdepth.mm > 50 & month %in% c('02', '03', '04', '05') )

overlapPlot(roedeer_ns$sun.time, roedeer_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("orange","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("NO SNOW (< 50 cm)", "SNOW (> 50 cm)"),
       lty=c(1, 1), col=c("orange", "blue"),
       text.col = c("orange","blue")) + title('Roe Deer Winter (February to May) Movement')
overlapEst(roedeer_s$sun.time, roedeer_ns$sun.time)
ks.test(roedeer_s$sun.time, roedeer_ns$sun.time)


## no snow control
RoeDeer<- filter(RoeDeer, cross_0  %in% c(1))

roedeer_cc_ns <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover > 50 & month %in% c('02', '03', '04', '05') )
roedeer_oc_ns <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover < 50 & month %in% c('02', '03', '04', '05') )
overlapPlot(roedeer_cc_ns$sun.time, roedeer_oc_ns$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('NO SNOW (< 40CM)')
overlapEst(roedeer_cc_ns$sun.time, roedeer_oc_ns$sun.time)

roedeer_cc_s <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover > 50 & month %in% c('02', '03', '04', '05') )
roedeer_oc_s <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover < 50 & month %in% c('02', '03', '04', '05') )
overlapPlot(roedeer_cc_s$sun.time, roedeer_oc_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('SNOW (> 40 CM)')
overlapEst(roedeer_cc_s$sun.time, roedeer_oc_s$sun.time)

# Calculate kernel density estimates for each dataset
density_ocns <- densityFit(sort(roedeer_oc_ns$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ocs <- densityFit(sort(roedeer_oc_s$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccns <- densityFit(sort(roedeer_cc_ns$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccs <- densityFit(sort(roedeer_cc_s$sun.time), seq(0, 2*pi, length=556), bw=10)

# Plot one of the density estimates
plot(seq(0, 2*pi, length=556), density_ocns, type = "l", col = "darkblue", lty=5,
     xlab = "Sun Time", ylab = "Density Estimate", xaxt = "n")
# Add the other density estimate to the same plot
lines(seq(0, 2*pi, length=556), density_ocs, col = "darkblue", lwd=5)
lines(seq(0, 2*pi, length=556), density_ccns, col = "pink", lty=5)
lines(seq(0, 2*pi, length=556), density_ccs, col = "pink", lwd=5)
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(pi/2, 3*pi/2), lty=3)

legend("topright", legend=c("OPEN NO SNOW", "OPEN SNOW", "CLOSED NO SNOW", "CLOSED SNOW"),
       col=c("darkblue", "darkblue", "pink", "pink"), lty=c(5, 1, 5, 1), lwd=c(1, 5, 1, 5))

ks.test(roedeer_cc_ns$sun.time, roedeer_oc_ns$sun.time)
ks.test(roedeer_cc_s$sun.time, roedeer_oc_s$sun.time)
ks.test(roedeer_cc_ns$sun.time, roedeer_cc_s$sun.time)
ks.test(roedeer_oc_s$sun.time, roedeer_oc_ns$sun.time)

# Add a legend


###############
roedeer_cc_es_ns <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover > 50 & month %in% c('02', '03') )
roedeer_oc_es_ns <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover < 50 & month %in% c('02', '03'))
overlapPlot(roedeer_cc_es_ns$sun.time, roedeer_oc_es_ns$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('Early Season NO SNOW (< 40CM)')
overlapEst(roedeer_cc_es_ns$sun.time, roedeer_oc_es_ns$sun.time)

roedeer_cc_es_s <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover > 50 & month %in% c('02', '03') )
roedeer_oc_es_s <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover < 50 & month %in% c('02', '03'))
overlapPlot(roedeer_cc_es_s$sun.time, roedeer_oc_es_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('Early Season SNOW (> 40CM)')
overlapEst(roedeer_cc_es_s$sun.time, roedeer_oc_es_s$sun.time)

density_ocns <- densityFit(sort(roedeer_oc_es_ns$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ocs <- densityFit(sort(roedeer_oc_es_s$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccns <- densityFit(sort(roedeer_cc_es_ns$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccs <- densityFit(sort(roedeer_cc_es_s$sun.time), seq(0, 2*pi, length=556), bw=10)

# Plot one of the density estimates
plot(seq(0, 2*pi, length=556), density_ocns, type = "l", col = "darkblue", lty=5,
     xlab = "Sun Time", ylab = "Density Estimate", xaxt = "n", ylim = c(0, 0.40))
# Add the other density estimate to the same plot
lines(seq(0, 2*pi, length=556), density_ocs, col = "darkblue", lwd=5)
lines(seq(0, 2*pi, length=556), density_ccns, col = "pink", lty=5)
lines(seq(0, 2*pi, length=556), density_ccs, col = "pink", lwd=5)
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(pi/2, 3*pi/2), lty=3)
ylim(0,0.30)
legend("topright", legend=c("OPEN NO SNOW", "OPEN SNOW", "CLOSED NO SNOW", "CLOSED SNOW"),
       col=c("darkblue", "darkblue", "pink", "pink"), lty=c(5, 1, 5, 1), lwd=c(1, 5, 1, 5))

ks.test(roedeer_cc_es_ns$sun.time, roedeer_oc_es_ns$sun.time)
ks.test(roedeer_cc_es_s$sun.time, roedeer_oc_es_s$sun.time)
ks.test(roedeer_cc_es_ns$sun.time, roedeer_cc_es_s$sun.time)
ks.test(roedeer_oc_es_s$sun.time, roedeer_oc_es_ns$sun.time)


###################

roedeer_cc_ls_ns <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover > 50 & month %in% c('04', '05') )
roedeer_oc_ls_ns <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover < 50 & month %in% c('04', '05'))
overlapPlot(roedeer_cc_ls_ns$sun.time, roedeer_oc_ls_ns$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('Late Season NO SNOW (< 40CM)')
overlapEst(roedeer_cc_ls_ns$sun.time, roedeer_oc_ls_ns$sun.time)

roedeer_cc_ls_s <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover > 50 & month %in% c('04', '05') )
roedeer_oc_ls_s <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover < 50 & month %in% c('04', '05'))
overlapPlot(roedeer_cc_ls_s$sun.time, roedeer_oc_ls_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('Late Season SNOW (> 40CM)')
overlapEst(roedeer_cc_ls_s$sun.time, roedeer_oc_ls_s$sun.time)


density_ocns <- densityFit(sort(roedeer_oc_ls_ns$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ocs <- densityFit(sort(roedeer_oc_ls_s$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccns <- densityFit(sort(roedeer_cc_ls_ns$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccs <- densityFit(sort(roedeer_cc_ls_s$sun.time), seq(0, 2*pi, length=556), bw=10)

# Plot one of the density estimates
plot(seq(0, 2*pi, length=556), density_ocns, type = "l", col = "darkblue", lty=5,
     xlab = "Sun Time", ylab = "Density Estimate", xaxt = "n", ylim = c(0, 0.60))
# Add the other density estimate to the same plot
lines(seq(0, 2*pi, length=556), density_ocs, col = "darkblue", lwd=5)
lines(seq(0, 2*pi, length=556), density_ccns, col = "pink", lty=5)
lines(seq(0, 2*pi, length=556), density_ccs, col = "pink", lwd=5)
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(pi/2, 3*pi/2), lty=3)
ylim(0,0.30)
legend("topright", legend=c("OPEN NO SNOW", "OPEN SNOW", "CLOSED NO SNOW", "CLOSED SNOW"),
       col=c("darkblue", "darkblue", "pink", "pink"), lty=c(5, 1, 5, 1), lwd=c(1, 5, 1, 5))

ks.test(roedeer_cc_ls_ns$sun.time, roedeer_oc_ls_ns$sun.time)
ks.test(roedeer_cc_ls_s$sun.time, roedeer_oc_ls_s$sun.time)
ks.test(roedeer_cc_ls_ns$sun.time, roedeer_cc_ls_s$sun.time)
ks.test(roedeer_oc_ls_s$sun.time, roedeer_oc_ls_ns$sun.time)
#######

roedeer_cc_s_lh <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover > 50 & month %in% c('02', '03', '04', '05') & humandens  < 50)
roedeer_oc_s_lh <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover < 50 & month %in% c('02', '03', '04', '05') & humandens  < 50)
overlapPlot(roedeer_cc_s_lh$sun.time, roedeer_oc_s_lh$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('SNOW (> 40CM) Humans (< 50/km2)')
overlapEst(roedeer_cc_s_lh$sun.time, roedeer_oc_s_lh$sun.time)

roedeer_cc_ns_lh <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover > 50 & month %in% c('02', '03', '04', '05') & humandens  < 50)
roedeer_oc_ns_lh <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover < 50 & month %in% c('02', '03', '04', '05') & humandens  < 50)
overlapPlot(roedeer_cc_ns_lh$sun.time, roedeer_oc_ns_lh$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('NO SNOW (< 40CM) Humans (< 50/km2)')
overlapEst(roedeer_cc_ns_lh$sun.time, roedeer_oc_ns_lh$sun.time)

density_ocns <- densityFit(sort(roedeer_oc_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ocs <- densityFit(sort(roedeer_oc_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccns <- densityFit(sort(roedeer_cc_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccs <- densityFit(sort(roedeer_cc_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)

# Plot one of the density estimates
plot(seq(0, 2*pi, length=556), density_ocns, type = "l", col = "darkblue", lty=5,
     xlab = "Sun Time", ylab = "Density Estimate", xaxt = "n", ylim = c(0, 0.60))
# Add the other density estimate to the same plot
lines(seq(0, 2*pi, length=556), density_ocs, col = "darkblue", lwd=5)
lines(seq(0, 2*pi, length=556), density_ccns, col = "pink", lty=5)
lines(seq(0, 2*pi, length=556), density_ccs, col = "pink", lwd=5)
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(pi/2, 3*pi/2), lty=3)
ylim(0,0.30)
legend("topright", legend=c("OPEN NO SNOW", "OPEN SNOW", "CLOSED NO SNOW", "CLOSED SNOW"),
       col=c("darkblue", "darkblue", "pink", "pink"), lty=c(5, 1, 5, 1), lwd=c(1, 5, 1, 5))

ks.test(roedeer_oc_ns_lh$sun.time, roedeer_cc_ns_lh$sun.time)
ks.test(roedeer_oc_ns_lh$sun.time, roedeer_oc_s_lh$sun.time)
ks.test(roedeer_cc_ns_lh$sun.time, roedeer_cc_s_lh$sun.time)
ks.test(roedeer_oc_s_lh$sun.time, roedeer_cc_s_lh$sun.time)

###############


roedeer_cc_s_hh <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover > 50 & month %in% c('02', '03', '04', '05') & humandens.x  > 50)
roedeer_oc_s_hh <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover < 50 & month %in% c('02', '03', '04', '05') & humandens.x  > 50)
overlapPlot(roedeer_cc_s_hh$sun.time, roedeer_oc_s_hh$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('SNOW (> 40CM) Humans (> 50/km2)')
overlapEst(roedeer_cc_s_hh$sun.time, roedeer_oc_s_hh$sun.time)

roedeer_cc_ns_hh <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover > 50 & month %in% c('02', '03', '04', '05') & humandens.x  > 50)
roedeer_oc_ns_hh <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover < 50 & month %in% c('02', '03', '04', '05') & humandens.x  > 50)
overlapPlot(roedeer_cc_ns_hh$sun.time, roedeer_cc_ns_hh$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('SNOW (> 40CM) Humans (> 50/km2)')
overlapEst(roedeer_cc_ns_hh$sun.time, roedeer_cc_ns_hh$sun.time)

####
## low human only because not enough data for high human density / probs because snow depths are high

roedeer_cc_es_s_lh <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover > 50 & month %in% c('02', '03') & humandens.x  < 50)
roedeer_oc_es_s_lh <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover < 50 & month %in% c('02', '03') & humandens.x  < 50)
overlapPlot(roedeer_cc_es_s_lh$sun.time, roedeer_oc_es_s_lh$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('Early Season SNOW (> 40CM) Humans (< 50/km2)')
overlapEst(roedeer_cc_es_s_lh$sun.time, roedeer_oc_es_s_lh$sun.time)

roedeer_cc_es_ns_lh <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover > 50 & month %in% c('02', '03') & humandens  < 50)
roedeer_oc_es_ns_lh <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover < 50 & month %in% c('02', '03') & humandens  < 50)
overlapPlot(roedeer_cc_es_ns_lh$sun.time, roedeer_oc_es_ns_lh$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('Early Season SNOW (> 40CM) Humans (< 50/km2)')
overlapEst(roedeer_cc_es_ns_lh$sun.time, roedeer_oc_es_ns_lh$sun.time)

density_ocns <- densityFit(sort(roedeer_oc_es_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ocs <- densityFit(sort(roedeer_oc_es_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccns <- densityFit(sort(roedeer_cc_es_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccs <- densityFit(sort(roedeer_cc_es_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)

# Plot one of the density estimates
plot(seq(0, 2*pi, length=556), density_ocns, type = "l", col = "darkblue", lty=5,
     xlab = "Sun Time", ylab = "Density Estimate", xaxt = "n", ylim = c(0, 0.60))
# Add the other density estimate to the same plot
lines(seq(0, 2*pi, length=556), density_ocs, col = "darkblue", lwd=5)
lines(seq(0, 2*pi, length=556), density_ccns, col = "pink", lty=5)
lines(seq(0, 2*pi, length=556), density_ccs, col = "pink", lwd=5)
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(pi/2, 3*pi/2), lty=3)
ylim(0,0.30)
legend("topright", legend=c("OPEN NO SNOW", "OPEN SNOW", "CLOSED NO SNOW", "CLOSED SNOW"),
       col=c("darkblue", "darkblue", "pink", "pink"), lty=c(5, 1, 5, 1), lwd=c(1, 5, 1, 5))

ks.test(roedeer_oc_es_ns_lh$sun.time, roedeer_cc_es_ns_lh$sun.time)
ks.test(roedeer_oc_es_s_lh$sun.time, roedeer_cc_es_s_lh$sun.time)
ks.test(roedeer_oc_es_s_lh$sun.time, roedeer_oc_es_ns_lh$sun.time)
ks.test(roedeer_cc_es_s_lh$sun.time, roedeer_cc_es_ns_lh$sun.time)

#####
## low human / late season

roedeer_cc_ls_s_lh <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover > 50 & month %in% c('04', '05') & humandens  < 50)
roedeer_oc_ls_s_lh <- RoeDeer %>%
  filter(snowdepth.mm > 40 & treecover < 50 & month %in% c('04', '05') & humandens  < 50)
overlapPlot(roedeer_cc_ls_s_lh$sun.time, roedeer_oc_ls_s_lh$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('Late Season SNOW (> 40CM) Humans (< 50/km2)')
overlapEst(roedeer_cc_ls_s_lh$sun.time, roedeer_oc_ls_s_lh$sun.time)

roedeer_cc_ls_ns_lh <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover > 50 & month %in% c('04', '05') & humandens  < 50)
roedeer_oc_ls_ns_lh <- RoeDeer %>%
  filter(snowdepth.mm < 40 & treecover < 50 & month %in% c('04', '05') & humandens  < 50)
overlapPlot(roedeer_cc_ls_ns_lh$sun.time, roedeer_oc_ls_ns_lh$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("forestgreen","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("CLOSED", "OPEN"),
       lty=c(1, 1), col=c("forestgreen", "blue"),
       text.col = c("forestgreen","blue")) + title('Late Season NO SNOW (< 40CM) Humans (< 50/km2)')
overlapEst(roedeer_cc_ls_ns_lh$sun.time, roedeer_oc_ls_ns_lh$sun.time)

density_ocns <- densityFit(sort(roedeer_oc_ls_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ocs <- densityFit(sort(roedeer_oc_ls_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccns <- densityFit(sort(roedeer_cc_ls_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccs <- densityFit(sort(roedeer_cc_ls_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)

# Plot one of the density estimates
plot(seq(0, 2*pi, length=556), density_ocns, type = "l", col = "darkblue", lty=5,
     xlab = "Sun Time", ylab = "Density Estimate", xaxt = "n", ylim = c(0, 0.60))
# Add the other density estimate to the same plot
lines(seq(0, 2*pi, length=556), density_ocs, col = "darkblue", lwd=5)
lines(seq(0, 2*pi, length=556), density_ccns, col = "pink", lty=5)
lines(seq(0, 2*pi, length=556), density_ccs, col = "pink", lwd=5)
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(pi/2, 3*pi/2), lty=3)
ylim(0,0.30)
legend("topright", legend=c("OPEN NO SNOW", "OPEN SNOW", "CLOSED NO SNOW", "CLOSED SNOW"),
       col=c("darkblue", "darkblue", "pink", "pink"), lty=c(5, 1, 5, 1), lwd=c(1, 5, 1, 5))

ks.test(roedeer_oc_ls_ns_lh$sun.time, roedeer_cc_ls_ns_lh$sun.time)
ks.test(roedeer_oc_ls_s_lh$sun.time, roedeer_cc_ls_s_lh$sun.time)
ks.test(roedeer_oc_ls_s_lh$sun.time, roedeer_cc_ls_ns_lh$sun.time)
ks.test(roedeer_cc_ls_s_lh$sun.time, roedeer_cc_ls_ns_lh$sun.time)

#######
### early vs. late season
###
density_ocns <- densityFit(sort(roedeer_oc_es_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ocs <- densityFit(sort(roedeer_oc_ls_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccns <- densityFit(sort(roedeer_cc_es_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccs <- densityFit(sort(roedeer_cc_ls_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)

# Plot one of the density estimates
plot(seq(0, 2*pi, length=556), density_ocns, type = "l", col = "darkblue", lty=5,
     xlab = "Sun Time", ylab = "Density Estimate", xaxt = "n", ylim = c(0, 0.60))
# Add the other density estimate to the same plot
lines(seq(0, 2*pi, length=556), density_ocs, col = "darkblue", lwd=5)
lines(seq(0, 2*pi, length=556), density_ccns, col = "pink", lty=5)
lines(seq(0, 2*pi, length=556), density_ccs, col = "pink", lwd=5)
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(pi/2, 3*pi/2), lty=3)
ylim(0,0.30)
legend("topright", legend=c("EARLY/ OPEN", "LATE / OPEN", "EARLY / CLOSED", "LATE / CLOSED"),
       col=c("darkblue", "darkblue", "pink", "pink"), lty=c(5, 1, 5, 1), lwd=c(1, 5, 1, 5))

ks.test(roedeer_oc_es_s_lh$sun.time, roedeer_oc_ls_s_lh$sun.time)
ks.test(roedeer_cc_es_s_lh$sun.time, roedeer_cc_ls_s_lh$sun.time)

## no snow
density_ocns <- densityFit(sort(roedeer_oc_es_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ocs <- densityFit(sort(roedeer_oc_ls_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccns <- densityFit(sort(roedeer_cc_es_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ccs <- densityFit(sort(roedeer_cc_ls_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)

# Plot one of the density estimates
plot(seq(0, 2*pi, length=556), density_ocns, type = "l", col = "darkblue", lty=5,
     xlab = "Sun Time", ylab = "Density Estimate", xaxt = "n", ylim = c(0, 0.60))
# Add the other density estimate to the same plot
lines(seq(0, 2*pi, length=556), density_ocs, col = "darkblue", lwd=5)
lines(seq(0, 2*pi, length=556), density_ccns, col = "pink", lty=5)
lines(seq(0, 2*pi, length=556), density_ccs, col = "pink", lwd=5)
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
axis(1, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(pi/2, 3*pi/2), lty=3)
ylim(0,0.30)
legend("topright", legend=c("EARLY/ OPEN", "LATE / OPEN", "EARLY / CLOSED", "LATE / CLOSED"),
       col=c("darkblue", "darkblue", "pink", "pink"), lty=c(5, 1, 5, 1), lwd=c(1, 5, 1, 5))

ks.test(roedeer_oc_es_ns_lh$sun.time, roedeer_oc_ls_ns_lh$sun.time)
ks.test(roedeer_cc_es_ns_lh$sun.time, roedeer_cc_ls_ns_lh$sun.time)



#####
## do we see more detections when the snow is harder?
# sin(sun time) * canopy class * season + cos(sun time) * canopy class * season
data_test <- data_main_test %>% filter(month %in% c('02','03', '04', '05'))  %>%
  mutate(season = ifelse(month %in% c('02', '03'), 0, 1)) %>%
  mutate(cnpyClass = ifelse(treecover < 50, 0, 1))

# predictedsnowhardness <- 0.17*(data_test$season*scale(cos(data_test$sun.time))) + -0.14*(data_test$cnpyClass*data_test$season*scale(cos(data_test$sun.time))) +
#   1.85*(data_test$season*scale(data_test$cnpyClass)) + -2.53*(data_test$season) + -0.3*(scale(sin(data_test$sun.time))*scale(data_test$cnpyClass)) +
#   -0.43*(scale(sin(data_test$sun.time))*data_test$season) + 0.27*scale(sin(data_test$sun.time))*scale(data_test$cnpyClass) + -0.37*scale(sin(data_test$sun.time)) +
#   -0.32*(scale(cos(data_test$sun.time))) + -0.07*scale(data_test$cnpyClass)*scale(cos(data_test$sun.time)) + -1.74*scale(data_test$cnpyClass)
# hist(predictedsnowhardness, breaks=50)
#sun.time
#season
#density
#avg.temp
#treecover (cnpyCvr)

## alternative
model <- readRDS("/Users/catherinebreen/Dropbox/Chapter3/linear_model.rds")
model_tweedie <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/linear_model_tweedie.rds')
bptest(model_tweedie)

## correct for heteroskedascity
library(sandwich)
#install.packages('sandwich')
coeftest(model_tweedie, vcov = vcovHC(model_tweedie, type = "HC1"))


summary(model)
residuals(model)
plot(residuals(model))
plot(density(residuals(model))) #A density plot
qqnorm(resid(model)) # A quantile normal plot - good for checking normality
qqline(resid(model))
library(DHARMa)
testDispersion(model)
simulationOutput <- simulateResiduals(fittedModel = model, plot = F)
plot(simulationOutput)

data_test2 <- na.omit(data_test[c('validated_species','sun.time', 'season', 'cnpyClass', 'cross_0')])
nrow(data_test2)
# Predicts the values with confidence interval
predictions <- stats::predict(model_tweedie, newdata = data_test2, type = 'response') # se.fit = TRUE)
hist(predictions)
data_test2$predictions <- predictions


# Create the histogram with ggplot
ggplot(data_test2, aes(x = predictions)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Predictions",
       x = "Predictions",
       y = "Frequency") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )


data_test2$roedeer_binary <- ifelse(data_test2$validated_species == "raadyr", 1, 0)
data_test2$hare_binary <- ifelse(data_test2$validated_species == "hare", 1, 0)

# Create the jitter plot
ggplot(data_test2, aes(x = predictions, y = roedeer_binary)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5) +
  labs(x = "Snow Hardness Predictions", y = "Roe Deer Binary", title = "Jitter Plot of Predictions vs. Roe Deer Binary") +
  theme_minimal()


# Create the jitter plot
ggplot(data_test2, aes(x = predictions, y = hare_binary)) +
  geom_jitter(width = 0.1, height = 0.1) +
  labs(x = "Snow Hardness Predictions", y = "Hare Binary", title = "Jitter Plot of Predictions vs. Hare Binary") +
  theme_minimal()

hist(as.integer(data_test$predictions))

data_test$predictions <- predictions

saveRDS(data_test, '/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM.RDS')

### overlap
roe_deer_test <- filter(data_test2, validated_species  %in% c("raadyr"))
data_test2_cross0 <- data_test2[data_test2['cross_0'] ==1,]
roe_deer_test <- roe_deer_test[roe_deer_test['cross_0'] ==1,]
hard <- roe_deer_test[roe_deer_test['predictions']<3, ]
soft <- roe_deer_test[roe_deer_test['predictions']>3, ]

densityPlot(roe_deer_test$sun.time)
library(dplyr)
test <- na.omit(roe_deer_test)
bins <- 24  # Number of bins (e.g., one bin per hour)
roe_deer_test$bins <- cut(roe_deer_test$sun.time, breaks = seq(0, 2 * pi, length.out = bins+1), include.lowest = TRUE)
# Calculate average snow hardness for each sun.time
data_test2$bins <- cut(data_test2$sun.time, breaks = seq(0, 2 * pi, length.out = bins+1), include.lowest = TRUE)

average_predictions <- roe_deer_test %>%
  group_by(bins) %>%
  summarise(predictions = mean(predictions, na.rm = TRUE))

bin_counts <- roe_deer_test %>%
  group_by(bins) %>%
  summarise(count = n())
bin_counts <- left_join(bin_counts, average_predictions, by = "bins")
bin_counts$labels <- seq(0,2*pi - 2*pi/24, 2*pi/24)

ggplot(bin_counts, aes(x = labels, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7, width =(2 * pi / bins), color = 'black') +
  geom_line(aes(y = predictions*15, color = 'Snow Hardness'),
            ## stack overflow
            data = rbind(bin_counts, transform(bin_counts[1, ], labels = 2 * pi)),
            group = 1,
            ##########
            # color = "red",
            size = 1) +  # Add line for average snow hardness
  coord_polar(start = ) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0), breaks = c(0, pi/2, pi, 3*pi/2),
                     ### stack overflow
                     limits = c(0, 2 * pi), oob = scales::oob_keep,
                     labels = c('Midnight', 'Sunrise', 'Noon', 'Sunset')) + theme(
    axis.text.x = element_text(size = 10),
    legend.position = "right"
    # plot.margin = unit(rep(2, 4), "cm"),  # Adjust legend position here
  ) + labs(x='') +ylim(0,NA)


# Merge average predictions into bin_counts2
bin_counts3 <- bin_counts2[bin_counts2$midpoint==6.1500, ]
bin_counts3$midpoint <- 0.13
bin_new <- rbind(bin_counts3, bin_counts2)
ggplot(bin_new, aes(x = midpoint, y = count, group =1)) +
  geom_bar(stat = "identity", fill = "skyblue", alpha = 0.7, width =(2 * pi / bins)) +
  geom_line(aes(y = predictions*100), color = "red", size = 1) +  # Add line for average snow hardness
  coord_polar(start = 0) +
  theme_minimal() +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2),
                     labels = c("Midnight", 'sunrise', "Noon", 'sunset')) +  # Added -0.001 for "Midnight" at 0 radians
  theme(
    axis.text.x = element_text(size = 10)
    # plot.margin = unit(rep(2, 4), "cm"),  # Adjust legend position here
  ) + labs(x='') + ylim(0,NA)


overlapPlot(hard$sun.time, soft$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("orange","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("HARD SNOW", "SOFT SNOW"),
       lty=c(1, 1), col=c("orange", "blue"),
       text.col = c("orange","blue")) + title('ROE DEER')

overlapEst(hard$sun.time, soft$sun.time)
library(circular)
sample1 <- circular(hard$sun.time)
sample2 <- circular(soft$sun.time)
result <- watson.wheeler.test(list(sample1, sample2))





data_test$sunhour <- as.integer(data_test$sun.time) #format(data_test$datetime, "%H")
data_test$pred_round <- as.integer(data_test$predictions)
min(data_test$pred_round)


# Define breaks for binning Snow_Hardness
breaks <- seq(0, 5, by = 1)

# Create labels for the bins
labels <- c("0",  "1",  "2",  "3",  "4",  "5")

# Group by Hour_of_Detection and Snow_Hardness_Group and count occurrences
summary <- data_test %>%
  group_by(sunhour, pred_round) %>%
  summarise(Frequency = n())

plot(summary$pred_round, summary$sunhour)

simp <- data_test[c('sunhour','predictions','snowdepth.mm','humandens.x')]
simp <- na.omit(simp)
simp$sunhour <- as.numeric(simp$sunhour)
simp$predictions <- as.numeric(simp$predictions)
simp$snowdepth.mm <- as.numeric(simp$snowdepth.mm)
simp$humandens.x <- as.numeric(simp$humandens.x)

cor.table <- cor(simp)
corrplot(cor.table, method="shade",shade.col=NA, tl.col="black", tl.srt=45)

activity_hard <- lm(sin(sunhour)~ scale(predictions) + scale(snowdepth.mm) + scale(humandens.x) + scale(predictions) * scale(snowdepth.mm), data = data_test, na.action= na.omit)
summary(activity_hard)

coefficients <- coef(activity_hard)[-1]  # Remove intercept
variables <- names(coefficients)
p_values <- summary(activity_hard)$coefficients[-1, "Pr(>|t|)"]

# Create a data frame
df <- data.frame(variable = variables, coefficient = coefficients, p_value = p_values)

# Determine significance level (e.g., 0.05)
significance_level <- 0.05
df$significant <- ifelse(df$p_value < significance_level, "Yes", "No")

# Plot the lollipop chart with significance indicators
ggplot(df, aes(x = variable, y = coefficient, color = significant)) +
  geom_segment(aes(x = variable, xend = variable, y = 0, yend = coefficient), color = "blue") +
  geom_point(size = 3) +
  geom_text(aes(label = round(coefficient, 2)), vjust = -0.5) +
  scale_color_manual(values = c("Yes" = "red", "No" = "black")) +  # Color for significance
  coord_flip() +
  labs(x = "Variable", y = "Coefficient") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))  # Adjust y-axis label size




##############
# Compute kernel density estimate
density_est <- density(roedeer_oc_es_s$sun.time)
# Plot kernel density estimate
plot(density_est, main = "Kernel Density Estimator Plot", xlab = "Values", ylab = "Density")

overlapPlot(roedeer_cc_es_ns$sun.time, roedeer_cc_es_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("orange","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("NO SNOW", "SNOW"),
       lty=c(1, 1), col=c("orange", "blue"),
       text.col = c("orange","blue")) + title('Early Season CLOSED CANOPY')

overlapEst(roedeer_cc_es_ns$sun.time, roedeer_cc_es_s$sun.time)
overlapTrueInv <- function(d1, d2=NULL) {
    # Deal with d1 as matrix:
    if(!is.null(ncol(d1)) && ncol(d1) == 2) {
      d2 <- d1[,2]
      d1 <- d1[,1]
    }
    # Remove first value if same as the last: (added 19 Oct 2012)
    if(identical(all.equal(d1[1], d1[length(d1)]), TRUE) &&
       identical(all.equal(d2[1], d2[length(d1)]), TRUE))  {
      d1 <- d1[-1]
      d2 <- d2[-1]
    }
    # Scale each distribution to add to 1:
    d1 <- d1 / sum(d1)
    d2 <- d2 / sum(d2)
    # Compute overlap:
    # higher_indices <- which(d1 > d2)
    # d1_max <- d1[higher_indices]
    # #print(d1_max)
    #print(sum(pmin(d1, d2)))
    print(sum(abs(d1-d2)))
    # Calculate the probability of difference
    # print(length(d1))
    # print(length(d2))
    # # probability_difference <- integrate(function(x) abs(d1 - d2), min(d1, d2), max(d1, d2))$value
    # # probability_difference
    # print(sum(pmax(d1,d2)))
    # print(sum(pmin(d1,d2)))
    # print(sum(pmax(d1,d2)-pmin(d1-d2)))
    #print(sum(d1_max))
}
## interpolate first
new_x <- seq(min(min(roedeer_cc_es_s$sun.time), min(roedeer_cc_es_ns$sun.time)),
             max(max(roedeer_cc_es_s$sun.time), max(roedeer_cc_es_ns$sun.time)), length.out = length(roedeer_cc_es_ns$sun.time))
overlapTrueInv(roedeer_cc_es_ns$sun.time, new_x)


# Calculate kernel density estimates for each dataset
density_ns <- densityFit(sort(roedeer_cc_es_ns$sun.time), seq(0, 2*pi, length=556), bw=10)
density_s <- densityFit(sort(roedeer_cc_es_s$sun.time), seq(0, 2*pi, length=556), bw=10)
density_s_inter <- densityFit(new_x, seq(0, 2*pi, length=556), bw=10)

# Plot one of the density estimates
plot(seq(0, 2*pi, length=556), density_ns, type = "l", col = "orange",
     xlab = "Sun Time", ylab = "Density Estimate", main = "Kernel Density Estimates")
# Add the other density estimate to the same plot
lines(seq(0, 2*pi, length=556), density_s, col = "blue")
# Add a legend


legend("bottom", legend = c("NO SNOW", "SNOW"), col = c("orange", "blue"), lty = 1)
# Find the points where density_s is greater than density_ns

# # Find the common x-values
exceed_indices <- which(density_s > density_ns)
x_values <- sort(roedeer_cc_es_ns$sun.time)[exceed_indices]
y_values_s <- density_s[exceed_indices]
y_values_ns <- density_ns[exceed_indices]
sum(density_s - density_ns)

# Find continuous segments where density_s is greater than density_ns
continuous_segments <- split(exceed_indices, cumsum(c(0, diff(exceed_indices) != 1)))
# Fill in the areas where density_s is greater than density_ns for continuous segments
for (segment in continuous_segments) {
  x_values <- sort(roedeer_cc_es_ns$sun.time)[segment]
  y_values_ns <- density_ns[segment]
  y_values_s <-density_s[segment]

  polygon(c(x_values, rev(x_values)), c(y_values_ns, rev(y_values_s)), col = "blue", border = NA)
}

overlapPlot(roedeer_oc_es_ns$sun.time, roedeer_oc_es_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("orange","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='grey', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("NO SNOW", "SNOW"),
       lty=c(1, 1), col=c("orange", "blue"),
       text.col = c("orange","blue"))  + title('Early Season OPEN CANOPY')
overlapEst(roedeer_oc_es_ns$sun.time, roedeer_oc_es_s$sun.time)

roedeer_cc_ls_s <- roedeer_cc_ls_s[!is.na(roedeer_cc_ls_s$sun.time),]
overlapPlot(roedeer_cc_ls_ns$sun.time, roedeer_cc_ls_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("orange","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='lightgray', rug= TRUE,
            extend= NULL, n.grid= 100, shade=TRUE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("NO SNOW", "SNOW"),
       lty=c(1, 1), col=c("orange", "blue"),
       text.col = c("orange","blue")) + title('Late Season CLOSED CANOPY')
overlapEst(roedeer_cc_ls_ns$sun.time, roedeer_cc_ls_s$sun.time)

roedeer_cc_ls_ns <- roedeer_cc_ls_ns[!is.na(roedeer_cc_ls_ns$sun.time),]
overlapPlot(roedeer_oc_ls_ns$sun.time, roedeer_oc_ls_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("orange","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='lightgray', rug= TRUE,
            extend= NULL, n.grid= 100,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("NO SNOW", "SNOW"),
       lty=c(1, 1), col=c("orange", "blue"),
       text.col = c("orange","blue")) + title('Late Season OPEN CANOPY')
overlapEst(roedeer_oc_ls_ns$sun.time, roedeer_oc_ls_s$sun.time)

# densityPlot(roedeer_oc_ls_ns$sun.time, rug=TRUE, main="Simulated data", extend='gold')
# densityPlot(roedeer_oc_ls_s$sun.time, rug=TRUE, main="Simulated data", extend='gold')
#### if we want to normalize the variables:
### just compare two histograms

library(circular)
X=rbeta(1000,shape1=2,shape2=4)*24
Omegat=2*pi*X/24
#Omegat=2*pi*trunc(X)/24
#https://www.r-bloggers.com/2011/03/circular-or-spherical-data-and-density-estimation/
Ht=circular(Omega,type="angle",units="radians",rotation="clock")
circ.dens1 = density(circular(roedeer_oc_ls_ns$sun.time), bw=20)
circ.dens2 = density(circular(roedeer_oc_ls_s$sun.time), bw=20)
plot(Ht, stack=TRUE, shrink=.35, cex=0, sep=0.0,
     axes=FALSE,tol=.8,zero=c(0),bins=24,
     xlim=c(-2,2),ylim=c(-2,2), ticks=TRUE, tcl=.075)
lines(circ.dens1, col="orange", lwd=3)
lines(circ.dens2, col="blue", lwd=3)

plot(density(circular(roedeer_oc_ls_ns$sun.time), bw=20))
#lines(density(sin(5), bw = 20)
# Get the intersection points between the density curves
# Find intersection points
intersection_points <- approx(circ.dens1$x, circ.dens1$y, xout = circ.dens2$x)$y

# Plot polygons between the density curves
polygon_x <- c(circ.dens2$x, rev(circ.dens2$x))
polygon_y <- c(intersection_points, rev(circ.dens1$y))
polygon(polygon_x, polygon_y, col = adjustcolor("gray", alpha.f = 0.5), border = NA)

text(0,0.8,"24", cex=2); text(0,-0.8,"12",cex=2);
text(0.8,0,"6",cex=2); text(-0.8,0,"18",cex=2)
overlapPlot(roedeer_oc_ls_ns$sun.time, roedeer_oc_ls_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("orange","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='lightgray', rug= TRUE,
            extend= NULL, n.grid= 100,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
polygon(1:9, c(0.02,0.1,0.2,NA, 0.01,0.2,0.001,2,1),
        density = c(10, 20), angle = c(-45, 45))
legend(10,0.02, cex=1, c("NO SNOW", "SNOW"),
       lty=c(1, 1), col=c("orange", "blue"),
       text.col = c("orange","blue")) + title('Late Season OPEN CANOPY')
overlapEst(roedeer_oc_ls_ns$sun.time, roedeer_oc_ls_s$sun.time)
# }
#

roedeer_oc_es_s_hh
roedeer_cc_es_s_hh
roedeer_oc_ls_ns_hh
roedeer_cc_ls_ns_hh

roedeer_oc_es_s_lh
roedeer_cc_es_s_lh
roedeer_oc_ls_ns_lh
roedeer_cc_ls_ns_lh

density_s_hh <- densityFit(sort(roedeer_oc_es_s_hh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ns_hh <- densityFit(sort(roedeer_oc_es_ns_hh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_s_lh <- densityFit(sort(roedeer_oc_es_s_lh$sun.time), seq(0, 2*pi, length=556), bw=10)
density_ns_lh <- densityFit(sort(roedeer_oc_es_ns_lh$sun.time), seq(0, 2*pi, length=556), bw=10)


# Plot one of the density estimates
plot(seq(0, 2*pi, length=556), density_ns_hh, type = "l", col = "orange",
     xlab = "Sun Time", ylab = "Density Estimate", main = "Kernel Density Estimates")
# Add the other density estimate to the same plot
lines(seq(0, 2*pi, length=556), density_s_hh, col = "blue")
lines(seq(0, 2*pi, length=556), density_s_lh, col = "purple")
lines(seq(0, 2*pi, length=556), density_ns_lh, col = "green")
# Add a legend
legend("bottom", legend = c("NO SNOW", "SNOW"), col = c("orange", "blue"), lty = 1)

overlapPlot(roedeer_cc_es_s_hh$sun.time, roedeer_cc_es_ns_hh$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("orange","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol='lightgray', rug= TRUE,
            extend= NULL, n.grid= 100,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(10,0.02, cex=1, c("NO SNOW", "SNOW"),
       lty=c(1, 1), col=c("orange", "blue"),
       text.col = c("orange","blue")) + title('Early Season / Closed Canopy / High Human Density')
overlapEst(roedeer_cc_es_s_hh$sun.time, roedeer_cc_es_ns_hh$sun.time)

#
# ######### ROE DEER ##############
# data_main_test2 <- data_main_test[data_main_test$cross_0>0,]
# RoeDeer<- filter(data_main_test2, validated_species  %in% c("raadyr"))
# roedeer_oc <- RoeDeer[RoeDeer$treecover < 50,]
# roedeer_cc <- RoeDeer[RoeDeer$treecover > 50,]
#
# roedeer_oc_es <- roedeer_oc[(roedeer_oc$month == '02') | (roedeer_oc$month == '03'),]
# roedeer_cc_es <- roedeer_cc[(roedeer_cc$month == '02') | (roedeer_cc$month == '03'),]
#
# roedeer_oc_ls <- roedeer_oc[(roedeer_oc$month == '04') | (roedeer_oc$month == '05'),]
# roedeer_cc_ls <- roedeer_cc[(roedeer_cc$month == '04') | (roedeer_cc$month == '05'),]
#
# roedeer_oc_es_s <- roedeer_oc_es[roedeer_oc_es$snowdepth.mm > 30,]
# roedeer_cc_es_s <- roedeer_cc_es[roedeer_cc_es$snowdepth.mm > 30,]
#
# roedeer_oc_es_ns <- roedeer_oc_es[roedeer_oc_es$snowdepth.mm < 30,]
# roedeer_cc_es_ns <- roedeer_cc_es[roedeer_cc_es$snowdepth.mm < 30,]
#
# roedeer_oc_ls_s <- roedeer_oc_ls[roedeer_oc_ls$snowdepth.mm > 30,]
# roedeer_cc_ls_s <- roedeer_cc_ls[roedeer_cc_ls$snowdepth.mm > 30,]
#
# roedeer_oc_ls_ns <- roedeer_oc_ls[roedeer_oc_ls$snowdepth.mm < 30,]
# roedeer_cc_ls_ns <- roedeer_cc_ls[roedeer_cc_ls$snowdepth.mm < 30,]
#
# plot.new()
# smoother = 100
# #par(mfrow=c(1,2))
# ############# NOT WORKING
#
# overlapPlot(roedeer_oc_es_s$sun.time, roedeer_cc_es_s$sun.time, xcenter= "noon", linetype= c(1,1),
#             linecol= c("black","blue"),
#             xaxt="n",
#             linewidth = c(3,3), olapcol=FALSE, rug= TRUE,
#             extend= NULL, n.grid= 100, shade=FALSE,
#             adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
# axis(1, at=c(0, 6, 12, 18, 24),
#      labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
# abline(v=c(6, 18), lty=3)
# legend(8,0.10,cex=2, #
#        c("OPEN", "CLOSED"),
#        lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.1, #0.2
#        text.col = c("black","blue")) + title('Early Season SNOW')
#
# overlapPlot(roedeer_oc_es_ns$sun.time, roedeer_cc_es_ns$sun.time, xcenter= "noon", linetype= c(1,1),
#             linecol= c("black","blue"),
#             xaxt="n",
#             shade=FALSE,
#             linewidth = c(3,3), olapcol=FALSE, rug= TRUE,
#             extend= NULL, n.grid= 100,
#             adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
# axis(1, at=c(0, 6, 12, 18, 24),
#      labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
# abline(v=c(6, 18), lty=3)
# legend(8,0.10,cex=.8, #
#        c("Roe Deer OC", "Roe Deer CC"),
#        lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.1, #0.2
#        text.col = c("black","blue")) + title('Early Season NO SNOW')
#
# roedeer_cc_ls_s <- roedeer_cc_ls_s[!is.na(roedeer_cc_ls_s$sun.time),]
# overlapPlot(roedeer_oc_ls_s$sun.time, roedeer_cc_ls_s$sun.time, xcenter= "noon", linetype= c(1,1),
#             linecol= c("black","blue"),
#             xaxt="n", ylim = c(0,0.16),
#             linewidth = c(3,3), olapcol=FALSE, rug= TRUE,
#             extend= NULL, n.grid= 100,
#             adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
# axis(1, at=c(0, 6, 12, 18, 24),
#      labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
# abline(v=c(6, 18), lty=3)
# legend(8,0.10,cex=2, #
#        c("OPEN", "CLOSED"),
#        lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.1, #0.2
#        text.col = c("black","blue")) + title('Late Season SNOW')
#
#
# roedeer_cc_ls_ns <- roedeer_cc_ls_ns[!is.na(roedeer_cc_ls_ns$sun.time),]
# overlapPlot(roedeer_oc_ls_ns$sun.time, roedeer_cc_ls_ns$sun.time, xcenter= "noon", linetype= c(1,1),
#             linecol= c("black","blue"),
#             xaxt="n", ylim = c(0,0.16),
#             linewidth = c(3,3), olapcol=FALSE, rug= TRUE,
#             extend= NULL, n.grid= 100,
#             adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
# axis(1, at=c(0, 6, 12, 18, 24),
#      labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
# abline(v=c(6, 18), lty=3)
# legend(8,0.10,cex=.8, #
#        c("OPEN ", "CLOSED "),
#        lty=c(1, 4), lwd=c(3,3),col=c("black", "blue"), bty='n', yjust=.1, #0.2
#        text.col = c("black","blue")) + title('Late Season NO SNOW')

#### if we want to normalize the variables:
### just compare two histograms


Hare<- filter(data_main_test, validated_species  %in% c("hare"))

## cross 0
Hare<- filter(Hare, cross_0  %in% c(1))

hare_oc <- Hare[Hare$treecover < 50,]
hare_cc <- Hare[Hare$treecover > 50,]

hare_oc_es <- hare_oc[(hare_oc$month == '02') | (hare_oc$month == '03'),]
hare_cc_es <- hare_cc[(hare_cc$month == '02') | (hare_cc$month == '03'),]

hare_oc_ls <- hare_oc[(hare_oc$month == '04') | (hare_oc$month == '05'),]
hare_cc_ls <- hare_cc[(hare_cc$month == '04') | (hare_cc$month == '05'),]

hare_oc_es_s <- hare_oc_es[hare_oc_es$snowdepth.mm > 30,]
hare_cc_es_s <- hare_cc_es[hare_cc_es$snowdepth.mm > 30,]

hare_oc_es_ns <- hare_oc_es[hare_oc_es$snowdepth.mm < 30,]
hare_cc_es_ns <- hare_cc_es[hare_cc_es$snowdepth.mm < 30,]

hare_oc_ls_s <- hare_oc_ls[hare_oc_ls$snowdepth.mm > 30,]
hare_cc_ls_s <- hare_cc_ls[hare_cc_ls$snowdepth.mm > 30,]

hare_oc_ls_ns <- hare_oc_ls[hare_oc_ls$snowdepth.mm < 30,]
hare_cc_ls_ns <- hare_cc_ls[hare_cc_ls$snowdepth.mm < 30,]

overlapPlot(hare_oc_es_s$sun.time, hare_cc_es_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol=FALSE, rug= TRUE,
            extend= NULL, n.grid= 100, shade=FALSE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=2, #
       c("OPEN", "CLOSED"),
       lty=c(1, 1), lwd=c(3,3),col=c("black", "blue"), bty='n', #0.2
       text.col = c("black","blue")) + title('Early Season SNOW')


overlapPlot(hare_oc_es_ns$sun.time, hare_cc_es_ns$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol=FALSE, rug= TRUE,
            extend= NULL, n.grid= 100, shade=FALSE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=2, #
       c("OPEN", "CLOSED"),
       lty=c(1, 1), lwd=c(3,3),col=c("black", "blue"), bty='n', #0.2
       text.col = c("black","blue")) + title('Early Season NO SNOW')


overlapPlot(hare_oc_ls_s$sun.time, hare_cc_ls_s$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol=FALSE, rug= TRUE,
            extend= NULL, n.grid= 100, shade=FALSE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=2, #
       c("OPEN", "CLOSED"),
       lty=c(1, 1), lwd=c(3,3),col=c("black", "blue"), bty='n', #0.2
       text.col = c("black","blue")) + title('Late Season SNOW')

overlapPlot(hare_oc_ls_ns$sun.time, hare_cc_ls_ns$sun.time, xcenter= "noon", linetype= c(1,1),
            linecol= c("black","blue"),
            xaxt="n",
            linewidth = c(3,3), olapcol=FALSE, rug= TRUE,
            extend= NULL, n.grid= 100, shade=FALSE,
            adjust= 1.5, main= "", xlab= "Time", ylab="Kernal Density Estimate")
axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
abline(v=c(6, 18), lty=3)
legend(8,0.10,cex=2, #
       c("OPEN", "CLOSED"),
       lty=c(1, 1), lwd=c(3,3),col=c("black", "blue"), bty='n', #0.2
       text.col = c("black","blue")) + title('Late Season NO SNOW')
plot(model_data$sun.time, model_data$avgsinkDp)

model_data <- readRDS("snow_hardness_covariates_df.rds")
model_data1 <- model_data %>%
  filter(Month %in% c("4", "4"))
ggplot(model_data1, aes(x = sun.time, y = avgsinkDp, group=cnpyClass, color =cnpyClass))+
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) + #method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs( x = "Time", #sun time
        y = "Average Sink Depth (cm) ") + theme_bw()

# Create an empty plot
library(gridExtra)
plot.new()
par(mfrow=c(1,1))
# Create the first plot
chippy <- function(x) sin(cos(x)*exp(-x/2))
overlapPlot(hare_oc_ls_ns$sun.time, hare_cc_ls_ns$sun.time, xcenter = "noon", linetype = c(1, 1),
            linecol = c("black", "blue"),
            xaxt = "n",
            linewidth = c(3, 3), olapcol = FALSE, rug = TRUE,
            extend = NULL, n.grid = 100, shade = FALSE,
            adjust = 1.5, main = "", xlab = "", ylab = "Kernel Density Estimate")
# axis(1, at = c(0, 6, 12, 18, 24),
#      labels = c("midnight", "sunrise", "noon", "sunset", "midnight"))
# abline(v = c(6, 18), lty = 3)
curve(sin, 6, 18, n = 2001)
legend(8, 0.10, cex = 2,
       c("OPEN", "CLOSED"),
       lty = c(1, 1), lwd = c(3, 3), col = c("black", "blue"), bty = 'n',
       text.col = c("black", "blue"))
title('Late Season NO SNOW')


# Create the second plot
plot2 <- ggplot(model_data1, aes(x = sun.time, y = avgsinkDp, group = cnpyClass, color = cnpyClass)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi),
                     labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs(x = "Time", y = "Average Sink Depth (cm)") +
  theme_bw()

# Arrange the plots using grid.arrange()
grid.arrange(plot1, ncol = 1)
# Reset par to default

















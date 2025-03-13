#* Catherine Breen
#* for Oikos submission
#* Part 2
#*
#*

################# 0. LOAD PACKAGES ########################
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

library(lmtest)
library(sandwich)
library(DHARMa)
library(cplm)
source("/Users/catherinebreen/Dropbox/Chapter3/code/utilityFunctions.R")



datamerge <- function(data, metadata){
  metadata$LokalitetID <- as.factor(metadata$LokalitetID)
  joined_tibble <- left_join(data, metadata, by = c("location_id" = "LokalitetID"))
  return(joined_tibble)
}

############ 1. LOAD SCANDCAM DETECTIONS ### #########
# Merge location information with detections #
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



####### 1a. PLOT LAT/LONG ## #########
# # plotting the map with some points on it

cameras <- distinct(data, Latitude, .keep_all = TRUE)

ggplot() +
  geom_point(data = cameras, aes(x = LongitudeNum, y = LatitudeNum, fill = "red", alpha = 0.8), size = 4, shape = 21)

# #Removed 451 rows containing missing values (geom_point). 451 cameras are missing coordinates!!
########### 1b. FORMAT DATE/TIME AND NARROW DOWN TO SPRING ### ######

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



############## 2A. ADD SNOW & TEMP COVARIATES ##############

## snow depth, minmaxtemp, avgtemp, and swe (for density)
snowdepth <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/senorge_spring_2015_2024_updloc.csv')
minmaxtemp <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/minmaxavg_temp_senorge_CAMERAS.csv')
avgtemp <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/avg_temp_senorge_CAMERAS.csv')
swe <- read.csv("/Users/catherinebreen/Dropbox/Chapter3/data/swe_senorge_spring_2015_2024_updloc.csv")

## set up columns to match data for the merge

data$date <- as.Date(data$date)
data$location_id <- as.numeric(data$location_id)

#snowdepth
snowdepth$location_id <- snowdepth$loc
snowdepth$date <- as.Date(snowdepth$date1)

## drop na values
data <- data[!is.na(data$location_id),]

data_wsnow <- dplyr::left_join(data, snowdepth, by = c('location_id', 'date'), unmatched = "drop", relationship = "many-to-many")

# swe
swe$location_id <- swe$loc
swe$date <- as.Date(swe$date1)
swe <- swe[c('location_id','swe','date')]

data_wsnowswe <- dplyr::left_join(data_wsnow, swe, by = c('location_id','date'), unmatched = "drop")
#data_wsnowswe$snowDensity.senorge <- data_wsnowswe$swe / data_wsnowswe$snowdepth.mm *100
#hist(data_wsnowswe$snowDensity.senorge)
head(data_wsnowswe)

# avg temp
avgtemp$location_id <- avgtemp$loc
avgtemp$date <- as.Date(avgtemp$date1)
avgtemp <- avgtemp[c('location_id','avg.temp','date')]
head(avgtemp)

# minmax temp
minmaxtemp$location_id <- minmaxtemp$loc
minmaxtemp$date <- as.Date(minmaxtemp$date1)
minmaxtemp <- minmaxtemp[c('location_id','date','min.temp','max.temp','cross_0')]
head(minmaxtemp)

data_wsnowswetemp <- dplyr::left_join(data_wsnowswe, avgtemp, by = c('location_id','date'), unmatched = "drop")
data_wsnowswetemp <- dplyr::left_join(data_wsnowswetemp, minmaxtemp, by = c('location_id','date'), unmatched = "drop")
head(data_wsnowswetemp)

######### now do the tree canopy cover ### ** could update this
############## 2B. TREE CANOPY COVER #############


## updated tree info:
treec <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/treecovariates.rds')

head(data_wsnowswetemp)
head(treec)
data_wsnowtree <- dplyr::left_join(data_wsnowswetemp, treec, by = c('location_id','year'), unmatched = "drop")
head(data_wsnowtree)



data_main <- data_wsnowtree[!is.na(data_wsnowtree$validated_species),]

############ 2C. HUMAN POPULATION DENSITY FOR NORWAY ################
# units are number of persons per sq km #
# humanDensity <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/humanDensityNorway_GEE.csv')
# humanDensity$humandens <- humanDensity$SAMPLE_humanPop1
# humanDensity$location_id <- humanDensity$LokalitetID
# humanDensity <- humanDensity[, c("location_id", "humandens")] ## this is 2018!!!
# head(humanDensity)
# mean(na.omit(humanDensity$humandens))
#
# data_main <- dplyr::left_join(data_main, humanDensity, by = c('location_id'), unmatched = "drop")
#
# data_main$snowDensity.senorge <- data_main$swe / data_main$snowdepth.mm *100
# hist(data_main$snowDensity.senorge)
#
# saveRDS(data_main, file = "/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_Feb25.rds")
# head(data_main)


################# 3.INTERMEDIATE SAVE (could start here) ###########
### not sure why i have so many NANs, but lets ignore for now
#data_main <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main.rds')
data_main <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_Feb25.rds')
### not sure why i have so many NANs, but lets ignore for now

################### 3A. PREDATOR DENSITY ###############
# pred_day <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pred_dens_day_all.rds')
# pred_month <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pred_dens_month_all.rds')
# pred_seas <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pred_dens_seas_all.rds')
# pred_year <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pred_dens_year_all.rds')
#
# ## conduct a bunch of left merges to get
# colnames(pred_day) <- c('location_id','date', 'pred_day')
# colnames(pred_month) <- c('location_id','month', 'year', 'pred_month')
# colnames(pred_seas) <- c('location_id','year', 'pred_seas')
# colnames(pred_year) <- c('location_id','year', 'pred_year')
#
# head(data_main)
#
# data_main <- left_join(data_main, pred_day, by = c('location_id','date'))
#
# data_main$month <- as.numeric(data_main$month)
# data_main$year <- as.numeric(data_main$year)
# data_main <- left_join(data_main, pred_month, by = c('location_id','month', 'year'))
#
# data_main <- left_join(data_main, pred_seas, by = c('location_id', 'year'))
# data_main <- left_join(data_main, pred_year, by = c('location_id', 'year'))
#
# head(data_main)


##################  3B. CALCULATE SUNTIME  #############

### this seems to be the only way I can get the timezone to work
data_main$Time <- format(data_main$datetime, "%H:%M:%S")
data_main$rads <- astroFns::hms2rad(data_main$Time)

#data4$date <- as.POSIXct(data4$captured_at_exif,  format="%Y:%m:%d")

###### Convert dates to a POSIXct object with the right time zone (European/Oslo):
## put this in your timezone
#Dates<- as.POSIXct(data_wsnow$captured_at_exif, tz = "data$timeZone")

#Create a SpatialPoints object with the location
data_main <- data_main[complete.cases(data_main$Longitude),]
data_main <- data_main[complete.cases(data_main$Latitude),]
print(length(unique(data_main$date)))
coords <- data_main[,c("LongitudeNum", "LatitudeNum")]
#Coords_SPDF <- SpatialPointsDataFrame(coords, proj4string = CRS("+init=epsg:4269 +proj=longlat +datum=NAD83+zone=33"), data = data_wsnow)
Coords_SPDF <- SpatialPointsDataFrame(coords, proj4string = CRS("+proj=longlat +datum=NAD83"), data = data_main)

#CRS("+init=epsg:4269 +proj=longlat +datum=NAD83")

# Convert times to suntime
#data_main$timeZone <- lutz::tz_lookup_coords(lat=data_main$LatitudeNum, lon = data_main$LongitudeNum, warn = FALSE, method = "accurate")
Dates<- as.POSIXct(data_main$datetime, tz = 'Europe/Oslo') #tz = 'Europe/Oslo') #
data_main$sun.time <- overlap::sunTime(data_main$rads, Dates, Coords_SPDF)

data_main <- data_main[complete.cases(data_main$sun.time),]
data_main_test <- data_main %>% drop_na() #data_main[!is.na(data_main$sun.time),]
head(data_main_test)
## sample sunTime comparison, example 121
### ALL SPECIES
densityPlot(data_main_test$sun.time, col='red', lwd=2,  main="Sun time")

axis(1, at=c(0, 6, 12, 18, 24),
     labels=c("midnight", "sunrise", "noon", "sunset", "midnight"))
densityPlot(data_main_test$rads, lwd=2, main="Clock time")

saveRDS(data_main_test, '/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_all_cov_mar13.rds')


######### 4. [start] PREDICT SNOW HARDNESS #########

data_main_test <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_all_cov_mar12.rds')

data_main_test$month <- as.numeric(data_main_test$month)
data_main_test$year <- as.numeric(data_main_test$year)


# make sure column names match
data_test <- data_main_test %>% filter(month %in% c(2,3,4,5))  %>%
  mutate(season = ifelse(month %in% c(2,3), 0, 1)) %>%
  mutate(season_F = as.factor(ifelse(month %in% c(2,3), 'early', 'late'))) %>%
  mutate(cnpyClass = ifelse(treecover < 50, 0, 1)) %>%
  mutate(cnpyClass_F = as.factor(ifelse(treecover < 50, 'open', 'closed'))) %>%
  mutate(avg.temp.c = avg.temp) %>%
  mutate(hotcold_F = as.factor(ifelse(avg.temp.c >= 0, 'warm', 'cold')))

data_test <- data_test %>%
  arrange(location_id, date) %>%  # Ensure data is sorted
  group_by(location_id) %>%  # Group by location
  mutate(avg.temp.c = avg.temp,
          avg.temp.c.prev = lag(avg.temp),
         avg.temp.c.prev2 = slide_mean(avg.temp.c, before = 2, after = -1, complete = TRUE)) %>%
  ungroup()



########### MODEL SNOW HARDNESS #########

# alternative #
modelhotcold <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/modelhotcold.rds')
top_modelhotcold <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_modelhotcold.rds')
modelhotcold2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/modelhotcold2.rds')
top_modelhotcold2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_modelhotcold2.rds')
modelhotcold3 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/modelhotcold3.rds')
top_modelhotcold3 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_modelhotcold3.rds')

# hurdles #
hurdlehotcold <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/hurdlehotcold.rds')
#top_hurdlehotcold <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_hurdlehotcold.rds')
hurdlehotcold2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/hurdlehotcold2.rds')
top_hurdlehotcold2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_hurdlehotcold2.rds')
hurdlehotcold3 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/hurdlehotcold3.rds')
top_hurdlehotcold3 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_hurdlehotcold3.rds')

# prev2
modelprev <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/model.avg.temp.prev.rds')


############# correct for heteroskedascity ###########
## adjusts the standard errors
bptest(modelhotcold)
vcovHC(modelhotcold)
coeftest(model_tweedie, vcov = vcovHC(model_tweedie, type = "HC1"))
coeftest(model_tweedie, vcov = vcovHC(model_tweedie, type = "HC3")) ## more conservative for small sample sizes, and influential observations
coeftest(model_aic_dens, vcov = vcovHC(model_aic_dens, type = "HC3")) #
summary(model_aic_dens)

model_tweedie_corr <- coeftest(model_tweedie, vcov = vcovHC(model_tweedie, type = "HC1"))
model_temp_corr <- coeftest(model_temp, vcov = vcovHC(model_temp, type='HC1'))

###### TEMP MODEL VISUALIZE PREDICTIONS ####
data_test$modelhotcold <- predict(modelhotcold, newdata = data_test, type = 'response', se.fit = TRUE)$fit #stats::
data_test$modelhotcold2 <- predict(modelhotcold2, newdata = data_test, type = 'response', se.fit = TRUE)$fit #stats::
data_test$top_modelhotcold2 <- predict(top_modelhotcold2, newdata = data_test, type = 'response', se.fit = TRUE)$fit #stats::
data_test$modelhotcold3 <- predict(modelhotcold3, newdata = data_test, type = 'response', se.fit = TRUE)$fit #stats::

data_test$modelprev <- predict(modelprev, newdata = data_test, type = 'response', se.fit = TRUE)$fit


ggplot(data_test, aes(x = top_modelhotcold2)) +
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

max(data_test$modelprev, na.rm = TRUE)


###### HURDLE MODEL VISUALIZE PREDICTIONS ####
data_test$hurdlehotcold <- predict(hurdlehotcold, newdata = data_test, type = 'response', se.fit = TRUE)
data_test$hurdlehotcold2 <- predict(hurdlehotcold2, newdata = data_test, type = 'response', se.fit = TRUE)
data_test$hurdlehotcold3 <- predict(hurdlehotcold3, newdata = data_test, type = 'response', se.fit = TRUE)

ggplot(data_test, aes(x = hurdlehotcold)) +
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


############ SAVE DATAFRAME ###############
# data_test$mean_predictions <- rowMeans(data_test[, c("predictionsCPGLM", "predictionsAIC")])
# data_test$sd_predictions <- apply(data_test[, c("predictionsCPGLM", "predictionsAIC")], 1, sd)

data_test$roedeer_binary <- ifelse(data_test$validated_species == "raadyr", 1, 0)
data_test$hare_binary <- ifelse(data_test$validated_species == "hare", 1, 0)

# Create the jitter plot
ggplot(data_test, aes(x = top_modelhotcold2, y = roedeer_binary)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5) +
  labs(x = "Snow Hardness Predictions", y = "Roe Deer Binary", title = "Jitter Plot of Predictions vs. Roe Deer Binary") +
  theme_minimal()


# Create the jitter plot
ggplot(data_test, aes(x = top_modelhotcold2, y = hare_binary)) +
  geom_jitter(width = 0.1, height = 0.1) +
  labs(x = "Snow Hardness Predictions", y = "Hare Binary", title = "Jitter Plot of Predictions vs. Hare Binary") +
  theme_minimal()

saveRDS(data_test, '/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred_mar13.RDS')

saveRDS(data_test, '/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred_mar3.RDS')
saveRDS(data_test, '/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred_mar12.RDS')
saveRDS(data_test, '/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred_mar12b.RDS')
#saveRDS(data_test, '/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred.RDS')
# saveRDS(data_test2, '/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpredUPD.RDS')
# saveRDS(data_test, '/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpredUPD2.RDS')
# saveRDS(data_test, '/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpredUPD2_AIC.RDS')


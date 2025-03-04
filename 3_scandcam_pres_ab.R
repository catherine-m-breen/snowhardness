
####### Convert ScandCam detections into occupancy df to model pres/absence in relation to snow hardness

############### 0. Load packages ##############

library(tidyverse)
library(tidyr)
library(lubridate)
library(lutz)
library(overlap)
library(leaflet)
library(dplyr)
library(ggplot2)
library(astroFns) # clock times to radians function
library(suncalc)
library(hms)
library(ggpubr)
library(rstatix)
library(sf)
library(sp)
source("/Users/catherinebreen/Dropbox/Chapter3/code/utilityFunctions.R")

datamerge <- function(data, metadata){
  metadata$LokalitetID <- as.factor(metadata$LokalitetID)
  joined_tibble <- left_join(data, metadata, by = c("location_id" = "LokalitetID"))
  return(joined_tibble)
}

############### 1. LOAD WILDLIFE DATA ###############

data <- readRDS("/Users/catherinebreen/Dropbox/Chapter3/Neri_data/DataKatie.rds")

############### 2. MIN/MAX DATES FOR ALLL CAMS & FILTER MONTH ################

# MIN AND MAX FOR ALL CAMERAS
data$datetime <- as.POSIXct(data$captured_at, '%Y:%m:%d %H:%M:%S', tz = "Europe/Oslo") ## the timezone is EDT but this wrong
first(sort(data$datetime)) ## 2017-01-01
last(sort(data$datetime)) ## 2024-01-31

rownames(data) <- NULL

check <- data[is.na(data$datetime), ]
as.POSIXct(data$captured_at[184660], '%Y:%m:%d %H:%M:%S', tz = "Europe/Oslo")
# they are the times between 2-3AM in March because of daylight savings, we will push them an hour ahead for now.
check$rownames_column <- rownames(check) # indices

# FIX DAYLIGHT SAVINGS TIME
for (i in 1:nrow(check)) {
  val <- check$captured_at[i]
  indx <- as.integer(check$rownames_column[i])
  new_val <- sub("^(.{11})02", "\\103", val)
  data$datetime[indx] <- as.POSIXct(new_val, format = '%Y:%m:%d %H:%M:%S', tz = "Europe/Oslo")
}

#SET UP DATE AND MONTH COL
data$date <- format(data$datetime, "%Y-%m-%d")
data$month <- format(data$datetime, "%m")
# data$year <- format(data$datetime, "%Y")
data$hour <- format(data$datetime, "%H")
data$hour <- as.numeric(data$hour)

# FILTER FOR SPRING MONTHS
data <- data[data$month %in% c('02', '03', '04', '05'), ]

table(data$validated_species)

## check the max and min dates again
first(sort(data$datetime)) # 2017
last(sort(data$datetime)) #2023

## we will make an occupancy df for presence/absence for the following species
unique(data$validated_species)
table(data$validated_species)

############### 3. THIN DATA (OPT) #####
# data
# data_thinned <- data %>%
#   mutate(datetime = as.POSIXct(paste(date, hour), format = "%Y-%m-%d %H")) %>%
#   group_by(location_id, date, hour, validated_species) %>%  # Add validated_species to grouping
#   arrange(datetime) %>%
#   filter(
#     !duplicated(cumsum(difftime(datetime, lag(datetime), units = "mins") > 30)) | is.na(lag(datetime))
#   ) %>%
#   ungroup()

############### 4. INSPECT DETECTIONS ###################

roedeer_num_detections <- data %>%
  filter(validated_species == 'raadyr', num_animals >= 1)
hare_num_detections <- data %>%
  filter(validated_species == 'hare', num_animals >= 1)
# hare
# bjorn
# hjort
# raadyr
# menneske
# gaupe
# motorsykkel
# elg
# ulv
# sykell

############### 5. CREATE HRLY P/A DF FOR ALL CAMERAS ########################

## We will do this to figure out how many detections per hour ##

all_dates17 <- seq.Date(from = as.Date('2017-02-01'), to = as.Date('2017-05-30'), by = "day")
all_dates18 <- seq.Date(from = as.Date('2018-02-01'), to = as.Date('2018-05-30'), by = "day")
all_dates19 <- seq.Date(from = as.Date('2019-02-01'), to = as.Date('2019-05-30'), by = "day")
all_dates20 <- seq.Date(from = as.Date('2020-02-01'), to = as.Date('2020-05-30'), by = "day")
all_dates21 <- seq.Date(from = as.Date('2021-02-01'), to = as.Date('2021-05-30'), by = "day")
all_dates22 <- seq.Date(from = as.Date('2022-02-01'), to = as.Date('2022-05-30'), by = "day")
all_dates23 <- seq.Date(from = as.Date('2023-02-01'), to = as.Date('2023-05-30'), by = "day")

all_dates <- as.character(c(all_dates17, all_dates18, all_dates19,
                            all_dates20, all_dates21, all_dates22, all_dates23))
all_hours <- 1:24

# Create a data frame with every combination of camera, date, and hour
all_cameras <- unique(data$location_id)
full_combinations <- expand.grid(location_id = all_cameras, date = all_dates, hour = all_hours)

# sort
full_combinations <-  full_combinations %>%
  arrange(location_id, date, hour)

### filter full_combinations so that the dates start with the first observation (might be biased )
first_observation_dates <- data %>%
  group_by(location_id) %>%
  summarize(first_observation_date = min(datetime))

last_observation_dates <- data %>%
  group_by(location_id) %>%
  summarize(last_observation_date = max(datetime))

# Filter `full_combinations` to remove rows before the first observation date for each camera
full_combinations$date <- as.Date(full_combinations$date)

filtered_combinations <- full_combinations %>%
  left_join(first_observation_dates, by = "location_id") %>%
  left_join(last_observation_dates, by = "location_id")

filtered_data <- filtered_combinations %>%
  filter(date >= first_observation_date, date <= last_observation_date)

#### convert to week

full_combinations <- filtered_data
full_combinations$week <- week(full_combinations$date)
full_combinations$year <- year(full_combinations$date)
head(full_combinations)

remove(filtered_combinations)

############### 6. SET ALL DETECTIONS TO ZERO #############

full_combinations$lynx <- 0
full_combinations$wolf <- 0
full_combinations$hare <- 0
full_combinations$roedeer <- 0
full_combinations$bear <- 0
full_combinations$human <- 0
full_combinations$reddeer <- 0
full_combinations$moose <- 0

head(full_combinations)
head(data)

nrow(data[data$validated_species =='raadyr',]) ### ~20,000 detections, but we reduce it quite a bit because of the model constraints


############### 7A. MERGE DETECTIONS WITH OCC. DF ##########

# Merge with original data
full_combinations$date <- as.character(full_combinations$date)
data <- data[data$validated_species != 'nothing',]

######### check this step!!!!!!!
full_combinations$location_id <- as.character(full_combinations$location_id)
data$location_id <- as.character(data$location_id)
full_combinations$date <- as.character(full_combinations$date)
data$date <- as.character(data$date)
full_combinations$hour <- as.integer(full_combinations$hour)
data$hour <- as.integer(data$hour)
# Check formats
str(full_combinations[, c("location_id", "date", "hour")])
str(data[, c("location_id", "date", "hour")])

#### there will be duplicates if we have scandcam detections for more than hour
merged_df <- left_join(full_combinations, data, by = c("location_id", "date", "hour"))
head(merged_df)
#
# check #
full_combinations %>% filter(location_id == '1121', hour == 9, date == '2020-04-24')
data  %>% filter(location_id == '1121', hour == 9, date == '2020-04-24')
merged_df %>% filter(location_id == '1121', hour == 9, date == '2020-04-24')



############### 7B. CLEAN UP NA VALUES ###############
merged_df$num_animals[is.na(merged_df$num_animals)] <- 0
merged_df$validated_species[is.na(merged_df$validated_species)] <- 'nothing'
head(merged_df)
# Create presence/absence columns for each species
# hare
# bjorn
# hjort
# raadyr
# menneske
# gaupe
# motorsykkel
# elg
# ulv
# sykell

# merged_df$lynx <- ifelse(merged_df$validated_species == 'gaupe', 1, 0)
# merged_df$wolf <- ifelse(merged_df$validated_species == 'ulv', 1, 0)
# merged_df$hare <- ifelse(merged_df$validated_species == 'hare', 1, 0)
# merged_df$roedeer <- ifelse(merged_df$validated_species == 'raadyr', 1, 0)
# merged_df$moose <- ifelse(merged_df$validated_species == 'elg', 1, 0)
# merged_df$reddeer <- ifelse(merged_df$validated_species == 'hjort', 1, 0)
# merged_df$bear <- ifelse(merged_df$validated_species == 'bjorn', 1, 0)
# merged_df$human <- ifelse(merged_df$validated_species %in% c('menneske', 'sykell', 'motorsykkel'), 1, 0)


# Create the new columns using mutate and case_when
merged_df <- merged_df %>%
  mutate(
    lynx = ifelse(validated_species == 'gaupe', num_animals, 0),
    wolf = ifelse(validated_species == 'ulv', num_animals, 0),
    hare = ifelse(validated_species == 'hare', num_animals, 0),
    roedeer = ifelse(validated_species == 'raadyr', num_animals, 0),
    moose = ifelse(validated_species == 'elg', num_animals, 0),
    reddeer = ifelse(validated_species == 'hjort', num_animals, 0),
    bear = ifelse(validated_species == 'bjorn', num_animals, 0),
    human = ifelse(validated_species %in% c('menneske', 'sykell', 'motorsykkel','kjoeretoey'), num_animals, 0)
  )

head(merged_df)
sum(merged_df$roedeer)

merged_df$lynx[is.na(merged_df$lynx)] <- 0
merged_df$wolf[is.na(merged_df$wolf)] <- 0
merged_df$hare[is.na(merged_df$hare)] <- 0
merged_df$roedeer[is.na(merged_df$roedeer)] <- 0
merged_df$moose[is.na(merged_df$moose)] <- 0
merged_df$reddeer[is.na(merged_df$reddeer)] <- 0
merged_df$bear[is.na(merged_df$bear)] <- 0
merged_df$human[is.na(merged_df$human)] <- 0


table(merged_df$lynx)
table(merged_df$human)
table(merged_df$roedeer)

# sort #
merged_df <-  merged_df %>%
  arrange(location_id, date, hour)

# fill NA values for non-detections #
merged_df$date <- as.Date(merged_df$date)
merged_df <- merged_df %>%
  mutate(year = ifelse(is.na(year), year(date), year))
merged_df <- merged_df %>%
  mutate(month = ifelse(is.na(month), month(date), month))
head(merged_df)

############### 7C. SUM ACROSS ROWS AND SAVE ##################

# because some hours have multiple detections we are going to summarize across rows

merged_df_hour <- merged_df %>%
  group_by(location_id, date, hour) %>%
  summarize(
    Latitude = first(Latitude),
    LatitudeNum = first(LatitudeNum),
    Longitude = first(Longitude),
    LongitudeNum = first(LongitudeNum),
    #week = first(week),
    lynx = sum(lynx),
    wolf = sum(wolf),
    hare = sum(hare),
    roedeer = sum(roedeer),
    bear = sum(bear),
    human = sum(human),
    reddeer = sum(reddeer),
    moose = sum(moose),
    num_animals = sum(num_animals),
    month = first(month),  # Assuming month is constant within a week
  )

# check # (should all be one row now)
merged_df %>% filter(location_id == '1121', hour == 9, date == '2020-04-24')
data  %>% filter(location_id == '1121', hour == 9, date == '2020-04-24')
merged_df_hour %>% filter(location_id == '1121', hour == 9, date == '2020-04-24')

# save #
saveRDS(merged_df_hour, '/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week_hour_mar3.RDS')


############### 8. ADD LOCATIONS & FILTER LATITUDE ###############

dat2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/Neri_data/LocationsKatie.rds')
dat2 <- dat2[,c('LokalitetID','LatitudeWeb','LongitudeWeb','geometry')]
dat2$Latitude <-  st_coordinates(dat2$geometry)[, "Y"]
dat2$Longitude <- st_coordinates(dat2$geometry)[, "X"]

dat2 <- dat2 %>% st_drop_geometry()

## put in string format so that we don't have issues with rounding errors
dat2$Latitude <- as.character(dat2$Latitude)
dat2$Longitude <- as.character(dat2$Longitude)

merged_df_hour <- datamerge(data=merged_df_hour,metadata=dat2) ## should be 5982000

merged_df_hour$LatitudeNum <- as.numeric(merged_df_hour$Latitude)
merged_df_hour$LongitudeNum <- as.numeric(merged_df_hour$Longitude)

# keep only data with coordinates
Cams_woCoordinates <- merged_df_hour[is.na(merged_df_hour$Latitude), ] ### all the values that don't have coordinates, only have 1 photo. They are probably a test of some sort?
length(table(Cams_woCoordinates$location_id)) ## 451 cameras without coordinates

check <-  merged_df_hour[is.na(merged_df_hour$Latitude), ]
merged_df_hour <- merged_df_hour[merged_df_hour$Latitude < 64,]
length(unique(merged_df_hour$location_id)) # 754 cameras

#saveRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week_hour_mar3.RDS')

############### 9. ADD SUNTIME (checkpoint 1) #############
# (checkpoint; optional)
#merged_df_hour <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week_hour_mar3.RDS')
#

coords <- merged_df_hour[,c("LongitudeNum", "LatitudeNum")]
merged_df_hour$coords <- coords
Coords_SPDF1 <- SpatialPointsDataFrame(merged_df_hour$coords, proj4string = CRS("+init=epsg:4269 +proj=longlat +datum=NAD83"), data = merged_df_hour$coords)
# Coords_SPDF2 <- Coords_SPDF1[1:10000,]
# Coords_SPDF3 <- Coords_SPDF1[10001:20000,]

merged_df_hour$rads <- astroFns::hms2rad(merged_df_hour$hour)
merged_df_hour$timeZone <- lutz::tz_lookup_coords(lat=merged_df_hour$LatitudeNum, lon = merged_df_hour$LongitudeNum, warn = FALSE, method = "accurate")
Dates<- as.POSIXct(merged_df_hour$date, tz = 'Europe/Oslo') #"data_main$timeZone")
suntimes <- overlap::sunTime(merged_df_hour$rads, Dates, Coords_SPDF1)
merged_df_hour$sun.time <- suntimes

check <- merged_df_hour[complete.cases(merged_df_hour$sun.time),]

#saveRDS(merged_df_hour, '/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week_hour_mar3_chkpt2.RDS')

############## 10. ADD SNOW DEPTH & SWE (for density) (checkpoint 2) ##############
#merged_df_hour <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week_hour_mar3_chkpt2.RDS')

## snow depth
senorge <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/senorge_spring_2015_2024_updloc.csv')

# set up columns to match data for the merge
senorge$location_id <- senorge$loc
senorge$date <- as.Date(senorge$date1)
merged_df_hour$date <- as.Date(merged_df_hour$date)
merged_df_hour$location_id <- as.numeric(merged_df_hour$location_id)

## check na values
check <- merged_df_hour[is.na(merged_df_hour$location_id),]

senorge2 <- distinct(senorge,location_id, date, snowdepth.mm)
data_wsnow <- dplyr::left_join(merged_df_hour, senorge2, by = c('location_id', 'date'), unmatched = "drop")

# swe
swe <- read.csv("/Users/catherinebreen/Dropbox/Chapter3/data/swe_senorge_spring_2015_2024_updloc.csv")
swe$location_id <- swe$loc
swe$date <- as.Date(swe$date1)
swe <- swe[c('location_id','swe','date')]
head(swe)
swe2 <- distinct(swe,location_id, date, swe)
data_wsnowswe <- dplyr::left_join(data_wsnow, swe2, by = c('location_id','date'), unmatched = "drop")
head(data_wsnowswe)
nrow(data_wsnowswe)

data_wsnowswe$snowDensity.senorge <- (data_wsnowswe$snowdepth.mm / data_wsnowswe$swe) * 1000
hist(data_wsnowswe$snowdepth.mm)
hist(data_wsnowswe$snowDensity.senorge, breaks = 100)
max(data_wsnowswe$snowDensity.senorge)

#saveRDS(data_wsnowswe, '/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week_hour_mar3_chkpt3.RDS')

############## 11. ADD CROSSS 0 & TEMP (checkpoint 3) ######

#data_wsnowswe <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week_hour_mar3_chkpt3.RDS')

cross0df <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/minmaxavg_temp_senorge_CAMERAS.csv')
cross0df$location_id <- cross0df$loc
cross0df$date <- as.Date(cross0df$date1)
cross0df <- cross0df[, c("location_id", 'date', "cross_0")]

data_wsnowswe_cross0 <- dplyr::left_join(data_wsnowswe, cross0df, by = c('location_id','date'), unmatched = "drop")
check <- data_wsnowswe_cross0[data_wsnowswe_cross0$cross_0==1,]

avgtemp <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/avg_temp_senorge_CAMERAS.csv')
avgtemp$location_id <- avgtemp$loc
avgtemp$date <- as.Date(avgtemp$date1)
avgtemp$year <- year(avgtemp$date)
avgtemp <- avgtemp[c('location_id','avg.temp','date','year')]
head(avgtemp)

avgtemp <- avgtemp %>%
  select(location_id, avg.temp, date, year) %>%
  arrange(location_id, date) %>%
  group_by(location_id) %>%
  mutate(prev_day_temp = lag(avg.temp)) %>%
  mutate(hotcold_F = ifelse(avg.temp>=0, 'plus', 'minus'))
  ungroup()

data_wsnowswe_cross0temp <- dplyr::left_join(data_wsnowswe_cross0, avgtemp, by = c('location_id','date'), unmatched = "drop")
head(data_wsnowswe_cross0temp)
nrow(data_wsnowswe_cross0temp)

############## 13. ADD CANOPY COVER #############

treec <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/treecovariates.rds')
treec <- treec %>%
  arrange(location_id, year)
head(treec)

head(data_wsnowswe_cross0temp$year)
head(treec$year)
treec$year <- as.numeric(treec$year)
data_wsnowswe_cross0temp$year <- year(data_wsnowswe_cross0temp$date)
data_wsnowtree <- dplyr::left_join(data_wsnowswe_cross0temp, treec, by = c('location_id','year'), unmatched = "drop")

head(data_wsnowtree)

############## 14. HUMAN POP FOR NORWAY (opt) #############
# units are number of persons per sq km
# humanDensity <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/humanDensityNorway_GEE.csv')
# humanDensity$humandens <- humanDensity$SAMPLE_humanPop1
# humanDensity$location_id <- humanDensity$LokalitetID
# humanDensity <- humanDensity[, c("location_id", "humandens")] ## this is 2018!!!
# head(humanDensity)
# mean(na.omit(humanDensity$humandens))
#
# data_main <- dplyr::left_join(data_main, humanDensity, by = c('location_id'), unmatched = "drop")
#
# data_main$density <- data_main$swe / data_main$snowdepth.mm *100
# hist(data_main$density)
#
# head(data_main)
# nrow(data_main)
# table(data_main$roedeer)
# saveRDS(data_main, file = "data_main_occ_count_day_hour2.rds")
# head(data_main)

############## 15. PREDATOR DENSITY (opt) #####

# ## predator density covariate
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
# head(data_main)
# data_main$month <- as.numeric(data_main$month)
# data_main$year <- as.numeric(data_main$year)
# data_main <- left_join(data_main, pred_month, by = c('location_id','month', 'year'))
#
# data_main <- left_join(data_main, pred_seas, by = c('location_id', 'year'))
# data_main <- left_join(data_main, pred_year, by = c('location_id', 'year'))
#
# head(data_main)
#
# #data_main_occ_count_day_hour.rds
# saveRDS(data_main, file = "/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_occ_count_day_hour_pred2.rds")

############## 16. SAVE HOURLY DATAFRAME (checkpoint 4) ##############

saveRDS(data_wsnowtree, '/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_df_dayhour_mar4.RDS')

############## 17. SAVE WEEKLY DATAFRAME ##############

data_wsnowtree$week <- week(data_wsnowtree$date)
head(data_wsnowtree$week)

table(data_wsnowtree$cross_0)
table(data_wsnowtree$roedeer)

data_wsnowtree_week <- data_wsnowtree %>%
  group_by(location_id, year, week, hour) %>%
  summarize(
    Latitude = first(Latitude),
    LatitudeNum = first(LatitudeNum),
    Longitude = first(Longitude),
    LongitudeNum = first(LongitudeNum),
    date = first(date),
    month = first(month),  # Assuming month is constant within a week
    rads = first(rads),
    sun.time = first(sun.time),
    lynx = sum(lynx),
    wolf = sum(wolf),
    hare = sum(hare),
    roedeer = sum(roedeer),
    bear = sum(bear),
    human = sum(human),
    reddeer = sum(reddeer),
    moose = sum(moose),
    num_animals = sum(num_animals),
    snowdepth.mm = mean(snowdepth.mm, na.rm = TRUE),
    swe = mean(swe, na.rm = TRUE),
    snowDensity.senorge = mean(snowDensity.senorge, na.rm = TRUE),
    avg.temp = mean(avg.temp, na.rm = TRUE),
    hotcold_F = mode(hotcold_F),
    prev_day_temp = mean(prev_day_temp, na.rm = TRUE),
    treeloss = mean(treeloss, na.rm = TRUE),
    treecover = mean(treecover, na.rm = TRUE),
    cross_0 = sum(cross_0, na.rm = TRUE), ## sum days that cross 0 instead of taking the average
  )

head(data_wsnowtree_week) ## cam year week hour
table(data_wsnowtree_week$roedeer)
table(data_wsnowtree_week$hare)
table(data_wsnowtree_week$lynx)

table(data_wsnowtree_week$cross_0)
unique(data_wsnowtree_week$cross_0)


saveRDS(data_wsnowtree_week, '/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_df_weekhour_mar4.RDS')








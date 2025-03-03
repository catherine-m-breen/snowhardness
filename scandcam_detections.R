
# This script is to identify the predator density at an hourly, daily,
# weekly, monthly, and seasonal level

###### 0. Load packages ##############

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

############### 1. load in the data ###############
datamerge <- function(data, metadata){
  metadata$LokalitetID <- as.factor(metadata$LokalitetID)
  joined_tibble <- left_join(data, metadata, by = c("location_id" = "LokalitetID"))
  return(joined_tibble)
}

##
dat1 <- readRDS("/Users/catherinebreen/Dropbox/Chapter3/Neri_data/DataKatie.rds")
head(dat1)

nrow(dat1[dat1$validated_species == 'raadyr',])

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

nrow(data3[data3$validated_species == 'raadyr',])

dat2[dat2$Latitude == '59.7521741752405',]
data3[data3$Latitude == '59.7521741752405',]

data3$LatitudeNum <- as.numeric(data3$Latitude)
data3$LongitudeNum <- as.numeric(data3$Longitude)


data3 <- data3[c('location_id','captured_at','validated_species','num_animals','datetime','Latitude',"Longitude", "LatitudeNum", 'LongitudeNum')]
head(data3)

unique(data3$validated_species)

## all check how many times we have more than one detection of roe deer and hare
roedeer_num_detections <- data3 %>%
  filter(validated_species == 'raadyr', num_animals >= 1)
roedeer_detections <- data3 %>% filter(validated_species == 'raadyr')

# keep only data with coordinates
Cams_woCoordinates <- data3[is.na(data3$Latitude), ] ### all the values that don't have coordinates, only have 1 photo. They are probably a test of some sort?
length(table(Cams_woCoordinates$location_id)) ## 451 cameras without coordinates

data3 <-  data3[!is.na(data3$Latitude), ]
head(data3)
data <- data3
## if just want a small section
#data <- data[data$Latitude > 60,]
data <- data[data$LatitudeNum < 64,]

table(data$validated_species)

# #Removed 451 rows containing missing values (geom_point). 451 cameras are missing coordinates!!

############### 2. Find min and max dates for each camera ################

# convert to captured_at_exif to datetime and find the min and max dates
data$datetime <- as.POSIXct(data$captured_at, '%Y:%m:%d %H:%M:%S', tz = "Europe/Oslo") ## the timezone is EDT but this wrong
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
# data$year <- format(data$datetime, "%Y")
data$hour <- format(data$datetime, "%H")
data$hour <- as.numeric(data$hour)
# data$week <- week(data$datetime)
## we just want spring values so we are going to filter out any month that's not F, M, A, M
data <- data[data$month %in% c('02', '03', '04', '05'), ]

table(data$validated_species)

## check the max and min dates again
first(sort(data$datetime)) ## 2015 # 2017
last(sort(data$datetime)) ## 2021 #2023

## we will make an occupancy df for presence/absence for the following species
unique(data$validated_species)
table(data$validated_species)

############### 3. Thin data here (OPT) #####
# data
# data_thinned <- data %>%
#   mutate(datetime = as.POSIXct(paste(date, hour), format = "%Y-%m-%d %H")) %>%
#   group_by(location_id, date, hour, validated_species) %>%  # Add validated_species to grouping
#   arrange(datetime) %>%
#   filter(
#     !duplicated(cumsum(difftime(datetime, lag(datetime), units = "mins") > 30)) | is.na(lag(datetime))
#   ) %>%
#   ungroup()

############### 4. Inspect detections ###################

roedeer_num_detections_mon <- data %>%
  filter(validated_species == 'raadyr', num_animals >= 1)
roedeer_detections_mon <- data %>%
  filter(validated_species == 'raadyr')
table(roedeer_detections_mon$num_animals)
hist(roedeer_detections_mon$num_animals)
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

############### 5. create hourly occupancy dataframe for each cam ########################

## we will make a few variations, first we will do a standard occupancy for data
# Generate all dates and hours
# Generate all dates and hours for the years 2015 to 2018

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
unique(full_combinations$hour)
## make it look prettier
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

head(filtered_data)


#### convert to week

full_combinations <- filtered_data
full_combinations$week <- week(full_combinations$date)
full_combinations$year <- year(full_combinations$date)
head(full_combinations)

remove(filtered_combinations)
# %>%
### add week


#   filter(date >= first_observation_date)# %>%
# select(-first_observation_date)  # Optionally, remove the `first_observation_date` column

############### 6. Add detections for week and hour #############

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

nrow(data[data$validated_species =='raadyr',]) ### 200000 detections, but we reduce it quite a bit because of the model constraints
table(data[data$validated_species =='raadyr',]$validated_species)
## now add the wildlife data:
# Merge with original data
full_combinations$date <- as.character(full_combinations$date)
## if more than one species are detected in an hour it will add rows
data <- data[data$validated_species != 'nothing',]


############### 7. merge occupancy with detections data to get the detection data with the occupancy df ##########
merged_df <- left_join(full_combinations, data, by = c("location_id", "date", "hour"))
#
# head(merged_df)
#
# full_combinations %>% filter(location_id == 3160, year == 2022, week == 14) #2022-04-02
# merged_df %>% filter(location_id == 3160, year == 2022, week == 14) #2022-04-02
# data  %>% filter(location_id == 3160, date =='2022-04-02')

# Replace NAs with 0 for number_of_animals_detected
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
    human = ifelse(validated_species %in% c('menneske', 'sykell', 'motorsykkel'), num_animals, 0)
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

############### 8. Inspect hourly detections ##################

head(merged_df)
hist(merged_df$human)
hist(merged_df$roedeer)
hist(merged_df$lynx)

table(merged_df$lynx)
table(merged_df$human)
table(merged_df$roedeer)

# sort #
merged_df <-  merged_df %>%
  arrange(location_id, date, hour)
head(merged_df)

# fill NA values for non-detections #
merged_df$date <- as.Date(merged_df$date)
merged_df <- merged_df %>%
  mutate(year = ifelse(is.na(year), year(date), year))
merged_df <- merged_df %>%
  mutate(month = ifelse(is.na(month), month(date), month))
head(merged_df)

test <- merged_df %>% filter(location_id == 3160, year == 2022, week == 14)
table(merged_df$roedeer)

############### 9. save df ##############

merged_df <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_df_day_weeK_count.RDS')
table(merged_df$hour)

## make sure there aren't duplicates of hours, and if so, sum detections
merged_df_hour <- merged_df %>%
  group_by(location_id, date, hour) %>%
  summarize(
    week = first(week),
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
saveRDS(merged_df_hour, '/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week_hour.RDS')

merged_df_year_week <- merged_df %>%
  group_by(location_id, year, week, hour) %>%
  summarize(
    week = first(week),
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
saveRDS(merged_df_year_week, '/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week.RDS')


head(merged_df)
head(merged_df_hour)
table(merged_df_hour$roedeer)

# saveRDS(merged_df_year_week_thinned, '/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week_hour_THINNED_clean.RDS')
saveRDS(merged_df_hour, '/Users/catherinebreen/Dropbox/Chapter3/pres_abs_cam_day_hour_clean.RDS')

####### match with other covariates


############## 10. add snow depth ##############
## snow depth
senorge <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/senorge_spring_2015_2024_updloc.csv')
## temperature
cross0df <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/minmaxavg_temp_senorge_CAMERAS.csv')
cross0df

merged_df_hour <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week_hour.RDS')
merged_df_week <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_cam_week.RDS')

remove(merged_df)
merged_df <- merged_df_hour

# set up columns to match data for the merge
senorge$location_id <- senorge$loc
senorge$date <- as.Date(senorge$date1)
merged_df$date <- as.Date(merged_df$date)
merged_df$location_id <- as.numeric(merged_df$location_id)

## drop na values
merged_df <- merged_df[!is.na(merged_df$location_id),]

## testing
senorge[senorge$location_id == 22,]
test <- merged_df[merged_df$location_id == 22,]

head(merged_df)
nrow(merged_df)
nrow(senorge)
head(senorge)
senorge2 <- distinct(senorge,location_id,date, snowdepth.mm)
head(senorge2)
data_wsnow <- dplyr::left_join(merged_df, senorge2, by = c('location_id', 'date'), unmatched = "drop")
nrow(merged_df)
nrow(data_wsnow)
head(data_wsnow)

############## 11. add avg temperature & and swe (for density) ######
swe <- read.csv("/Users/catherinebreen/Dropbox/Chapter3/data/swe_senorge_spring_2015_2024_updloc.csv")
swe$location_id <- swe$loc
swe$date <- as.Date(swe$date1)
swe <- swe[c('location_id','swe','date')]
head(swe)
swe2 <- distinct(swe,location_id,date, swe)
data_wsnowswe <- dplyr::left_join(data_wsnow, swe2, by = c('location_id','date'), unmatched = "drop")
head(data_wsnowswe)
nrow(data_wsnowswe)

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
  ungroup()


data_wsnowswetemp <- dplyr::left_join(data_wsnowswe, avgtemp, by = c('location_id','date'), unmatched = "drop")
head(data_wsnowswetemp)
nrow(data_wsnowswetemp)

############## 12. separated by species and add canopy cover #############

treec <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/treecovariates.rds')
treec <- treec %>%
  arrange(location_id, year)
head(treec)

head(data_wsnowswetemp$year)
head(treec$year)
treec$year <- as.numeric(treec$year)
nrow(data_wsnowswetemp)
data_wsnowswetemp$year <- year(data_wsnowswetemp$date)
head(data_wsnowswetemp)
data_wsnowtree <- dplyr::left_join(data_wsnowswetemp, treec, by = c('location_id','year'), unmatched = "drop")
head(data_wsnowtree)
nrow(data_wsnowtree)

############## 13. add cross0 #####
head(cross0df)
cross0df$location_id <- cross0df$loc
cross0df$date <- as.Date(cross0df$date1)
head(cross0df)
cross0df <- cross0df[, c("location_id", 'date', "cross_0")] ##
data_wsnowtreecross0 <- dplyr::left_join(data_wsnowtree, cross0df, by = c('location_id', 'date'), unmatched = "drop")
head(data_wsnowtreecross0)
nrow(data_wsnowtreecross0)
data_wsnowtreecross0[data_wsnowtreecross0$cross_0==1,]

data_main <- data_wsnowtreecross0

############## 14. Human pop for Norway (opt) #############
# units are number of persons per sq km
humanDensity <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/humanDensityNorway_GEE.csv')
humanDensity$humandens <- humanDensity$SAMPLE_humanPop1
humanDensity$location_id <- humanDensity$LokalitetID
humanDensity <- humanDensity[, c("location_id", "humandens")] ## this is 2018!!!
head(humanDensity)
mean(na.omit(humanDensity$humandens))

data_main <- dplyr::left_join(data_main, humanDensity, by = c('location_id'), unmatched = "drop")

data_main$density <- data_main$swe / data_main$snowdepth.mm *100
hist(data_main$density)

head(data_main)
nrow(data_main)
table(data_main$roedeer)
saveRDS(data_main, file = "data_main_occ_count_day_hour2.rds")
head(data_main)

############## 15. Predator density (opt) #####

## predator density covariate
pred_day <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pred_dens_day_all.rds')
pred_month <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pred_dens_month_all.rds')
pred_seas <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pred_dens_seas_all.rds')
pred_year <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pred_dens_year_all.rds')

## conduct a bunch of left merges to get
colnames(pred_day) <- c('location_id','date', 'pred_day')
colnames(pred_month) <- c('location_id','month', 'year', 'pred_month')
colnames(pred_seas) <- c('location_id','year', 'pred_seas')
colnames(pred_year) <- c('location_id','year', 'pred_year')

head(data_main)

data_main <- left_join(data_main, pred_day, by = c('location_id','date'))
head(data_main)
data_main$month <- as.numeric(data_main$month)
data_main$year <- as.numeric(data_main$year)
data_main <- left_join(data_main, pred_month, by = c('location_id','month', 'year'))

data_main <- left_join(data_main, pred_seas, by = c('location_id', 'year'))
data_main <- left_join(data_main, pred_year, by = c('location_id', 'year'))

head(data_main)

#data_main_occ_count_day_hour.rds
saveRDS(data_main, file = "/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_occ_count_day_hour_pred2.rds")

############## 16. Save dataframes ##############

data_main$week <- week(data_main$date)

hist(data_main$density)
data_main_simp <- data_main[c('location_id','date','hour','week','year','lynx','wolf','hare','roedeer','bear',
                              'human','reddeer','moose','num_animals', 'density',
                              'month','snowdepth.mm','avg.temp','treeloss','treecover',
                              'cross_0','humandens')]
# head(data_main_simp)
# data_main_simp$density
# # l <- readRDS('data_main_occ_pred_count_day_simp.rds')
# # nrow(l)
# # nrow(data_main_simp)
# head(data_main_simp) ## cam year week hour
# table(data_main_simp$roedeer)
# table(data_main_simp$hare)
# table(data_main_simp$lynx)


table(data_wsnowtreecross0$cross_0)
saveRDS(data_main_simp, file = "data_main_occ_pred_count_cam_day_hour_simp2.rds") ###**** this one!
saveRDS(data_main, file = "/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_occ_pred_count_cam_day_hour_simp_NEW.rds") ###**** this one!
hist(data_main$density)



############# 17. Load dataframes ############
# fewer columns
data_main_simp <- readRDS("/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_occ_pred_count_cam_day_hour_simp2.rds")
# all columns
data_main <- readRDS('saveRDS(data_main, file = "/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_occ_pred_count_cam_day_hour_simp_NEW.rds') ###**** this one!')

data_main_year_day_hour <- data_main %>%
  group_by(location_id, date, hour) %>%
  summarize(
    year = first(year),
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
    snowdepth.mm = mean(snowdepth.mm, na.rm = TRUE),
    avg.temp = mean(avg.temp, na.rm = TRUE),
    treeloss = mean(treeloss, na.rm = TRUE),
    treecover = mean(treecover, na.rm = TRUE),
    cross_0 = sum(cross_0, na.rm = TRUE), ## sum days that cross 0 instead of taking the average
    humandens = mean(humandens, na.rm = TRUE),
    pred_day = mean(pred_day, na.rm = TRUE),
    pred_month = mean(pred_month, na.rm = TRUE),
    pred_seas = mean(pred_seas, na.rm = TRUE),
    pred_year = mean(pred_year, na.rm = TRUE)
  )

saveRDS(data_main_year_day_hour, file = "/Users/catherinebreen/Dropbox/Chapter3/data_main_occ_pred_count_cam_year_day_hour_simp.rds")
table(data_main_year_day_hour$cross_0)
table(data_main_year_day_hour$roedeer)

data_main_year_week <- data_main_simp %>%
  group_by(location_id, year, week, hour) %>%
  summarize(
    date = first(date),
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
    snowdepth.mm = mean(snowdepth.mm, na.rm = TRUE),
    avg.temp = mean(avg.temp, na.rm = TRUE),
    treeloss = mean(treeloss, na.rm = TRUE),
    treecover = mean(treecover, na.rm = TRUE),
    cross_0 = sum(cross_0, na.rm = TRUE), ## sum days that cross 0 instead of taking the average
    humandens = mean(humandens, na.rm = TRUE),
    pred_day = mean(pred_day, na.rm = TRUE),
    pred_month = mean(pred_month, na.rm = TRUE),
    pred_seas = mean(pred_seas, na.rm = TRUE),
    pred_year = mean(pred_year, na.rm = TRUE)
  )

head(data_main_year_week) ## cam year week hour
table(data_main_year_week$roedeer)
table(data_main_year_week$hare)
table(data_main_year_week$lynx)

table(data_main_year_week$cross_0)
unique(data_main_year_week$cross_0)
table(data_main_year_week$cross_0)


sum(data_main_year_week$roedeer)


#saveRDS(data_main_year_week, file = "data_main_occ_pred_count_cam_year_hour_simp_THINNED.rds")
saveRDS(data_main_year_week, file = " /Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_occ_pred_count_cam_year_hour_simp.rds")

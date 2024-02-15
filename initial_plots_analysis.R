library(glmmTMB)
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(corrplot)
library(lubridate)
library(gridExtra)
#install.packages('purrr')
library(purrr)
library(mgcv)
library(Metrics)

#install.packages('glmmTMB')

######################
# Data Prep
# fdata <- read.csv('/Users/catherinebreen/Documents/Chapter3/fieldwork/snowDat/2023fielddata_R.csv')
#
# # # just want camera data (transect is for supplemental)
# fdata <- fdata[fdata$Type == 'Camera',]
# fdata <- fdata[fdata$Weather != 'Snow',]
# fdata <- fdata[fdata$Weather != 'Snow/Hail',]
# fdata <- fdata[,c('county', 'visit','Date', 'Time', 'Latitude','Longitude','Elevation..m.', 'Aspect..d.', 'AirTemp', 'SnwDpth',
#                  'CnpyCvr', 'Hour','avgsinkDp', 'CldCvr', 'Slope', 'Weather', 'snwDnsty10cm')]
# head(fdata)
# nrow(fdata)
# #
# #
# # ## all camera trap temperature data for both N and S-facing cameras
# tdata <- read.csv('/Users/catherinebreen/Documents/Chapter3/fieldwork/temperatureData/all_temperature_data2.csv')
# #
# # # separate N and S camera data, bc we will need to do two separate joins
# tdataN <- tdata[tdata$aspect == 'N',]
# tdataS <- tdata[tdata$aspect == 'S',]
# #
# tdataN <- tdataN[,c('county','date', 'time', 'cam_temp')]
# colnames(tdataN) <- c('county','Date', 'Time', 'cam_tempN')
# #
# tdataS <- tdataS[,c('county','date', 'time', 'cam_temp')]
# colnames(tdataS) <- c('county','Date', 'Time', 'cam_tempS')
# #
# head(fdata)
# head(tdataN)
# head(tdataS)
# #
# # ## conduct a left merge to combine fdata w tdata
# fdata_tdataN <- dplyr::left_join(fdata, tdataN, by = c('county','Date', 'Time'))
# #
# fdata_tdata <- dplyr::left_join(fdata_tdataN, tdataS, by = c('county','Date', 'Time'), relationship="many-to-many")
#
# write.csv(fdata_tdata, 'fdata_tdata_snowDensity.csv')

fdata <- read.csv('/Users/catherinebreen/Documents/Chapter3/fdata_tdata_snowDensity_fromR.csv')
fdata$Date <- as.character(as.Date(fdata$Date, format = "%m/%d/%y"))
#remove(fdata_tdataN)

#senorge outputs
# We pulled this from Neri's R code:
senorge <- read.csv('/Users/catherinebreen/Documents/Chapter3/data/senorge_outputs_Jan1_May17-23.csv')
colnames(senorge)[2] <- 'Latitude'
colnames(senorge)[4] <- 'Date'
senorge <- senorge[,c('Latitude','snowdepth.mm', 'Date', 'freshsnow_waterEqui.mm','freshsnowdepth.mm','dsls', 'precipitation.mm', 'SWE.mm', 'snowmelt.mm','liqwatercontent')]
senorge['snowDensity.senorge'] <- senorge$SWE.mm / senorge$snowdepth.mm * 100
head(senorge)


## senorge temperature outputs
senorge_temp <- read.csv('/Users/catherinebreen/Documents/Chapter3/minmaxavg_temp_senorge.csv')
colnames(senorge_temp)[2] <- 'Latitude'
colnames(senorge_temp)[5] <- 'Date'
senorge_temp <- senorge_temp[,c('Latitude','Date', 'max.temp.c','min.temp.c','avg.temp.c','cross_0')]
head(senorge_temp)

data <- dplyr::left_join(fdata, senorge, by = c('Latitude','Date'), relationship="many-to-many")
data <-dplyr::left_join(data, senorge_temp, by = c('Latitude','Date'), relationship="many-to-many")

head(data)
data$avgsinkDp <- as.numeric(data$avgsinkDp)
data <- data[complete.cases(data$avgsinkDp),]


data['temp_swing'] <- 0

## cross 0?
for (i in seq(1: nrow(data))) {
  diff <- data$max.temp.c - data$min.temp.c
  print(diff)
  data$temp_swing <- diff
}

head(diff)

data$cnpyClass <- 0

## cross 0?
for (i in seq(1: nrow(data))) {
  if (data$CnpyCvr[i] < 50){
    data$cnpyClass[i] <- 0
  }
  else{data$cnpyClass[i] <- 1}
}

head(diff)

data$season <- 0

## cross 0?
for (i in seq(1: nrow(data))) {
  if (data$Month[i] <= 3){
    data$season[i] <- 'early'
  }
  else{data$season[i] <- 'late'}
}

head(diff)


## some group variables

## group by visit because we want peak and trough
result <- data %>%
  group_by(Date) %>%
  summarize(max_hard = max(avgsinkDp),
            min_hard = min(avgsinkDp),
            mean_hard = mean(avgsinkDp))

# View the result
print(result)

data <- left_join(data, result)


head(data)


#write.csv(data,'fdata_wsenorge_andtemp.csv')

#wind <- read.csv('/Users/catherinebreen/Documents/Chapter3/data/seklima_wind.csv') ## less ideal because will be hard to get for all cameras because I got this for weather stations

#clear sky radiation
# csr <- read.csv('/Users/catherinebreen/Documents/Chapter3/data/clear_sky_radiation_at_sites.csv')
# csr <- csr[,c('Latitude', 'Longitude','Date', 'Hour', 'csr')]
#
# data <- dplyr::left_join(data, csr, by = c('Date','Hour','Latitude', 'Longitude'))
# head(data)
#write.csv(data1, "fielddata_wCamTemp.csv")

#data <- read.csv('/Users/catherinebreen/Documents/Chapter3/fielddata_wCamTemp.csv')
# OR
#data <- data2

### suntime
data$rads <- astroFns::hms2rad(data$Hour)

coords <- data[,c("Longitude", "Latitude")]
Coords_SPDF <- SpatialPointsDataFrame(coords, proj4string = CRS("+init=epsg:4269 +proj=longlat +datum=NAD83"), data = data)
data$timeZone <- lutz::tz_lookup_coords(lat=data$Latitude, lon = data$Longitude, warn = FALSE, method = "accurate")
# Function to concatenate two strings
concatenate_strings <- function(date, time) {
  return(paste(date, time))
}

# Using mapply to apply the function to each corresponding pair of elements
data$datetime <- mapply(concatenate_strings, data$Date, data$Time)

Dates<- as.POSIXct(data$datetime, tz = "Europe/Oslo")


data$sun.time <- sunTime(data$rads, Dates, Coords_SPDF)

data$contTime <- as.numeric(difftime(data$datetime, '2023-01-01', units = "hours"))




head(data)
nrow(data)

data$visit <- as.factor(data$visit)
data$Month <- as.integer(data$Month)
data$Latitude <- as.numeric(data$Latitude)
data$Longitude <- as.numeric(data$Longitude)
data$Elevation..m. <- as.numeric(data$Elevation..m.)
data$Aspect..d. <- as.numeric(data$Aspect..d.)
data$CnpyCvr <- as.numeric(data$CnpyCvr)
data$Hour <- as.numeric(data$Hour)
data$cam_tempN <- as.numeric(data$cam_tempN)
data$cam_tempS <- as.numeric(data$cam_tempS)
data$doy <- as.numeric(as.character(yday(data$Date)))
data$dsls <- as.numeric(data$dsls)

data$AirTemp <- as.numeric(data$AirTemp)
data$SnwDpth <- as.numeric(data$SnwDpth)
data$CldCvr <- as.numeric(data$CldCvr)
data$precipitation.mm <- as.numeric(data$precipitation.mm)
data$SWE.mm <- as.numeric(data$SWE.mm)
data$snowmelt.mm <- as.numeric(data$snowmelt.mm)
data$liqwatercontent <- as.numeric(data$liqwatercontent)


### it's correlated

cor.test(data$AirTemp, data$avg.temp.c)
cor.test(data$snowDensity.senorge, data$snwDnsty10cm)

#data$FreezeThaw <- factor(data$FreezeThaw)

#data$Slope <- as.integer(data$Slope)
#data$Weather <- as.factor(data$Weather)

#data$Relative.Humidity <- as.integer(data$Relative.Humidity)
#data$sinkDp1 <- as.integer(data$sinkDp1)
#data$sinkDp2 <- as.integer(data$sinkDp2)
#data$snwDnsty10cm <- as.integer(data$snwDnsty10cm)

##### visualize ######
data <- data[data$Weather == 'None',]
## get out outliers
data <- data[data$avgsinkDp < 10,]


## for simple visualization, we will use a GAM
## https://ggplot2.tidyverse.org/reference/geom_smooth.html#:~:text=Controls%20the%20amount%20of%20smoothing,are%20fewer%20than%201%2C000%20observations.

plot1 <-  ggplot(data, aes(x = data$Hour, y = data$avgsinkDp)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc"))+ #method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  labs(title = 'all data hour of data with GAM',
    x = "Hour of Day",
    y = "Average Sink Depth (cm) ") + theme_bw()
plot1

plot2 <-  ggplot(data, aes(x = data$sun.time, y = data$avgsinkDp)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) + #method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs(title = 'all data sun time with GAM',
       x = "Time", #sun time
       y = "Average Sink Depth (cm) ") + theme_bw()
plot2

plot2a <- ggplot(data, aes(x = data$sun.time, y = data$avgsinkDp, group=cnpyClass, color =cnpyClass)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) + #method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs(title = 'all data sun time with GAM',
       x = "Time", #sun time
       y = "Average Sink Depth (cm) ") + theme_bw()
plot2a
plot2a + facet_wrap(~season)
plot2a + facet_wrap(~county, nrow=3)
plot2a + facet_wrap(season~county, nrow=3)

grid.arrange(plot1,plot2)

plot3 <-  ggplot(data, aes(x = data$Hour, y = data$avgsinkDp)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  labs(title = 'all data hour of data with sine lm',
       x = "Hour of Day",
       y = "Average Sink Depth (cm) ") + theme_bw()
plot3

data <- data[data$avgsinkDp < 10,]
plot4 <-  ggplot(data, aes(x = data$sun.time, y = data$avgsinkDp)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ sin(2 * pi * x/(2*pi)) + cos(2 * pi * x/(2*pi)), se = TRUE, color = "blue") +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs(title = 'all data sun time with sin and cos lm',
       x = "sun time",
       y = "Average Sink Depth (cm) ") + theme_bw()
plot4

grid.arrange(plot3,plot4)

#############
data[data$visit ==13,]

data1 <- data[data$Cycle ==1,]
data_oc <- data1[data1$CnpyCvr < 50,]
plot5 <-  ggplot(data_oc, aes(x = data_oc$sun.time, y = data_oc$avgsinkDp, group = data_oc$visit, color = snwDnsty10cm)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_line() + #method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs(title = 'open canopy sun time',
       x = "Time", #sun time
       y = "Average Sink Depth (cm) ") + theme_bw()
plot5

data_cc <- data1[data1$CnpyCvr > 50,]
data_cc <- data_cc[data_cc$avgsinkDp < 7,]
plot6 <-  ggplot(data_cc, aes(x = data_cc$sun.time, y = data_cc$avgsinkDp, group = data_cc$visit, color = snwDnsty10cm)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_line() + #method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs(title = 'closed canopy sun time',
       x = "Time", #sun time
       y = "Average Sink Depth (cm) ") + theme_bw()
plot6

plot5 + ylim(0,9) +
  facet_wrap(~visit, ncol = 4)
plot6 + ylim(0,9) +
  facet_wrap(~visit, ncol = 4)


data_cc$densityStart_num <-  as.numeric(data_cc$densityStart)
plot6 <-  ggplot(data_cc, aes(x = data_cc$cam_tempN, y = data_cc$avgsinkDp, group = data_cc$visit, color = data_cc$densityStart_num )) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = 'closed canopy sun time',
       x = "Temperature", #sun time
       y = "Average Sink Depth (cm) ") + theme_bw()
plot6

data_oc$densityStart_num <-  as.numeric(data_oc$densityStart)
plot6oc <-  ggplot(data_oc, aes(x = data_oc$cam_tempN, y = data_oc$avgsinkDp, group = data_oc$visit, color = data_oc$densityStart_fact )) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = 'open canopy sun time',
       x = "Temperature", #sun time
       y = "Average Sink Depth (cm) ") + theme_bw()
plot6oc

plot5 + ylim(0,9) +
  facet_wrap(~visit, ncol = 4)
plot6 + ylim(0,9) +
  facet_wrap(~visit, ncol = 4)




######
### redo gams

head(data)
plot7 <-  ggplot(data, aes(x = data$sun.time, y = data$avgsinkDp, group = data$cnpyClass, color = cnpyClass)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) + #method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs(title = 'closed canopy sun time',
       x = "Time", #sun time
       y = "Average Sink Depth (cm) ") + theme_bw()
plot7 + facet_wrap(~county, nrow=3)

#### temperature
plotT <- ggplot(data, aes(x = data$sun.time, y = data$AirTemp)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) + #method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs(title = 'air temp',
       x = "Time", #sun time
       y = "Average Sink Depth (cm) ") + theme_bw()
plotT + facet_wrap(~season, nrow=3)


plotT <- ggplot(data, aes(x = data$sun.time, y = data$cam_tempN)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) + #method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs(title = 'camera temp',
       x = "Time", #sun time
       y = "Temperature") + theme_bw()
plotT + facet_wrap(~season, nrow=3) #+

model_test <- gam(avgsinkDp ~ s(sun.time, bs = 'cc') + season + cnpyClass, data = data)
summary(model_test)


plotT <- ggplot(data, aes(x = data$sun.time, y = data$avg.temp.c)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc")) + #method = "lm", formula = y ~ sin(2 * pi * x/24), se = TRUE, color = "blue") +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  labs(title = 'air temp',
       x = "Time", #sun time
       y = "Average Sink Depth (cm) ") + theme_bw()
plotT + facet_wrap(~season, nrow=3) #+
   #geom_point(aes(y = avgsinkDp, group = cnpyClass, color = cnpyClass), position = position_jitter(width = 0.2, height = 0.2), color = "red", alpha = 0.5) +
  #geom_smooth(aes(y = avgsinkDp, group = cnpyClass, color = cnpyClass), method = "gam", formula = y ~ s(x, bs = "cc")) +
  #scale_y_continuous(name = "avgsinkDp", limits = c(0, 15), breaks = seq(0, 15, by = 5), sec.axis = )

#### testing if density is a meaniful intercept!!
cor.test(data$snowDensity.senorge, data$snwDnsty10cm)
test_data_ <- na.omit(data)
test_data_$dsls_factor <- as.factor(test_data_$dsls)
test_data_$snwDnsty10cm <- as.factor(test_data_$snwDnsty10cm)
test_data_$densityClass <- as.numeric(test_data_$densityClass)
test_data_$densityClass
test_data_$visit <- as.numeric(test_data_$visit)

cor.test(data$densityClass, data$sun.time)

z_model <- lme4::lmer(avgsinkDp ~ scale(sin(sun.time)) + scale(AirTemp) + (1 | snwDnsty10cm) , data = test_data_)
summary(z_model)
MuMIn::r.squaredGLMM(z_model)

z_model <- lme4::lmer(avgsinkDp ~ scale(sin(sun.time)) + scale(AirTemp) + (1 | snowDensity.senorge) , data = test_data_)
summary(z_model)
MuMIn::r.squaredGLMM(z_model)


library(car)
vif(z_model)
# Plot for AirTemp
cor.test(data$dsls, data$snowDensity.senorge)
cor.test(data$dsls, data$snwDnsty10cm)

hist(data$snwDnsty10cm, breaks= 20)
hist(data$dsls)
cor(test_data_[,c('avgsinkDp', 'sun.time', 'AirTemp', 'densityClass','CnpyCvr')])
##

## alternatives could be using the kernel density estimator
# Create a kernel density estimate plo

#######################

data.simp <- data[,c('visit', 'Latitude','Longitude','cam_tempN','cam_tempS', 'CnpyCvr', 'Hour','avgsinkDp', 'doy', 'dsls','AirTemp', 'SnwDpth', 'CldCvr', 'sun.time', 'avg.temp.c',
                     'cross_0', 'min.temp.c','max.temp.c','temp_swing','Month','cnpyClass', 'max_hard','min_hard','mean_hard',
                     'contTime', 'snwDnsty10cm', 'densityClass', 'densityStart', 'snowDensity.senorge')]

data.simp$visit <- as.numeric(data.simp$visit)
data.simp.na <- na.omit(data.simp)

# f.data.H.na <- f.data.Hsimp[complete.cases(f.data.Hsimp$cam_tempN), ]
# f.data.H.na <- f.data.H.na[complete.cases(f.data.H.na$dsls), ]
# f.data.H.na <- f.data.H.na[complete.cases(f.data.H.na$AirTemp), ]

sapply(data.simp.na, function(x) class(x))

## check for correlations
cor.table <- cor(data.simp.na)
corrplot(cor.table, method="shade",shade.col=NA, tl.col="black", tl.srt=45)

data.simp.na2 <- data.simp.na[,c('cam_tempN','cam_tempS', 'CnpyCvr', 'sun.time',
                         'contTime', 'snwDnsty10cm', 'snowDensity.senorge')]
cor.table2 <- cor(data.simp.na2)

corrplot(cor.table2, method="shade",shade.col=NA, tl.col="black", tl.srt=45)


#####################
data.simp.na_oc <- data.simp.na[data.simp.na$cnpyClass == 0,]
data.simp.na_cc <- data.simp.na[data.simp.na$cnpyClass == 1,]

model1a <- stats::lm(avgsinkDp ~ cam_tempN , data = data.simp.na_oc)
summary(model1a)
MuMIn::r.squaredGLMM(model1a)
model1b <- lm(avgsinkDp ~ cam_tempN , data = data.simp.na_cc)
summary(model1b)
model1c <- lm(avgsinkDp ~ cam_tempS , data = data.simp.na_oc)
summary(model1c)
model1d <- lm(avgsinkDp ~ cam_tempS , data = data.simp.na_cc)
summary(model1d)
model1e <- lm(avgsinkDp ~ AirTemp , data = data.simp.na_oc)
summary(model1e)

#data.simp.na_cc$visit <- as.factor(data.simp.na_cc$visit)
#data.simp.na_oc$visit <- as.factor(data.simp.na_oc$visit)
model2a <- lme4::lmer(avgsinkDp ~ cam_tempN + (1|visit) , data = data.simp.na_oc)
summary(model2a)
MuMIn::r.squaredGLMM(model2a)

model2b <- lme4::lmer(avgsinkDp ~ cam_tempN + (1|visit), data = data.simp.na_cc)
summary(model2b)
MuMIn::r.squaredGLMM(model2b)

model2c <- lme4::lmer(avgsinkDp ~ cam_tempS + (1|visit) , data = data.simp.na_oc)
summary(model2c)
MuMIn::r.squaredGLMM(model2c)

model2d <- lme4::lmer(avgsinkDp ~ cam_tempS + (1|visit) , data = data.simp.na_cc)
summary(model2d)
MuMIn::r.squaredGLMM(model2d)

model2e <- lme4::lmer(avgsinkDp ~ AirTemp + (1|visit) , data = data.simp.na_oc)
summary(model2e)
MuMIn::r.squaredGLMM(model2e)

model2e <- lme4::lmer(avgsinkDp ~ AirTemp + (1|visit) , data = data.simp.na_cc)
summary(model2e)
MuMIn::r.squaredGLMM(model2e)

################################

## HOUR ONLY

mode3a <- stats::lm(avgsinkDp ~ sun.time , data = data.simp.na_oc)
summary(mode3a)

model3 <- lm(avgsinkDp ~ sin(sun.time) , data = data.simp.na_oc)
summary(model3)
MuMIn::r.squaredGLMM(model3)

plot3 <-  ggplot(data, aes(x = data$sun.time, y = data$avgsinkDp, group= cnpyClass, color = cnpyClass)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ sin(2*pi * x/(2*pi)), se = TRUE, aes(color = cnpyClass)) +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc"), se = TRUE, aes(color = cnpyClass)) +
  labs(title = 'all data hour of data with sine lm',
       x = "hour in radians",
       y = "Average Sink Depth (cm) ") + theme_bw() +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight"))
plot3 +facet_wrap(~season)

plot3 <-  ggplot(data, aes(x = data$sun.time, y = data$avgsinkDp, group= cnpyClass, color = cnpyClass)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  #geom_smooth(method = "lm", formula = y ~ sin(2*pi * x/(2*pi)), se = TRUE, aes(color = cnpyClass)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cc"), se = TRUE, aes(color = cnpyClass)) +
  labs(title = 'all data hour of data with GAM',
       x = "hour in radians",
       y = "Average Sink Depth (cm) ") + theme_bw() +
  scale_x_continuous(breaks = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("midnight", "sunrise", "noon", "sunset", "midnight"))
plot3 +facet_wrap(~season)



## models
data.simp.na_oc_es <- data[data$season == 'early',]
data.simp.na_oc_es <- data.simp.na_oc_es[data.simp.na_oc_es$cnpyClass == 0,]
model3_es_oc <- lm(avgsinkDp ~ sin(sun.time) , data = data.simp.na_oc_es)
summary(model3_es_oc)
MuMIn::r.squaredGLMM(model3_es_oc)


### nonlinear least fit
plot3z <-  ggplot(data, aes(x = data$sun.time, y = data$avgsinkDp, group= cnpyClass, color = cnpyClass)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2), alpha = 0.5) +
  geom_smooth(method = "nls", formula = y ~a* sin(b* x),
                                                se =  FALSE, # this is important
                                                method.args = list(start = list(a = 2, b = 2)),
                                                aes(color = cnpyClass)) +
  labs(title = 'all data hour of data with sine lm',
       x = "hour in radians",
       y = "Average Sink Depth (cm) ") + theme_bw()
plot3z +facet_wrap(~season)



MuMIn::r.squaredGLMM(mode3a)
model3b <- lm(avgsinkDp ~ as.circular(sun.time) , data = data.simp.na_cc)
summary(model3b)


model3d <- lm(avgsinkDp ~ sin(sun.time) , data = data.simp.na_cc)
summary(model3d)
MuMIn::r.squaredGLMM(model3d)

model3c <- lme4::lmer(avgsinkDp ~ sin(2 * pi * (sun.time/(2*pi))) + (1|visit) , data = data.simp.na_cc)
summary(model3c)
MuMIn::r.squaredGLMM(model3c)



##########

plot_theme <- theme(
  text = element_text(size = 22),
  panel.background = element_blank(),
  legend.position = "bottom",
  axis.line = element_line(colour = "grey"),
  axis.text.x = element_text(
    angle = 0,
    hjust = 0.5,
    vjust = 1
  ))

######
## plot all data

# response <- 'avgsinkDp'
# expl <- names(f.data.H)[1:ncol(f.data.H)]
# response <- set_names(response)
# expl <- set_names(expl)


#### make this have open and closed canopy

scatter_fun <- function(x, y){
  ggplot(data.simp.na, aes(x = data.simp.na[[x]], y = data.simp.na[[y]], color = cnpyClass )) +
    geom_point() + geom_smooth(aes(group=cnpyClass), method = "loess", se = TRUE) + plot_theme + labs(x = x, y = y) +
    scale_color_manual(
      values = c("red", "blue"),
      breaks = unique(data.simp.na$cnpyClass),  # Set explicit breaks
      labels = unique(data.simp.na$cnpyClass)   # Set labels for breaks
    )
}

###########
# inspect all explanatory variables with the response variable to get a sense of whether certain variables are linear vs. non-linear
data.simp.na$cnpyClass <- as.factor(data.simp.na$cnpyClass)
head(data.simp.na)
### hourly variables
grid.arrange(scatter_fun("cam_tempN", "avgsinkDp"),
             scatter_fun("cam_tempS", "avgsinkDp"),
             scatter_fun("sun.time", "avgsinkDp"),
             scatter_fun("contTime", "avgsinkDp"),
             nrow = 2)

## daily variables
## min crust
grid.arrange(scatter_fun("dsls", "avgsinkDp"),
             scatter_fun("snowDensity.senorge", "avgsinkDp"),
             scatter_fun("snwDnsty10cm", "avgsinkDp"), nrow = 2)

# Interaction plots using ggplot2
plot_inter <- ggplot(data.simp.na, aes(x = snwDnsty10cm, y = avgsinkDp, color = sun.time)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_line(aes(group = sun.time), position = position_dodge(0.5), size = 1) +
  labs(title = "Density X Sun Time", x = "density", y = "sinking depth", color = "sun time") +
  theme_minimal()

plot_inter2 <- ggplot(data.simp.na, aes(x = snwDnsty10cm, y = avgsinkDp, color = cross_0)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_smooth(aes(group = cross_0), position = position_dodge(0.5), size = 1) +
  labs(title = "density X cross 0", x = "density", y = "sinking depth", color = "sun time") +
  theme_minimal()
plot_inter2

plot_inter3 <- ggplot(data.simp.na, aes(x = cam_tempS, y = avgsinkDp, color = cross_0)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_smooth(aes(group = cross_0), position = position_dodge(0.5), size = 1) +
  labs(title = "cam temp X cross 0", x = "cam temp", y = "sinking depth", color = "sun time") +
  theme_minimal()
plot_inter3
#### add canopy cover

#### add canopy cover
# Interaction plot using ggplot2 with multi-panel for 'Canopy'
ggplot(data.simp.na, aes(x = snwDnsty10cm, y = avgsinkDp, color = Hour)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_smooth(aes(group = Hour), position = position_dodge(0.5), size = 1) +
  facet_wrap(~cross_0, scales = "free_y") +
  labs(title = "Interaction Plot", x = "Factor1", y = "Response", color = "Factor2") +
  theme_minimal()

ggplot(data.simp.na, aes(x = cam_tempN, y = avgsinkDp, color = Month)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_smooth(aes(group = Month), position = position_dodge(0.5), size = 1) +
  labs(title = "cam temp X month", x = "cam temp", y = "sink depth", color = "Month") +
  theme_minimal()

ggplot(data.simp.na, aes(x = cam_tempN, y = snwDnsty10cm, color = cross_0)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_smooth(aes(group = cross_0), position = position_dodge(0.5), size = 1) +
  facet_wrap(~cnpyClass, scales = "free_y") +
  labs(title = "cam_temp x cross 0", x = "cam temp", y = "density", color = "Month") +
  theme_minimal()

ggplot(data.simp.na, aes(x = snwDnsty10cm, y = avgsinkDp)) +
         geom_point(position = position_dodge(0.5), size = 3) +
         geom_smooth()

grid.arrange(scatter_fun("dsls", "avgsinkDp"),
             scatter_fun("snowDensity.senorge", "avgsinkDp"),
             scatter_fun("snwDnsty10cm", "avgsinkDp"),
             #plot_inter,
             nrow = 2)

## max crust
grid.arrange(
             scatter_fun("dsls", "max_hard"),
             scatter_fun("doy", "max_hard"), nrow = 2)

## avg crust
grid.arrange(
             scatter_fun("dsls", "mean_hard"),
             scatter_fun("doy", "mean_hard"), nrow = 2)



########### CROSS ZERO INTERACTION ##########
# scatter_fun_cross0 <- function(x, y){
#   ggplot(data.simp.na, aes(x = data.simp.na[[x]], y = data.simp.na[[y]], color = cross_0 )) +
#     geom_point() + geom_smooth(aes(group=cross_0), method = "loess", se = TRUE) + plot_theme + labs(x = x, y = y) +
#     scale_color_manual(
#       values = c("red", "blue"),
#       breaks = unique(data.simp.na$cross_0),  # Set explicit breaks
#       labels = unique(data.simp.na$cross_0)   # Set labels for breaks
#     )
# }
# data.simp.na$cross_0 <- as.factor(data.simp.na$cross_0)
#
# grid.arrange(scatter_fun_cross0("cam_tempN", "avgsinkDp"),
#              scatter_fun_cross0("cam_tempS", "avgsinkDp"),
#              scatter_fun_cross0("sun.time", "avgsinkDp"),
#              scatter_fun_cross0("Hour", "avgsinkDp"),
#              nrow = 2)
#


## just data that crosses 0 only:

data.simp.na.cross0 <- data.simp.na[data.simp.na$cross_0 != 0,]
scatter_fun_onlycross1 <- function(x, y){
  ggplot(data.simp.na.cross0, aes(x = data.simp.na.cross0[[x]], y = data.simp.na.cross0[[y]], color = cnpyClass )) +
    geom_point() + geom_smooth(aes(group=cnpyClass), method = "loess", se = TRUE) + plot_theme + labs(x = x, y = y) +
    scale_color_manual(
      values = c("red", "blue"),
      breaks = unique(data.simp.na.cross0$cnpyClass),  # Set explicit breaks
      labels = unique(data.simp.na.cross0$cnpyClass)   # Set labels for breaks
    )
}

grid.arrange(scatter_fun_onlycross1("cam_tempN", "avgsinkDp"),
             scatter_fun_onlycross1("cam_tempS", "avgsinkDp"),
             scatter_fun_onlycross1("sun.time", "avgsinkDp"),
             scatter_fun_onlycross1("contTime", "avgsinkDp"),
             nrow = 2)


## split data for an 80/20 train/test split
index <- sample(1:nrow(data.simp.na), 0.8 * nrow(data.simp.na))
train_data <- data.simp.na[index, ]
test_data <- data.simp.na[-index, ]


######################3
# work following meeting with scandcam team: Neri Horndtvedt, John Linnell, John Odden, Inger Maren Rivrud.
## concerns about statistical significance. IF latitude is a big driver in snow hardness, then we might not have a big enough area to cover
## We could just take it out of the model, and assumee that the range 60-63 is roughly the same. Otherwise we might have to limit study area to Evenstad, etc
## Also said to cut Aspect and Slope, since those are likely included liqwatercontent
## They were also skeptical that preciptation, dsls, liqwatercontent should all be interactions.

## we have three data frames
# f.data.H
# f.data.H_oc
# f.data.H_cc

library(circular)
# f.data.H_oc$Hour_circle <- circular(f.data.H_oc$Hour, type = "angles", units = "hours", template = "clock24")
# f.data.H_cc$Hour_circle <- circular(f.data.H_cc$Hour, type = "angles", units = "hours", template = "clock24")
#
# ### or put it in radians
# f.data.H_oc$rads <- astroFns::hms2rad(f.data.H_oc$Hour)
# f.data.H_cc$rads <- astroFns::hms2rad(f.data.H_cc$Hour)

head(train_data)

cor.test(data.simp.na$cam_tempN, data.simp.na$avgsinkDp)
cor.test(data.simp.na$cam_tempS, data.simp.na$avgsinkDp)

## cam temp S
model <- lm(scale(avgsinkDp) ~ scale(cam_tempS)*scale(CnpyCvr) + scale(dsls)*scale(CnpyCvr) +
              scale(snwDnsty10cm)*scale(CnpyCvr) + scale(sun.time)*scale(CnpyCvr), data = train_data)
dredge(model)
best_model <- lm(scale(avgsinkDp) ~ scale(cam_tempS) + scale(CnpyCvr) + scale(dsls) + scale(snwDnsty10cm) + scale(sun.time) +
                   scale(cam_tempS)*scale(CnpyCvr) + scale(dsls)*scale(CnpyCvr) +
                   scale(snwDnsty10cm)*scale(CnpyCvr) + scale(sun.time)*scale(CnpyCvr), data = train_data)
summary(best_model)
r.squaredGLMM(best_model) ## R2 = 0.91
predicts <- predict(best_model, test_data)
rmse(test_data$avgsinkDp, predicts)

#####################
## cutting the nonsignificant values
model_simpl <- lm(scale(avgsinkDp) ~ scale(cam_tempS) + scale(CnpyCvr) + scale(snwDnsty10cm) + scale(sun.time) +
                    scale(cam_tempS)*scale(CnpyCvr) + scale(dsls)*scale(CnpyCvr) +
                   scale(sun.time)*scale(CnpyCvr), data = train_data)
r.squaredGLMM(model_simpl) ## R2 = 0.91
predicts <- predict(model_simpl, test_data)
rmse(test_data$avgsinkDp, predicts)
####################

## cam temp N
model <- lm(scale(avgsinkDp) ~ scale(cam_tempN)*scale(CnpyCvr) + scale(dsls)*scale(CnpyCvr) +
              scale(snwDnsty10cm)*scale(CnpyCvr) + scale(sun.time)*scale(CnpyCvr), data = train_data)
dredge(model)
best_model <- lm(scale(avgsinkDp) ~ scale(cam_tempN) + scale(CnpyCvr) + scale(dsls) + scale(snwDnsty10cm) + scale(sun.time) +
                   scale(cam_tempN)*scale(CnpyCvr) + scale(dsls)*scale(CnpyCvr) +
                   scale(snwDnsty10cm)*scale(CnpyCvr) + scale(sun.time)*scale(CnpyCvr), data = train_data)
summary(best_model)
r.squaredGLMM(best_model) ## R2 = 0.91
predicts <- predict(model, test_data)
rmse(test_data$avgsinkDp, predicts)

####
model <- lm(scale(avgsinkDp) ~ scale(cam_tempN)*scale(CnpyCvr) + scale(dsls)*scale(CnpyCvr) +
              scale(snowDensity.senorge)*scale(CnpyCvr) + scale(sun.time)*scale(CnpyCvr) +
              scale(CldCvr)*scale(CnpyCvr), data = train_data)
dredge(model)
best_model <- lm(scale(avgsinkDp) ~ scale(cam_tempN) + scale(CnpyCvr) + scale(dsls) + scale(snwDnsty10cm) + scale(sun.time) +
                   scale(cam_tempN)*scale(CnpyCvr) + scale(dsls)*scale(CnpyCvr) + scale(snowDensity.senorge)*scale(CnpyCvr) +
                   scale(snowDensity.senorge)*scale(CnpyCvr) + scale(sun.time)*scale(CnpyCvr), data = train_data)
summary(best_model)
r.squaredGLMM(best_model) ## R2 = 0.91
predicts <- predict(model, test_data)
rmse(test_data$avgsinkDp, predicts)

######## what about just density
model_dens <- lm(scale(avgsinkDp) ~ scale(cam_tempN)*scale(CnpyCvr) +
                   scale(snwDnsty10cm), data = train_data)
dredge(model_dens)
summary(model_dens)
r.squaredGLMM(model_dens) ## R2 = 0.91
predicts <- predict(model_dens, test_data)
rmse(test_data$avgsinkDp, predicts)

model_dens <- lm(scale(avgsinkDp) ~ scale(cam_tempN)*scale(CnpyCvr) +
                   scale(snowDensity.senorge), data = train_data)
dredge(model_dens)
summary(model_dens)
r.squaredGLMM(model_dens) ## R2 = 0.91
predicts <- predict(model_dens, test_data)
rmse(test_data$avgsinkDp, predicts)

## separated for open and closed canopy
train_data_oc <- train_data[train_data$CnpyCvr > 50,]
train_data_cc <- train_data[train_data$CnpyCvr < 50,]
test_data_oc <- test_data[test_data$CnpyCvr > 50,]
test_data_cc <- test_data[test_data$CnpyCvr < 50,]

model_oc <-glm(avgsinkDp ~ scale(cam_tempN) + scale(sun.time) + scale(snwDnsty10cm) +scale(CldCvr) +
                scale(dsls) + scale(doy), data = train_data_oc)
dredge(model_oc)
best_model_oc <- glm(avgsinkDp ~ scale(cam_tempN) + scale(snwDnsty10cm) +
                      scale(dsls) + scale(doy), data = train_data_oc)
summary(best_model_oc)
r.squaredGLMM(best_model_oc) ## R2 = 0.91
predicts <- predict(best_model_oc, test_data_oc)
rmse(test_data_oc$avgsinkDp, predicts)

model_cc <-lm(scale(avgsinkDp) ~ scale(cam_tempN) + scale(sun.time) + scale(snwDnsty10cm) +
                scale(dsls) + scale(doy) + scale(CldCvr), data = train_data_cc)
dredge(model_cc)
best_model_cc <- lm(scale(avgsinkDp) ~ scale(snwDnsty10cm) + scale(CldCvr) +
                      scale(doy), data = train_data_cc)
summary(best_model_cc)
r.squaredGLMM(best_model_cc) ## R2 = 0.91
predicts <- predict(best_model_cc, test_data_cc)
rmse(test_data_cc$avgsinkDp, predicts)
############################

###############################################
## do the whole thing but look at when it crosses 0
## should this be one of the constraints of the model?

train_data_c0 <- train_data[train_data$cross_0 > 0,]
test_data_c0 <- test_data[test_data$cross_0 > 0,]

model_c0 <- glm(avgsinkDp ~ scale(cam_tempS) + scale(CnpyCvr) + scale(snwDnsty10cm) + scale(CldCvr) + scale(cam_tempS)*scale(CnpyCvr), data = train_data_c0)
dredge(model_c0)
model_c0_best <- glm(avgsinkDp ~ scale(cam_tempS) + scale(CnpyCvr) + scale(snwDnsty10cm), data = train_data_c0)
summary(model_c0_best)
r.squaredGLMM(model_c0_best) ## R2 = 0.91
predicts <- predict(model_c0_best, test_data_c0)
rmse(test_data_c0$avgsinkDp, predicts)


train_data_c0_oc <- train_data_c0[train_data_c0$CnpyCvr < 50,]
train_data_c0_cc <- train_data_c0[train_data_c0$CnpyCvr > 50,]

test_data_c0_oc <- test_data_c0[test_data_c0$CnpyCvr < 50,]
test_data_c0_cc <- test_data_c0[test_data_c0$CnpyCvr > 50,]

model_c0_oc <- glm(avgsinkDp ~ scale(cam_tempS) + scale(CnpyCvr) + scale(snwDnsty10cm) + scale(CldCvr) + scale(cam_tempS)*scale(CnpyCvr), data = train_data_c0)
dredge(model_c0_oc)
model_c0_oc_best <- glm(avgsinkDp ~ scale(cam_tempS) + scale(CnpyCvr) + scale(snwDnsty10cm), data = train_data_c0_oc)
summary(model_c0_oc_best)
r.squaredGLMM(model_c0_oc_best) ## R2 = 0.91
predicts <- predict(model_c0_oc_best, test_data_c0_oc)
rmse(test_data_c0_oc$avgsinkDp, predicts)


###########################################
###########################################
######################################################################################
###########################################

# Sunday, February 11, 2024
## simplify model to airtemp, density and suntime and see what happens

print(train_data)
print(train_data_cc)
print(train_data_oc)
print(train_data_c0)
print(train_data_c0_cc)
print(train_data_c0_oc)

print(test_data)
print(test_data_cc)
print(test_data_oc)
print(test_data_c0)
print(test_data_c0_cc)
print(test_data_c0_oc)


## now we will test the simple model for R2 and RMSE
cor.test(sin(train_data_oc$sun.time), cos(train_data_oc$sun.time))
model_feb11a <- lme4::lmer(avgsinkDp ~ scale(AirTemp) + scale(sin(sun.time)) + scale(cos(sun.time)) + (1| snwDnsty10cm), data = train_data_oc)
model_feb11 <- lme4::lmer(avgsinkDp ~ scale(AirTemp) + scale(sin(sun.time)) + scale(cos(sun.time)) + (1| densityClass), data = train_data_oc)
model_feb11 <- lme4::lmer(avgsinkDp ~ scale(cam_tempS) + scale(sin(sun.time)) + scale(cos(sun.time)) + (1| densityClass), data = train_data_oc)
model_feb11 <- lme4::lmer(avgsinkDp ~ scale(cam_tempN) + scale(sin(sun.time)) + scale(cos(sun.time)) + (1| densityClass), data = train_data_oc)

## experiment with log link and guassian distribution
model_feb11 <- lme4::lmer(avgsinkDp ~ cam_tempN + sin(sun.time) + cos(sun.time) + (1| densityClass), data = train_data_oc,
                          family = gaussian()) #Gamma(link = "log")


summary(model_feb11)
r.squaredGLMM(model_feb11) ## R2 = 0.9
predicts <- predict(model_feb11, test_data_oc)
rmse(test_data_oc$avgsinkDp, predicts)

#### closed canopy
model_feb11a <- lm(avgsinkDp ~ scale(AirTemp) + scale(sin(sun.time)) + scale(cos(sun.time)) + (snwDnsty10cm), data = train_data_cc)
summary(model_feb11a)
model_feb11 <-  lme4::lmer(avgsinkDp ~ scale(AirTemp)+ scale(sin(sun.time)) + scale(cos(sun.time)) + (1| densityClass), data = train_data_cc)
model_feb11 <-lm(avgsinkDp ~scale(sin(sun.time)) + scale(cos(sun.time)) + scale(snwDnsty10cm) +scale(Latitude), data = train_data_cc)
model_feb11 <- lm(avgsinkDp ~ scale(cam_tempN) + scale(sin(sun.time)) + scale(cos(sun.time)) + (1|densityClass), data = train_data_cc)
#dredge(model_feb11)
library(DHARMa)
anova(model_feb11)
plot(model_feb11)
qqnorm(resid(model_feb11))
summary(model_feb11)
vif(model_feb11)
global_model_sims <- simulateResiduals(model_feb11)
plot(global_model_sims)
summary(testZeroInflation(model_feb11))
summary(model_feb11)
testDispersion(global_model_sims)
r.squaredGLMM(model_feb11) ## R2 = 0.9
predicts <- predict(model_feb11, test_data_cc)
rmse(test_data_cc$avgsinkDp, predicts)

model_feb11 <- mgcv::gam(avgsinkDp ~ s(scale(sun.time), bs= 'cc') , data = train_data_oc)
model_feb11 <- mgcv::gam(avgsinkDp ~ s(scale(cam_tempS)) + s(scale(sun.time), bs='cc') + s(densityClass,bs="re"), data = train_data_oc)
model_feb11 <- mgcv::gam(avgsinkDp ~ s(scale(cam_tempS)) + s(scale(sun.time), bs='cc') + s(densityClass,bs="re"), data = train_data_oc)

# model_feb11 <- mgcv::gam(avgsinkDp ~ scale(cam_tempS) + scale(sin(sun.time)) + (1| densityClass), data = train_data_oc)
# model_feb11 <- mgcv::gam(avgsinkDp ~ scale(cam_tempN) + scale(sin(sun.time)) + (1| densityClass), data = train_data_oc)
summary(model_feb11)
r.squaredGLMM(model_feb11) ## R2 = 0.9
predicts <- predict(model_feb11, test_data_oc)
rmse(test_data_oc$avgsinkDp, predicts)


cor.test(data$AirTemp, (data$Hour))


model_feb11 <- lme4::lmer(avgsinkDp ~ scale(AirTemp)*cnpyClass + scale(sin(sun.time))*cnpyClass + scale(cos(sun.time))*cnpyClass + (1| densityClass), data = train_data)
model_feb11 <- lme4::lmer(avgsinkDp ~ scale(cam_tempS)*cnpyClass + scale(sin(sun.time))*cnpyClass + scale(cos(sun.time))*cnpyClass + (1| densityClass), data = train_data)
model_feb11 <- lme4::lmer(avgsinkDp ~ scale(cam_tempN)*cnpyClass + scale(sin(sun.time))*cnpyClass +
                             scale(cos(sun.time))*cnpyClass + scale(Latitude)*cnpyClass + (1|densityClass), data = train_data) #, family = gaussian(link = "log")
plot(model_feb11)
qqnorm(resid(model_feb11))
#dredge(model_feb11)
summary(model_feb11)
r.squaredGLMM(model_feb11) ## R2 = 0.9
predicts <- predict(model_feb11, test_data)
rmse(test_data$avgsinkDp, predicts)




#############
#############
# spatial autocorrelation??
library(spdep)
library(dplyr)
dat_katie <- data
dens_katie <- data.frame(unique(data$densityClass))
names(dens_katie)[names(dens_katie)=="unique.data.densityClass."] <- "densityClass"

gis_katie <- data.frame(unique(data[c("densityClass", "Latitude", "Longitude")]))
gis_katie <- as.matrix(gis_katie[,2:3])

nb_katie <- dnearneigh(gis_katie,0,0.01) # 0.01 dec. deg. = 1.11 km

dat_katie <- dat_katie %>%
  group_by(densityClass) %>%
  mutate(avg.sinkDp = mean(avgsinkDp))
moran <- dat_katie %>%
  dplyr::select(densityClass, avg.sinkDp)
moran <- unique(moran)
moran <- left_join(dens_katie, moran)
moran <- moran %>%
  arrange(densityClass)
NDSI_cor_moran_pvalue <- moran.test(moran$avg.sinkDp,
                                    nb2listw(nb_katie, style="W",
                                             zero.policy = TRUE),
                                    zero.policy = TRUE,
                                    na.action = na.omit)




###########################################
###########################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################

model <- lmer(scale(avgsinkDp) ~ scale(cam_tempS) + (1|visit), data = data.simp.na)
dredge(model)
summary(model)
r.squaredGLMM(model) ## R2 = 0.91
AIC(model) ##

model <- lm(scale(avgsinkDp) ~ scale(cam_tempS)*scale(CnpyCvr), data = train_data)
dredge(model)
summary(model)
best_model <- lm(scale(avgsinkDp) ~ scale(cam_tempS) + scale(CnpyCvr) + scale(cam_tempS)*scale(CnpyCvr), data = train_data)
r.squaredGLMM(best_model) ## R2 = 0.27
AIC(best_model) ##


model <- lm(scale(avgsinkDp) ~ scale(cam_tempS)*scale(CnpyCvr) + scale(dsls)*scale(CnpyCvr), data = train_data)
dredge(model)
summary(model)
#best_model <- lm(scale(avgsinkDp) ~ scale(cam_tempS) + scale(CnpyCvr) + scale(cam_tempS)*scale(CnpyCvr), data = train_data)
r.squaredGLMM(model) ## R2 = 0.27
AIC(model) ##

train_data$cross_0 <- as.numeric(train_data$cross_0)
model <- lm(scale(avgsinkDp) ~ scale(cam_tempS)*scale(CnpyCvr) + scale(cam_tempS)*scale(cross_0) + scale(cam_tempS)*scale(dsls), data = train_data)
dredge(model)
summary(model)
best_model <- lm(scale(avgsinkDp) ~ scale(cam_tempS) + scale(CnpyCvr) + scale(cam_tempS)*scale(CnpyCvr), data = train_data)
r.squaredGLMM(model) ## R2 = 0.27
AIC(model) ##

############# only days that crossed 0 #############
train_data_cross0 <- train_data[train_data$cross_0 != 0,]
model <- lm(scale(avgsinkDp) ~ scale(cam_tempS)*scale(CnpyCvr) + scale(cam_tempS)*scale(dsls), data = train_data_cross0)
dredge(model)
summary(model)
r.squaredGLMM(model) ## R2 = 0.27
AIC(model) ##

###############
model <- lm(scale(avgsinkDp) ~ scale(cam_tempS)*scale(CnpyCvr) + scale(cam_tempS)*scale(cross_0) + scale(cam_tempS)*scale(dsls) , data = train_data)
dredge(model)
summary(model)
#best_model <- lm(scale(avgsinkDp) ~ scale(cam_tempS) + scale(CnpyCvr) + scale(cam_tempS)*scale(CnpyCvr), data = train_data)
r.squaredGLMM(model) ## R2 = 0.27
AIC(model) ##

a <- train_data[train_data$cross_0 ==0,]
unique(a['visit'])

model <- lm(scale(avgsinkDp) ~ scale(cam_tempS)*scale(CnpyCvr) + scale(cam_tempS)*scale(cross_0) + scale(cam_tempS)*scale(dsls) + scale(contTime) + scale(sun.time) + scale(SnwDpth), data = train_data)
dredge(model)
summary(model)
#best_model <- lm(scale(avgsinkDp) ~ scale(cam_tempS) + scale(CnpyCvr) + scale(cam_tempS)*scale(CnpyCvr), data = train_data)
r.squaredGLMM(model) ## R2 = 0.27
AIC(model) ##

#snowDensity.senorge

model <- lm(scale(avgsinkDp) ~ scale(cam_tempS)*scale(CnpyCvr) + scale(dsls)*scale(CnpyCvr) + scale(sun.time)*scale(CnpyCvr) + scale(snwDnsty10cm)*scale(CnpyCvr), data = train_data)
dredge(model)
summary(model)
#best_model <- lm(scale(avgsinkDp) ~ scale(cam_tempS) + scale(CnpyCvr) + scale(cam_tempS)*scale(CnpyCvr), data = train_data)
r.squaredGLMM(model) ## R2 = 0.27
AIC(model) ##


cor.test(data$dsls, data$snowDensity.senorge) ## they are moderately correlated
model <- lm(scale(avgsinkDp) ~ scale(cam_tempS)*scale(CnpyCvr) + scale(dsls)*scale(CnpyCvr) + scale(sun.time)*scale(CnpyCvr) + scale(snowDensity.senorge)*scale(CnpyCvr), data = train_data)
dredge(model)
summary(model)
#best_model <- lm(scale(avgsinkDp) ~ scale(cam_tempS) + scale(CnpyCvr) + scale(cam_tempS)*scale(CnpyCvr), data = train_data)
r.squaredGLMM(model) ## R2 = 0.27
AIC(model) ##


install.packages("AICcmodavg")
library(AICcmodavg)

models <- list(model, modSimp, modSimp1)
mod.names <- c('full model', 'simp1', 'simp2')
aictab(cand.set = models, modnames = mod.names)

# dsls & latitude are correlated

# big model
all.data.model <- lm(avgsinkDp ~ cam_tempN + Hour + Latitude + Aspect..d. + CnpyCvr + doy + dsls + CldCvr + liqwatercontent, data = f.data.all.na)
dredge(all.data.model)

all.data.model.best <- lm(avgsinkDp ~ Hour + Latitude + Aspect..d. + CnpyCvr + doy, data = f.data.all.na)
r.squaredGLMM(all.data.model.best)
AIC(all.data.model.best)

all.data.model.best.dsls <- lm(avgsinkDp ~ Hour + dsls + Aspect..d. + CnpyCvr + doy, data = f.data.all.na)
r.squaredGLMM(all.data.model.best.dsls)

all.data.model.best.dsls <- lm(avgsinkDp ~ csr + dsls + CnpyCvr + liqwatercontent, data = f.data.all.na)
r.squaredGLMM(all.data.model.best.dsls)

#include canopy cover if we don't have data
# ask Laura what to do when the canopy cover effect is happening in different directions
all.data.model.ccInterac <- lm(avgsinkDp ~ cam_tempN*CnpyCvr + Hour*CnpyCvr + Latitude*CnpyCvr + Aspect..d.*CnpyCvr + doy*CnpyCvr, data = f.data.all.na)
dredge(all.data.model.ccInterac) # this model doesn't really make sense because it doesn't include hour of the day

# big data with an interaction with canopy cover
all.data.model.ccInterac.best <- lm(avgsinkDp ~ Aspect..d. + cam_tempN + CnpyCvr + doy + Latitude +
                                      Aspect..d.*CnpyCvr + cam_tempN*CnpyCvr + Latitude*CnpyCvr + doy*CnpyCvr, data = f.data.all.na)
r.squaredGLMM(all.data.model.ccInterac.best)

#Try it with Hour*Latitude*doy effect because those three represent solar radiation:
# I also added dsls and CldCvr since those will be driving forces as well.
all.data.model.ccInter.LaHrDoyInter <- lm(avgsinkDp ~ CldCvr*CnpyCvr + CldCvr*csr + csr*CnpyCvr + cam_tempN*CnpyCvr + Latitude*doy*Hour*CnpyCvr + Aspect..d.*CnpyCvr + (dsls*CnpyCvr)^2 + dsls*CldCvr + Aspect..d.*csr, data = f.data.all.na)
output <- dredge(all.data.model.ccInter.LaHrDoyInter) #
head(output)

all.data.model.ccInterac.LaHrDoyInter.best <- lm(avgsinkDp ~ Aspect..d. + cam_tempN + CnpyCvr + doy + dsls + Hour + Latitude +
                                                   Aspect..d.*CnpyCvr + cam_tempN*CnpyCvr + CnpyCvr*Hour + Latitude*CnpyCvr + doy*Latitude + Hour*Latitude, data = f.data.all.na)
r.squaredGLMM(all.data.model.ccInterac.LaHrDoyInter.best)


all.data.model.ccInterac.clcv.LaHrDoyInter.best <- lm(avgsinkDp ~ Aspect..d. + cam_tempN + CnpyCvr + doy + dsls + Hour + Latitude + csr + CldCvr +
                                                        Aspect..d.*CnpyCvr + CldCvr*csr + cam_tempN*CnpyCvr + CnpyCvr*Hour + Latitude*CnpyCvr + doy*Latitude + CnpyCvr*doy + Hour*Latitude + csr*CnpyCvr + CldCvr*CnpyCvr, data = f.data.all.na)
r.squaredGLMM(all.data.model.ccInterac.clcv.LaHrDoyInter.best)


##################
# attempting to clean up models:

csr.model <- lm(csr ~ Latitude*Hour*doy*Aspect..d.*Elevation..m., data=f.data.all.na)
r.squaredGLMM(csr.model) ## this should have a pretty high R2 value

## do i need to separate the interactions? What happens if they go in different directions, aren't linear?
alldata.allinter.model <- lm(avgsinkDp ~ Aspect..d.*Latitude*Hour*doy + cam_tempN*Latitude*Hour*doy + dsls*Latitude*Hour*doy, data = f.data.all.na)
alldata.allinter.model.output <- dredge(alldata.allinter.model)

alldata.allinter.csr.model <- lm(avgsinkDp ~ Aspect..d.*csr*CnpyCvr + cam_tempN*csr*CnpyCvr + dsls*csr*CnpyCvr + precipitation.mm.*CnpyCvr, data = f.data.all.na)
alldata.allinter.csr.model.output <- dredge(alldata.allinter.csr.model)
head(alldata.allinter.csr.model.output)

model10 <- lm(avgsinkDp ~ Aspect..d. + cam_tempN + CnpyCvr + csr + dsls + cam_tempN*CnpyCvr + CnpyCvr*dsls + csr*dsls, data = f.data.all.na )
r.squaredGLMM(model10)

alldata.interOnly.model <- lm(avgsinkDp ~ Aspect..d.*csr*CnpyCvr*cam_tempN*dsls*precipitation.mm., data = f.data.all.na)
alldata.interOnly.model.output <- dredge(alldata.interOnly.model)
head(alldata.allinter.csr.model.output)

model11 <- lm(avgsinkDp ~ Aspect..d. + cam_tempN + CnpyCvr + csr + dsls + Aspect..d.*csr + cam_tempN*CnpyCvr + cam_tempN*csr + CnpyCvr*dsls + csr*dsls, data = f.data.all.na )
r.squaredGLMM(model11)

alldata.interOnly.model2 <- lm(avgsinkDp ~ Aspect..d.*csr*CnpyCvr*cam_tempN*dsls*precipitation.mm., data = f.data.all.na)

alldata.interOnly.model2 <- lm(avgsinkDp ~ csr*CnpyCvr*CldCvr + cam_tempN*(CnpyCvr)^2 + snowdepth.mm*dsls*precipitation.mm*liqwatercontent, data = f.data.all.na)
dredge(alldata.interOnly.model2)
model12 <- lm(avgsinkDp ~ cam_tempN + CldCvr + CnpyCvr + csr + dsls + liqwatercontent + precipitation.mm + snowdepth.mm + cam_tempN*CnpyCvr +
                CldCvr*csr + CnpyCvr*csr, data = f.data.all.na)
r.squaredGLMM(model12)

#alldata.interOnly.model.output <- dredge(alldata.interOnly.model2)
#head(alldata.allinter.csr.model.output)


## to do: use chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://cran.r-project.org/web/packages/sirad/sirad.pdf
## calculate solar radiation
install.packages('sirad')
library(sirad)
head(f.data.all.na)
extrat(63, 31)
extrat(61, 31)

sirad::ap("2023-01-30", 63.39, 10.30, SSD = 4)


#########################################################################################

cc.data.model <- lm(avgsinkDp ~ cam_tempN + Hour + Latitude + Aspect..d. + doy + dsls, data = fdata.closedCnpy.na)
dredge(cc.data.model)
# best model for cc
cc.data.model.best <- lm(avgsinkDp ~ scale(cam_tempN) + scale(Latitude), data = fdata.closedCnpy.na)
r.squaredGLMM(cc.data.model.best)

oc.data.model <- lm(avgsinkDp ~ cam_tempN + Hour + Latitude + Aspect..d. + doy + dsls, data = fdata.openCnpy.na)
dredge(oc.data.model)

oc.data.model.best <- lm(avgsinkDp ~ Hour + Aspect..d. + doy + dsls, data = fdata.openCnpy.na)
r.squaredGLMM(oc.data.model.best)


## takeaways:
#* Canopy cover is a huge driver of snow hardness, and open canopy locations
#* Might make sense to have two separate models that separates for open canopy and closed canopy cameras

############################

# Do it one more time but see if it changes when we normalize the variables

all.data.model.norm <- lm(avgsinkDp ~ scale(cam_tempN) + scale(Hour) + scale(Latitude) + scale(Aspect..d.) + scale(CnpyCvr) + scale(doy), data = f.data.all.na)
dredge(all.data.model.norm)

#take out canopy cover if we don't have data
all.data.model.woCC.norm <- lm(avgsinkDp ~ scale(cam_tempN) + scale(Hour) + scale(Latitude) + scale(Aspect..d.) + scale(doy), data = f.data.all.na)
dredge(all.data.model.woCC.norm) # this model doesn't really make sense because it doesn't include hour of the day

cc.data.model.norm <- lm(avgsinkDp ~ scale(cam_tempN) + scale(Hour) + scale(Latitude) + scale(Aspect..d.) + scale(doy), data = fdata.openCnpy.na)
dredge(cc.data.model.norm)

oc.data.model.norm <- lm(avgsinkDp ~ scale(cam_tempN) + scale(Hour) + scale(Latitude) + scale(Aspect..d.) + scale(doy), data = fdata.closedCnpy.na)
dredge(oc.data.model.norm)



##


# oc.airt.plot <- ggplot(data = fdata.openCnpy, aes(x=AirTemp, y = avgsinkDp, color = visit))+geom_point()+
#   scale_y_continuous(limits = c(0, 15))+geom_smooth(method = "lm", se = FALSE) + theme_bw() +ylim(0,15)
# oc.airt.plot
#
# model.RE <- lme4::lmer(abs(avgsinkDp) ~ AirTemp + (1|visit), data = fdata.openCnpy) #
# summary(model.RE)
#
# MuMIn::r.squaredGLMM(model.RE)

## camTemp S
oc.ctS.plot <- ggplot(data = fdata.openCnpy, aes(x=cam_tempS, y = avgsinkDp, color = visit))+geom_point()+
  scale_y_continuous(limits = c(0, 15))+geom_smooth(method = "lm", se = FALSE)+xlab('South-facing camera temperature (C)') + ylab('Sinking depth (cm)')+plot_theme

model <- lm(abs(avgsinkDp) ~ cam_tempS, data = fdata.openCnpy.na) #
summary(model)
r.squaredGLMM(model)

model.RE <- lme4::lmer(abs(avgsinkDp) ~ cam_tempS + (1|visit), data = fdata.openCnpy) #
summary(model.RE)

r.squaredGLMM(model.RE) ## for each day

## camTemp N
oc.ctN.plot <- ggplot(data = fdata.openCnpy, aes(x=cam_tempN, y = avgsinkDp, color = visit))+geom_point()+
  scale_y_continuous(limits = c(0, 15))
oc.ctN.plot
oc.ctN.plot <- oc.ctN.plot+geom_smooth(method = "lm", se = FALSE)+theme_bw()

model <- lm(abs(avgsinkDp) ~ cam_tempS, data = fdata.openCnpy) #
summary(model)
r.squaredGLMM(model)

model.RE <- lme4::lmer(abs(avgsinkDp) ~ cam_tempN + (1|visit), data = fdata.openCnpy) #
summary(model.RE)
r.squaredGLMM(model.RE) ## for each day

grid.arrange(oc.ctS.plot, oc.ctN.plot, nrow = 1)
#oc.airt.plot
################################



##########

fdata.openCnpy$Snow.depth <- as.integer(fdata.openCnpy$Snow.depth)
model.mixed <- lme4::lmer(abs(avgSinkDp) ~ cam_tempS + (1|visit) + Snow.depth + Latitude, data = fdata.openCnpy) #
summary(model.mixed)

r.squaredGLMM(model.mixed) ## for each day

options(na.action = "na.fail")
fdata.openCnpy.na <- na.omit(fdata.openCnpy)
fmodel.opencanpy.lm <- lm(avgsinkDp ~ cam_tempS + cam_tempN + AirTemp + SnwDpth + CldCvr + Weather , data = fdata.openCnpy.na)
dredge(fmodel.opencanpy.lm)



fdata.openCnpy.na <- na.omit(fdata.openCnpy)
fdata.openCnpy.na.cor <- fdata.openCnpy.na[,c('Latitude','Elevation..m.',
                                              'CnpyCvr', 'Hour', 'CldCvr','AirTemp',
                                              'avgsinkDp')]
fdata.openCnpy.na.cor <- #sapply(fdata.openCnpy.na.cor,as.numeric)
  #fdata.openCnpy.na.cor <- corrplot::corrplot(as.matrix(fdata.openCnpy.na.cor), is.corr=FALSE)

  cor.table <- cor(fdata.openCnpy.na.cor)
corrplot(cor.table, method="shade",shade.col=NA, tl.col="black", tl.srt=45)

fmodel.lm <- lm(avgsinkDp ~ cam_tempS + cam_tempN + AirTemp + SnwDpth + CldCvr + Weather + Latitude, data = fdata.openCnpy.na)
dredge(fmodel.lm)


########################
# closed canopy

fdata.closedCnpy <- data %>% filter (CnpyCvr > 40)

## airTemp
cc.airt.plot <- ggplot(data = fdata.closedCnpy, aes(x=AirTemp, y = avgsinkDp, color = visit))+geom_point()+geom_smooth(method = "lm", se = FALSE) + ylim(0,15) + theme(
  text = element_text(size = 22),
  panel.background = element_blank(),
  axis.line = element_line(colour = "grey"),
  axis.text.x = element_text(
    angle = 0,
    hjust = 0.5,
    vjust = 1
  ))
cc.airt.plot

## cam_tempS
cc.ctS.plot <- ggplot(data = fdata.closedCnpy, aes(x=cam_tempS, y = avgsinkDp, color = visit))+geom_point()+geom_smooth(method = "lm", se = FALSE)+ ylim(0,15) +
  xlab('South-facing camera temperature (C)') + ylab('Sinking depth (cm)')+ theme(
    text = element_text(size = 22),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 1
    ))
cc.ctS.plot

## cam_tempN
cc.ctN.plot <- ggplot(data = fdata.closedCnpy, aes(x=cam_tempN, y = avgsinkDp, color = visit))+geom_point()+geom_smooth(method = "lm", se = FALSE)+theme_bw()
cc.ctN.plot

model <- lm(abs(avgsinkDp) ~ AirTemp, data = fdata.closedCnpy) #
summary(model)
r.squaredGLMM(model)

model <- lm(abs(avgSinkDp) ~ cam_tempS, data = fdata.closedCnpy) #
summary(model)
r.squaredGLMM(model)

options(na.action = "na.fail")
fdata.closedCnpy.na <- na.omit(fdata.closedCnpy)
model.RE <- lme4::lmer(abs(avgsinkDp) ~ cam_tempS + (1|visit), data = fdata.closedCnpy.na) #
summary(model.RE)

r.squaredGLMM(model.RE) ## for each day


######################3

covariates <- read.csv('/Users/catherinebreen/Documents/Chapter3/data/senorge_outputs_Jan1_May17-23.csv')
head(covariates)
head(fdata.openCnpy)

dat_mod <- merge(fdata.openCnpy, covariates, by.x=c('Date', 'Latitude'), by.y=c('date1', 'loc'))
head(dat_mod)
# library(lubridate)
# day<-"31/08/2011"
# as.Date(parse_date_time(covariates['date1'][1,],"dmy"))
# as.Date(parse_date_time(fdata.openCnpy['Date'][1,],"dmy"))
#
# ## convert fdata.openCnpy Date column to correct format:
# covariates['date1'] <- as.Date(covariates['date1'])
# covariates['date1'] <- format(covariates['date1'], "%m-%d-%y")
#

## normalize all variables?
## Pros: 1) ensure that covariates with large magnitudes don't dominate results; 2) facilitate interpreting to the same scale
## Cons: we want to maintain everything in original units because they represent physical quantities with known units

## check for correlations to avoid over fitting the model


## sample dredge
options(na.action = "na.fail")
dat.mod.na <- na.omit(dat_mod)

## snow density
## this model doesn't make sense because avgSinkDp is changing hourly, but the covariates in this model will be changing daily
model.lme <- lmer(avgSinkDp ~ cam_tempS + cam_tempN + AirTemp + snowdepth.mm. + freshsnow_waterEqui.mm. + freshsnowdepth.mm. + precipitation.mm. + (1 | visit) , data = dat.mod.na)
model.lm <- lm(avgsinkDp ~ cam_tempS + cam_tempN + AirTemp + snowdepth.mm. + freshsnow_waterEqui.mm. + freshsnowdepth.mm. + precipitation.mm., data = dat.mod.na)
dredge(model.lme)
dredge(model.lm)

## instead we can predict the minimum sinking depth per day
## it will likely be related to liquid water content minimum air temp or something?
## i found these values by looking them in the field_summ_data.R script using the summarize/groupby functions in R.

## out of canopy
out.snowCrst.tbl <- data.frame(visit = c('1','2','3','4','5','6','7','8','9','10','11','12'),
                               snwCrust = c(6,5,3,4,0,0,0,3,5,3,0,0),
                               snCrusFirstHr = c(10,9,10,1,13,18,5,2,11,5,17,23),
                               snCrustLastHr = c(11,9,13,9,15,18,7,5,15,8,19,11))

in.snowCrst.tbl <-data.frame(visit = c('1','2','3','4','5','6','7','8','9','10','11','12'),
                             snwCrust = c(6,0,0,0,0,0,0,0,1,1,0,0),
                             snCrusFirstHr = c(),
                             snCrustLastHr = c())


dat.mod.na$snCrusHr <- c()
dat.mod.na$snCrust <- c()


## and also the hour at which that occurred



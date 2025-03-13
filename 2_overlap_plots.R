## JULY 23 2024

library(dplyr)
library(tidyr)
library(MuMIn)
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
library(overlap)
library(stringr)
library(sf)
library(sf)
library(sp)
library(cowplot)
library(circular)

##########################################################
###################### OVERLAP ANALYSIS #################
##########################################################

#data0 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/scandcam_obs_forGAM_wpredUPD2.RDS')
data0 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/scandcam_obs_forGAM_wpredUPD2_AIC.RDS')
data0 <- data0[c("location_id", "captured_at", "validated_species", "num_animals", "datetime", "Latitude", "Longitude",
                 "LatitudeNum","LongitudeNum", 'season',"date","month","year","hour","snowdepth.mm","swe",'density','avg.temp','cross_0','sun.time',
                 "treecover","treeloss",'cnpyClass','roedeer_binary','hare_binary')]

data0 <- data0 %>%
  mutate(snowDensity.senorge = density,
         cnpyClass_F = as.factor(cnpyClass),
         cnpyClass_F = ifelse(cnpyClass_F == 1, 'closed', 'open'),
         avg.temp.c = avg.temp,
         hotcold_F = as.factor(ifelse(avg.temp.c >= 0, 'warm', 'cold')),
         snowdepth.cm = snowdepth.mm
  )


modelhotcold2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/modelhotcold2.rds')
top_modelhotcold2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_modelhotcold2.rds')
data0$top_modelhotcold2 <- predict(top_modelhotcold2, newdata = data0, type = 'response', se.fit = TRUE)$fit #stats::

####################
### old code (to delete) ################
#data1 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred_feb25.RDS')
# data1 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred_mar3.RDS')
#
# mismatched_rows <- anti_join(data0, data1, by = c("location_id", "captured_at", "validated_species",
#                                                   "num_animals", "datetime", "Latitude", "Longitude",
#                                                   'treecover','treeloss','swe','cross_0'))
#
# print(mismatched_rows)
# hist(data0$sun.time)
# hist(data1$sun.time)
#
# data2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred_mar12b.RDS')
# data3 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred_mar12.RDS')
# data4 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred_mar13.RDS')
##############

table(data0$validated_species == 'hare')
table(data1$validated_species == 'hare')
table(data0$validated_species == 'raadyr')
table(data1$validated_species == 'raadyr')

data <- data0 #data0 #data1 #[data1['cross_0'] ==1,]



### overlap
roe_deer_simp1 <- data %>%
  filter(validated_species == "raadyr", snowdepth.cm > 10, cross_0 == 0) # snowdepth.mm < 150  ) # snowdepth.mm > 1)

roe_deer_simp2 <- data %>%
  filter(validated_species == "raadyr", snowdepth.cm > 10, cross_0 == 1) #snowdepth.mm < 150 ) # snowdepth.mm > 1)

# Combine the two datasets and add a condition column
roe_deer_combined <- bind_rows(
  roe_deer_simp1 %>% mutate(condition = "Non-FT Day"),
  roe_deer_simp2 %>% mutate(condition = "FT Day")
)
# Plot the density plot
roedeer1 <- ggplot(roe_deer_combined, aes(x = sun.time, color = condition)) +
  geom_density(alpha = 0.5, size = 2) +
  labs(
       x = "",
       y = "Density",
       color = "Condition",
       title = 'A)') +
  theme_classic() +
  geom_vline(xintercept = 3*pi/2, linetype = "dashed", color = "grey") + # Add vertical line at 3*pi/2
  geom_vline(xintercept = pi/2, linetype = "dashed", color = "grey") + # Add vertical
  scale_color_manual(values = c("Non-FT Day" = "#440154FF", "FT Day" = "#1F968BFF")) + # Set colors for conditions
  scale_x_continuous(
    breaks = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
    labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  theme(legend.position = "none", #c(0.5,0.95),
        axis.text.x = element_text(size = 16, angle = 0), # Rotate x-axis labels
        axis.title.x = element_text(size = 16),       # Increase x-axis title text size
        axis.text.y = element_text(size =16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20),
        plot.margin = margin(r = 30, l = 20),
        #panel.border = element_rect(color = "black", fill = NA, size = 1),
  ) + scale_y_continuous(
    name = "Kernel Density Esimate",
    limits = c(0,0.3),) +
  guides(color = guide_legend(title = NULL)) +
  geom_rug(aes(x = sun.time), sides = "b", alpha = 0.10)#+
  #eom_smooth(data= roe_deer_simp, aes(x = sun.time, y = predictionsAIC/20), linetype='dashed', se = FALSE, inherit.aes = FALSE)

sample1 <- circular(roe_deer_simp1$sun.time)
sample2 <- circular(roe_deer_simp2$sun.time)
result <- watson.wheeler.test(list(sample1, sample2))

t0 <- overlapEst(roe_deer_simp1$sun.time, roe_deer_simp2$sun.time,
                 type="Dhat1")

bt <- overlap::bootstrap(roe_deer_simp1$sun.time, roe_deer_simp2$sun.time, nb = 1000)
bootCI(t0, bt, conf=0.95)

############# hare ###################
hare_simp1 <- data %>%
  filter(validated_species == "hare", snowdepth.cm < 10, cross_0 == 0)

hare_simp2 <- data %>%
  filter(validated_species == "hare", snowdepth.cm > 10, cross_0 == 1)

hare_combined <- bind_rows(
  hare_simp1 %>% mutate(condition = "Non-FT Day"),
  hare_simp2 %>% mutate(condition = "FT Day")
)
# Plot the density plot
hare1 <- ggplot(hare_combined, aes(x = sun.time, color = condition)) +
  geom_density(alpha = 0.5, size = 2) +
  labs(
    x = "",
    y = "Density",
    color = "Condition",
    title = 'B)') +
  theme_classic() +
  geom_vline(xintercept = 3*pi/2, linetype = "dashed", color = "grey") + # Add vertical line at 3*pi/2
  geom_vline(xintercept = pi/2, linetype = "dashed", color = "grey") + # Add vertical
  scale_x_continuous(
    breaks = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
    labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  scale_color_manual(values = c("Non-FT Day" = "#440154FF", "FT Day" = "#1F968BFF")) + # Set co
  theme(legend.position = 'none', # c(0.5,0.95),
        axis.text.x = element_text(size = 16, angle = 0), # Rotate x-axis labels
        axis.title.x = element_text(size = 16),       # Increase x-axis title text size
        axis.text.y = element_text(size =16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size = 20),
        plot.margin = margin(r = 30, l = 20),
        #panel.border = element_rect(color = "black", fill = NA, size = 1),
  ) + scale_y_continuous(
    name = "Kernel Density Estimate",
    limits = c(0,0.4),) +
  guides(color = guide_legend(title = NULL)) +
  #geom_rug(aes(x = sun.time), sides = "b", alpha = 0.10) +
  # Second geom_rug (on top)
  geom_rug(data = hare_simp2, aes(x = sun.time), sides = "b", alpha = 0.5, color = "#1F968BFF") +
  geom_rug(data = hare_simp1, aes(x = sun.time), sides = "b", alpha = 0.1, color = "#440154FF")
  # This rug will appear on top#+
#eom_smooth(data= roe_deer_simp, aes(x = sun.time, y = predictionsAIC/20), linetype='dashed', se = FALSE, inherit.aes = FALSE)

grid.arrange(roedeer1, hare1, ncol=2)

sample1 <- circular(hare_simp1$sun.time)
sample2 <- circular(hare_simp2$sun.time)
result <- watson.wheeler.test(list(sample1, sample2))

t0 <- overlapEst(hare_simp1$sun.time, hare_simp2$sun.time,
                 type="Dhat1")

bt <- overlap::bootstrap(hare_simp1$sun.time, hare_simp2$sun.time, nb = 1000)
bootCI(t0,bt,conf=0.95)


#### sensitivity analysis for thresholds

# analyze_roe_deer <- function(data, human_density_threshold, predator_density_threshold, snowH) {
#   roe_deer <- data %>%
#     filter(validated_species == "raadyr", snowdepth.mm > 30, pred_month <= predator_density_threshold, humandens <= human_density_threshold)
#   hard <- roe_deer[roe_deer['mean_predictions'] < snowH, ]
#   soft <- roe_deer[roe_deer['mean_predictions'] > snowH, ]
#   sample1 <- circular(hard$sun.time)
#   sample2 <- circular(soft$sun.time)
#   result <- watson.wheeler.test(list(sample1, sample2))
#   t0 <- overlapEst(hard$sun.time, soft$sun.time, type = "Dhat1")
#   bt <- overlap::bootstrap(hard$sun.time, soft$sun.time, nb = 1000)
#   bt2 <- bootCI(t0, bt, conf = 0.95)
#   print(t0)
#   print(bt2)
#   print(result)
# }
#
# humans <- c(10, 20, 30, 40, 50 , 60, 70 , 80)
# preds <- c(0, 4, 8, 10)
# for (human_density_threshold in humans) {
#   for (predator_density_threshold in preds) {
#     analyze_roe_deer(data, human_density_threshold, predator_density_threshold, snowH=2)
#   }
# }
#
# analyze_roe_deer(data, human_density_threshold=40, predator_density_threshold=0, snowH=3)
#
#




##############
# Compute kernel density estimate

roe_deer_es <- data %>%
  filter(validated_species == "raadyr", snowdepth.cm > 10,  cross_0 == 1, hotcold_F =='cold' )
roe_deer_ls <- data %>%
  filter(validated_species == "raadyr", snowdepth.cm > 10,  cross_0 == 1, hotcold_F =='warm')

## sample sizes
test <- data %>%
  filter(validated_species == "hare",snowdepth.cm > 10)

hare_es <- data %>%
  filter(validated_species == "hare", snowdepth.cm > 10, cross_0 == 1, hotcold_F == 'cold') #hotcold_F == 'cold')#hotcold_F =='minus' )
hare_ls <- data %>%
  filter(validated_species == "hare", snowdepth.cm > 10, cross_0 == 1, hotcold_F == 'warm') #hotcold_F == 'warm') #hotcold_F =='plus')


sample1 <- circular(roe_deer_es$sun.time)
sample2 <- circular(roe_deer_ls$sun.time)
result <- watson.wheeler.test(list(sample1, sample2))
rd_t0 <- overlapEst(roe_deer_es$sun.time, roe_deer_ls$sun.time,
                 type="Dhat1")
rd_bt <- overlap::bootstrap(roe_deer_es$sun.time, roe_deer_ls$sun.time, nb = 1000)
bootCI(rd_t0,rd_bt,conf=0.95)


sample1 <- circular(hare_es$sun.time)
sample2 <- circular(hare_ls$sun.time)
result <- watson.wheeler.test(list(sample1, sample2))
t0 <- overlapEst(hare_es$sun.time, hare_ls$sun.time,
                 type="Dhat1")
bt <- overlap::bootstrap(hare_es$sun.time, hare_ls$sun.time, nb = 1000)
bootCI(t0,bt,conf=0.95)

roe_deer <- data %>%
  filter(validated_species == "raadyr", snowdepth.mm > 10, cross_0 ==1) #snowdepth.mm > 30, pred_month == 0, humandens < 40)
nrow(roe_deer)
hare <- data %>%
  filter(validated_species == "hare", snowdepth.mm > 10, cross_0 == 1) #snowdepth.mm > 30, pred_month == 0, humandens < 40)
nrow(hare)

#predictionsAIC
#predictions
#mean_predictions
#predictions_model4Hurdle
#predictions_modeltempB
plot_season_2roedeer <- ggplot(roe_deer, aes(x = sun.time, color= factor(hotcold_F))) +
  geom_density(alpha = 0.5, size = 2) +
  labs(x = "", y = "Kernel Density Estimate", title = 'C)') +
  scale_color_manual(values = c("#39568CFF", "#73D055FF"), labels = c("Cold", "Warm")) +
  geom_smooth(data = data, aes(x = sun.time, y = top_modelhotcold2 / 20), linetype = 'dashed', se = FALSE) +
  geom_vline(xintercept = c(pi / 2, 3 * pi / 2), linetype = "dashed", color = "grey") +
  scale_x_continuous(
    breaks = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
    limits = c(0, 2 * pi),
    labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  geom_rug(sides = "b", alpha = 0.6) +  # Add a rug plot on the bottom (b)
  theme_classic() +
  theme(legend.position = 'none', #c(0.8,0.8),
        axis.text.x = element_text(size = 16, angle = 0), # Rotate x-axis labels
        axis.title.x = element_text(size = 16),           # Increase x-axis title text size
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        #panel.border = element_rect(color = "black", fill = NA, size = 1),
        # axis.title.y = element_blank(),
        plot.title = element_text(size = 20)) +
  scale_y_continuous(
    name = "Kernel Density Estimate",
    limits = c(0, 0.4),
    sec.axis = sec_axis(~ . * 20, name = "Tuna Can Sinking Depth (cm)")) +
  guides(color = guide_legend(title = NULL))

plot_season_2hare <- ggplot(hare, aes(x = sun.time, color= factor(hotcold_F))) +
  geom_density(alpha = 0.5, size = 2) +
  labs(x = "", y = "Kernel Density Estimate", title = 'D)') +
  scale_color_manual(values = c("#39568CFF", "#73D055FF"), labels = c("Cold", "Warm")) +
  geom_smooth(data = data, aes(x = sun.time, y = top_modelhotcold2 / 20), linetype = 'dashed',
              se = FALSE, show.legend = TRUE) +
  geom_vline(xintercept = c(pi / 2, 3 * pi / 2), linetype = "dashed", color = "grey") +
  scale_x_continuous(
    breaks = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
    limits = c(0, 2 * pi),
    labels = c("midnight", "sunrise", "noon", "sunset", "midnight")) +
  geom_rug(sides = "b", alpha = 0.6) +  # Add a rug plot on the bottom (b)
  theme_classic() +
  theme(legend.position = 'none',# c(0.8,0.8),
        axis.text.x = element_text(size = 16, angle = 0), # Rotate x-axis labels
        axis.title.x = element_text(size = 16),           # Increase x-axis title text size
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        #panel.border = element_rect(color = "black", fill = NA, size = 1),
        # axis.title.y = element_blank(),
        plot.title = element_text(size = 20)) +
  scale_y_continuous(
    name = "Kernel Density Estimate",
    limits = c(0, 0.4),
    sec.axis = sec_axis(~ . * 20, name = "Tuna Can Sinking Depth (cm)")) +
  guides(color = guide_legend(title = NULL),
         linetype = guide_legend(title = NULL))


library(cowplot)
plot_grid(roedeer1, hare1, plot_season_2roedeer, plot_season_2hare, ncol=2)
grid.arrange(roedeer1, hare1, plot_season_2roedeer, plot_season_2hare, ncol=2)
grid.arrange(roedeer1, hare1, ncol=2) ## 1000 x 400
grid.arrange(plot_season_2roedeer, plot_season_2hare, ncol=2)


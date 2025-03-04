
# Catherine Breen
# March 4 2025
# GAM Modeling with updated model

############ 0. LOAD PACKAGES ###############
library(mgcv)
library(dplyr)
library(lutz)
library(beepr)
library(overlap)
library(ggplot2)
library(itsadug)
library(gridExtra)

datamerge <- function(data, metadata){
  metadata$LokalitetID <- as.factor(metadata$LokalitetID)
  joined_tibble <- left_join(data, metadata, by = c("location_id" = "LokalitetID"))
  return(joined_tibble)
}

############ 1. LOAD P/A DATA & FILTER FOR CROSS_0  #################
#dat <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_occ_pred_count_cam_year_hour_simp.rds')
#dat <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_occ_count_day_hour_pred.rds')
#dat <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/data_main_occ_pred_count_cam_day_hour_simp_NEW.rds')
#dat <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/scandcam_obs_forGAM_wpred_feb25.RDS')
#data_main_occ_count_day_hour_pred.rds
##data_main_occ_count_day_hour.rds
# dat <- readRDS("/Users/catherinebreen/Dropbox/Chapter3/r_outputs/ddata_main_year_week_Feb25.rds")

hourdat <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_df_dayhour_mar4.RDS')
weekdat <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/pres_abs_df_weekhour_mar4.RDS')

dat <- as.data.frame(weekdat)

dat$roedeer_binary <- ifelse(dat$roedeer >= 1, 1, 0)
dat$hare_binary <- ifelse(dat$hare >= 1, 1, 0)
dat$lynx_binary <- ifelse(dat$lynx >= 1, 1, 0)

dat <- dat[dat$cross_0>3,]

############ 2. PREDICT SNOW HARDNESS ###########

## predictions
# model_tweedie <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/linear_model_tweedie.rds')
# model_cpglm_base <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/model_cpglm_tweedie.rds')
# model_cpglm_temp <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/cpglm_tweedie_wmintemp.rds')
# model_temperature <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/models/model_GLM_TEMP.rds')
# model_tempCont <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_modeltempSimp.rds')
# model_tempBin <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_modeltempBsimp.rds')

#regular models
modelhotcold <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/modelhotcold.rds')
top_modelhotcold <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_modelhotcold.rds')
modelhotcold2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/modelhotcold2.rds')
top_modelhotcold2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_modelhotcold2.rds')
modelhotcold3 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/modelhotcold3.rds')
top_modelhotcold3 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_modelhotcold3.rds')

#hurdle models
hurdlehotcold <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/hurdlehotcold.rds')
#top_hurdlehotcold <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_hurdlehotcold.rds')
hurdlehotcold2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/hurdlehotcold2.rds')
top_hurdlehotcold2 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_hurdlehotcold2.rds')
hurdlehotcold3 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/hurdlehotcold3.rds')
top_hurdlehotcold3 <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/top_hurdlehotcold3.rds')

dat$modelhotcold <- predict(modelhotcold, newdata = dat, type = 'response', se.fit = TRUE)$fit
dat$modelhotcold2 <- predict(modelhotcold2, newdata = dat, type = 'response', se.fit = TRUE)$fit
dat$top_modelhotcold2 <- predict(top_modelhotcold2, newdata = dat, type = 'response', se.fit = TRUE)$fit
dat$modelhotcold3 <- predict(modelhotcold3, newdata = dat, type = 'response', se.fit = TRUE)$fit
dat$hurdlehotcold <- predict(hurdlehotcold, newdata = dat, type = 'response', se.fit = TRUE)
dat$hurdlehotcold2 <- predict(hurdlehotcold2, newdata = dat, type = 'response', se.fit = TRUE)
dat$hurdlehotcold3 <- predict(hurdlehotcold3, newdata = dat, type = 'response', se.fit = TRUE)


############ 3. ADD SNOW DEPTH BINS ################

dat$snowdepth_bin <- cut(dat$snowdepth.mm,
                                breaks = c(-Inf, 0, 30, 60, 90, 120, 150),
                                labels = c("0", "1-30", "30-60", "60-90", "90-120", "120-150"),
                                right = FALSE)

dat$snowdepth_bin_2 <- cut(dat$snowdepth.mm,
                                  breaks = c(-Inf, 40, 80, 120),
                                  labels = c("0-40", "40-80", "80-120"),
                                  right = FALSE)

############ 4. FILTER ################

hist(dat$snowdepth.mm, breaks=200)
dat0_150 <- filter(dat, cross_0 > 3, snowdepth.mm < 150)

table(dat0_150$roedeer)
table(dat0_150$hare)

# main_data_snow10 <- filter(main_data2, cross_0 > 3, snowdepth.mm > 10)
# #main_data_snow10 <- filter(main_data2, cross_0 > 3, snowdepth.mm > 10, snowdepth.mm <200)
# # main_data_snow10_200 <- filter(main_data2, cross_0 > 3, snowdepth.mm > 10, snowdepth.mm <200)
# # main_data_snow10_220 <- filter(main_data2, cross_0 > 3, snowdepth.mm > 10, snowdepth.mm <220)
# # main_data_snow10_180 <- filter(main_data2, cross_0 > 3, snowdepth.mm > 10, snowdepth.mm <180)
# main_data_snow_150 <- filter(main_data2, cross_0 > 3, snowdepth.mm <150)
# main_data_snow_10_150 <- filter(main_data2, cross_0 > 3, snowdepth.mm <150, snowdepth.mm > 10)
# main_data_snow_10_120 <- filter(main_data2, cross_0 > 3, snowdepth.mm <120, snowdepth.mm > 10)
# # main_data_snow10_noTroCam <- filter(main_data2, cross_0 > 3, snowdepth.mm > 10,
# #                                     location_id != '4652', location_id != '4655', location_id != '4654')
# main_data_snow15 <- filter(main_data2, cross_0 > 3, snowdepth.mm > 15)
# main_data_snow20 <- filter(main_data2, cross_0 > 3, snowdepth.mm > 20)
# head(main_data_snow10)

dat0_150_clean <- dat0_150 %>%
  filter(!is.na(roedeer_binary) & !is.na(sun.time) & !is.na(predictions_cpglm)
         & !is.na(snowdepth.mm))
table(dat0_150_clean$roedeer_binary)

# main_data_snow_10_120_clean <- main_data_snow_10_120 %>%
#   filter(!is.na(roedeer_binary) & !is.na(sun.time) & !is.na(predictions_cpglm)
#          & !is.na(snowdepth.mm))
# table(main_data_snow_10_120_clean$roedeer_binary)

############ 5. GAM MODEL FOR ANIMAL ACTIVITY #################

r_week_snowdepth <- bam(roedeer~ s(sun.time, bs='cc') +
                        s(predictions_cpglm, bs='tp') +
                        s(snowdepth.mm, bs='tp') +
                        te(sun.time, predictions_cpglm, snowdepth.mm, bs=c('cc','tp','tp'), k = c(5,5,5)),
                      data = dat0_150_clean, family = tw(), method = 'fREML', na.rm = TRUE)
beep()
saveRDS(r_week_snowdepth,'/Users/catherinebreen/Dropbox/Chapter3/r_outputs/r_week_snowdepth0_150.rds')
summary(r_week_snowdepth)
gam.check(r_week_snowdepth)

r_week_snowdepthB <- bam(roedeer_binary ~ s(sun.time, bs='cc') +
                          s(predictions_cpglm, bs='tp') +
                          s(snowdepth.mm, bs='tp') +
                          te(sun.time, predictions_cpglm, snowdepth.mm, bs=c('cc','tp','tp'), k = c(5,5,5)),
                        data = main_data_snow_150_clean, family = binomial(), method = 'fREML', na.rm = TRUE)
beep()
saveRDS(r_week_snowdepthB,'/Users/catherinebreen/Dropbox/Chapter3/r_outputs/r_weekB_snowdepth0_150.rds')
summary(r_week_snowdepthB)
gam.check(r_week_snowdepthB)

h_week_snowdepth <- bam(hare~ s(sun.time, bs='cc') +
                          s(predictions_cpglm, bs='tp') +
                          s(snowdepth.mm, bs='tp') +
                          te(sun.time, predictions_cpglm, snowdepth.mm, bs=c('cc','tp','tp'), k = c(5,5,5)),
                        data = main_data_snow_150_clean, family = tw(), method = 'fREML', na.rm = TRUE)
beep()
saveRDS(h_week_snowdepth,'/Users/catherinebreen/Dropbox/Chapter3/r_outputs/h_week_snowdepth0_150.rds')
summary(h_week_snowdepth)
gam.check(h_week_snowdepth)
gratia::draw(h_week_snowdepth,
             select = 4,
             scales='free',
             rug=NULL,
             continuous_fill = paletteer::scale_fill_paletteer_c("grDevices::Viridis"))


h_week_snowdepthB <- bam(hare_binary~ s(sun.time, bs='cc') +
                          s(predictions_cpglm, bs='tp') +
                          s(snowdepth.mm, bs='tp') +
                          te(sun.time, predictions_cpglm, snowdepth.mm, bs=c('cc','tp','tp'), k = c(5,5,5)),
                        data = main_data_snow_150_clean, family = binomial(), method = 'fREML', na.rm = TRUE)
saveRDS(h_week_snowdepthB,'/Users/catherinebreen/Dropbox/Chapter3/r_outputs/h_weekB_snowdepth0_150.rds')
beep()
summary(h_week_snowdepthB)
gam.check(h_week_snowdepthB)
gratia::draw(h_week_snowdepthB,
             select = 4,
             scales='free',
             rug=NULL,
             continuous_fill = paletteer::scale_fill_paletteer_c("grDevices::Viridis"))

############ 6. BINNED SNOW DEPTH #################

r_week_snowdepthbin <- gam(roedeer~ s(sun.time, bs='cc') +
                             s(predictions_cpglm, bs='tp') +
                             snowdepth_bin_2 +
                             te(sun.time, predictions_cpglm, by = snowdepth_bin_2, bs=c('cc','tp'), k = c(5,5)),
                           data = main_data_snow_10_120_clean, family = tw(), method = 'REML', na.rm = TRUE)
beep()
gam.check(r_week_snowdepth)
gam.check(r_week_snowdepthbin)
r_week_snowdepth_de <- summary(r_week_snowdepth)$dev.expl*100
r_week_snowdepthbin_de <- summary(r_week_snowdepthbin)$dev.expl*100


h_week_snowdepth <- gam(hare~ s(sun.time, bs='cc') +
                          s(predictions_cpglm, bs='tp') +
                          s(snowdepth.mm, bs='tp') +
                          te(sun.time, predictions_cpglm, snowdepth.mm, bs=c('cc','tp','tp'), k = c(5,5,5)),
                        data = main_data_snow_150, family = tw(), method = 'REML', na.rm = TRUE)

h_week_snowdepthbin <- gam(hare~ s(sun.time, bs='cc') +
                             s(predictions_cpglm, bs='tp') +
                             snowdepth_bin +
                             te(sun.time, predictions_cpglm, by = snowdepth_bin, bs=c('cc','tp'), k = c(5,5)),
                           data = main_data_snow_150, family = tw(), method = 'REML', na.rm = TRUE)

beep()





############# ROE DEER MODELING [old] ###############

# modelr <- gam(roedeer_binary ~ s(humandens), family = binomial(), data = main_data2, method = "REML")
# summary(modelr)
# plot(modelr, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
#      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])
#
# modelr_cts <- gam(roedeer ~ s(humandens), data = main_data2, method = "REML")
# summary(modelr_cts)
# tidy(modelr_cts)
# plot(modelr_cts, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
#      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])
#
# modelr_p <- gam(roedeer_binary ~ s(pred_month), family = binomial(), data = main_data2, method = "REML")
# summary(modelr_p)
# tidy(modelr_p)
# plot(modelr_p, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
#      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])
#
# modelr_p_cts <- gam(roedeer ~ s(pred_month), data = main_data2, method = "REML")
# summary(modelr_p)
# tidy(modelr_p_cts)
# plot(modelr_p_cts, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
#      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])

# modelr_ps <- gam(roedeer_binary ~ s(pred_seas), family = binomial(), data = main_data2, method = "REML")
# summary(modelr_ps)
# plot(modelr_ps, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
#      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])

# modelr_pd <- gam(roedeer ~ s(pred_day), data = main_data2, method = "REML")
# summary(modelr_pd)
# plot(modelr_pd, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
#      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])
#
# modelr_py <- gam(roedeer_binary ~ s(pred_year), family = binomial(), data = main_data2, method = "REML")
# summary(modelr_py)
# plot(modelr_py, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
#      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])


############# HARE MODELING [old] ##########################

# modelh_h <- gam(hare_binary ~ s(humandens), family = binomial(), data = main_data2, method = "REML")
# summary(modelh_h)
# tidy(modelh_h)
# plot(modelh_h, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
#      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])
#
# modelh_h_cts <- gam(hare ~ s(humandens), data = main_data2, method = "REML")
# summary(modelh_h_cts)
# tidy(modelh_h_cts)
# plot(modelh_h_cts, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
#      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])
#
# modelh_p <- gam(hare_binary ~ s(pred_month), family = binomial(), data = main_data2, method = "REML")
# summary(modelh_p)
# tidy(modelh_p)
# plot(modelh_p, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
#      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])
#
# modelh_p_cts <- gam(hare ~ s(pred_month), data = main_data2, method = "REML")
# summary(modelh_p_cts)
# tidy(modelh_p_cts)
# # plot(modelh_p_cts, pages = 1, scheme = 1, all.terms=TRUE, rug=TRUE, shade=TRUE,
# #      shade.col = 'lightblue', seWithMean=TRUE, shift=coef(modelr)[1])
# #
#
# # main_data_cross_0 <- main_data1[main_data1$cross_0 == 1,]
# mean(na.omit(main_data2$humandens))
# roedeer_plot_H <- ggplot(data = main_data2, aes(x = humandens, y = roedeer)) +
#   geom_jitter(height = 0.1) +
#   labs(x = "Human Density",
#        y = "Roe Deer Counts per Cam-Year-Week-Hour") +
#   theme_bw() +
#   theme(text = element_text(size = 14)) +
#   scale_y_continuous(breaks = c(0, 1))  +
#   geom_vline(xintercept = 47, linetype = "dashed", color = "red")
# roedeer_plot_H
#
# roedeer_plot_P <- ggplot(data = main_data2, aes(x = pred_month, y = roedeer)) +
#   geom_jitter(height = 0.1) +
#   labs(x = "Monthly Predator Density",
#        y = "Roe Deer Detection Presence (0,1) per Camera-Day-Hour") +
#   theme_bw() +
#   theme(text = element_text(size = 14)) +
#   scale_y_continuous(breaks = c(0, 1))  +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red")
# roedeer_plot_P
#
# hare_plot_H <- ggplot(data = main_data2, aes(x = humandens, y = hare)) +
#   geom_jitter(height = 0.1) +
#   labs(x = "Human Density",
#        y = "Roe Deer Counts per Cam-Year-Week-Hour") +
#   theme_bw() +
#   theme(text = element_text(size = 14)) +
#   scale_y_continuous(breaks = c(0, 1))  +
#   geom_vline(xintercept = 47, linetype = "dashed", color = "red")
# hare_plot_H
#
# hare_plot_P <- ggplot(data = main_data2, aes(x = pred_month, y = hare)) +
#   geom_jitter(height = 0.1) +
#   labs(x = "Monthly Predator Density",
#        y = "Roe Deer Detection Presence (0,1) per Camera-Day-Hour") +
#   theme_bw() +
#   theme(text = element_text(size = 14)) +
#   scale_y_continuous(breaks = c(0, 1))  +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red")
# hare_plot_P
#
# grid.arrange(roedeer_plot_H, roedeer_plot_P, hare_plot_H, hare_plot_P)

############# OTHER MODELS #####################
r_week_snowdepth120 <- gam(roedeer~ s(sun.time, bs='cc') +
                          s(predictions_cpglm, bs='tp') +
                          s(snowdepth.mm, bs='tp') +
                          te(sun.time, predictions_cpglm, snowdepth.mm, bs=c('cc','tp','tp'), k = c(5,5,5)),
                        data = main_data_snow_120_clean, family = tw(), method = 'REML', na.rm = TRUE)
beep()
r_week_snowdepth120 <- readRDS('r_week_snowdepth120.rds')
summary(r_week_snowdepth120)
gam.check(r_week_snowdepth120)

r_week_snowdepth_lin <- gam(roedeer~ s(sun.time, bs='cc') +
                          s(predictions_cpglm, bs='tp') +
                          snowdepth.mm +
                          te(sun.time, predictions_cpglm, snowdepth.mm, bs=c('cc','tp'), k = c(5,5)),
                        data = main_data_snow_10_120_clean, family = tw(), method = 'REML', na.rm = TRUE)
beep()
saveRDS(r_week_snowdepth_lin, 'r_week_snowdepth_lin.rds')
summary(r_week_snowdepth_lin)
gam.check(r_week_snowdepth_lin)
gratia::draw(r_week_snowdepth_lin,
             select = 3,
             scales='free',
             rug=NULL,
             continuous_fill = paletteer::scale_fill_paletteer_c("grDevices::Viridis"))


r_week_snowdepthlin_hardlin <- gam(roedeer~ s(sun.time, bs='cc') +
                              predictions_cpglm +
                              snowdepth.mm +
                              te(sun.time, predictions_cpglm, snowdepth.mm, bs=c('cc')),
                            data = main_data_snow_10_120_clean, family = tw(), method = 'REML', na.rm = TRUE)
beep()
saveRDS(r_week_snowdepthlin_hardlin, 'r_week_snowdepthlin_hardlin.rds')
summary(r_week_snowdepthlin_hardlin)
gratia::draw(r_week_snowdepthlin_hardlin,
             select = 2,
             scales='free',
             rug=NULL,
             continuous_fill = paletteer::scale_fill_paletteer_c("grDevices::Viridis"))









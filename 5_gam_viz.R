library(mgcv)
library(gratia)
library(dplyr)
library(ggplot2)
# Create new data for prediction


new_data <- dat0_150_clean %>%
  mutate(snowdepth_bin = ifelse(snowdepth.cm > 40, "Deep (40-150)", "Shallow (0-40)")) %>%
  group_by(sun.time, top_modelhotcold2, snowdepth_bin) %>%
  summarise(snowdepth.cm = mean(snowdepth.cm), .groups = "drop")  %>% # Take the mean within bins %>$
  sample_frac(0.1)  # Sample 10% of the data

table(new_data$snowdepth_bin)

## models #
# roe deer
r_week_snowdepthB_ti <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/r_weekB_snowdepth0_150_ti.rds')
# hare
h_week_snowdepthB_ti <- readRDS('/Users/catherinebreen/Dropbox/Chapter3/r_outputs/h_weekB_snowdepth0_150_ti.rds')


# Get predictions
preds_rd <- predict(r_week_snowdepthB_ti, newdata = new_data, type = "response", se.fit = TRUE)
preds_h <- predict(h_week_snowdepthB_ti, newdata = new_data, type = "response", se.fit = TRUE)

# Combine rd and h data with df
new_data$rd_fit <- preds_rd$fit
new_data$rd_se.fit <- preds_rd$se.fit

new_data$h_fit <- preds_h$fit
new_data$h_se.fit <- preds_h$se.fit

summary(new_data$rd_fit)

ggplot(new_data, aes(x = sun.time, y = top_modelhotcold2, color = rd_fit)) +
  geom_point(size = 3) +
  facet_wrap(~snowdepth_bin) +
  scale_color_viridis_c() +
  theme_minimal()

ggplot(new_data, aes(x = sun.time, y = top_modelhotcold2, color = h_fit)) +
  geom_point(size = 3) +
  facet_wrap(~snowdepth_bin) +
  scale_color_viridis_c() +
  theme_minimal()

###### heatmap ##############

# library(tidyr)
# library(heatmaply)
#
# mat <- new_data %>%
#   select(top_modelhotcold2, sun.time, rd_fit) %>%
#   pivot_wider(names_from = sun.time, values_from = rd_fit)
# mat_matrix <- as.matrix(mat[,-1])  # Remove 'top_modelhotcold2' column
# rownames(mat_matrix) <- mat$top_modelhotcold2
#
# heatmaply(mat_matrix,
#           xlab = "Sun Time",
#           ylab = "Top Model Hot/Cold",
#           main = "Predicted Roe Deer Activity")

###############################
### this one works!

new_data <- expand.grid(sun.time = seq(min(dat0_150_clean$sun.time), max(dat0_150_clean$sun.time), length.out = 100),
                        top_modelhotcold2 = seq(min(dat0_150_clean$top_modelhotcold2), max(dat0_150_clean$top_modelhotcold2), length.out = 5),
                        snowdepth.cm = seq(0, 150, length.out = 75))

new_data$snowbin <- cut(new_data$snowdepth.cm,
                        breaks = c(-0.1, 50, 150),
                        labels = c("shallow snow (<50 cm)", "deep snow (>50 cm)"),
                         include.lowest = TRUE)

new_data$snowbin2 <- cut(new_data$snowdepth.cm,
                        breaks = c(-0.1, 40, 150),
                        labels = c("shallow snow (<50 cm)", "deep snow (>50 cm)"),
                        include.lowest = TRUE)

new_data$snowbin3 <- cut(new_data$snowdepth.cm,
                         breaks = c(-0.1, 50, 100, 150),  # Slight negative to include 0 cm
                         labels = c("0-50", "51-100", "101-150"),
                         include.lowest = TRUE)

new_data$snowbin4 <- cut(new_data$snowdepth.cm,
                        breaks = c(-0.1, 0.1, 50, 100, 150),  # Slight negative to include 0 cm
                        labels = c("0", "1-50", "51-100", "101-150"),
                        include.lowest = TRUE)

new_data$rd_fit <- predict(r_week_snowdepthB_ti, newdata = new_data, type = "response")
new_data$h_fit <- predict(h_week_snowdepthB_ti, newdata = new_data, type = "response")

ggplot(new_data, aes(x = sun.time, y = top_modelhotcold2, fill = rd_fit)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0,0.02)) +  # Set consistent limits
  labs(x = "Sun Time", y = "Snow Hardness", fill = "Predicted Activity") +
  theme_minimal() +
  facet_wrap(~snowbin)

ggplot(new_data, aes(x = sun.time, y = top_modelhotcold2, fill = rd_fit)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0,0.02)) +  # Set consistent limits
  labs(x = "Sun Time", y = "Snow Hardness", fill = "Predicted Activity") +
  theme_minimal() +
  facet_wrap(~snowbin2)

ggplot(new_data, aes(x = sun.time, y = top_modelhotcold2, fill = rd_fit)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0,0.02)) +  # Set consistent limits
  labs(x = "Sun Time", y = "Snow Hardness", fill = "Predicted Activity") +
  theme_minimal() +
  facet_wrap(~snowbin3)

# ggplot(new_data, aes(x = sun.time, y = top_modelhotcold2, fill = rd_fit)) +
#   geom_tile() +
#   scale_fill_viridis_c(limits = c(0,0.05)) +  # Set consistent limits
#   labs(x = "Sun Time", y = "Snow Hardness", fill = "Predicted Activity") +
#   theme_minimal() +
#   facet_wrap(~snowbin4)


######## Hare

ggplot(new_data, aes(x = sun.time, y = top_modelhotcold2, fill = h_fit)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0,0.05)) +  # Set consistent limits
  labs(x = "Sun Time", y = "Snow Hardness", fill = "Predicted Activity") +
  theme_minimal() +
  facet_wrap(~snowbin)


##### final plot ######

text_size = 12
tick_positions <- c(0, pi/2, pi, 3*pi/2, 2*pi)
tick_labels <- c("midnight", "sunrise", "noon", "sunset", "midnight")

a <- ggplot(new_data, aes(x = sun.time, y = top_modelhotcold2, fill = rd_fit)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0,0.02)) +  # Set consistent limits
  labs(x = "Sun Time", y = "Snow Hardness", fill = "Predicted \n Activity") +
  theme_minimal() +
  facet_wrap(~snowbin) +
  xlab('')+
  ylab('Pred. Tuna Can Sinking Depth (cm)') +
  scale_x_continuous(breaks = tick_positions, labels = tick_labels) +
  ggtitle('A') +
  theme_classic() +
  theme(
        axis.text.x = element_text(size = text_size, angle = 0, colour = 'black'), # Rotate x-axis labels
        axis.title.x = element_text(size = text_size, colour = 'black'),       # Increase x-axis title text size
        axis.text.y = element_text(size =text_size, colour = 'black'),
        axis.title.y = element_text(size = text_size, colour = 'black'),
        plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"),
        plot.title = element_text(size = 20, colour = 'black'),
        panel.spacing = unit(1.5, "cm"))


b <- ggplot(new_data, aes(x = sun.time, y = top_modelhotcold2, fill = h_fit)) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0,0.03)) +  # Set consistent limits
  labs(x = "Sun Time", y = "Snow Hardness", fill = "Predicted \n Activity") +
  theme_minimal() +
  facet_wrap(~snowbin) +
  xlab('')+
  ylab('Pred. Tuna Can Sinking Depth (cm)') +
  scale_x_continuous(breaks = tick_positions, labels = tick_labels) +
  ggtitle('B') +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = text_size, angle = 0, colour = 'black'), # Rotate x-axis labels
    axis.title.x = element_text(size = text_size, colour = 'black'),       # Increase x-axis title text size
    axis.text.y = element_text(size =text_size, colour = 'black'),
    axis.title.y = element_text(size = text_size, colour = 'black'),
    plot.margin = unit(c(0.5, 1, 0.5, 0.5), "cm"),
    plot.title = element_text(size = 20, colour = 'black'),
    panel.spacing = unit(1.5, "cm"))

grid.arrange(a,b, ncol=1)



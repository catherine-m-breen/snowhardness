## create covariate for tree canopy cover for each year, using tree loss to account for how much tree loss has occurred

library(tidyverse)
library(tidyr)
library(lubridate)
library(dplyr)
library(ggplot2)

## tree canopy
treecanopy <- read.csv('/Users/catherinebreen/treecanopycover.csv')

treec <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/2015treecanopy_May2024.csv')
colnames(treec) <- c('location_id','2015')
treeloss <- read.csv('/Users/catherinebreen/Dropbox/Chapter3/data/treeloss23_May2024.csv')
colnames(treeloss) <- c('location_id','treeloss')
treec <- left_join(treec, treeloss)
head(treec)

# Initialize tree16 column with tree15 values
treec$'2014' <- treec$'2015'
treec$'2015' <- treec$'2015'
treec$'2016' <- treec$'2015'
treec$'2017' <- treec$'2015'
treec$'2018' <- treec$'2015'
treec$'2019' <- treec$'2015'
treec$'2020' <- treec$'2015'
treec$'2021' <- treec$'2015'
treec$'2022' <- treec$'2015'
treec$'2023' <- treec$'2015'
treec$'2024' <- treec$'2015'

# Replace tree16 values where treeloss >= 16 with 0
treec$'2016'[treec$treeloss >= 14 & treec$treeloss <= 16] <- 0
treec$'2017'[treec$treeloss >= 14 & treec$treeloss <= 17] <- 0
treec$'2018'[treec$treeloss >= 14 & treec$treeloss <= 18] <- 0
treec$'2019'[treec$treeloss >= 14 & treec$treeloss <= 19] <- 0
treec$'2020'[treec$treeloss >= 14 & treec$treeloss <= 20] <- 0
treec$'2021'[treec$treeloss >= 14 & treec$treeloss <= 21] <- 0
treec$'2022'[treec$treeloss >= 14 & treec$treeloss <= 22] <- 0
treec$'2023'[treec$treeloss >= 14 & treec$treeloss <= 23] <- 0
treec$'2024'[treec$treeloss >= 14 & treec$treeloss <= 24] <- 0

long <- treec %>%
  pivot_longer(
    cols = `2014`:`2024`,
    names_to = "year",
    values_to = "treecover"
  )

treec <- long[c('location_id','treeloss','year','treecover')]
# treec$year <- as.numeric(treec$year)
treec$location_id <- as.numeric(treec$location_id)
head(treec)

saveRDS(treec, '/Users/catherinebreen/Dropbox/Chapter3/treecovariates.rds')





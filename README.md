This repository supports the analysis for the paper in prep, "Diel and seasonal cycles of snow hardness predict changes in activity patterns for roe deer but not mountain hares."

# Data availability 

- All snow hardness data is available at snow_measurements_dat.csv
- Wildlife observation data from the Scandcam camera-trap network is availabe upon request. 

# Code availability
  
There are several coding scripts to support analysis 
- snowhardness_model.R: cleans field data and derives snow hardness linear model
- scandcam_analysis.R: cleans data and makes snow hardness predictions from snow hardness model
- overlapPlots.R: Identifies overlap depending on different snow hardness conditions
- gam_model.R: Idenifies frequency depending on hourly snow hardness values
- treecanoycover.R: identifies tree canopy for each camera (covariates)

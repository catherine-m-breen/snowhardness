This repository supports the analysis for the paper in prep, "Diel and seasonal cycles of snow hardness predict changes in activity patterns for roe deer but not mountain hares."

# Data availability 

- All snow hardness data is available at snow_measurements_dat.csv
- Wildlife observation data from the Scandcam camera-trap network is availabe upon request. 

# Code availability
  
There are several coding scripts to support analysis 
- 0_snowhardness_model.R: cleans field data and derives snow hardness linear model
- 1_scandcam_predictions.R: predicts snowhardness values at scandcam detections
- 2_overlapPlots.R: Identifies overlap depending on different snow hardness conditions
- 3_scandcam_pres_ab.R: converts scandcam detections to pres/abs framework for GAM modeling
- 4_gam_predictions.R: uses GAM models to predict hourly frequency of animals in relation to snow hardness
- utility scripts folder:
  - treecanoycover.R: identifies tree canopy for each camera (covariates)
  - find_lag.R: calculates lag from snow hardness model
    
The following data folder contains data used in analysis: 
- fdata_tdata_snowDensity_fromR.csv -- cleaned field data from 2023 field season

The following folder(s) is for reference only: 
- old_scripts: former scripts using during initial stages of analysis (for reference only)
  

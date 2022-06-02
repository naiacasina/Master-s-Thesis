# Master-s-Thesis

## Code in R

### Bangladesh.R
This is the main file to create the long-format dataframe. 
- I gather all the files of interest for the analysis from the BIHS
- The datasets are harmonized so that they match with admin3 shapefile
- I create a long format dataframe for each hh between 2006-2019  with the dependent variable of analysis (migration), independent variables of interest (natural hazards) and a set of individual-level controls and Upazila-level controls.

### cropping_and_aggregating.R
First I gather all raster data of interest for the project:
a) Air quality (proxied by pm2.5)
b) Flooding events
c) Food security (proxied by child stunting)
d) Drought (proxied by SPI values)
e) Population
- I extract zonal statistics for each Upazila-level of the raster files
- I save layered rasters to Rdata for further computations in the folder Cropped and Aggregated

### correlations.R

I gather all the flooding events between 2006-2019 cropped to the boundaries of Bangladesh and aggregated to Upazila-level.
Using it as an independent var, I run a regression of Reported Losses due to flooding on actual flooding events and get that actual flooding events are statistically very significant in explaining the reported losses of the respondents, which validates the flooding estimated metric as a measure. 


### ncdf_to_raster.R

This file converts air quality pm2.5 data (used as a proxy for Air Quality) in netcdf format to a raster file GTiff format.

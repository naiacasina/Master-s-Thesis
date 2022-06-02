# ========================================================================
# cropping_aggregating.R    -   Naia Ormaza Zulueta   -  May 2022
# In this file:
# - I gather all raster data of interest for the project:
#    a) Air quality (proxied by pm2.5)
#    b) Flooding events
#    c) Food security (proxied by child stunting)
#    d) Drought (proxied by SPI values)
#    e) Population
# - I extract zonal statistics for each Upazila-level of the raster files
# - I save layered rasters to Rdata for further computations
# ========================================================================
library(terra)

# bangladesh shapefile
bang <- vect("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/Bangladesh/gadm40_BGD_shp/gadm40_BGD_3.shp")


# ------------ Raster data: Air Quality ------------
filenames <- list.files("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Air Quality/Rasters", pattern="*.tif", full.names=TRUE)
ldf <- lapply(filenames, rast)

myfxn <- function(var1,var2,var3){
  extract(var1,var2,var3)
}

cropped_air <- lapply(ldf, myfxn, var2=bang, var3='mean')
save(cropped_air, file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Cropped and Aggregated/air.RData")


# ------------ Raster data: Flooding ------------
filenames <- list.files("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Flooding/tifs", pattern="*.tif", full.names=TRUE)
ldf <- lapply(filenames, rast)
#dfList1 <- lapply(ldf, function(x) replace(x, is.na(x), 0))

myfxn <- function(var1,var2,var3){
  extract(var1,var2,na.rm=TRUE,var3)
}

cropped_fl <- lapply(ldf, myfxn, var2=bang, var3='sum')
save(cropped_fl, file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Cropped and Aggregated/flood.RData")


# ------------ Raster data: Food sec ------------
filenames <- list.files("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Food Insecurity/Stunting Prevalence", pattern="*.tif", full.names=TRUE)
ldf <- lapply(filenames, rast)
#dfList1 <- lapply(ldf, function(x) replace(x, is.na(x), 0))

myfxn <- function(var1,var2,var3){
  extract(var1,var2,na.rm=TRUE,var3)
}

cropped_fsec <- lapply(ldf, myfxn, var2=bang, var3='mean')
save(cropped_fsec, file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Cropped and Aggregated/food_sec.RData")

# ------------ Raster data: Population ------------
filenames <- list.files("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Food Insecurity/Stunting Prevalence", pattern="*.tif", full.names=TRUE)
ldf <- lapply(filenames, rast)
#dfList1 <- lapply(ldf, function(x) replace(x, is.na(x), 0))

myfxn <- function(var1,var2,var3){
  extract(var1,var2,na.rm=TRUE,var3)
}

cropped_fsec <- lapply(ldf, myfxn, var2=bang, var3='mean')
save(cropped_fsec, file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Cropped and Aggregated/food_sec.RData")


# ------------ Raster data: Drought ------------
# Read from folder
filenames <- list.files("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Drought", pattern="*.tif", full.names=TRUE)
ldf <- lapply(filenames, rast)

myfxn <- function(var1,var2,var3){
  extract(var1,var2,var3)
}

cropped_dr <- lapply(ldf, myfxn, var2=bang, var3='mean')
save(cropped_dr, file="/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Cropped and Aggregated/drought.RData")


dr <- rast("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Drought/SPI/spg01_m_wld_20150101_20151201_m/spg01_m_wld_20150601_m.tif")


flood$floodMapGL_rp10y[is.na(flood$floodMapGL_rp10y)] <- 0

# ------------ Zonal Statistics ------------
air_mean <- extract(airQuality, bang, 'mean')
water_mean <- extract(water, bang, 'mean')
pop_sum <- extract(pop, bang, 'sum')
heat_mean <- extract(heat, bang, 'mean')
flood_mean <- extract(flood, bang, 'mean')
food_mean <- extract(food, bang, 'mean')

df <- as.data.frame(c(as.data.frame(air_mean$V5GL02.HybridPM25c_0p10.Global.202001.202012),
                      as.data.frame(water_mean$ws_avg),
                      as.data.frame(pop_sum$gpw_v4_population_count_rev11_2020_2pt5_min),
                      as.data.frame(heat_mean$heat),
                      as.data.frame(flood_mean$floodMapGL_rp10y),
                      as.data.frame(food_mean$IHME_LMIC_CGF_2000_2017_STUNTING_PREV_MEAN_2017_Y2020M01D08)))

df$flood_mean.floodMapGL_rp10y[is.na(df$flood_mean.floodMapGL_rp10y)] <- 0
df$water_mean.ws_avg[is.na(df$water_mean.ws_avg)] <- 0

df$name <- bang$NAME_3
df <- df[,c(7,1:6)]
# ========================================================================
# ncdf_to_raster.R    -   Naia Ormaza Zulueta   -  May 2022
# This file converts air quality pm2.5 data in netcdf format to a raster
# file GTiff format
# ========================================================================

library(raster)
library(rasterVis)
library(ncdf4)
library(lattice)
library(stringi)

filenames <- list.files("/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Air Quality/GWRPM25-selected", pattern="*.nc", full.names=TRUE)

for (ncfname in filenames) {
  ncfile = ncdf4::nc_open(ncfname)
  names(ncfile$var)
  # set input path
  input_nc <-  ncfname
  varname <- 'GWRPM25'
  nc2raster <- stack(input_nc,varname = varname)
  
  outpath <- "/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Migration/HRV/Air Quality/Rasters/"
  ncname <- stri_sub(ncfname,-16,-4)
  outname <- ncname
  output <- paste(outpath, outname, ".tif", sep="")
  
  # write raster to file
  writeRaster(nc2raster,output,format = 'GTiff',overwrite = TRUE)
}

ncfname <- paste(ncpath, ncname5, ".nc", sep="")


ncfile = ncdf4::nc_open(ncfname)
names(ncfile$var)


# set input path
input_nc <-  ncfname
varname <- 'GWRPM25'
#nc2raster <- raster(input_nc,varname = varname,band = 1)

nc2raster <- stack(input_nc,varname = varname)

outpath <- "/Users/naiacasina/Documents/IDEA SECOND/Sem 3/ENVS/Codes and Data/Air Quality/Raster/"
outname <- "airQuality"
output <- paste(outpath, outname, ".tif", sep="")

# write raster to file
writeRaster(nc2raster,output,format = 'GTiff',overwrite = TRUE)


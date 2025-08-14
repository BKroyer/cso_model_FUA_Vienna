# Script to extract precipitation time series for each settlement within the area of interest
# Written by Steffen Kittlaus based on a draft by Nina Kleemeyer

library(terra)
library(data.table)
library(future.apply)
library(ncdf4)
library(exactextractr)

# Define input data
# Precipitation data need to be already downloaded and cropped to the area of interest in the folder /data/nc_cropped
# Settlements
settlements <- vect("Q:/GIS-Daten/Europe/Klaeranlagen/Agglomerations/Small_agglomerations/11270_2022_5880_MOESM1_ESM/agglo.shp")
settlements_id <- "gridcode"

# Define area of interest to which all geo data are cropped for faster processing
area_of_interest <- vect("Q:/Projekte/PROMISCES/Modeling/MoRE catchments/catchment_units.shp")

date_begin <- "2012-01-01"
date_end <- "2012-01-03"

# Preprocess geo data
# prepare area of interest
aoi_dissolved <- aggregate(area_of_interest)
aoi_epsg4326 <- project(aoi_dissolved, "EPSG:4326")
settlements_epsg4326 <- project(settlements, "EPSG:4326")
urb <- crop(settlements_epsg4326, aoi_epsg4326)
writeVector(urb, "data/settlements_cropped.gpkg")
urb <- vect("data/settlements_cropped.gpkg")

# load the NetCDF files
nc_files_cropped <- data.table(list.files("output/nc_cropped", pattern = "\\.nc$", full.names = TRUE))
nc_files_cropped[, timestep := as.posix]

precip_rast <- rast(nc_files_cropped)
time_steps <- as.POSIXct(time(precip_rast))

# extract the precipitation for all settlements:

precip_urb <- exact_extract(precip_rast, sf::st_as_sf(urb), fun = "mean", append_cols = settlements_id, stack_apply = TRUE)
# alternative with terra function. Less precise and very slow.
# precip_urb <- extract(precip_rast, urb, fun = mean, ID = TRUE, na.rm = TRUE)
setDT(precip_urb)
precip_dt <- melt(precip_urb, id.vars = settlements_id, value.name = "precipitation_mm")
precip_dt[, time:=as.POSIXct(variable, format = "mean.%Y%j.%H")]
precip_dt[, variable:=NULL]

fwrite(precip_dt, "data/precipitation_ts_settlements.csv")
# Script to extract precipitation time series for each settlement within the area of interest
# Written by Steffen Kittlaus based on a draft by Nina Kleemeyer
library(ProjectTemplate)
load.project()

# load the NetCDF files
nc_files_cropped <- list.files(path_cropped_nc, pattern = "\\.nc$", full.names = TRUE)
#nc_files_cropped[, timestep := as.POSIXct()]

precip_rast <- rast(nc_files_cropped)
precip_rast_attributes <- data.table(time_steps = as.POSIXct(time(precip_rast)),
                                     names = names(precip_rast))
precip_rast_attributes[, duplicates := duplicated(names)]


# extract the precipitation for all settlements:
terra::gdalCache(30000)
precip_urb <- exact_extract(precip_rast, sf::st_as_sf(urb_4326), fun = "mean", append_cols = settlements_id, stack_apply = TRUE)
# alternative with terra function. Less precise and very slow.
# precip_urb <- extract(precip_rast, urb, fun = mean, ID = TRUE, na.rm = TRUE)
setDT(precip_urb)
rm(precip_rast, urb, nc_files_cropped)
gc()
precip_dt <- melt(precip_urb, id.vars = settlements_id, value.name = "precipitation_mm")
saveRDS(precip_dt,"data/intermediate_results/precipitation_ts_settlements_prelim.rds")
rm(precip_urb)
gc()
setkeyv(precip_dt, settlements_id)
settlement_ids <- unique(precip_dt[,..settlements_id])
precip_dt[, time:=as.POSIXct(variable, format = "mean.%Y%j.%H"), by = gridcode]

precip_dt[, variable:=NULL]

# Save data table as RDS as this saves much space
saveRDS(precip_dt, file.path(path_intermediate_res, "precipitation_ts_settlements.rds"))
# remove preliminary data
file.remove("data/intermediate_results/precipitation_ts_settlements_prelim.rds")
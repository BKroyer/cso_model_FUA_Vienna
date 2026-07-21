# Script to extract precipitation time series for each settlement within the area of interest
# Written by Steffen Kittlaus based on a draft by Nina Kleemeyer
# Adapted to get prec data by year by Bettina Kroyer

library(ProjectTemplate)
load.project()

# load the NetCDF files
nc_files_cropped <- list.files(path_cropped_nc, pattern = "\\.nc$", full.names = TRUE)
#nc_files_cropped[, timestep := as.POSIXct()]


# Function to extract the correct files from nc_cropped
extract_prec_year <- function(path, year_str, nc_files_cropped, nr_timesteps = 8){
    nc_files_yr <- nc_files_cropped[grepl(paste0("^", year_str, "[0-9]{3}\\."), basename(nc_files_cropped))]

    days_yr <- 365
    if (as.numeric(year_str) %% 4 == 0){
        days_yr <- 366
    }
    nr_supposed <- days_yr * nr_timesteps * length(year_str)

    nr_missing <- nr_supposed - length(nc_files_yr)
    if (nr_missing > 0){
        print(paste0("Not all files found / cropped yet. Number of missing files: ", nr_missing, " out of ", nr_supposed, " total files"))
    }

    return(nc_files_yr)
}


# loop to save extracted prec time series as rds for every specified year separately
for (current_years in as.character(c(2021:2024))){
    nc_files_yr <- extract_prec_year(path_cropped_nc, current_years, nc_files_cropped)

    precip_rast <- rast(nc_files_yr)
    precip_rast_attributes <- data.table(time_steps = as.POSIXct(time(precip_rast)),
                                         names = names(precip_rast))
    precip_rast_attributes[, duplicates := duplicated(names)]


    # extract the precipitation for all settlements:
    terra::gdalCache(30000)

    precip_urb <- exact_extract(precip_rast, sf::st_as_sf(urb_4326), fun = "mean", append_cols = gridcode_nam, stack_apply = TRUE) # prec data in in EPSG:4326

    # alternative with terra function. Less precise and very slow.
    # precip_urb <- extract(precip_rast, urb, fun = mean, ID = TRUE, na.rm = TRUE)
    setDT(precip_urb)
    rm(precip_rast)#, nc_files_cropped)
    gc()

    mean_cols <- grep("^mean\\.", names(precip_urb), value = TRUE)
    precip_long <- melt(
        precip_urb,
        id.vars = gridcode_nam,
        measure.vars = mean_cols,
        variable.name = "mean_var",
        value.name = "precipitation_mm"
    )

    precip_long[, timestamp := sub("^mean\\.", "", mean_var)]

    precip_dt <- precip_long[, mean_var := NULL]

    setkeyv(precip_dt, gridcode_nam)
    settlement_ids <- unique(precip_dt[,..gridcode_nam])
    precip_dt[, time:=as.POSIXct(timestamp, format = "%Y%j.%H"), by = gridcode_nam]

    precip_dt[, timestamp:=NULL]

    # Save data table as RDS as this saves much space
    saveRDS(precip_dt, file.path(path_intermediate_res, paste0("precipitation_ts", path_nam_preprocess, current_years,".rds")))

}


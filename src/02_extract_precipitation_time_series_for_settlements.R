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




# Validation Einzugsgebiete
#val_einzug <- vect("C:\\Users\\simulation\\bkroyer\\git_clone\\cso-modell-upper-danube\\data\\Validation_Einzugsgebiete\\Validation_Einzugsgebiete.shp")
#val_4326 <- project(val_einzug, urb_4326)


# loop to save extracted prec time series as rds for every specified year separately
for (current_years in as.character(c(2010:2016))){
    nc_files_yr <- extract_prec_year(path_cropped_nc, current_years, nc_files_cropped)

    precip_rast <- rast(nc_files_yr)
    precip_rast_attributes <- data.table(time_steps = as.POSIXct(time(precip_rast)),
                                         names = names(precip_rast))
    precip_rast_attributes[, duplicates := duplicated(names)]


    # extract the precipitation for all settlements:
    terra::gdalCache(30000)

    precip_urb <- exact_extract(precip_rast, sf::st_as_sf(urb_4326), fun = "mean", append_cols = settlements_id, stack_apply = TRUE) # prec data in in EPSG:4326

    precip_urb$NAME_nr <- gridcode_to_process

    precip_urb <- merge(precip_urb, anteil, by.x = "NAME_nr", by.y = "settlement_id")


    # for the validation regions
    #precip_urb <- exact_extract(precip_rast, sf::st_as_sf(val_4326), fun = "mean", append_cols = settlements_id, stack_apply = TRUE) # prec data in in EPSG:4326

    # alternative with terra function. Less precise and very slow.
    # precip_urb <- extract(precip_rast, urb, fun = mean, ID = TRUE, na.rm = TRUE)
    setDT(precip_urb)
    rm(precip_rast)#, nc_files_cropped)
    gc()

    mean_cols <- grep("^mean\\.", names(precip_urb), value = TRUE)
    precip_long <- melt(
        precip_urb,
        id.vars = c("NAME.x", "Anteil"),
        measure.vars = mean_cols,
        variable.name = "mean_var",
        value.name = "precipitation_mm"
    )

    precip_long[, timestamp := sub("^mean\\.", "", mean_var)]
    precip_long[, weighted_precip := precipitation_mm * Anteil]
    result <- precip_long[
        , .(precipitation_mm = sum(precipitation_mm * Anteil, na.rm = TRUE) / sum(Anteil, na.rm = TRUE)),
        by = .(NAME_nr = get("NAME.x"), timestamp)
    ]
    precip_dt <- result
    names(precip_dt)[1] <- c("NAME")

    #precip_dt <- melt(precip_urb, id.vars = settlements_id, value.name = "precipitation_mm")
    # saveRDS(precip_dt,"data/intermediate_results/precipitation_ts_settlements_prelim.rds")
    # rm(precip_urb)
    # gc()
    setkeyv(precip_dt, settlements_id)
    settlement_ids <- unique(precip_dt[,..settlements_id])
    #precip_dt[, time:=as.POSIXct(variable, format = "mean.%Y%j.%H"), by = gridcode]
    precip_dt[, time:=as.POSIXct(timestamp, format = "%Y%j.%H"), by = NAME]

    precip_dt[, timestamp:=NULL]

    # Save data table as RDS as this saves much space
    #saveRDS(precip_dt, file.path(path_intermediate_res, paste0("precipitation_ts_settlements_", current_years,".rds")))

    # aggregate according to Anteil


    saveRDS(precip_dt, file.path(path_intermediate_res, paste0("precipitation_ts_AUT_WWTP", current_years,".rds")))
    # remove preliminary data
    # file.remove("data/intermediate_results/precipitation_ts_settlements_prelim.rds")

}


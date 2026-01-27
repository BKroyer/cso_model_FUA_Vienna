# Script to wrap data preprocessing (AoI, settlements)
# Written by Bettina Kroyer


# Wrapper function to apply cso_model ---------------------------------------------------------------------------
run_cso_for_single_gridcode <- function(gridcode_temp,
                                        params,
                                        pop_dt,
                                        imp_dt,
                                        share_dt,
                                        prec_dt,
                                        save_single_files = FALSE,
                                        print_params = FALSE) {

    # extract single rows
    p     <- params[gridcode == gridcode_temp]
    # print(p)
    pop   <- pop_dt[settlement_id  == gridcode_temp]
    imp   <- imp_dt[settlement_id  == gridcode_temp]
    share <- share_dt[gridcode == gridcode_temp]

    # precipitation time series
    prec  <- prec_dt[gridcode == gridcode_temp]

    cat("-------------------------------------------\n")
    cat("Starting gridcode:", gridcode_temp, "\n")
    t1 <- Sys.time()

    used_vals <- cbind(
        p,
        population = pop$population,
        imp_area_km2 = imp$imp_area_km2,
        share_served_by_CS = share$share_served_by_CS
    )

    if (imp$imp_area_km2 == 0){ # skipping settlements with 0 km² impervious surface
        single_params <- cbind(data.table(gridcode = gridcode_temp), used_vals)
        single_row <- data.table(gridcode = gridcode_temp)
            return(list(
                single_params,
                single_row
            ))
    }

    # run model
    res <- cso_model(
        population = pop$population,
        area = imp$imp_area_km2,
        share_served_by_CS = share$share_served_by_CS,
        time = prec$time,
        precipitation = prec$precipitation_mm,
        dwf_per_capita = p$dwf_per_capita,
        W0 = p$W0,
        k0 = p$k0,
        dn = p$dn,
        dt = p$dt,
        W1 = p$W1,
        W2 = p$W2,
        print_params = print_params
    )

    t2 <- Sys.time()
    cat("Finished gridcode:", gridcode_temp, "in", round(difftime(t2, t1, units="secs"),1), "seconds\n")
    cat("-------------------------------------------\n")

    # ensure data.table + attach gridcode
    #res_dt <- as.data.table(res) # too large for that idea of rbinding everything
    #res_dt[, gridcode := gridcode]

    # save used values
    # used_vals <- cbind(p, pop$population, imp$imp_area_km2, share$share_served_by_CS)



    min_time <- min(prec$time) #get(min_time)
    max_time <- max(prec$time) #get(max_time)

    location <- as.character(gridcode_temp)
    results_nam <- paste0(location, paste0("_", datum, "_y", substr(min_time, 1, 4), "_",  substr(max_time, 1, 4)))
    path_out <- file.path(path_intermediate_res, results_nam)


    if (save_single_files){
        wb_path <- process_and_plot_results(res, path_out, location = location, used_params = used_vals, save_single_files = save_single_files, gridcode_temp = gridcode_temp)#, validation_data = data_vienna, validation_area = 70)
        return(wb_path)

    }else{
        single_row_results <-  process_and_plot_results(res, path_out, location = location, used_params = used_vals, save_single_files = save_single_files, gridcode_temp = gridcode_temp)

        single_params <- single_row_results$used_params
        single_row <- single_row_results$collected_res_mm

        gridcode_dt <- data.table(gridcode = gridcode_temp)
        # pop_temp <- data.table(population = single_params$population)
        single_row <- cbind(gridcode_dt, single_row)
        return(list(
            single_params,
            single_row
            ))
    }




}



# Reads AoI and settlements, projects to EPSG 3035, crops settlements to AoI, writes vectors to files ---------------------------------------
wrapper_preprocess_data <- function(area_of_interest, settlements){

    area_of_interest <- vect(area_of_interest)
    aoi_dissolved <- aggregate(area_of_interest)
    aoi_epsg4326 <- project(aoi_dissolved, "EPSG:4326")
    aoi_epsg3035 <- project(aoi_dissolved, "EPSG:3035")
    writeVector(aoi_epsg3035, "data/intermediate_results/aoi_epsg3035.gpkg", overwrite = TRUE)

    # buffering for precipitation extraction
    aoi_buffered <- buffer(aoi_dissolved, 10000) # Fix potential geometry issues and create buffer 10 km
    aoi_buffered_epsg4326 <- project(aoi_buffered, "EPSG:4326")
    writeVector(aoi_buffered_epsg4326, "data/intermediate_results/aoi_buffered_epsg4326.gpkg", overwrite = TRUE)

    ### Settlements
    # thesis: agglo.shp from Pistoccio 2022
    settlements <- vect(settlements)
    # settlements_id <- "settlements_id"
    settlements_epsg3035 <- project(settlements, "EPSG:3035")
    urb_3035 <- crop(settlements_epsg3035, aoi_epsg3035)
    #all(is.valid(urb_3035)) # check validity of geometries. Should result in TRUE (did results in TRUE Oct31)
    writeVector(urb_3035, file.path(path_intermediate_res, "settlements_cropped_epsg3035.gpkg"), overwrite = TRUE)


    settlements_epsg4326 <- project(settlements, "EPSG:4326")
    urb_4326 <- crop(settlements_epsg4326, aoi_epsg4326)
    writeVector(urb_4326, file.path(path_intermediate_res, "settlements_cropped_epsg4326.gpkg"), overwrite = TRUE)
}

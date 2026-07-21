# Script to run cso function on single gridcode for parallel processing
# Written by Bettina Kroyer


# Wrapper function to apply cso_model ---------------------------------------------------------------------------
run_cso_for_single_gridcode <- function(gridcode_temp,
                                        params,
                                        pop_dt,
                                        design_pop_dt = NA,
                                        imp_dt,
                                        share_dt,
                                        prec_dt,
                                        manual_pop = FALSE,
                                        year_temp = year_temp,
                                        print_params = FALSE,
                                        ln_A_B = ln_A_B) {

    sourceCpp("lib/cso_model_helpers.cpp")


    # extract single rows
    share <- share_dt[get(gridcode_nam) == gridcode_temp]


    p     <- params[gridcode == gridcode_temp]

    if (!manual_pop){
        pop <- as.numeric(pop_dt[get(gridcode_nam)  == gridcode_temp]$population * share$share_served_by_CS) # the population actually discharging into the combined sewer system!
        #print(pop)
    } else{
        pop <- as.numeric(manual_pop)
    }

    if (any(!is.na(design_pop_dt))){
        design_pop <- as.numeric(design_pop_dt[get(gridcode_nam)  == gridcode_temp]$Bemessungswert * share$share_served_by_CS) # designed for ALL people so also from SS
    }else{
        design_pop <- NA
    }


    imp   <- imp_dt[get(gridcode_nam)  == gridcode_temp]


    # precipitation time series
    prec  <- prec_dt[gridcode == gridcode_temp]

    used_vals <- cbind(
        p,
        population = as.numeric(pop/share$share_served_by_CS),
        population_connected = pop,
        population_design = design_pop,
        imp_area_km2 = imp$imp_area_km2,
        share_served_by_CS = share$share_served_by_CS,
        mean_annual_prec = mean(prec$precipitation_mm * 8 * 365, na.rm=T)
    )

    if (imp$imp_area_km2 == 0){ # skipping settlements with 0 km² impervious surface
        single_params <- cbind(data.table(gridcode = gridcode_temp), used_vals)
        single_row <- data.table(year = year_temp, gridcode = gridcode_temp, imp_area_0 = TRUE)
        #runtime <- 0
        return(list(
            gridcode = gridcode_temp,
            #runtime_secs = runtime,
            single_params = single_params,
            single_row = single_row
        ))
    }


    # run model
    res <- cso_model(
        population = pop,
        design_population = design_pop,
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
        print_params = print_params,
        ln_A_B = ln_A_B
    )

    min_time <- min(prec$time) #get(min_time)
    max_time <- max(prec$time) #get(max_time)

    location <- as.character(gridcode_temp)
    results_nam <- paste0(location, paste0("_", datum, "_y", substr(min_time, 1, 4), "_",  substr(max_time, 1, 4)))


    path_out <- file.path(path_intermediate_res, area_of_interest_name_temp)

    single_row_results <-  process_and_plot_results(res, path_out, location = location, used_params = used_vals, gridcode_temp = gridcode_temp)

    single_params <- single_row_results$used_params
    single_row <- single_row_results$collected_res_mm
    event_res <- single_row_results$event_res

    # add year and gridcode to single row
    gridcode_dt <- data.table(year = year_temp, gridcode = gridcode_temp)
    single_row <- cbind(gridcode_dt, single_row)
    event_res <- cbind(gridcode_dt, event_res)
    return(list(
        gridcode = gridcode_temp,
        #runtime_secs = runtime,
        single_params = single_params,
        single_row = single_row,
        event_res = event_res
    ))

}





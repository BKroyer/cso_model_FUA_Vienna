# Written by Bettina Kroyer

## get right cols from validation and modelled data

get_cols <- function(var_nam, data){
    nam <- names(data)[grepl(tolower(var_nam), tolower(names(data))) & grepl("overflow", tolower(names(data)))]
    if (length(nam) != 1){
        print(nam)
        index <- readline(prompt = "More than one column found, input the correct index:")
        nam <- nam[as.integer(index)]
    }
    return(data[[nam]])
}

## create and save plots
plot_compare_data <- function(var_nam, compare_data, path_out){
    # mod vs orig
    lim_min <- 0
    lim_max <- ceiling(max(c(compare_data$mod, compare_data$orig), na.rm=T))
    ggsave(
        file.path(path_out, paste0(var_nam, "_mod_vs_orig_scatter.pdf")),
        ggplot(compare_data, aes(x = orig, y = mod, color = scenario)) +
            geom_point(alpha = 0.5, size = 4) +
            geom_abline(slope = 1, intercept = 0) +
            xlab(paste0("original ",var_nam," overflow in mm (Excel)")) +
            ylab(paste0("modeled ",var_nam," overflow in mm (R)")) +
            scale_x_continuous(breaks = seq(lim_min, lim_max, 1), limits = c(lim_min, lim_max)) +
            scale_y_continuous(breaks = seq(lim_min, lim_max, 1), limits = c(lim_min, lim_max)) +
            theme_bw(),
        width = 8,
        height = 6
    )

    # mod-orig
    compare_data[, diff_to_orig := mod - orig]
    ggsave(
        file.path(path_out, paste0(var_nam, "_diff_mod_minus_orig.pdf")),
        ggplot(compare_data, aes(x = time, y = diff_to_orig, color = scenario)) +
            geom_point(alpha = 0.5, size = 4) +
            ylab(paste0("modeled minus original ", var_nam)) +
            xlab("") +
            scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
            theme_bw(),
        width = 9,
        height = 7
    )
}

process_and_plot_results <- function(mod_res, path_out, used_params, validation_data = NULL, validation_area = NULL, location = "", mask = NULL,
                                     time_step = 3, round_to = 4, gridcode_temp = gridcode_temp){
    #' @param mod_res the model result data frame (output of model_cso)
    #' @param path_out path to save the plots to; will be created but not overwritten
    #' @param used_params data.table of the parameters used in that model run, to be saved to results tables
    #' @param validation_data data frame with same timestep as mod_res and columns for network and tank overflow, if NULL, only model results are summarized
    #' @param validation_area option to specify reference area of validation dataset in case if differs from modelled unit
    #' @param location name of the processed location as a string, will be added to the result file names
    #' @param mask the row indices to consider in mean and total computation
    #' @param time_step the time step in hours, default is 3
    #' @param round_to the digits to round results to, default is 4
    #'
    #' @returns print of results, saves plots and table results

    # create the folder
    # path_out <- normalizePath(path_out, mustWork = FALSE)

    #if (!dir.exists(path_out)){
    #    dir.create(path_out, recursive = TRUE, showWarnings = FALSE)
    #}


    has_validation <- !is.null(validation_data)

    area <- unique(mod_res$area)
    if (is.null(validation_area)){
        validation_area <- area
    }

    population <- used_params$population


    if (is.null(mask)){
        mask <- 2:nrow(mod_res) # first row undefined in many variables (due to shift calculations)
    }

    time_mod <- as.POSIXct(format(mod_res$time, "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT-1")

    if (has_validation){

        # get datasets onto same timeframe
        validation_data$DateValue <- as.POSIXct(format(validation_data$DateValue, "%Y-%m-%d %H:%M:%S"), tz = "Etc/GMT-1")
        time_orig <- validation_data$DateValue
        min_time <- max(min(time_mod), min(time_orig))
        max_time <- min(max(time_mod), max(time_orig))

        mod_res <- mod_res[time >= min_time & time <= max_time]
        setDT(validation_data)
        validation_data <- validation_data[DateValue >= min_time & DateValue <= max_time]

    } else{

        min_time <- min(time_mod)
        max_time <- max(time_mod)
    }

    # give warnings if aligned timeframe below one year # accept Jan 1st to Dec 31 st
    if (difftime(max_time, min_time, units = "days") < 364){
        warning(paste0("Gridcode ", gridcode_temp, " has less than one year of aligned data, may not be representing annual volumes correctly."))
    }

    ## aggregate to annual mean values
    annualise <- function(x) {
        mean(x[mask], na.rm = TRUE) * (24 / as.integer(time_step)) * 365
    }


    ## TANK =================

    tank_mod <- get_cols("tank", mod_res)
    annual_mean_tank <- annualise(tank_mod)

    if (has_validation) {
        tank_orig <- get_cols("tank", validation_data)
        annual_mean_tank_orig <- annualise(tank_orig)

        plot_compare_data(
            "tank",
            data.table(
                time = mod_res$time,
                orig = tank_orig,
                mod  = tank_mod,
                scenario = mod_res$scenario
            ),
            path_out
        )
    }


    ## NETWORK ==============

    network_mod <- get_cols("network", mod_res)
    annual_mean_network <- annualise(network_mod)

    if (has_validation) {
        network_orig <- get_cols("network", validation_data)
        annual_mean_network_orig <- annualise(network_orig)

        plot_compare_data(
            "network",
            data.table(
                time = mod_res$time,
                orig = network_orig,
                mod  = network_mod,
                scenario = mod_res$scenario
            ),
            path_out
        )
    }


    ## TOTALS ===============

    total_overflow <- annual_mean_tank + annual_mean_network
    DWF_volume_mm <- sum(mod_res$DWF_volume_mm, na.rm=T)
    DWF_volume_Mm3 <- sum(mod_res$DWF_volume_m3, na.rm=T) / 10^6

    if (has_validation) {
        total_overflow_orig <- annual_mean_tank_orig + annual_mean_network_orig
    }


    if (!is.na(area)) {
        total_overflow_Mm3y <- total_overflow * area / 1000
        if (has_validation) {
            total_overflow_Mm3y_orig <- total_overflow_orig * validation_area / 1000
        }
    } else {
        total_overflow_Mm3y <- NA
        if (has_validation) total_overflow_Mm3y_orig <- NA
    }


    event_metrics <- mod_res[, .("time" = time,
                                 "overflow_duration_h" = duration * unique(diff(time_mod)), # 1 in duration means overflow in that time step
                                 "rain_end" = rain_end, # 1 in rain_end marks end of rain event
                                 "rain_event" = rain_event %in% c(1,2), # 1 then means there was rain in that time step
                                 "rain_duration_h" = rain_event %in% c(1,2) * unique(diff(time_mod)),
                                 "total_overflow_m3" = total_overflow_m3,
                                 "precipitation_m3" = precipitation_m3
                                 )]


    # save mm results for tank, network, total as rows
    collected_res_mm <- data.table(
        # gridcode = is added later
        #prec_area_weighted_sum = prec_sum,
        imp_area_served_by_CS_km2 = area,
        annual_mean_tank_mm = annual_mean_tank, #mm
        annual_mean_network_mm = annual_mean_network, #mm
        total_overflow_mm = total_overflow, #mm
        total_overflow_Mm3y = total_overflow_Mm3y,
        annual_mean_network_orig = if (has_validation) annual_mean_network_orig else NA_real_,
        annual_mean_tank_orig = if (has_validation) annual_mean_tank_orig else NA_real_,
        total_overflow_orig = if (has_validation) total_overflow_orig else NA_real_,
        total_overflow_Mm3y_orig = if (has_validation) total_overflow_Mm3y_orig else NA_real_,
        validation_area = if (has_validation) validation_area else NA_real_,
        DWF_volume_mm = DWF_volume_mm,
        DWF_volume_Mm3 = DWF_volume_Mm3
    )

    event_res <- event_metrics

    list_savee <- list(
        used_params = used_params,
        collected_res_mm = collected_res_mm,
        event_res = event_res)

    return(list_savee)
}

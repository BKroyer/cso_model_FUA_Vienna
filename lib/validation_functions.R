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
                                     time_step = 3, round_to = 4, save_single_files = FALSE, gridcode_temp = gridcode_temp){
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
    dir.create(path_out, recursive = TRUE, showWarnings = FALSE)


    has_validation <- !is.null(validation_data)

    area <- unique(mod_res$area)
    if (is.null(validation_area)){
        validation_area <- area
    }

    population <- used_params$population

    # prec_sum <-


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

    print(paste0("Aligned common timeframe: ",min_time, " to ", max_time))

    # give warnings if aligned timeframe below one year
    if (difftime(max_time, min_time, units = "days") < 365){
        warning(paste0("Gridcode ", gridcode_temp, " has less than one year of aligned data, may not be representing annual volumes correctly."))
    }

    # specify actual timeframe for naming later
    #assign("min_time", min_time)
    #assign("max_time", max_time)



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


    overflow_duration <- (mean(mod_res$frequency_overflow, na.rm = TRUE) *
                              (24 / as.integer(time_step)) * 365) * time_step

    cso_volume_per_event <- sum(mod_res$cso_volume_per_event, na.rm = TRUE) /
        sum(mod_res$rain_end, na.rm = TRUE)



    if (save_single_files){
        cat("\n--- CSO summary ---\n")
        cat("CSO duration (mean): ",
            round(overflow_duration, round_to), " hrs/year\n")

        cat("CSO volume per event: ",
            round(cso_volume_per_event, round_to), " mm/event\n\n")

        cat("Network overflow: ",
            round(annual_mean_network, round_to), " mm/y",
            if (has_validation)
                paste0(" (validation: ",
                       round(annual_mean_network_orig, round_to), " mm/y)"),
            "\n")

        cat("Tank overflow: ",
            round(annual_mean_tank, round_to), " mm/y",
            if (has_validation)
                paste0(" (validation: ",
                       round(annual_mean_tank_orig, round_to), " mm/y)"),
            "\n")

        cat("Total overflow: ",
            round(total_overflow, round_to), " mm/y",
            if (has_validation)
                paste0(" (validation: ",
                       round(total_overflow_orig, round_to), " mm/y)"),
            "\n")

        cat("Total overflow: ",
            round(total_overflow_Mm3y, round_to), " Mm³/y",
            if (has_validation)
                paste0(" (validation: ", round(total_overflow_Mm3y_orig, round_to), " Mm³/y)"),
            "\n")


        ## SUMMARY TABLES =======

        summary_overflow <- data.table(
            metric = c("network [mm]", "tank [mm]", "total [mm]", "total [Mm3y]"),
            model  = round(c(annual_mean_network,
                             annual_mean_tank,
                             total_overflow,
                             total_overflow_Mm3y), round_to),
            validation = if (has_validation)
                round(c(annual_mean_network_orig,
                        annual_mean_tank_orig,
                        total_overflow_orig,
                        total_overflow_Mm3y_orig), round_to)
            else NA_real_
        )

        event_metrics <- data.table(
            metric = c("CSO duration (hrs/y)", "CSO volume per event (mm)"),
            value  = round(c(overflow_duration, cso_volume_per_event), round_to)
        )


        wb <- createWorkbook()

        sheet <- paste0("params_", location)
        addWorksheet(wb, sheet)
        writeData(wb, sheet, used_params, colNames = TRUE)
        setColWidths(wb, sheet, cols = 1:10, widths = 15)

        sheet <- paste0("overflow_", location)
        addWorksheet(wb, sheet)
        writeData(wb, sheet, summary_overflow)
        setColWidths(wb, sheet, cols = 1:3, widths = 25)

        sheet <- paste0("event_metrics_", location)
        addWorksheet(wb, sheet)
        writeData(wb, sheet, event_metrics)
        setColWidths(wb, sheet, cols = 1:3, widths = 25)

        # fwrite(
        #     summary_overflow,
        #     file = file.path(path_out,
        #                      paste0("summary_overflow_", location, ".csv"))
        # )

        #
        # fwrite(
        #     event_metrics,
        #     file = file.path(path_out,
        #                      paste0("summary_event_metrics_", location, ".csv"))
        # )

        #saveWorkbook(wb, file = file.path(path_out, paste0("cso_summaries_", location, ".xlsx")), overwrite = TRUE)


        ## RETURN ========

        invisible(list(
            summary_overflow = summary_overflow,
            event_metrics = event_metrics
        ))

        return(list(
            # has_validation = has_validation,
            wb = wb,
            path_out = path_out
        ))
    }else{ # save mm results for tank, network, total as rows

        collected_res_mm <- data.table(
            # gridcode = is added later
            #prec_area_weighted_sum = prec_sum,
            imp_area_served_by_CS_km2 = area,
            population = population,
            annual_mean_tank_mm = annual_mean_tank, #mm
            annual_mean_network_mm = annual_mean_network, #mm
            total_overflow_mm = total_overflow, #mm
            total_overflow_Mm3y = total_overflow_Mm3y,
            annual_mean_network_orig = if (has_validation) annual_mean_network_orig else NA_real_,
            annual_mean_tank_orig = if (has_validation) annual_mean_tank_orig else NA_real_,
            total_overflow_orig = if (has_validation) total_overflow_orig else NA_real_,
            total_overflow_Mm3y_orig = if (has_validation) total_overflow_Mm3y_orig else NA_real_,
            validation_area = if (has_validation) validation_area else NA_real_,
            overflow_duration = overflow_duration,
            cso_volume_per_event = cso_volume_per_event
        )

        return(list(
            used_params = used_params,
            collected_res_mm = collected_res_mm))

        # list(has_validation = has_validation,
        #     collected_res_mm = collected_res_mm)
    }

}

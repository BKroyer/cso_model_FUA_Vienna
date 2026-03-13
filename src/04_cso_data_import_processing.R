# Script to run cso model on time series
# Written by Steffen Kittlaus based on a draft by Nina Kleemeyer
# Heavily modified by Bettina Kroyer

rm(list=ls())


library(ProjectTemplate)
load.project()


##################################################################
# CSO Data Import and Processing                                 #
# Purpose:                                                       #
# 1. Import precipitation and FUA data, prepare and clean data   #
# 2. Calculate CSO (Combined Sewer Overflow) metrics             #
# 3. Export results for each file                                #
##################################################################


######################################################################
# 1. Import & prepare pre-processed precipitation and settlement data ------------------------------------------------------
######################################################################


# Import population data
if (time_period_of_interest == "2010_2016"){
    pop_dt <- readRDS(file.path(path_intermediate_res, "population_2015.rds"))
}else{
    if(time_period_of_interest == "2021_2024"){
        pop_dt <- readRDS(file.path(path_intermediate_res, "population_2021.rds"))
    }else{
        errorCondition("use either 2010_2016 or 2021_2024 as time_period_of_interest")
    }
}
setDT(pop_dt, key = "settlement_id")
pop_dt <- pop_dt[.(gridcode_to_process)]
setkey(pop_dt, settlement_id)
pop_dt[, population := as.integer(population)]

sum(pop_dt$population)


# Import impervious area data
if (time_period_of_interest == "2010_2016"){
    imp_dt <- readRDS(file.path(path_intermediate_res, "impervious_area_2015.rds"))
}else{
    if(time_period_of_interest == "2021_2024"){
        imp_dt <- readRDS(file.path(path_intermediate_res, "impervious_area_2021.rds"))
    }else{
        errorCondition("use either 2010_2016 or 2021_2024 as time_period_of_interest")
    }
}
setDT(imp_dt, key = "settlement_id")
imp_dt <- imp_dt[.(gridcode_to_process)]
setkey(imp_dt, settlement_id)


# Import share served by CS
# share_dt <- data.table(gridcode = unique(prec_dt$gridcode), share_served_by_CS = rep(0.28, length(gridcode_to_process)), key = "gridcode")
share_dt <- readRDS(file.path(path_intermediate_res, "share_CS.rds"))
setDT(share_dt, key = "gridcode")
share_dt <- share_dt[.(gridcode_to_process)]
setkey(share_dt, gridcode)

# custom CS share
# share_dt$share_served_by_CS <- 0.7


# Import precipitation data just for gridcodes needed
# one year after the other - for memory reaons, crashes for ~ 7 years of data and above
combine_pre_years <- function(current_years) {

    files <- file.path(
        path_intermediate_res,
        paste0("precipitation_ts_settlements_", current_years, ".rds")
    )

    dt_list <- vector("list", length(files))

    for (i in seq_along(files)) {
        dt_list[[i]] <- readRDS(files[i])
    }

    gc()

    combined_temp <- data.table::rbindlist(dt_list, use.names = TRUE)
    data.table::setkey(combined_temp, gridcode, time)

    rm(dt_list)
    gc()

    return(combined_temp)
}

file_path <- file.path(
    path_intermediate_res,
    paste0("precipitation_ts_settlements_", nam, ".rds")
)

prec_dt <- if (file.exists(file_path)) {
    readRDS(file_path)
} else {
    combine_pre_years(current_years)
}


setkey(prec_dt, gridcode, time) # Memory issues?
prec_dt <- prec_dt[
    data.table(gridcode = unique(gridcode_to_process)),
    on = .(gridcode)
    ] #[time >= date_begin & time <= date_end] #option to filter by time again, only relevant if not entire years are used

prec_dt$time <- as.POSIXct( # fix the time format (CET/CEST because of summer time, but I need the physical time)
    format(prec_dt$time, "%Y-%m-%d %H:%M:%S"),
    tz = "Etc/GMT-1"
)


print(paste0("The mean annual prec is:", sum(prec_dt$precipitation_mm)/(length(prec_dt$time)/8/365)))




# apply cso_model to gridcodes_to_process (real data) ---------------------------------------------------------------------------------------

plan(multisession, workers = availableCores() - 1)

# run if future crashed
# plan(sequential)
# gc()


mod_results <- future_map(
    gridcode_to_process,
    run_cso_for_single_gridcode,
    params = params,
    pop_dt = pop_dt,
    imp_dt = imp_dt,
    share_dt = share_dt,
    prec_dt = prec_dt,
    save_single_files = save_single_files,
    print_params = FALSE,
    .options = furrr_options(seed = 123)
)

runtime_dt <- rbindlist(lapply(mod_results, function(x) data.table(gridcode = x$gridcode, runtime_secs = x$runtime_secs)))


if (save_single_files){
    walk(mod_results, function(res) {
        wb_path <- res$workbook_path$wb        # the workbook object
        path_temp <- res$workbook_path$path_out  # the path

        sheet_name <- names(wb_path)[1]
        gc <- sub("CSnew_params_", "", sheet_name)

        saveWorkbook(
            wb_path,
            file.path(path_temp, paste0("cso_summaries_", gc, ".xlsx")),
            overwrite = TRUE
        )
    })
}else{

    dt_params <- rbindlist(
        lapply(mod_results, function(x) x$single_params),
        use.names = TRUE,
        fill = TRUE
    )

    dt_results <- rbindlist(
        lapply(mod_results, function(x) x$single_row),
        use.names = TRUE,
        fill = TRUE
    )

    summarized_stats <- as.data.table(t(colSums(dt_results, na.rm = TRUE)), keep.rownames = TRUE)

    prec_sum <- sum(dt_params$mean_annual_prec * (dt_params$imp_area_km2 / sum(dt_params$imp_area_km2, na.rm=T)), na.rm = T)

    summary_overflow <- data.table(
        metric = c("total population", "total impervious area served by CS [km²]", "total annual precipitation [mm]",
                   "network [mm]", "tank [mm]", "total [mm]", "total [Mm3y]"),
        model = round(c(summarized_stats$population,
                        summarized_stats$imp_area_served_by_CS_km2,
                        prec_sum,
                        summarized_stats$annual_mean_network_mm,
                        summarized_stats$annual_mean_tank_mm,
                        summarized_stats$total_overflow_mm,
                        summarized_stats$total_overflow_Mm3y), round_to),
        validation = c(NA, round(c(summarized_stats$validation_area,
                                   NA, # this is the groundtruth precipitation that led to the event, we do not know this
                                   summarized_stats$annual_mean_network_orig,
                                   summarized_stats$annual_mean_tank_orig,
                                   summarized_stats$total_overflow_orig,
                                   summarized_stats$total_overflow_Mm3y_orig), round_to))
    )

    # change the sum here, does not make sense for the duration if multiple gridcodes, might have same hour overflow
    event_metrics <- data.table(
        # metric = c("CSO duration (hrs/y)", "CSO volume per event (mm)"),
        # value  = round(c(summarized_stats$overflow_duration, summarized_stats$cso_volume_per_event), round_to)
    )

    wb <- createWorkbook()

    widths_temp <- 23
    cols_temp <- 20

    sheet <- "used_params"
    addWorksheet(wb, sheet)
    writeData(wb, sheet, dt_params)
    setColWidths(wb, sheet, cols = 1:cols_temp, widths = widths_temp)

    sheet <- "results_per_gridcode"
    addWorksheet(wb, sheet)
    cols_to_round <- c(2, 4:14)
    dt_results[, (cols_to_round) := lapply(.SD, round, digits = round_to),
               .SDcols = cols_to_round]
    writeData(wb, sheet, dt_results, rowNames = FALSE)
    setColWidths(wb, sheet, cols = 1:cols_temp, widths = widths_temp)

    sheet <- "overflow"
    addWorksheet(wb, sheet)
    writeData(wb, sheet, summary_overflow)
    setColWidths(wb, sheet, cols = 1:cols_temp, widths = 35)

    sheet <- paste0("event_metrics")
    addWorksheet(wb, sheet)
    writeData(wb, sheet, event_metrics)
    setColWidths(wb, sheet, cols = 1:cols_temp, widths = widths_temp)


    if (length(gridcode_to_process) > 2){
        results_nam <- paste0(area_of_interest_name, paste0("_", datum), ".xlsx")

    }else{
        results_nam <- paste0(paste(gridcode_to_process, collapse = "_"), paste0("_", datum), ".xlsx")

    }
    path_out <- file.path(path_intermediate_res, results_nam)
    saveWorkbook(wb, path_out, overwrite = TRUE)

}

print(summarized_stats$total_overflow_Mm3y)
print(runtime_dt)

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
pop_dt <- readRDS(file.path(path_intermediate_res, "population.rds"))
setDT(pop_dt, key = "settlement_id")
pop_dt <- pop_dt[.(gridcode_to_process)]
setkey(pop_dt, settlement_id)
pop_dt[, population := as.integer(population)]


# Import impervious area data
imp_dt <- readRDS(file.path(path_intermediate_res, "impervious_area_2015.rds"))
setDT(imp_dt, key = "settlement_id")
imp_dt <- imp_dt[.(gridcode_to_process)]
setkey(imp_dt, settlement_id)


# Import precipitation data just for gridcodes needed
prec_dt <- readRDS(file.path(path_intermediate_res, paste0("precipitation_ts_settlements", nam,".rds"))) #has gridcode, precipitation value in mm, timestamp; for 2010 - 2016: precipitation_ts_settlements.rds
# setkey(prec_dt, gridcode, time)
# prec_dt <- prec_dt[gridcode %in% gridcode_to_process &  time >= date_begin & time <= date_end] # if too slow, change to data.table filtering but mind the two keys



setkey(prec_dt, gridcode, time)

prec_dt <- prec_dt[
    data.table(gridcode = unique(gridcode_to_process)),
    on = .(gridcode)
    ][time >= date_begin & time <= date_end]

prec_dt$time <- as.POSIXct( # fix the time format (CET/CEST because of summer time, but I need the physical time)
    format(prec_dt$time, "%Y-%m-%d %H:%M:%S"),
    tz = "Etc/GMT-1"
)


print(paste0("The mean annual prec is:", sum(prec_dt$precipitation_mm)/(length(prec_dt$time)/8/365)))

# Import share served by CS # will get that data, properly add to setup then
# share_dt <- data.table(gridcode = unique(prec_dt$gridcode), share_served_by_CS = rep(0.28, length(gridcode_to_process)), key = "gridcode")
share_dt <- readRDS(file.path(path_intermediate_res, "share_CS.rds"))
setDT(share_dt, key = "gridcode")
share_dt <- share_dt[.(gridcode_to_process)]
setkey(share_dt, gridcode)

# custom CS share
# share_dt$share_served_by_CS <- 0.7


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

    summary_overflow <- data.table(
        metric = c("total population", "total impervious area served by CS [m²]",
                   "network [mm]", "tank [mm]", "total [mm]", "total [Mm3y]"),
        model = round(c(summarized_stats$population,
                        summarized_stats$imp_area_served_by_CS_km2,
                        summarized_stats$annual_mean_network_mm,
                        summarized_stats$annual_mean_tank_mm,
                        summarized_stats$total_overflow_mm,
                        summarized_stats$total_overflow_Mm3y), round_to),
        validation = c(NA, round(c(summarized_stats$validation_area,
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
    setColWidths(wb, sheet, cols = 1:cols_temp, widths = widths_temp)

    sheet <- paste0("event_metrics")
    addWorksheet(wb, sheet)
    writeData(wb, sheet, event_metrics)
    setColWidths(wb, sheet, cols = 1:cols_temp, widths = widths_temp)


    if (length(gridcode_to_process) > 5){
        results_nam <- paste0(area_of_interest_name, paste0("_", datum), ".xlsx")

    }else{
        results_nam <- paste0(paste(gridcode_to_process, collapse = "_"), paste0("_", datum), ".xlsx")

    }
    path_out <- file.path(path_intermediate_res, results_nam)
    saveWorkbook(wb, path_out, overwrite = TRUE)

}

print(summarized_stats$total_overflow_Mm3y)
print(runtime_dt)
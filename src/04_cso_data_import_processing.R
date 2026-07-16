# Script to run cso model on time series
# Written by Steffen Kittlaus based on a draft by Nina Kleemeyer
# Heavily modified by Bettina Kroyer

rm(list=ls())


library(ProjectTemplate)
load.project()


##################################################################
# CSO Data Import and Processing                                 #
# Purpose:                                                       #
# 1. Import all input data for the settlements                   #
# 2. Calculate CSO (Combined Sewer Overflow) metrics             #
# 3. Export results for each file                                #
##################################################################


################################################################################
# 1. Import & prepare pre-processed precipitation, CS share and settlement data ------------------------------------------------------
################################################################################


if (!validation_region){
    # Import population data
    pop_dt <- readRDS(file.path(path_input, filenam_pop_data))

    setDT(pop_dt, key = "settlement_id")
    pop_dt <- pop_dt[.(gridcode_to_process)]
    setkey(pop_dt, settlement_id)
    pop_dt[, population := as.numeric(population)]#as.integer(population)]
    pop_dt[is.na(population), population := 0] # one cropped settlement had NA as population

    sum(pop_dt$population)


    # change pop to validation data
    # val_data <- setDT(read.xlsx("data/Validation_data.xlsx"))
    # val_data$NAME[val_data$NAME == "Wien Kanal"] <- "ebswien kläranlage & tierservice"
    # duplrow <- val_data[1]
    # duplrow$NAME <- "ARA Pulkau"
    # val_data[1, NAME := "ARA Schrattenthal"]
    # val_data <- rbind(val_data, duplrow)
    # setnames(val_data, "NAME", "settlement_id")
    #
    # pop_dt[val_data, population := i.EW, on = "settlement_id"] # i.EW or i.Pop2018_2023
    # setnames(val_data, "settlement_id", "gridcode")

    # get the design population data
    if (!is.na(path_design_population)) { # use the design population and utilisation rate
        design_pop_dt <- read.xlsx(path_design_population)
        setDT(design_pop_dt)
        setnames(design_pop_dt, gridcode_nam, "settlement_id")

        # if no design population is found, use the EUROSTAT population
        design_pop_dt <- design_pop_dt[is.na(Bemessungswert), Bemessungswert := pop_dt[.SD, on = "settlement_id", x.population]]

    } else {
        design_pop_dt <- NA
    }


    # Import impervious area data
    imp_dt <- readRDS(file.path(path_input, filenam_imp_data))

    setDT(imp_dt, key = "settlement_id")
    imp_dt <- imp_dt[.(gridcode_to_process)]
    setkey(imp_dt, settlement_id)


    # Import share served by CS
    share_dt <- readRDS(file.path(path_input, filenam_cs_data))
    names(share_dt)[1] <- "gridcode"
    setDT(share_dt, key = "gridcode")
    share_dt <- share_dt[.(gridcode_to_process)]
    setkey(share_dt, gridcode)
    share_dt[share_served_by_CS>1, share_served_by_CS := share_served_by_CS/100]


    # change CS share to validation data
    # but not for ... E, F ,J ?
    #val_data <- val_data[!(Code %in% c("H", "E", "F"))]
    #share_dt[val_data, share_served_by_CS := i.CS, on = "gridcode"]




    # custom CS share
    if (manual_CS){
        share_dt$share_served_by_CS <- manual_CS
        print(paste0("Warning: Manual CS of ", manual_CS," is used!"))
    }
} else{
    # Import population data
    if (time_period_of_interest == "2010_2016"){
        if (!manual_pop){
            manual_pop <- readline(prompt = "Specify number of people connected to the WWTP: ")
        }
        pop_dt <- data.table(settlement_id = gridcode_to_process, population = manual_pop)
    }else{
        if(time_period_of_interest == "2021_2024"){
            pop_dt <- data.table(settlement_id = gridcode_to_process, population = manual_pop) # using the specified mean pop from 2018-2023
            #pop_dt <- readRDS(file.path(path_input, "population_2021_validation_einzugsgebiete.rds"))
        }else{
            errorCondition("use either 2010_2016 or 2021_2024 as time_period_of_interest")
        }
    }
    setDT(pop_dt, key = "settlement_id")
    pop_dt <- pop_dt[.(gridcode_to_process)]
    setkey(pop_dt, settlement_id)
    pop_dt[, population := as.numeric(population)]#as.integer(population)]
    pop_dt[is.na(population), population := 0] # one cropped settlement had NA as population

    sum(pop_dt$population)


    # Import impervious area data
    if (time_period_of_interest == "2010_2016"){
        imp_dt <- readRDS(file.path(path_input, "impervious_area_2015_validation_einzugsgebiete.rds"))
    }else{
        if(time_period_of_interest == "2021_2024"){
            imp_dt <- readRDS(file.path(path_input, "impervious_area_2021_validation_einzugsgebiete.rds"))
        }else{
            errorCondition("use either 2010_2016 or 2021_2024 as time_period_of_interest")
        }
    }
    setDT(imp_dt, key = "settlement_id")
    imp_dt <- imp_dt[.(gridcode_to_process)]
    setkey(imp_dt, settlement_id)


    # Import share served by CS
    # share_dt <- data.table(gridcode = unique(prec_dt$gridcode), share_served_by_CS = rep(0.28, length(gridcode_to_process)), key = "gridcode")
    share_dt <- readRDS(file.path(path_input, "share_CS_validation_einzugsgebiete.rds"))
    setDT(share_dt, key = "gridcode")
    share_dt <- share_dt[.(gridcode_to_process)]
    setkey(share_dt, gridcode)
    share_dt[share_served_by_CS>1, share_served_by_CS := share_served_by_CS/100]

    # custom CS share
    if (manual_CS){
        share_dt$share_served_by_CS <- manual_CS
        print(paste0("Warning: Manual CS of ", manual_CS," is used!"))
    }
}

rm(urb_3035, urb_3035_cropped_to_aoi, urb_4326, aoi_3035, aoi_buffered_epsg4326, aoi_epsg3035)

for (i in c(1:length(area_of_interest_name))){ # if not sensitivity analysis, only does one loop run

    area_of_interest_name_temp <- area_of_interest_name[i]
    params_temp <- params[[i]]

    # create Workbook with all sheets
    wb <- createWorkbook()

    widths_temp <- 23
    cols_temp <- 20

    sheet_params <- "used_params"
    addWorksheet(wb, sheet_params)

    sheet_results <- "results_per_gridcode"
    addWorksheet(wb, sheet_results)

    sheet_overflow_yearly <- "overflow_yearly"
    addWorksheet(wb, sheet_overflow_yearly)

    sheet_overflow <- "overflow_annual_mean"
    addWorksheet(wb, sheet_overflow)

    sheet_events_metrics_yearly <- "event_metrics_yearly"
    addWorksheet(wb, sheet_events_metrics_yearly)

    sheet_events_metrics <- "event_metrics_annual_mean"
    addWorksheet(wb, sheet_events_metrics)

    # fill workbook for all years

    # read validation_einzugsgebiete if validation_region!!!

    summary_overflow_all_years <- NULL

    for (year_temp in current_years){


        if (validation_region){
            file_path <- file.path(
                path_input,
                paste0("precipitation_ts_validation_einzugsgebiete_", year_temp, ".rds")
            )
        } else {
            file_path <- file.path(
                path_input,
                paste0(filenam_prec_data_base, year_temp, ".rds")
            )
        }



        if (file.exists(file_path)){
            prec_dt_year <- readRDS(file_path)
        }else{
            errorCondition(paste0("RDS data for precipitation for year ",year_temp," not found in specified path."))
        }

        names(prec_dt_year)[1] <- "gridcode"

        setkey(prec_dt_year, gridcode, time)
        prec_dt_year <- prec_dt_year[ data.table(gridcode = unique(gridcode_to_process)), on = .(gridcode) ]#[time >= date_begin &
        # time <= date_end] #option to filter by time again, only relevant if not entire years are used

        prec_dt_year$time <- as.POSIXct( # fix the time format (CET/CEST because of summer time, but I need the physical time)
            format(prec_dt_year$time, "%Y-%m-%d %H:%M:%S"),
            tz = "Etc/GMT-1"
        )

        prec_dt_year$precipitation_mm <- prec_dt_year$precipitation_mm * params_temp$factor_enlarge_prec

        print(paste0("The mean annual prec (year ",year_temp,") is: ", sum(prec_dt_year$precipitation_mm)/(length(prec_dt_year$time)/8/365), " mm"))
        gc()

        # apply cso_model to gridcodes_to_process (real data) ---------------------------------------------------------------------------------------

        plan(multisession, workers = availableCores() - 2)

        # run if future crashed
        # plan(sequential)
        # gc()


        mod_results <- future_map(
            gridcode_to_process,
            run_cso_for_single_gridcode,
            params = params_temp[, 1:8], # no factor_enlarge_prec used in the model, would be 9th
            pop_dt = pop_dt,
            design_pop_dt = design_pop_dt,
            imp_dt = imp_dt,
            share_dt = share_dt,
            manual_pop = manual_pop,
            prec_dt = prec_dt_year,
            year_temp = year_temp,
            print_params = FALSE,
            ln_A_B = ln_A_B,
            .options = furrr_options(seed = 123)
        )

        rm(prec_dt_year)

        #runtime_dt <- rbindlist(lapply(mod_results, function(x) data.table(gridcode = x$gridcode, runtime_secs = x$runtime_secs)))

        # dt_results and dt_events need separate saving else memory error when processing AoI

        # parameters ---------------------------------------------------
        dt_params <- rbindlist(
            lapply(mod_results, function(x) x$single_params),
            use.names = TRUE,
            fill = TRUE
        )

        if (year_temp == current_years[1]){
            writeData(wb, sheet_params, dt_params)
            setColWidths(wb, sheet_params, cols = 1:cols_temp, widths = widths_temp)
        }

        prec_sum <- sum(dt_params$mean_annual_prec * (dt_params$imp_area_km2 / sum(dt_params$imp_area_km2, na.rm=T)), na.rm = T)

        rm(dt_params)
        gc()


        # main results (prec, overflow, pop) ---------------------------
        dt_results <- rbindlist(
            lapply(mod_results, function(x) x$single_row),
            use.names = TRUE,
            fill = TRUE
        )

        full_area <- dt_results[, sum(imp_area_served_by_CS_km2, na.rm=T)]

        # weighted annual mean depths (mm) per dataset column
        dt_results[, `:=`(
            annual_mean_tank_mm = annual_mean_tank_mm * (imp_area_served_by_CS_km2/full_area),
            annual_mean_network_mm = annual_mean_network_mm * (imp_area_served_by_CS_km2/full_area),
            DWF_volume_mm = DWF_volume_mm * (imp_area_served_by_CS_km2/full_area)
        )]

        summarized_stats <- as.data.table(t(colSums(dt_results[,-c("year", "gridcode")], na.rm = TRUE)), keep.rownames = TRUE)
        summarized_stats[, total_overflow_mm := sum(annual_mean_tank_mm, annual_mean_network_mm)]

        summary_overflow <- data.table(
            metric = c("total impervious area served by CS [km²]", "total annual precipitation [mm]", #"total population (connected)",
                       "network [mm]", "tank [mm]", "total [mm]", "total [Mm3y]", "DWF volume [mm]", "DWF volume [Mm3]"),
            model = round(c(#summarized_stats$population_connected,
                summarized_stats$imp_area_served_by_CS_km2,
                prec_sum,
                summarized_stats$annual_mean_network_mm,
                summarized_stats$annual_mean_tank_mm,
                summarized_stats$total_overflow_mm,
                summarized_stats$total_overflow_Mm3y,
                summarized_stats$DWF_volume_mm,
                summarized_stats$DWF_volume_Mm3), round_to),
            validation = c(round(c(summarized_stats$validation_area,
                                   NA, # this is the groundtruth precipitation that led to the event, we do not know this
                                   summarized_stats$annual_mean_network_orig,
                                   summarized_stats$annual_mean_tank_orig,
                                   summarized_stats$total_overflow_orig,
                                   summarized_stats$total_overflow_Mm3y_orig,
                                   NA,
                                   NA), round_to))
        )

        summary_overflow <- cbind(data.table(year = year_temp), summary_overflow)
        summary_overflow_all_years <- rbind(summary_overflow_all_years, summary_overflow)


        if (year_temp == current_years[1]){

            cols_to_round <- c(3, 5:13)
            dt_results[, (cols_to_round) := lapply(.SD, round, digits = round_to),
                       .SDcols = cols_to_round]
            writeData(wb, sheet_results, dt_results)
            setColWidths(wb, sheet_results, cols = 1:cols_temp, widths = widths_temp)


            summary_overflow_per_year <- summary_overflow[which(summary_overflow$metric == "total annual precipitation [mm]"):which(summary_overflow$metric == "DWF volume [Mm3]"), ]
            writeData(wb, sheet_overflow_yearly, summary_overflow_per_year)
            setColWidths(wb, sheet_overflow_yearly, cols = 1:cols_temp, widths = 35)


        } else {

            cols_to_round <- c(3, 5:13)
            dt_results[, (cols_to_round) := lapply(.SD, round, digits = round_to),
                       .SDcols = cols_to_round]
            startrow <- 2 + (which(year_temp == current_years)-1) * (nrow(dt_results))
            writeData(wb, sheet_results, dt_results, startRow = startrow, colNames = FALSE)
            setColWidths(wb, sheet_results, cols = 1:cols_temp, widths = widths_temp)


            summary_overflow_per_year <- summary_overflow[which(summary_overflow$metric == "total annual precipitation [mm]"):
                                                              which(summary_overflow$metric == "DWF volume [Mm3]"), ]
            startrow <- 2 + (which(year_temp == current_years)-1) * (nrow(summary_overflow_per_year))
            writeData(wb, sheet_overflow_yearly, summary_overflow_per_year, startRow = startrow, colNames = FALSE)
            setColWidths(wb, sheet_overflow_yearly, cols = 1:cols_temp, widths = 35)
        }


        if (year_temp == current_years[length(current_years)]){ # only needed once when all results are in

            # keep the two specific metrics (one value per year)
            part1 <- unique(summary_overflow_all_years[
                metric %in% c("total population", "total impervious area served by CS [km²]"),
                .(metric = metric, annual_means = model)
            ])

            # compute mean(model) for the range of metrics between the two names
            i1 <- match("total annual precipitation [mm]", summary_overflow_all_years$metric)
            i2 <- match("DWF volume [Mm3]", summary_overflow_all_years$metric)

            block_metrics <- unique(summary_overflow_all_years[min(i1,i2):max(i1,i2), metric])

            part2 <- summary_overflow_all_years[
                metric %in% block_metrics,
                .(annual_means = mean(model, na.rm = TRUE)),
                by = metric
            ]

            summary_overflow_means <- rbind(part1, part2)

            summary_overflow_means[, annual_means_scaled := annual_means * factor_enlarge_validation]


            writeData(wb, sheet_overflow, summary_overflow_means)
            setColWidths(wb, sheet_overflow, cols = 1:cols_temp, widths = 35)

        }

        print(paste0("The total overflow is ", summarized_stats$total_overflow_Mm3y, " Mm³"))
        # print(runtime_dt)

        rm(dt_results) # free memory
        gc()



        # event stats ---------------------------------------------------
        dt_event <- rbindlist(
            lapply(mod_results, `[[`, "event_res"),
            use.names = TRUE,
            fill = TRUE
        )

        rm(mod_results)

        rain_area <- dt_event[, .(rain_event = any(rain_event),
                                  overflow_event = any(overflow_duration_h > 0),
                                  rain_volume = sum(precipitation_m3, na.rm=T),
                                  overflow_volume = sum(total_overflow_m3, na.rm=T)),
                              by = time][order(time)]

        rain_area <- rain_area[, `:=` (rain_end = fifelse(rain_event != 0 & data.table::shift(rain_event, type = "lead", fill = 0) == 0, 1L, 0L),
                                       overflow_end =  fifelse(overflow_event != 0 & data.table::shift(overflow_event, type = "lead", fill = 0) == 0, 1L, 0L),
                                       rain_duration = rain_event * min(diff(time)),
                                       overflow_duration = overflow_event * min(diff(time))
        )]

        rain_area<- rain_area[rain_end == 1, rain_event_id := seq_along(rain_end)]
        rain_area<- rain_area[overflow_end == 1, overflow_event_id := seq_along(overflow_end)]

        event_metrics <- rain_area[, .(
            nr_of_rain_events = sum(rain_end, na.rm=T),
            mean_rain_duration_per_event = sum(rain_duration, na.rm=T)/max(rain_event_id, na.rm=T),
            mean_rain_volume_per_event_mm = sum(rain_volume, na.rm=T)/max(rain_event_id, na.rm=T)/full_area/1000,

            nr_of_overflow_events = sum(overflow_end, na.rm=T),
            mean_overflow_duration_per_event = sum(overflow_duration, na.rm=T)/max(overflow_event_id, na.rm=T),
            mean_overflow_volume_per_event_mm = sum(overflow_volume, na.rm=T)/max(overflow_event_id, na.rm=T)/full_area/1000,
            mean_overflow_volume_per_event_m3 = sum(overflow_volume, na.rm=T)/max(overflow_event_id, na.rm=T),
            mean_overflow_volume_per_rain_event_m3 =  sum(overflow_volume, na.rm=T)/max(rain_event_id, na.rm=T)
        )]

        event_metrics <- cbind(data.table(year = year_temp), event_metrics)


        if (year_temp == current_years[1]){ # only needed once

            cols_to_round <- c(3, 5:13)

            event_metrics[, c(3:4,6:8) := lapply(.SD, round, digits = round_to),
                          .SDcols =  c(3:4,6:8)]
            writeData(wb, sheet_events_metrics_yearly, event_metrics)
            setColWidths(wb, sheet_events_metrics_yearly, cols = 1:cols_temp, widths = 35)

            event_metrics_rows <- event_metrics


        } else {

            cols_to_round <- c(3, 5:13)

            event_metrics[, c(3:4,6:8) := lapply(.SD, round, digits = round_to),
                          .SDcols =  c(3:4,6:8)]
            startrow <- 2 + (which(year_temp == current_years)-1) * (nrow(event_metrics))
            writeData(wb, sheet_events_metrics_yearly, event_metrics, startRow = startrow, colNames = FALSE)
            setColWidths(wb, sheet_events_metrics_yearly, cols = 1:cols_temp, widths = 35)

            event_metrics_rows <- rbind(event_metrics_rows, event_metrics)
        }


        if (year_temp == current_years[length(current_years)]){ # only needed once when all results are in


            event_metrics_means_numbers <- event_metrics_rows[, lapply(.SD, function(x) {
                if (inherits(x, "difftime")) {
                    mean(as.numeric(x), na.rm = TRUE)
                } else {
                    mean(x, na.rm = TRUE)
                }
            }),
            .SDcols = c("nr_of_rain_events", "nr_of_overflow_events")]


            event_metrics_means_weighted_by_rain_event <- event_metrics_rows[, lapply(.SD, function(x) {
                if (inherits(x, "difftime")) {
                    sum(as.numeric(x) * nr_of_rain_events, na.rm=T)/sum(nr_of_rain_events, na.rm=T)
                } else {
                    sum(x * nr_of_rain_events, na.rm = TRUE)/sum(nr_of_rain_events, na.rm=T)
                }
            }),
            .SDcols = c("mean_rain_duration_per_event", "mean_rain_volume_per_event_mm", "mean_overflow_volume_per_rain_event_m3")]


            event_metrics_means_weighted_by_overflow_event <- event_metrics_rows[, lapply(.SD, function(x) {
                if (inherits(x, "difftime")) {
                    sum(as.numeric(x) * nr_of_overflow_events, na.rm=T)/sum(nr_of_overflow_events, na.rm=T)
                } else {
                    sum(x * nr_of_overflow_events, na.rm = TRUE)/sum(nr_of_overflow_events, na.rm=T)
                }
            }),
            .SDcols = c("mean_overflow_duration_per_event", "mean_overflow_volume_per_event_mm", "mean_overflow_volume_per_event_m3")]


            event_metrics_means <- cbind(event_metrics_means_numbers, event_metrics_means_weighted_by_rain_event, event_metrics_means_weighted_by_overflow_event)

            event_metrics_means[, c(3:8) := lapply(.SD, round, digits = round_to),
                                .SDcols = c(3:8)]

            writeData(wb, sheet_events_metrics, event_metrics_means)
            setColWidths(wb, sheet_events_metrics, cols = 1:cols_temp, widths = widths_temp)

        }

        rm(dt_event) # free memory
        gc()


        if (length(gridcode_to_process) > 2 | validation_region == TRUE){
            results_nam <- paste0(area_of_interest_name_temp, paste0("_", datum), ".xlsx")

        }else{
            results_nam <- paste0(paste(gridcode_to_process, collapse = "_"), paste0("_", datum), ".xlsx")

        }

    }

    path_out <- file.path(path_intermediate_res, results_nam)
    saveWorkbook(wb, path_out, overwrite = TRUE)

}














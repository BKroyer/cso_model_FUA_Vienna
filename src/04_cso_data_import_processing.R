# Script to run cso model on time series
# Written by Steffen Kittlaus based on a draft by Nina Kleemeyer
# Heavily modified by Bettina Kroyer
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
imp_dt <- readRDS(file.path(path_intermediate_res, "impervious_area.rds"))
setDT(imp_dt, key = "settlement_id")
imp_dt <- imp_dt[.(gridcode_to_process)]
setkey(imp_dt, settlement_id)


# Import precipitation data just for gridcodes needed
prec_dt <- readRDS(file.path(path_intermediate_res, "precipitation_ts_settlements.rds")) #has gridcode, precipitation value in mm, timestamp
setkey(prec_dt, gridcode, time)
prec_dt <- prec_dt[gridcode %in% gridcode_to_process &  time >= date_begin & time <= date_end] # if too slow, change to data.table filtering but mind the two keys

prec_dt$time <- as.POSIXct( # fix the time format (CET/CEST because of summer time, but I need the physical time)
    format(prec_dt$time, "%Y-%m-%d %H:%M:%S"),
    tz = "Etc/GMT-1"
)

# Import share served by CS # will get that data, properly add to setup then
share_dt <- data.table(gridcode = unique(prec_dt$gridcode), share_served_by_CS = rep(0.28, length(gridcode_to_process)), key = "gridcode")




# apply cso_model to gridcodes_to_process (real data) ---------------------------------------------------------------------------------------

plan(multisession, workers = availableCores() - 2)

gridcodes <- params$gridcode[1:3]

mod_results <- future_map(
    gridcodes,
    run_cso_for_single_gridcode,
    params = params,
    pop_dt = pop_dt,
    imp_dt = imp_dt,
    share_dt = share_dt,
    prec_dt = prec_dt,
    .options = furrr_options(seed = TRUE)
)

walk(mod_results, function(wb_path) {
    wb_temp <- wb_path$wb
    path_temp <- wb_path$path_out
    # Extract gridcode from first sheet
    sheet_name <- names(wb_temp)[1]
    gc <- sub("params_", "", sheet_name)

    saveWorkbook(
        wb_temp,
        file.path(path_temp, paste0("cso_summaries_", gc, ".xlsx")),
        overwrite = TRUE
    )
})

# mod_res <- cso_model(population = pop_dt$population,
#                      area = imp_dt$imp_area_km2,
#                      share_served_by_CS = share_dt$share_served_by_CS,
#                      time = time_phys,
#                      precipitation = prec_dt$precipitation_mm,
#                      dwf_per_capita = params$dwf_per_capita,
#                      W0 = params$W0,
#                      k0 = params$k0,
#                      dn = params$dn,
#                      dt = params$dt,
#                      W1 = params$W1,
#                      W2 = params$W2)



# single gridcode validation --------------------------------------------------------------------------------------------------------------------------------
# location <- "Vienna"
# results_nam <- paste0(location, paste0("_", datum, "_y", substr(min_time, 1, 4), "_",  substr(max_time, 1, 4)))
# path_out <- file.path(path_intermediate_res, results_nam)
# process_and_plot_results(mod_res, path_out, location = location, validation_data = data_vienna, validation_area = 70)

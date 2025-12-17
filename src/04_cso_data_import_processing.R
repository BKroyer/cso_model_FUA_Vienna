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


# Wrapper function to apply cso_model ---------------------------------------------------------------------------

#wrapper_cso_model <- function(id, ) # parallel processing über die einzelnen settlements, quasi-sequenziell


###########################################################
# 1. Import & prepare pre-processed precipitation and settlement data ------------------------------------------------------
###########################################################



# get the gridcodes within Austria # in setup
# load the precipiation values for these gridcodes
# run model for all those gridcodes with the same (?) 6 parameters
# aggregate results and compare to paper



# import population data
pop_dt <- readRDS(file.path(path_intermediate_res, "population.rds"))
setDT(pop_dt, key = "settlement_id")
pop_dt <- pop_dt[.(gridcode_to_process)]
pop_dt[, population := as.integer(population)]


# import impervious area data
imp_dt <- readRDS(file.path(path_intermediate_res, "impervious_area.rds"))
setDT(imp_dt, key = "settlement_id")
imp_dt <- imp_dt[.(gridcode_to_process)]


# Import precipitation data just for gridcodes needed
prec_dt <- readRDS(file.path(path_intermediate_res, "precipitation_ts_settlements.rds")) #has gridcode, precipitation value in mm, timestamp
setDT(prec_dt, key = "gridcode")
prec_dt <- prec_dt[.(gridcode_to_process)]
prec_dt <- prec_dt[time >= date_begin & time <= date_end]


# Import share served by CS # will get that data, add to setup then
share_dt <- data.table(gridcode = unique(prec_dt$gridcode), share_served_by_CS = 0.28, key = "gridcode")







# apply cso_model to Vienna (real data) ---------------------------------------------------------------------------------------------------

# params for Vienna from Excel, share CS guessed from their area # Add this to setup as well once figured out what to specify in case of all
params <- data.table(k0 = 0.3, W0 = 1.5, dn = 29, dt = 2, W1 = 5, W2 = 1.5, dwf_per_capita = 0.2, qdwf = 0.55)

# fix the time format (CET/CEST because of summer time, but I need the physical time)
time_ts <- prec_dt$time
time_phys <- as.POSIXct(
    format(time_ts, "%Y-%m-%d %H:%M:%S"),
    tz = "Etc/GMT-1"   # CET without DST
)

# applying the model
mod_res <- cso_model(population = pop_dt$population,
                     area = imp_dt$imp_area_km2,
                     share_served_by_CS = share_dt$share_served_by_CS,
                     time = time_phys,
                     precipitation = prec_dt$precipitation_mm,
                     dwf_per_capita = params$dwf_per_capita,
                     # qdwf = params$qdwf, # from Excel, not sure how they got there
                     W0 = params$W0,
                     k0 = params$k0,
                     dn = params$dn,
                     dt = params$dt,
                     W1 = params$W1,
                     W2 = params$W2)

# qdwf: pop_density * dwf_per_capita / timesteps_per_day / 1000 = 27100 * 0.2 / 8 / 1000 = 0.6775

location <- "Vienna"
results_nam <- paste0(location, paste0("_dwf02_", datum, "_y", substr(min_time, 1, 4), "_",  substr(max_time, 1, 4)))
path_out <- file.path(path_intermediate_res, results_nam)
process_and_plot_results(mod_res, path_out, location = location, validation_data = data_vienna, validation_area = 70)

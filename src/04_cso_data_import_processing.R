# Script to run cso model on time series
# Written by Steffen Kittlaus based on a draft by Nina Kleemeyer
# Heavily modified by Bettina Kroyer
library(ProjectTemplate)
load.project()

######################################################
# CSO Data Import and Processing                     #
# Purpose:                                           #
# 1. Import precipitation and FUA data               #
# 2. Prepare and clean data                          #
# 3. Calculate CSO (Combined Sewer Overflow) metrics #
# 4. Export results for each file                    #
######################################################

########################################################
# Comparing to Excel (city-wise)
########################################################

# validation functions  -----------------------------------------------------------------------------------------------
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

process_and_plot_results <- function(mod_res, path_out, validation_data = NULL, validation_area = NULL, location = "", mask = NULL, time_step = 3, round_to = 4 ){
    #' @param mod_res the model result data frame (output of model_cso)
    #' @param path_out path to save the plots to; will be created but not overwritten
    #' @param validation_data data frame with same timestep as mod_res and columns for network and tank overflow, if NULL, only model results are summarized
    #' @param validation_area option to specify reference area of validation dataset in case if differs from modelled unit
    #' @param location name of the processed location as a string, will be added to the result file names
    #' @param mask the row indices to consider in mean and total computation
    #' @param time_step the time step in hours, default is 3
    #' @param round_to the digits to round results to, default is 4
    #'
    #' @returns print of results, saves plots and table results

    # create the folder
    dir.create(path_out, recursive = TRUE, showWarnings = FALSE)


    has_validation <- !is.null(validation_data)

    area <- unique(mod_res$area)
    if (is.null(validation_area)){
        validation_area <- area
    }


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

    # specify actual timeframe for naming later
    assign("min_time", min_time)
    assign("max_time", max_time)



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
        total_overflow_Mm3y <- round(total_overflow * area / 1000, round_to)
        if (has_validation) {
            total_overflow_Mm3y_orig <- round(total_overflow_orig * validation_area / 1000, round_to)
        }
    } else {
        total_overflow_Mm3y <- NA
        if (has_validation) total_overflow_Mm3y_orig <- NA
    }


    overflow_duration <- (mean(mod_res$frequency_overflow, na.rm = TRUE) *
                              (24 / as.integer(time_step)) * 365) * time_step

    cso_volume_per_event <- sum(mod_res$cso_volume_per_event, na.rm = TRUE) /
        sum(mod_res$rain_end, na.rm = TRUE)




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
        total_overflow_Mm3y, " Mm³/y",
        if (has_validation)
            paste0(" (validation: ", total_overflow_Mm3y_orig, " Mm³/y)"),
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

    saveWorkbook(wb, file = file.path(path_out, paste0("cso_summaries_", location, ".xlsx")), overwrite = TRUE)


    ## RETURN ========

    invisible(list(
        summary_overflow = summary_overflow,
        event_metrics = event_metrics
    ))
}


# SANTIAGO ------------------------------------------------------------------------------------------------------------
data_santi <- read_excel(file.path(path_intermediate_res, "model_prototype (3hourly)_Santiago_2.xlsx"), sheet = 2)
precipitation <- data_santi$P
time <- data_santi$DateValue
mod_res <- cso_model(W1 = 5,
                     population = 100000,
                     dwf_per_capita = 0.275,
                     area = 9.4,
                     share_served_by_CS = (390*0.8+550*0.2)/940, # share weighted mean by area
                     k0 = 0.3,
                     dn = 7,
                     W2 = 2,
                     dt = 1.5,
                     W0 = 1.5,
                     time = time,
                     precipitation = precipitation)
# LODZ2 ------------------------------------------------------------------------------------------------------------
data_lodz2 <- read_excel(file.path(path_intermediate_res, "model_prototype (3hourly)_Lodz_2.xlsx"), sheet = 3)
precipitation <- data_lodz2$P
time <- data_lodz2$DateValue
mod_res <- cso_model(W1 = 5,
                     pop_density = 5600,
                     qdwf = 1.722287,
                     area = 17.14/0.3,
                     share_served_by_CS = 0.30,
                     k0 = 0.3,
                     dn = 7,
                     W2 = 2,
                     dt = 3.6,
                     W0 = 1.5,
                     time = time,
                     precipitation = precipitation)
# ECULLY ----------------------------------------------------------------------------------------------------------
data_ecully <- read_excel(file.path(path_intermediate_res, "model_prototype (3hourly)_Ecully.xlsx"), sheet = 3)
#mask <- c(1:120)
precipitation <- data_ecully$P#[mask]
time <- data_ecully$DateValue#[mask]


mod_res <- cso_model(W1 = 13,
                     pop_density = 5600,
                     qdwf = 0.44,
                     area = 64, # the impervious one
                     share_served_by_CS = 1, # not given, but to obtain correct area impervious
                     k0 = 0.3,
                     dn = 7,
                     W2 = 2,
                     dt = 4,
                     W0 = 1.5,
                     dwf_per_capita = 0.231,
                     time = time,
                     precipitation = precipitation)
# BARCELONA -----------------------------------------------------------------------------------------------------------

# mmc2_path <- file.path(path_intermediate_res, "1-s2.0-S2214581822000933-mmc2_corrected_SK.xlsx")
# validation_data <- read_excel(mmc2_path, sheet = 3)
# validation_param <- read_excel(mmc2_path, sheet = 2)
#
#
#
# setDT(validation_data)
#
data_barcelona <- read_excel(file.path(path_intermediate_res, "model_prototype (3hourly)_Barcelona_corrected_SK.xlsx"), sheet = 2)
#mask <- c(1:120)
precipitation <- data_barcelona$P#[mask]
barcelona_time <- data_barcelona$DateValue#[mask]
area <- 12

# try barcelona
mod_res <- cso_model(population = 200000,
                     area = 12,
                     share_served_by_CS = 0.75,
                     W1 = 5,
                     k0 = 0.3,
                     dn = 7,
                     W2 = 6.1111111,
                     dt = 23.76, #as.numeric(validation_param[10, 5][[1]]),
                     W0 = 1.5, # as.numeric(validation_param[10, 7][[1]]),
                     dwf_per_capita = 0.2,
                     time = barcelona_time,
                     precipitation = precipitation) #validation_data$P

# blabla

# INNSBRUCK -------------------------------------------------------------------------------------------------------------
data_innsbruck <- read_excel(file.path(path_intermediate_res, "model_prototype (3hourly)_Innsbruck.xlsx"), sheet = 3)
#mask <- c(1:1460) # Innsbruck 1450 has scenario c
precipitation <- data_innsbruck$P#[mask]
innsbruck_time <- data_innsbruck$DateValue#[mask]

mod_res_innsbruck <- cso_model(population = 165000,
                     area = 9.15,
                     share_served_by_CS = 1,
                     #pop_density = 222,
                     W1 = 2.99027, # input parameter, in Innsbruck calculated as 27361/ imp area in m² * 1000 --> the 27361 must be m³ of network volume
                     k0 = 0.3,
                     dn = 25, #as 5000/(qdwf*1000) so 5000/ Qdwf in L --> 5000 must be network volume in L
                     W2 = 0.51366, # calculated from tank volume of 4700 m³ --> divided by the imp. area in m² and times 1000 to get to mm, 4700 m³ as the physical tank volume
                     dt = 11, # 2200/200; 200 L/day Qdwf so must be 2200 L/day as dilution reference volume
                     W0 = 0.55738, #as 5100 / imp. area in m² times 1000 to get to mm --> 5100 m³ assumed in total on surface
                     dwf_per_capita = 0.2,
                     time = innsbruck_time,
                     precipitation = precipitation) #validation_data$P
location <- "Innsbruck_EXCEL"
results_nam <- paste0(location, paste0("_lnA_B_", datum, "_y", substr(date_begin, 1, 4), "_",  substr(date_end, 1, 4)))
path_out <- file.path(path_intermediate_res, results_nam)
process_and_plot_results(mod_res_innsbruck, path_out, location = location, validation_data = data_innsbruck)



# STUTTGART -----------------------------------------------------------------------------------------------------------
data_stuttgart <- read_excel(file.path(path_intermediate_res, "model_prototype (3hourly)_Stuuttgart_datecorr.xlsx"), sheet = 3)
precipitation <- data_stuttgart$P
stuttgart_time <- data_stuttgart$DateValue
summary(stuttgart_time)
area <- 35
Qdwf <- 22985*1000/(35000000*0.5)/8 # from Excel
pop_dens <- 160000/(3500*0.5)
qdwf_per_cap <- (Qdwf * 10 * 8) / (pop_dens)
mod_res <- cso_model(population = 160000,
                     area = area,
                     share_served_by_CS = 0.5,
                     #pop_density = 222,
                     W1 = 5, # W1
                     k0 = 0.3, # k0
                     dn = 40, # Excel states 40, but uses different parameters later, cannot access those # dn
                     W2 = 2, # W2
                     dt = 4, # dt
                     W0 = 1.5, # W0
                     dwf_per_capita = qdwf_per_cap,
                     time = stuttgart_time,
                     precipitation = precipitation)









# WIEN ------------------------------------------------------------------------------------------------------------------------
data_vienna <- read_excel(file.path(path_intermediate_res, "model_prototype (3hourly)_Vienna_with_params_fixing_rows.xlsx"), sheet = 3)
params <- data.table(k0 = 0.3, W0 = 1.5, dn = 29, dt = 2, W1 = 5, W2 = 1.5, dwf_per_capita = 0.2, qdwf = 0.55)
mod_res_vienna_excel <- cso_model(population = 1897000,
                     area = 140,
                     share_served_by_CS = 0.5,
                     time = data_vienna$DateValue,
                     precipitation = data_vienna$P,
                     # dwf_per_capita = params$dwf_per_capita,
                     qdwf = params$qdwf, # from Excel, not sure how they got there
                     W0 = params$W0,
                     k0 = params$k0,
                     dn = params$dn,
                     dt = params$dt,
                     W1 = params$W1,
                     W2 = params$W2)
location <- "Vienna_EXCEL"
results_nam <- paste0(location, paste0("_lnB_A_", datum, "_y", substr(date_begin, 1, 4), "_",  substr(date_end, 1, 4)))
path_out <- file.path(path_intermediate_res, results_nam)
process_and_plot_results(mod_res_vienna_excel, path_out, location = location, validation_data = data_vienna, mask = 4:nrow(mod_res_vienna_excel))

# validation -------------------------------------------------------------------------------------------------------------------

validation_data <- data_ecully
#mask <- 37985:40904 # just 2014
mask <- 3:46753
#mask <- 31650:34577
results_nam <- "Ecully_testing_corr_eq"
path_out <- file.path(path_intermediate_res, results_nam)

process_and_plot_results(mod_res, validation_data, mask, path_out)






########################################################
# Wrapper function to apply cso_model
########################################################

#wrapper_cso_model <- function(id, ) # parallel processing über die einzelnen settlements, quasi-sequenziell


########################################################
# Using pre-processed precipitation and settlement data
########################################################



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







# apply cso_model to Vienna

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

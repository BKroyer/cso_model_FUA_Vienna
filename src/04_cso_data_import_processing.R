# Script to run cso model on time series
# Written by Steffen Kittlaus based on a draft by Nina Kleemeyer
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


# mmc2_path <- file.path(path_intermediate_res, "1-s2.0-S2214581822000933-mmc2_corrected_SK.xlsx")
# validation_data <- read_excel(mmc2_path, sheet = 3)
# validation_param <- read_excel(mmc2_path, sheet = 2)
#
#
#
# setDT(validation_data)
#
# data_barcelona <- read_excel(file.path(path_intermediate_res, "model_prototype (3hourly)_Barcelona_corrected_SK.xlsx"), sheet = 2)
# mask <- c(1:120)
# precipitation <- data_barcelona$P[mask]
# barcelona_time <- data_barcelona$DateValue[mask]
#
# # try barcelona
# mod_res <- cso_model(population = 200000,
#                      area = 12,
#                      pop_density = 222,
#                      network_storage = 5,
#                      rate_constant_surface_storage = 0.3,
#                      network_dwf_dilution_rate = 7,
#                      tank_storage = 6.1111111,
#                      tank_volume_init = as.numeric(validation_param[10, 6][[1]]), ##?
#                      tank_dwf_dilution_rate = 23.76, #as.numeric(validation_param[10, 5][[1]]),
#                      catchment_surface_storage = 1.5, # as.numeric(validation_param[10, 7][[1]]),
#                      dwf_per_capita = 0.2,
#                      time = barcelona_time,
#                      precipitation = precipitation) #validation_data$P

data_innsbruck <- read_excel(file.path(path_intermediate_res, "model_prototype (3hourly)_Innsbruck.xlsx"), sheet = 3)
mask <- c(1:1460) # Innsbruck 1450 has scenario c
precipitation <- data_innsbruck$P#[mask]
innsbruck_time <- data_innsbruck$DateValue#[mask]

# try innsbruck
area <- 9.15
mod_res <- cso_model(population = 165000,
                     area = 9.15,
                     share_served_by_CS = 1,
                     #pop_density = 222,
                     network_storage = 2.99027, # input parameter, in Innsbruck calculated as 27361/ imp area in m² * 1000 --> the 27361 must be m³ of network volume
                     rate_constant_surface_storage = 0.3,
                     network_dwf_dilution_rate = 25, #as 5000/(qdwf*1000) so 5000/ Qdwf in L --> 5000 must be network volume in L
                     tank_storage = 0.51366, # calculated from tank volume of 4700 m³ --> divided by the imp. area in m² and times 1000 to get to mm, 4700 m³ as the physical tank volume
                     tank_dwf_dilution_rate = 11, # 2200/200; 200 L/day Qdwf so must be 2200 L/day as dilution reference volume
                     catchment_surface_storage = 0.55738, #as 5100 / imp. area in m² times 1000 to get to mm --> 5100 m³ assumed in total on surface
                     dwf_per_capita = 0.2,
                     time = innsbruck_time,
                     precipitation = precipitation) #validation_data$P



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
            xlab(paste0("original ",var_nam," in mm (Excel)")) +
            ylab(paste0("modeled ",var_nam," in mm (R)")) +
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


process_and_plot_results <- function(mod_res, validation_data, mask, path_out, time_step = 3, round_to = 4){
    #' @param mod_res the model result data frame (output of model_cso)
    #' @param validation_data data frame with same timestep as mod_res and columns for network and tank overflow
    #' @param mask the row indices to consider in mean and total computation
    #' @param path_out path to save the plots to
    #' @param time_step the time step in hours, default is 3
    #' @param round_to the digits to round results to, default is 4
    #'
    #' @returns print of results, saves plots and table results


    ## TANK
    var_nam <- "tank"
    # get the right columns
    tank_mod <- get_cols(var_nam, mod_res)
    tank_orig <- get_cols(var_nam, validation_data)
    tank_compare_data <- data.table(time = mod_res$time, orig = tank_orig, mod = tank_mod, scenario = mod_res$scenario)
    # annual mean overflows
    annual_mean_tank <- mean(tank_mod[mask], na.rm=T) * (24/as.integer(time_step)) * 365
    annual_mean_tank_orig <- mean(tank_orig[mask], na.rm=T) * (24/as.integer(time_step)) * 365
    # plot
    plot_compare_data(var_nam, tank_compare_data, path_out)


    ## NETWORK
    var_nam <- "network"
    # get the right columns
    network_mod <- get_cols(var_nam, mod_res)
    network_orig <- get_cols(var_nam, validation_data)
    network_compare_data <- data.table(time = mod_res$time, orig = network_orig, mod = network_mod, scenario = mod_res$scenario)
    # annual mean overflows
    annual_mean_network <- mean(network_mod[mask], na.rm=T) * (24/as.integer(time_step)) * 365
    annual_mean_network_orig <- mean(network_orig[mask], na.rm=T) * (24/as.integer(time_step)) * 365
    # plot
    plot_compare_data(var_nam, network_compare_data, path_out)


    # total overflow in Mm³/year respecting the mask
    total_overflow <- annual_mean_tank + annual_mean_network
    total_overflow_Mm3y <- total_overflow * area / 1000 # total overflow in million m³ per year for entire study area
    total_overflow_Mm3y_orig <- (annual_mean_network_orig + annual_mean_tank_orig) * area / 1000

    print(paste0("the modeled network overflow is: ", round(annual_mean_network, round_to), " mm/y/m² (vs. validation: ", round(annual_mean_network_orig, round_to)," mm/y/m²)"))
    print(paste0("the modeled tank overflow is: ", round(annual_mean_tank, round_to), " mm/y/m² (vs. validation: ", round(annual_mean_tank_orig, round_to)," mm/y/m²)"))
    print(paste0("the modeled total overflow is: ", round(total_overflow_Mm3y, round_to), " Mm³/y (vs. validation: ", round(total_overflow_Mm3y_orig, round_to), " Mm³/y)"))



    # save print result in tables





}



validation_data <- data_innsbruck
# Innsbruck starts validation at timestep 4 (4:46735) --> here 3 to 46734
# 1.1.2001 6:00 to 29.12.2016 15:00
mask <- 3:46754
results_nam <- "Innsbruck_Nov26"
path_out <- file.path(path_intermediate_res, results_nam)

process_and_plot_results(mod_res, validation_data, mask, path_out)




# mod_res <- cso_model(population = 200000,
#                      area = 12,
#                      pop_density = 222,
#                      network_storage = validation_param[8, 2][[1]],
#                      rate_constant_surface_storage = validation_param[6, 2][[1]],
#                      network_dwf_dilution_rate = validation_param[7, 2][[1]],
#                      tank_storage = 6.1111111,
#                      tank_volume_init = as.numeric(validation_param[10, 6][[1]]), ##?
#                      tank_dwf_dilution_rate = 23.76, #as.numeric(validation_param[10, 5][[1]]),
#                      catchment_surface_storage = 1.5, # as.numeric(validation_param[10, 7][[1]]),
#                      dwf_per_capita = validation_param[3, 2][[1]],
#                      time = validation_data$DateValue,
#                      precipitation = validation_data$P)
#
# orig <- validation_data$Ec
# mod <- mod_res$ec
# identical(orig, mod)
# compare_data <- data.table(orig = round(orig, 3), mod = round(mod, 3))
# compare_data[, identical:= orig == mod]
# identical(compare_data$orig, compare_data$mod)
#
# ggplot(compare_data, aes(x = orig, y = mod)) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0)



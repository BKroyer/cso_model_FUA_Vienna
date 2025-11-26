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

validation_data <- data_innsbruck
orig <- validation_data$`Network overflow E't`
mod <- mod_res$network_overflow

# Innsbruck starts validation at timestep 4 (4:46735) --> here 3 to 46734
# 1.1.2001 6:00 to 29.12.2016 15:00

### CHANGE TO FULL DATA

## compute Mm³/year
orig_total_overflow_Mm3_y <- mean(validation_data$`Network overflow E't` + validation_data$`Tank overflow E''t`, na.rm=T) * 8 * 365


#orig <- validation_data$`Tank overflow E''t`
#mod <- mod_res$tank_overflow
#identical(orig, mod)
#compare_data <- data.table(orig = round(orig, 3), mod = round(mod, 3))
compare_data <- data.table(orig = orig, mod = mod, scenario = mod_res$scenario)
#compare_data[, identical:= orig == mod]
#identical(compare_data$orig, compare_data$mod)

ggplot(compare_data, aes(x = orig, y = mod, color = scenario)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    theme_bw()

mod_res$diff_to_orig <- mod - orig
ggplot(mod_res, aes(x = time, y = diff_to_orig, color = scenario)) +
    geom_point(alpha = 0.6) +
    #geom_line() +
    theme_bw()

# annual mean overflows # in Excel they exclude the first two values (Innsbruck 3 to 46734)
mask <- 3:46754
mean(mod_res$tank_overflow[mask], na.rm=T) * 8 * 365 # 74.47652 (vs Excel: 74.47651)
mean(mod_res$network_overflow[mask], na.rm=T) * 8 * 365 # 21.81888 (vs Excel: 21.6777); becomes 15.78918 if B/A for case b!!!!!!



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



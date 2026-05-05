# Written by Bettina Kroyer
library(ProjectTemplate)
load.project()


########################################################
# Comparing to Excel (city-wise)
########################################################


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
                     dt = 23.76,
                     W0 = 1.5,
                     dwf_per_capita = 0.2,
                     time = barcelona_time,
                     precipitation = precipitation)


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
                               precipitation = precipitation)
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
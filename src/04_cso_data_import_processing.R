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

# Libraries
#library(sf)
#library(dplyr)
#library(nkcso)

library(readxl)
library(ggplot2)
validation_data <- read_excel("D:/Nextcloud/Daten_SK/Studienarbeiten/Nina Kleemeyer/Data_Quaranta_et_al/1-s2.0-S2214581822000933-mmc2_corrected_SK.xlsx", sheet = 3)
validation_param <- read_excel("D:/Nextcloud/Daten_SK/Studienarbeiten/Nina Kleemeyer/Data_Quaranta_et_al/1-s2.0-S2214581822000933-mmc2_corrected_SK.xlsx", sheet = 2)


setDT(validation_data)
#data_bacelona[46753, DateValue := as.POSIXct("2017-01-01 00:00:00", tz="GMT")]

mod_res <- cso_model(population = 200000,
                     area = 1200,
                     pop_density = validation_param[2, 2][[1]],
                     network_storage = validation_param[8, 2][[1]],
                     rate_constant_surface_storage = validation_param[6, 2][[1]],
                     network_dwf_dilution_rate = validation_param[7, 2][[1]],
                     tank_storage = as.numeric(validation_param[10, 6][[1]]),
                     tank_volume_init = as.numeric(validation_param[10, 6][[1]]),
                     tank_dwf_dilution_rate = as.numeric(validation_param[10, 5][[1]]),
                     catchment_surface_storage = as.numeric(validation_param[10, 7][[1]]),
                     dwf_per_capita = validation_param[3, 2][[1]],
                     time = validation_data$DateValue,
                     precipitation = validation_data$P)

orig <- validation_data$Ec
mod <- mod_res$ec
identical(orig, mod)
compare_data <- data.table(orig = round(orig, 3), mod = round(mod, 3))
compare_data[, identical:= orig == mod]
identical(compare_data$orig, compare_data$mod)
ggplot(compare_data, aes(x = orig, y = mod)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)



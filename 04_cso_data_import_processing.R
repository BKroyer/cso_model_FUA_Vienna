##########################################################################################################
###                                                                                                    ###
### Hydrological Modeling of Combined Sewer Overflows (CSOs)                                           ###
### An R-Based Implementation of Quaranta et al.`s Approach                                            ###
### by Nina Kleemeyer, B.Sc.                                                                           ###
###                                                                                                    ###
### Supervisor: Univ. Prof. Dipl.-Ing. Dr. techn. Matthias Zessner, TU Wien                            ###
###                                                                                                    ###
### 4/5 CSO Data Import and Processing                                                                  ###
##########################################################################################################

######################################################
# CSO Data Import and Processing                     #  
# Purpose:                                           #
# 1. Import precipitation and FUA data               #
# 2. Prepare and clean data                          #
# 3. Calculate CSO (Combined Sewer Overflow) metrics #
# 4. Export results for each file                    #
######################################################

# Libraries
library(sf)        
library(dplyr)     
library(nkcso)      

# Set default CSO parameters 
cso_defaults(silent = 1)


import_file <- function(filename) {
  
  cat(paste("Importing file:", filename, "\n"))
  
  #  Read spatial CSV file
  df <- st_read(filename, quiet = TRUE, rmNA = TRUE)
  
  #  Rename columns to standardized names
  df <- df %>% 
    rename(
      precipitation = precip_mm_3h,
      fua           = eFUA_name,
      area          = FUA_area,
      population    = FUA_p_2015
    )
  
  #  Remove unnecessary columns
  df <- df %>%
    select(
      -UC_num, -UC_IDs, -Commuting, -FUA_ID, -time,
      -file, -eFUA_ID, -UC_area, -UC_p_2015, -Com_p_2015
    )
  
  #  Sort data by FUA and timestep (if available)
  if ("timestep" %in% names(df)) {
    df <- df[with(df, order(fua, timestep)), ]
  } else {
    df <- df[order(fua), ]  # fallback if timestep not present
  }
  
  #  Run CSO calculations from package nkcso
  data <- cso_data(df)
  
  #  Filter: keep only events with CSO volume > 0
  result_cso_total_volume_per_event <- subset(data, cso_total_volume_per_event > 0)
  
  # Create output filenames
  output_filename <- paste0("results/", gsub(".csv", "_result.csv", filename))
  output_cso_filename <- paste0("results/", gsub(".csv", "_result_cso.csv", filename))
  
  #  Export results as CSV
  cso_save_csv(data, output_filename)
  cso_save_csv(result_cso_total_volume_per_event, output_cso_filename)
  
  return(TRUE)
}


# Set working directory to input folder
setwd("C:/Users/sim06/Git/cso-modell/output_by_country")

# Create a "results" directory if not existing
if (!dir.exists("results")) {
  dir.create("results")
}

# List all files to the pattern: *merged.csv
files <- list.files(pattern = "*merged.csv")

results <- lapply(files, import_file)

cat("Processing completed successfully.\n")

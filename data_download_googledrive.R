##########################################################################################################
###                                                                                                    ###
### Hydrological Modeling of Combined Sewer Overflows (CSOs)                                           ###
### An R-Based Implementation of Quaranta et al.`s Approach                                            ###
### by Nina Kleemeyer, B.Sc.                                                                           ###
###                                                                                                    ###
### Supervisor: Univ. Prof. Dipl.-Ing. Dr. techn. Matthias Zessner, TU Wien                            ###
###                                                                                                    ###
### 3/5 Data Download from GoogleDrive and Processing                                                               ###
##########################################################################################################


#########################################################################
# CSO NetCDF Data Download and Processing                               #
# Purpose:                                                              #
# 1. Download all NetCDF precipitation data for 2015 from Google Drive  #
# 2. Filter FUAs for selected European countries                        # 
# 3. Extract precipitation values per FUA                               #   
# 4. Export results as CSV per country                                  # 
#########################################################################

# Libraries
library(googledrive)
library(terra)
library(data.table)
library(pbapply)
library(future.apply)

# Parameters
folder_id <- "FOLDER ID"
fua_path <- "C:/GEOPACKAGE.gpkg"
output_dir <- "C:/Output_Path"
nc_dir <- file.path(output_dir, "nc_files")
dir.create(nc_dir, showWarnings = FALSE)

countries_to_process <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
  "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX",
  "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR"
)

# Load FUAs 
cat("Loading FUAs...\n")
fua <- vect(fua_path)
fua <- project(fua, "EPSG:4326")
fua <- fua[fua$Cntry_ISO %in% countries_to_process, ]
fua$ID <- seq_len(nrow(fua))
fua_dt <- as.data.table(fua)
cat("FUAs loaded:", nrow(fua_dt), "\n")

# Prepare NetCDF files
cat("Fetching NetCDF file list from Google Drive...\n")
file_list <- drive_find(
  q = sprintf("'%s' in parents and name contains '2015' and name contains '.nc'", folder_id)
)
cat("", nrow(file_list), "files found.\n")

for (i in seq_len(nrow(file_list))) {
  file_path <- file.path(nc_dir, file_list$name[i])
  if (!file.exists(file_path)) {
    cat("⬇ Downloading:", file_list$name[i], "\n")
    drive_download(as_id(file_list$id[i]), path = file_path, overwrite = TRUE)
  }
}

nc_files <- list.files(nc_dir, pattern = "\\.nc$", full.names = TRUE)

# Parallel Processing Setup
plan(multisession, workers = max(1, parallel::detectCores() - 1))

# Process Countries
for (cc in countries_to_process) {
  cat("\n Processing country:", cc, "\n")
  
  country_fua <- fua[fua$Cntry_ISO == cc, ]
  if (nrow(country_fua) == 0) next
  
  save_path <- file.path(output_dir, paste0("precip_", cc, ".csv"))
  
  if (file.exists(save_path)) {
    cat("⏩ Skipping", cc, "(already processed)\n")
    next
  }
  
  pb <- txtProgressBar(min = 0, max = length(nc_files), style = 3)
  results <- future_lapply(seq_along(nc_files), function(i) {
    nc_file <- nc_files[i]
    setTxtProgressBar(pb, i)
    
    r <- rast(nc_file)
    if (crs(r) != "EPSG:4326") r <- project(r, "EPSG:4326")
    r_crop <- crop(r, country_fua)
    
    precip_dt <- extract(r_crop, country_fua, fun = mean, ID = TRUE, na.rm = TRUE)
    setDT(precip_dt)
    precip_dt <- melt(precip_dt, id.vars = "ID", value.name = "precipitation")
    
    fname <- basename(nc_file)
    ddd <- as.integer(substr(fname, 5, 7))
    hh  <- as.integer(substr(fname, 9, 10))
    tstamp <- as.POSIXct("2015-01-01", tz = "UTC") + (ddd - 1) * 86400 + hh * 3600
    
    precip_dt[, `:=`(time = tstamp, variable = NULL)]
    precip_dt <- merge(precip_dt, as.data.table(country_fua), by = "ID")
    precip_dt[, file := fname]
    
    return(precip_dt)
  })
  close(pb)
  
  final_result <- rbindlist(results)
  fwrite(final_result, save_path)
  cat("Completed:", cc, "\n")
}

cat("\n All countries successfully processed \n")


###########################################################
# CSO FUA Clipping for NetCDF data                        #
# Purpose:                                                #
# 1. Clip FUAs to match NetCDF files                      #
# 2. Extract precipitation values using exact extraction  # 
# 3. Export results per country                           # 
###########################################################

library(terra)
library(sf)
library(data.table)
library(future.apply)
library(exactextractr)

fua_path <- "C:/Geopackage.gpkg"
nc_dir <- "C:/nc_files"
output_dir <- "C:/Output_Path"
dir.create(output_dir, showWarnings = FALSE)

countries_to_process <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN",
  "FRA","DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX",
  "MLT","NLD","POL","PRT","ROU","SVK","SVN","ESP","SWE","GBR"
)

# Load and filter FUAs
fua <- st_read(fua_path)
fua <- st_transform(fua, 3035)
fua <- fua[fua$Cntry_ISO %in% countries_to_process, ]

# Optional buffer for performance (e.g. 1 km)
fua <- st_buffer(fua, 1000)

fua$FUA_ID <- seq_len(nrow(fua))
fua_attributes <- as.data.table(st_drop_geometry(fua))

nc_files <- list.files(nc_dir, pattern = "\\.nc$", full.names = TRUE)

plan(multisession, workers = parallel::detectCores() - 1)

future_lapply(nc_files, function(nc_file) {
  r <- rast(nc_file)
  if (crs(r) != "EPSG:3035") {
    r <- project(r, "EPSG:3035")
  }
  
  values <- exact_extract(r, fua, "mean")
  
  result <- data.table(
    FUA_ID = fua$FUA_ID,
    precip_mm_3h = values,
    time = time(r),
    file = basename(nc_file)
  )
  
  result <- merge(result, fua_attributes, by = "FUA_ID", all.x = TRUE)
  
  for (cc in unique(result$Cntry_ISO)) {
    country_data <- result[Cntry_ISO == cc]
    out_file <- file.path(output_dir, paste0("precip_", cc, ".csv"))
    fwrite(country_data, out_file, append = file.exists(out_file))
  }
  
  NULL
}, future.seed = TRUE)

cat("FUA clipping and export completed\n")


## This script downloads and crop Precipitation raster data from GloH2O MSWEP
## Based on an initial draft by Nina Kleemeyer, written by Steffen Kittlaus

# Libraries
library(googledrive)
library(terra)
library(data.table)
library(ncdf4)

# Define area of interest to which (adding a 10 km tolerance) all raster are cropped and stored
area_of_interest <- vect("Q:/Projekte/PROMISCES/Modeling/MoRE catchments/catchment_units.shp")

# Give Time period of interest:
date_begin <- "2012-01-01"
date_end <- "2012-01-03"

# Authenticate for googledrive and allow gargle to access googledrive.
drive_auth()

# For very small data amounts you can use 
drive_user()


# Decide for GloH20 MSWEP product. From the GloH2O FAQ:
# The ‘Past’ and ‘Past_nogauge’ variants represent the historical satellite-reanalysis merge including and excluding gauge corrections, respectively.
# The ‘NRT’ variant represents the near real-time extension of the historical record to the present (with a latency of ~3 hours). 
# We recommend using the ‘Past_nogauge’ variant in precipitation product performance evaluations using gauge observations as reference, and the ‘Past’ variant for any other purpose. 
product <- "Past"

# Identify needed files on Googledrive
identify_needed_nc_files <- function(begin_date, end_date, product){
  # Convert strings to Date objects (ignoring time part if present)
  begin_date <- as.Date(begin_date)
  end_date <- as.Date(end_date)
  # Calculate start and end times (beginning at 00:00:00, ending at 23:59:59)
  start_time <- as.POSIXct(begin_date, tz = "UTC")
  end_time <- as.POSIXct(end_date, tz = "UTC") + 86400 - 1  # 86400 seconds = 1 day For including the whole last day.
  # Generate 3-hour sequence
  time_series <- seq(start_time, end_time, by = "3 hours")
  years <- unique(year(time_series))
  # Connect to GloH20 and identify needed data
  folder <- fcase(product == "Past", "https://drive.google.com/drive/folders/1DVR90Ud1C444bTOPeENX-3I7tgqPLnao",
                  product == "Past_nogauge", "https://drive.google.com/drive/folders/1MnCbZPTCYV1VrNf5eV1YelTeq0EP4HPH",
                  product == "NRT","https://drive.google.com/drive/folders/1XBonFS0_t3aSM_C4CYobwsN-spmj29vo",
                  default = stop("Product not known, select from 'Past', 'Past_nogauge' or 'NRT'."))
  query <- paste0("(", paste(sprintf("name contains '%s'", years), collapse = " or "),")")
  file_list <- drive_ls(path = folder, q = query)
  cat(" ", nrow(file_list), " files found for year(s) ", paste(years, collapse = " and ") ,".\n", sep = "")
  # identify needed files
  needed_files <- format(time_series, format = "%Y%j.%H.nc")
  files_needed <- subset(file_list, name %in% needed_files)
  cat(" ", nrow(files_needed), " files found of needed ", length(needed_files) ,".\n", sep = "")
  return(files_needed)
}

file_list <- identify_needed_nc_files(begin_date = date_begin, end_date = date_end, product = product)

if(!dir.exists("output/nc_raw")){dir.create("output/nc_raw", recursive = TRUE)}
if(!dir.exists("output/nc_cropped")){dir.create("output/nc_cropped", recursive = TRUE)}

# prepare area of interest
aoi_dissolved <- aggregate(area_of_interest)
aoi_buffered <- buffer(aoi_dissolved, 10000) # Fix potential geometry issues and create buffer 10 km
aoi_buffered_epsg4326 <- project(aoi_buffered, "EPSG:4326")


load_and_crop_nc_files <- function(file_row, area_of_interest){
  file_name <- file_row$name
  file_id <- file_row$id
  file_path <- file.path("output","nc_raw",file_name)
  file_path_out <- file.path("output","nc_cropped",file_name)
  cat("⬇ Downloading:", file_name, "\n")
  #drive_deauth()
  drive_download(as_id(file_id), path = file_path, overwrite = TRUE)
  r <- rast(file_path)
  if (crs(r) != "EPSG:4326") {r <- project(r, "EPSG:4326")}
  r_croped <- crop(r, aoi_buffered_epsg4326)
  time_step <- as.POSIXlt(file_name, format = "%Y%j.%H.nc", tz = "GMT")
  time(r_croped) <- time_step
  writeCDF(r_croped, file_path_out, overwrite =TRUE)
  file.remove(file_path)
  cat(" File ", file_name, " cropped. \n")
}

setDT(file_list)  # convert dribble to data.table 
# Be carefull, you can only download a file several times from googledrive, afterwards its blocked for ~24 h
file_list[, load_and_crop_nc_files(.SD, area_of_interest = extent_aoi_epsg4326), by = seq_len(nrow(file_list))]
drive_deauth()
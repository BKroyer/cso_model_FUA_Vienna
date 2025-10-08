## This script downloads and crop Precipitation raster data from GloH2O MSWEP
## Based on an initial draft by Nina Kleemeyer, written by Steffen Kittlaus
library(ProjectTemplate)
load.project()

# Define area of interest to which (adding a 10 km tolerance) all raster are cropped and stored
area_of_interest <- vect("Q:/Projekte/PROMISCES/Modeling/MoRE catchments/catchment_units.shp")

# Give Time period of interest:
date_begin <- "2010-12-15"
date_end <- "2020-12-31"

# Authenticate for googledrive and allow gargle to access googledrive.
googledrive::drive_auth()

# For very small data amounts you can use
googledrive::drive_user()


# prepare area of interest
aoi_dissolved <- aggregate(area_of_interest)
aoi_buffered <- buffer(aoi_dissolved, 10000) # Fix potential geometry issues and create buffer 10 km
aoi_buffered_epsg4326 <- project(aoi_buffered, "EPSG:4326")
writeVector(aoi_buffered_epsg4326, "data/intermediate_results/aoi_buffered_epsg4326.gpkg", overwrite = TRUE)


# Decide for GloH20 MSWEP product. From the GloH2O FAQ:
# The ‘Past’ and ‘Past_nogauge’ variants represent the historical satellite-reanalysis merge including and excluding gauge corrections, respectively.
# The ‘NRT’ variant represents the near real-time extension of the historical record to the present (with a latency of ~3 hours).
# We recommend using the ‘Past_nogauge’ variant in precipitation product performance evaluations using gauge observations as reference, and the ‘Past’ variant for any other purpose.
product <- "Past"

# Identify needed files on Googledrive

file_list <- identify_needed_nc_files(begin_date = date_begin,
                                      end_date = date_end,
                                      product = product)

setDT(file_list)  # convert dribble to data.table
# Be carefull, you can only download a file several times from googledrive, afterwards its blocked for ~24 h
file_list[, load_and_crop_nc_files(.SD, area_of_interest = extent_aoi_epsg4326), by = seq_len(nrow(file_list))]
#drive_deauth()

# File 2011273.03.nc is damaged and cannot be processed. Replace with previous timestamp:
r <- rast(file.path("data","intermediate_results","nc_cropped","2011273.00.nc"))
time_step <- as.POSIXlt("2011273.03.nc", format = "%Y%j.%H.nc", tz = "GMT")
time(r) <- time_step
names(r) <- "2011273.03.nc"
sources(r)
writeCDF(r,file.path("data","intermediate_results","nc_cropped","2011273.03.nc"), overwrite =TRUE, varname = "2011273.03.nc")
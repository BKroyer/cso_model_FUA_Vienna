## This script downloads and crop Precipitation raster data from GloH2O MSWEP
## Based on an initial draft by Nina Kleemeyer, written by Steffen Kittlaus
library(ProjectTemplate)
load.project()

# area of interest from setup

## Precipitation data

# Authenticate for googledrive and allow gargle to access googledrive.
googledrive::drive_auth()

# For very small data amounts you can use
googledrive::drive_user()


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
file_list[, load_and_crop_nc_files(.SD, area_of_interest = aoi_buffered_epsg4326), by = seq_len(nrow(file_list))]
#drive_deauth()

# File 2011273.03.nc is damaged and cannot be processed. Replace with previous timestamp:
r <- rast(file.path("data","intermediate_results","nc_cropped","2011273.00.nc"))
time_step <- as.POSIXlt("2011273.03.nc", format = "%Y%j.%H.nc", tz = "GMT")
time(r) <- time_step
names(r) <- "2011273.03.nc"
sources(r)
writeCDF(r,file.path("data","intermediate_results","nc_cropped","2011273.03.nc"), overwrite = TRUE, varname = "2011273.03.nc")
# File that defines data, constants or variables for the analysis and will be run automatically
# by Bettina Kroyer

already_processed <- TRUE
datum <- format(Sys.Date(), format="%b%d") # for naming reasons
ln_A_B <- FALSE # calculating with typo in ln network flow scenario b?
round_to <- 4 # number of digits to round results to (only applied to final results)


# Data paths ---------------------------------------------------------------------------------------------------------------------

path_intermediate_res <- file.path("data", "intermediate_results") # for the results and processed input data
path_raw_nc <- file.path(path_intermediate_res, "nc_raw") # for the precipitation data extraction for settlements
path_cropped_nc <- file.path(path_intermediate_res, "nc_cropped") # for the precipitation data extraction for settlements


# Time period of interest (Used in precipitation data extraction & mask when applying model) -------------------------------------

date_begin <- "2010-01-01" # 2010-01-01
date_end <- "2016-12-31" # 2020-12-31

# used for precipitation extraction originally:
# date_begin <- "2010-12-15"
# date_end <- "2020-12-31"

# used in paper
#date_begin <- "2001-01-01"
#date_end <- "2016-12-31"

# Area of interest ---------------------------------------------------------------------------------------------------------------

area_of_interest <- file.path(path_intermediate_res, "FUA_vienna", "FUA_vienna.shp") # "Q:/GIS-Daten/Oesterreich/Verwaltungsgrenzen/Bundeslaender.shp" # "Q:/Projekte/PROMISCES/Modeling/MoRE catchments/catchment_units.shp"
area_of_interest_name <- "FUA_Vienna_paper_dafault_2010_2016" #Austria_default_params
settlements <- "Q:/GIS-Daten/Europe/Klaeranlagen/Agglomerations/Small_agglomerations/11270_2022_5880_MOESM1_ESM/agglo.shp" # else FUA

# Gridcode specification (NULL to process all within AoI, else vector of gridcodes to process) -----------------------------------

gridcode_to_process <- c(253253) #c(253253, 261635) # 253253 is wien, the others are random to test multiple processing #, 259334, 266367, 265844

# model params for the gridcodes, either one for all or one per gridcode (vector of length of gridcodes)

k0 <- 0.3
W0 <- 1.5
dn <- 7 #29
dt <- 4#2
W1 <- 5
W2 <- 1.5
dwf_per_capita <- 0.2

# save gridcodes one by one or summarise over all?
save_single_files = FALSE # if TRUE, one Excel file is created per gridcode


# Data processing / calling -------------------------------------------------------------------------------------------------------

if (already_processed){
    urb_4326 <- vect("data/intermediate_results/settlements_cropped_epsg4326.gpkg")
    urb_3035 <- vect("data/intermediate_results/settlements_cropped_epsg3035.gpkg")
    aoi_buffered_epsg4326 <- vect("data/intermediate_results/aoi_buffered_epsg4326.gpkg")
    aoi_epsg3035 <- vect("data/intermediate_results/aoi_epsg3035.gpkg")


}else{

    wrapper_preprocess_data(area_of_interest, settlements)
    ### Area of interest: thesis: Upper Danube Basin (catchment_units); paper: Europe FUA (671) (lavalle)

}



# AoI  ---------------------------------------------------------------------------------------------------------------------------
aoi <- vect(area_of_interest)
if (crs(aoi, describe=T)$code != "3035"){
    aoi_3035 <- project(aoi, crs(urb_3035)) # using urb_3035 from setup as reference as has gridcodes and spatially defined
} else {
    aoi_3035 <- aoi # alreaady in the correct crs
}

urb_3035_cropped_to_aoi <- intersect(urb_3035, aoi_3035)
gridcode_in_aoi <- unique(urb_3035_cropped_to_aoi$gridcode)

if (!is.null(gridcode_to_process) & !(all(gridcode_to_process %in% gridcode_in_aoi))){
    gridcode_outside <- gridcode_to_process[!(gridcode_to_process %in% gridcode_in_aoi)]
    errorCondition(cat("Error: Specified gridcode(s) to process not within AoI: ", paste(gridcode_outside, collapse = "; ")))
}

if (is.null(gridcode_to_process)){
    gridcode_to_process <- gridcode_in_aoi
}



# collect parameters for the actual gridcodes to process -------------------------------------------------------------------------

params <- data.table(gridcode = gridcode_to_process, k0 = k0, W0 = W0, dn = dn, dt = dt, W1 = W1, W2 = W2, dwf_per_capita = dwf_per_capita, key = "gridcode")

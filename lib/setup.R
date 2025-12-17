# File that defines data, constants or variables for the analysis and will be run automatically
# by Bettina Kroyer

already_processed <- TRUE
datum <- format(Sys.Date(), format="%b%d") # for naming reasons
ln_A_B <- FALSE # calculating with typo in ln network flow scenario b?



# Time period of interest (Used in precipitation data extraction & mask when applying model) -------------------------------------

date_begin <- "2001-01-01"
date_end <- "2016-12-31"

# used for precipitation extraction originally:
# date_begin <- "2010-12-15"
# date_end <- "2020-12-31"

# used in paper
#date_begin <- "2001-01-01"
#date_end <- "2016-12-31"

# Gridcode specification (NULL to process all within AoI, else vector of gridcodes to process) -----------------------------------

gridcode_to_process <- c(253253) # 253253 is wien



# Data processing / calling -------------------------------------------------------------------------------------------------------

if (already_processed){
    urb_4326 <- vect("data/intermediate_results/settlements_cropped_epsg4326.gpkg")
    urb_3035 <- vect("data/intermediate_results/settlements_cropped_epsg3035.gpkg")
    aoi_buffered_epsg4326 <- vect("data/intermediate_results/aoi_buffered_epsg4326.gpkg")
    aoi_epsg3035 <- vect("data/intermediate_results/aoi_epsg3035.gpkg")


}else{

    ### Area of interest
    # thesis: Upper Danube Basin (catchment_units)
    # paper: Europe FUA (671) (DATA MISSING)
    area_of_interest <- vect("Q:/Projekte/PROMISCES/Modeling/MoRE catchments/catchment_units.shp")
    aoi_dissolved <- aggregate(area_of_interest)
    aoi_epsg4326 <- project(aoi_dissolved, "EPSG:4326")
    aoi_epsg3035 <- project(aoi_dissolved, "EPSG:3035")
    writeVector(aoi_epsg3035, "data/intermediate_results/aoi_epsg3035.gpkg", overwrite = TRUE)

    # buffering for precipitation extraction
    aoi_buffered <- buffer(aoi_dissolved, 10000) # Fix potential geometry issues and create buffer 10 km
    aoi_buffered_epsg4326 <- project(aoi_buffered, "EPSG:4326")
    writeVector(aoi_buffered_epsg4326, "data/intermediate_results/aoi_buffered_epsg4326.gpkg", overwrite = TRUE)

    ### Settlements
    # thesis: agglo.shp from Pistoccio 2022
    settlements <- vect("Q:/GIS-Daten/Europe/Klaeranlagen/Agglomerations/Small_agglomerations/11270_2022_5880_MOESM1_ESM/agglo.shp")
    settlements_id <- "settlements_id"
    settlements_epsg3035 <- project(settlements, "EPSG:3035")
    urb_3035 <- crop(settlements_epsg3035, aoi_epsg3035)
    #all(is.valid(urb_3035)) # check validity of geometries. Should result in TRUE (did results in TRUE Oct31)
    writeVector(urb_3035, file.path(path_intermediate_res, "settlements_cropped_epsg3035.gpkg"), overwrite = TRUE)


    settlements_epsg4326 <- project(settlements, "EPSG:4326")
    urb_4326 <- crop(settlements_epsg4326, aoi_epsg4326)
    writeVector(urb_4326, file.path(path_intermediate_res, "settlements_cropped_epsg4326.gpkg"), overwrite = TRUE)
}






# AoI  ---------------------------------------------------------------------------------------------------------------------------
aoi <- vect("Q:/GIS-Daten/Oesterreich/Verwaltungsgrenzen/Bundeslaender.shp")
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

# Script to wrap data preprocessing (AoI, settlements)
# Written by Bettina Kroyer

# Reads AoI and settlements, projects to EPSG 3035, crops settlements to AoI, writes vectors to files
wrapper_preprocess_data <- function(area_of_interest, settlements){

    area_of_interest <- vect(area_of_interest)
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
    settlements <- vect(settlements)
    settlements_id <- "settlements_id"
    settlements_epsg3035 <- project(settlements, "EPSG:3035")
    urb_3035 <- crop(settlements_epsg3035, aoi_epsg3035)
    #all(is.valid(urb_3035)) # check validity of geometries. Should result in TRUE (did results in TRUE Oct31)
    writeVector(urb_3035, file.path(path_intermediate_res, "settlements_cropped_epsg3035.gpkg"), overwrite = TRUE)


    settlements_epsg4326 <- project(settlements, "EPSG:4326")
    urb_4326 <- crop(settlements_epsg4326, aoi_epsg4326)
    writeVector(urb_4326, file.path(path_intermediate_res, "settlements_cropped_epsg4326.gpkg"), overwrite = TRUE)
}

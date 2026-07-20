# Script to wrap data preprocessing (AoI, settlements)
# Written by Bettina Kroyer

# Reads AoI and settlements, projects to EPSG 3035, crops settlements to AoI, writes vectors to files ---------------------------------------
wrapper_preprocess_data <- function(area_of_interest, settlements){

    area_of_interest_vect <- vect(area_of_interest)
    aoi_dissolved <- aggregate(area_of_interest_vect)
    aoi_epsg4326 <- project(aoi_dissolved, "EPSG:4326")
    aoi_epsg3035 <- project(aoi_dissolved, "EPSG:3035")
    #writeVector(aoi_epsg3035, "data/aoi_epsg3035.gpkg", overwrite = TRUE)

    # buffering for precipitation extraction
    aoi_buffered <- buffer(aoi_dissolved, 10000) # Fix potential geometry issues and create buffer 10 km
    aoi_buffered_epsg4326 <- project(aoi_buffered, "EPSG:4326")
    #writeVector(aoi_buffered_epsg4326, "data/aoi_buffered_epsg4326.gpkg", overwrite = TRUE)

    ### Settlements
    settlements <- vect(settlements)

    settlements_epsg4326 <- project(settlements, "EPSG:4326")
    intersects_idx <- terra::relate(settlements_epsg4326, aoi_epsg4326, relation = "intersects")
    settlements_intersect <- settlements_epsg4326[intersects_idx, ]
    intersect_geom <- terra::intersect(settlements_intersect, aoi_epsg4326)
    intersect_area <- terra::expanse(intersect_geom)  # area of intersection
    original_area <- terra::expanse(settlements_intersect)
    threshold <- 0.9  # at least 90% overlap
    keep_idx <- (intersect_area / original_area) >= threshold
    urb_4326 <- settlements_intersect[keep_idx, ]
    #writeVector(urb_4326, "data/settlements_cropped_epsg4326.gpkg", overwrite = TRUE)

    settlements_epsg3035 <- project(settlements, "EPSG:3035")
    urb_3035 <- project(urb_4326, "EPSG:3035")
    # writeVector(urb_3035, "data/settlements_cropped_epsg3035.gpkg", overwrite = TRUE)

    out <- list(
        aoi_epsg4326 = aoi_epsg4326,
        aoi_epsg3035 = aoi_epsg3035,
        aoi_buffered_epsg4326 = aoi_buffered_epsg4326,
        settlements_epsg3035 = settlements_epsg3035,
        urb_3035 = urb_3035,
        settlements_epsg4326 = settlements_epsg4326,
        urb_4326 = urb_4326
    )

    return(out)
}

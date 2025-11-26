# File that defines data, constants or variables for the analysis and will be run automatically
# by Bettina Kroyer

already_processed <- TRUE


# Time period of interest (Used in precipitation data extraction) -----------------------------------------------------------------
date_begin <- "2010-12-15"
date_end <- "2020-12-31"



# data processing / calling -------------------------------------------------------------------------------------------------------

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




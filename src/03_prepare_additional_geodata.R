## This script extracts population and impervious area for the settlements
## written by Steffen Kittlaus
library(ProjectTemplate)
load.project()

# Define area of interest to which (adding a 10 km tolerance) all raster are cropped and stored
area_of_interest <- vect("Q:/Projekte/PROMISCES/Modeling/MoRE catchments/catchment_units.shp")

# prepare area of interest
aoi_dissolved <- aggregate(area_of_interest)
aoi_epsg3035 <- project(aoi_dissolved, "EPSG:3035")
writeVector(aoi_epsg3035, "data/intermediate_results/aoi_epsg3035.gpkg", overwrite = TRUE)

# Define input data
# Precipitation data need to be already downloaded and cropped to the area of interest in the folder /data/nc_cropped
# Settlements
settlements <- vect("Q:/GIS-Daten/Europe/Klaeranlagen/Agglomerations/Small_agglomerations/11270_2022_5880_MOESM1_ESM/agglo.shp")
settlements_id <- "gridcode"
settlements_epsg3035 <- project(settlements, "EPSG:3035")
urb <- crop(settlements_epsg3035, aoi_epsg3035)
all(is.valid(urb)) # check validity of geometries. Should result in TRUE
writeVector(urb, file.path(path_intermediate_res, "settlements_cropped_epsg3035.gpkg"), overwrite = TRUE)
#urb <- vect("data/intermediate_results/settlements_epsg3035.gpkg")
urb_area <- expanse(urb, unit = "km")

# Impervious area
imp_input <- rast("Q:/GIS-Daten/Europe/Copernicus_HRL_imperviousness/DATA/IMD_2018_010m_eu_03035_V2_0.tif")
urb_proj_imp <- project(urb, imp_input)
imp_urb <- exact_extract(imp_input, sf::st_as_sf(urb_proj_imp), fun = "mean", append_cols = settlements_id)
imp <- data.table(settlement_id = imp_urb$gridcode, mean_imperviousness = imp_urb$mean/100, settlement_area = urb_area)
imp[, imp_area_km2 := settlement_area * mean_imperviousness]
saveRDS(imp[,.(settlement_id, imp_area_km2)], file.path(path_intermediate_res, "impervious_area.rds"))

# population density
popdens_raw <- rast("Q:/GIS-Daten/Europe/Population_density/EUROSTAT_GISCO_popdens_grid_2021/Eurostat_Census-GRID_2021_V2.2/ESTAT_OBS-VALUE-T_2021_V2.tiff")
urb_proj_pop <- project(urb, popdens_raw)
pop_urb <- exact_extract(popdens_raw, sf::st_as_sf(urb_proj_pop), fun = "sum", append_cols = settlements_id)
pop <- data.table(settlement_id = pop_urb$gridcode, population = pop_urb$sum )
saveRDS(pop[,.(settlement_id, population)], file.path(path_intermediate_res, "population.rds"))

# Plausibility check for Austria
# austria <- vect("Q:/GIS-Daten/Oesterreich/Verwaltungsgrenzen/Bundeslaender.shp")
# pop_austria <- exact_extract(popdens_raw, sf::st_as_sf(austria), fun = "sum", append_cols = "BL")
# sum(pop_austria$sum)
# Result: 8 966 859

## This script extracts population and impervious area for the settlements
## written by Steffen Kittlaus
library(ProjectTemplate)
load.project()

# urb_3035 from setup
urb_area <- expanse(urb_3035, unit = "km")

# Impervious area
imp_input <- rast("Q:/GIS-Daten/Europe/Copernicus_HRL_imperviousness/DATA/IMD_2018_010m_eu_03035_V2_0.tif")
# 2018 used; paper used 2015; 2015 data on D:/
urb_proj_imp <- project(urb_3035, imp_input)
imp_urb <- exact_extract(imp_input, sf::st_as_sf(urb_proj_imp), fun = "mean", append_cols = settlements_id)
imp <- data.table(settlement_id = imp_urb$gridcode, mean_imperviousness = imp_urb$mean/100, settlement_area = urb_area)
imp[, imp_area_km2 := settlement_area * mean_imperviousness]
saveRDS(imp[,.(settlement_id, imp_area_km2)], file.path(path_intermediate_res, "impervious_area.rds"))

# population density
popdens_raw <- rast("Q:/GIS-Daten/Europe/Population_density/EUROSTAT_GISCO_popdens_grid_2021/Eurostat_Census-GRID_2021_V2.2/ESTAT_OBS-VALUE-T_2021_V2.tiff") # T means total population; resolution 1km²x1km²
urb_proj_pop <- project(urb_3035, popdens_raw)
pop_urb <- exact_extract(popdens_raw, sf::st_as_sf(urb_proj_pop), fun = "sum", append_cols = settlements_id)
pop <- data.table(settlement_id = pop_urb$gridcode, population = pop_urb$sum )
saveRDS(pop[,.(settlement_id, population)], file.path(path_intermediate_res, "population.rds"))

# Plausibility check for Austria
# austria <- vect("Q:/GIS-Daten/Oesterreich/Verwaltungsgrenzen/Bundeslaender.shp")
# pop_austria <- exact_extract(popdens_raw, sf::st_as_sf(austria), fun = "sum", append_cols = "BL")
# sum(pop_austria$sum)
# # Result: 8 966 859

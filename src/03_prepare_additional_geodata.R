## This script extracts population, CS share and impervious area for the settlements
## written by Steffen Kittlaus
## updated to include CS share & restructured by Bettina Kroyer

library(ProjectTemplate)
load.project()


# urb_3035 from setup
plot(urb_3035)
urb_area <- expanse(urb_3035, unit = "km")




# Impervious area --------------------------------------------------------------

imp_input <- rast("C:/Users/bkroyer/cso-modell-upper-danube/data/imp2021_AoI_incl_AUT.tif")
urb_proj_imp <- project(urb_3035, imp_input)
imp_urb <- exact_extract(imp_input, sf::st_as_sf(urb_proj_imp), fun = "mean", append_cols = gridcode_nam)
imp <- data.table(settlement_id = imp_urb[, gridcode_nam], mean_imperviousness = imp_urb$mean/100, settlement_area = urb_area)
setnames(imp, "settlement_id", gridcode_nam)
imp[, imp_area_km2 := settlement_area * mean_imperviousness]
saveRDS(imp[, .SD, .SDcols = c(gridcode_nam, "imp_area_km2")], file.path(path_intermediate_res, paste0("impervious_area_2021",path_nam_preprocess,".rds")))




# population -------------------------------------------------------------------

popdens_raw <- rast("C:/Users/bkroyer/cso-modell-upper-danube/data/ESTAT_OBS-VALUE-T_2021_V2.tiff") # T means total population; resolution 1km²x1km²
urb_proj_pop <- project(urb_3035, popdens_raw)
pop_urb <- exact_extract(popdens_raw, sf::st_as_sf(urb_proj_pop), fun = "sum", append_cols = gridcode_nam)
pop <- data.table(
    pop_urb[, gridcode_nam],
    pop_urb$sum
)
setnames(pop, c(gridcode_nam, "population"))

saveRDS(pop[, .SD, .SDcols = c(gridcode_nam, "population")], file.path(path_intermediate_res, paste0("population_2021",path_nam_preprocess,".rds")))




# not used in the working example
# get mean of 2011 and 2018 dataset for a 2015 equivalent
# both 2011 and 2018 dataset intersected with smaller settlements in QGIS
pop_2011 <- vect(file.path(path_intermediate_res, "pop_2011_on_smaller_settlements", "pop_2011_AUT_w_gridcode.shp"))
pop_2011$weight <- expanse(pop_2011, unit = "km") # original resolution: 1 km²
pop_2011_dt <- values(pop_2011)
setDT(pop_2011_dt, key = "gridcode")
pop_2011_dt[, T_POP := as.numeric(GEOSTAT_gr)]
pop_2011_dt[, POP_2011 := sum(T_POP * weight), by = gridcode]
pop_2011_dt <- unique(pop_2011_dt, by = "gridcode")[
    , c("T_POP", "weight", "GRD_ID", "TOT_P") := NULL #, "GEOSTAT_gr"
]

pop_2018 <- vect(file.path(path_intermediate_res, "pop_2018_on_smaller_settlements", "pop_2018_AUT_w_gridcode.shp"))
pop_2018$weight <- expanse(pop_2018, unit = "km") # original resolution: 1 km²
pop_2018_dt <- values(pop_2018)
setDT(pop_2018_dt, key = "gridcode")
pop_2018_dt[, POP_2018 := sum(TOT_P_2018 * weight), by = gridcode]
pop_2018_dt <- unique(pop_2018_dt, by = "gridcode")[
    , c("OBJECTID", "weight", "GRD_ID", "TOT_P_2018", "Method", "pop_2011_T", "Country", "Date", "CNTR_ID", "Shape_Area", "Shape_Leng") := NULL
]

pop_2015 <- merge(pop_2011_dt, pop_2018_dt, by = "gridcode")
pop_2015$TOT_P_2015 <- rowMeans(pop_2015[, c("POP_2018", "POP_2011")])
pop_2015 <- data.table(settlement_id = pop_2015$gridcode, population = pop_2015$TOT_P_2015)

saveRDS(pop_2015[,.(settlement_id, population)], file.path(path_intermediate_res, paste0("population_2015",path_nam_preprocess,".rds"))) # not for validatation region, requires pre-computing of 2011 and 2018 data in QGIS again




# share of CS ------------------------------------------------------------------

share_CS_input <- vect(file.path("C:/Users/bkroyer/cso-modell-upper-danube/data", "combined.gpkg"))
share_CS_input$share_num[share_CS_input$CNTR_CODE. == "CZ"] <- 0.7 # updating CZ data to 70\%, see Cools et al (2016)
urb_proj_share_CS <- project(urb_3035, share_CS_input)

cs_int <- terra::intersect(
    share_CS_input[, "share_num"],
    urb_proj_share_CS
)
cs_int$int_area <- terra::expanse(cs_int, unit = "m")


cs_dt <- as.data.table(cs_int)[,
                               .(share_served_by_CS = sum(share_num * int_area) / sum(int_area)),

                               by = gridcode_nam
]

share_CS <- cs_dt

# setting those with NA to 0.5 for now
share_CS[is.na(share_served_by_CS), share_served_by_CS := 0.5]

# making sure its limited to 1
share_CS[share_served_by_CS >1, share_served_by_CS := share_served_by_CS/100]



saveRDS(share_CS, file.path(path_intermediate_res, paste0("share_CS",path_nam_preprocess,".rds")))



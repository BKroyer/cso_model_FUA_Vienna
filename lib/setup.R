# File that defines data, constants or variables for the analysis and will be run automatically
# by Bettina Kroyer

already_processed <- TRUE
datum <- format(Sys.Date(), format="%b%d") # for naming reasons
ln_A_B <- FALSE # calculating with typo in ln network flow scenario b?
round_to <- 4 # number of digits to round results to (only applied to final results)
validation_region <- FALSE
validation_code <- "J"
use_default_params <- TRUE # using the default model parameters, and dn 30 for Austria and Germany


# Data paths ---------------------------------------------------------------------------------------------------------------------

path_intermediate_res <- file.path("data_NOTREAD", "intermediate_results") # for the results and processed input data
path_input <- file.path("data")
path_raw_nc <- file.path(path_intermediate_res, "nc_raw") # for the precipitation data extraction for settlements
path_cropped_nc <- file.path(path_intermediate_res, "nc_cropped") # for the precipitation data extraction for settlements


# Time period of interest (Used in precipitation data extraction & mask when applying model) -------------------------------------

date_begin <- "2021-01-01" # 2010-01-01
date_end <- "2024-12-31" # 2020-12-31


time_period_of_interest <- "2021_2024" # either "2010_2016" (for data from 2015 in imp and pop) or "2021_2024" (for data from 2021 used in imp and pop)

# used for precipitation extraction originally:
# date_begin <- "2010-12-15"
# date_end <- "2020-12-31"

# used in paper
#date_begin <- "2001-01-01"
#date_end <- "2016-12-31"

# get years between begin and end date
yr_begin <- year(date_begin)
yr_end <- year(date_end)
current_years_num <- seq(yr_begin, yr_end)
current_years <- as.character(current_years_num)
nam <- paste(current_years, collapse = "_")

# Area of interest ---------------------------------------------------------------------------------------------------------------

area_of_interest <- file.path(path_intermediate_res, "FUA_vienna", "FUA_vienna.shp")

#file.path(path_intermediate_res, "Einzugsgebiet_Traisen", "traisen_reduced.shp")
#file.path(path_intermediate_res, "Einzugsgebiet_Bad_Leonfelden", "Einzugsgebiet_Bad_Leonfelden.shp")
#file.path(path_intermediate_res, "Einzugsgebiet_Traisen", "Einzugsgebiet_AnDerTraisen_larger4.shp")
#file.path(path_intermediate_res, "Einzugsgebiet_Eisenstadt", "Einzugsgebiet_Eisenstadt.shp")
# file.path(path_intermediate_res, "FUA_vienna", "FUA_vienna.shp")
# "Q:/GIS-Daten/Oesterreich/Verwaltungsgrenzen/Bundeslaender.shp"
# "Q:/Projekte/PROMISCES/Modeling/MoRE catchments/catchment_units.shp"
#"C:\\Users\\simulation\\bkroyer\\git_clone\\cso-modell-upper-danube\\data\\Validation_Einzugsgebiete\\Validation_Einzugsgebiete.shp"


# finds factor to scale results for validation regions crossing border of Upper Danube Basin
if (validation_region){
    aoi_temp <- project(vect(area_of_interest), "EPSG:3035")
    aoi_temp <- aoi_temp[aoi_temp$gridcode == validation_code]
    full_area_aoi <- sum(expanse(aoi_temp, unit = "km"))
    upperdanube <- vect("data/aoi_epsg3035.gpkg")
    intersected_area_aoi <- sum(expanse(terra::crop(aoi_temp, upperdanube), unit = "km"))

    factor_enlarge_validation <- full_area_aoi / intersected_area_aoi

    if (is.na(factor_enlarge_validation)){
        factor_enlarge_validation <- 1
    }

    print(paste0("Factor to scale results for validation region: ", factor_enlarge_validation))

    rm(upperdanube, aoi_temp)

    val_data <- setDT(read.xlsx(file.path(path_intermediate_res, "Validation_data.xlsx")))
    val_value <- val_data[Code == validation_code, Value]
    manual_CS <- FALSE
    #manual_CS <- val_data[Code == validation_code, CS]
    manual_pop <- val_data[Code == validation_code, Pop_2015]


}else{
    factor_enlarge_validation <- 1
}


#area_of_interest_name <- paste0(validation_code,"_default_params_", nam) #Austria_default_params
area_of_interest_name <- paste0("Vienna_try_2021_2024", nam) #Austria_default_params
settlements <- "Q:/GIS-Daten/Europe/Klaeranlagen/Agglomerations/Small_agglomerations/11270_2022_5880_MOESM1_ESM/agglo.shp"

#"Q:/GIS-Daten/Europe/Klaeranlagen/Agglomerations/Small_agglomerations/11270_2022_5880_MOESM1_ESM/agglo.shp" # else FUA or the validation regions; used in the wrapper_preprocess_data only! so check if processing needed
#"C:\\Users\\simulation\\bkroyer\\git_clone\\cso-modell-upper-danube\\data\\Validation_Einzugsgebiete\\Validation_Einzugsgebiete.shp"#

# Gridcode specification (NULL to process all within AoI, else vector of gridcodes to process) -----------------------------------

gridcode_to_process <- NULL#c(253253, 261635) #NULL#c(253253) #c(253253, 261635) # 253253 is wien, the others are random to test multiple processing #, 259334, 266367, 265844

# model params for the gridcodes, either one for all or one per gridcode (vector of length of gridcodes)

# default:              0.3     1.5     7       4       5       2
# Vienna (paper appendix):       0.3     1.5     29      2       5       1.5

if (use_default_params){
    k0 <- 0.3
    W0 <- 1.5
    dn <- 7
    dt <- 4
    W1 <- 5
    W2 <- 2

} else {

    # option to manually change the parameters
    k0 <- 0.3
    W0 <- 0.5
    dn <- 6 #7 #29
    dt <- 3 #4#2
    W1 <- 1.2
    W2 <- 0.5
}

dwf_per_capita <- 0.2

# save gridcodes one by one or summarise over all?
save_single_files = FALSE # if TRUE, one Excel file is created per gridcode

if (!validation_region){ # for validation regions: Cs share and population from data shared and Emreg
    manual_pop <- FALSE #FALSE#19640#88361 # specify number of connected inhabitants or set to FALSE to use Eurostat population data
    manual_CS <- FALSE #0.86 # specify the CS share or set to FALSE to extract from CS share data
}


# not implemented yet
sum_input <- FALSE # decide if all input should be aggregated prior to the model as one big gridcell


# Data processing / calling -------------------------------------------------------------------------------------------------------

if (already_processed){
    urb_4326 <- vect("data/settlements_cropped_epsg4326.gpkg")
    urb_3035 <- vect("data/settlements_cropped_epsg3035.gpkg")

    if (validation_region){
        val_einzug <- vect("C:\\Users\\simulation\\bkroyer\\git_clone\\cso-modell-upper-danube\\data\\Validation_Einzugsgebiete\\Validation_Einzugsgebiete.shp")
        val_3035 <- project(val_einzug, urb_3035)
        urb_3035 <- val_3035
    } else {
        urb_3035 <- vect("data/settlements_cropped_epsg3035.gpkg")
    }

    aoi_buffered_epsg4326 <- vect("data/aoi_buffered_epsg4326.gpkg")

    aoi_epsg3035 <- vect("data/aoi_epsg3035.gpkg") # 186058 km²


}else{

    wrapper_preprocess_data(area_of_interest, settlements)
    ### Area of interest: thesis: Upper Danube Basin (catchment_units); paper: Europe FUA (671) (lavalle)

}



# AoI  ---------------------------------------------------------------------------------------------------------------------------
if (!validation_region){
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
} else {
    gridcode_to_process <- validation_code
}



# collect parameters for the actual gridcodes to process -------------------------------------------------------------------------

if (use_default_params){

    params <- data.table(gridcode = gridcode_to_process, k0 = k0, W0 = W0, dn = dn, dt = dt, W1 = W1, W2 = W2, dwf_per_capita = dwf_per_capita, key = "gridcode")

    if (!already_processed){

        austria_border <- project(vect("Q:/GIS-Daten/Oesterreich/Verwaltungsgrenzen/Bundeslaender.shp"), urb_3035)
        germany_border <- project(vect("Q:\\GIS-Daten\\Other Countries\\Germany\\germany_border\\germany_border.shp"), urb_3035)

        aut_ger <- terra::aggregate(rbind(austria_border, germany_border))
        gridcodes_in_aut_germ <- unique(intersect(urb_3035, aut_ger)$gridcode)

        gridcodes_in_aut_germ <- c(gridcodes_in_aut_germ, LETTERS[1:10]) # adding the validation region gridcodes

        saveRDS(gridcodes_in_aut_germ, file.path(path_input, "gridcodes_in_aut_germ.rds"))

    } else{

        gridcodes_in_aut_germ <- readRDS(file.path(path_input, "gridcodes_in_aut_germ.rds"))
    }

    params[gridcode %in% gridcodes_in_aut_germ, dn := 30]


} else {
    params <- data.table(gridcode = gridcode_to_process, k0 = k0, W0 = W0, dn = dn, dt = dt, W1 = W1, W2 = W2, dwf_per_capita = dwf_per_capita, key = "gridcode")
}




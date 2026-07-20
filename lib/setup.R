# File that defines data, constants or variables for the analysis and will be run automatically
# by Bettina Kroyer

already_processed <- FALSE # are the aoi and settlements cropped and in the right EPSG? if so, files settlements_cropped_epsg3035.gpkg etc. must exist, see further down
datum <- format(Sys.Date(), format="%b%d") # for naming reasons
ln_A_B <- FALSE # calculating with typo in ln network flow scenario b?
round_to <- 4 # number of digits to round results to (only applied to final results)
validation_region <- FALSE # is it a validation region? If so, validation data is loaded
validation_code <- "G"
use_default_params <- FALSE # using the default model parameters, and dn 30 for Austria and Germany

sensitivity_analysis <- FALSE # if TRUE, adjust settings at bottom of this script


# Data paths ---------------------------------------------------------------------------------------------------------------------

path_intermediate_res <- file.path("data_NOTREAD", "intermediate_results") # for the results and processed input data
path_input <- file.path("data")
path_raw_nc <- file.path(path_intermediate_res, "nc_raw") # for the precipitation data extraction for settlements
path_cropped_nc <- file.path(path_input, "nc_cropped_AUT") # for the precipitation data extraction for settlements
path_design_population <- NA # either the path to the data with gridcodes or NA
# if design population is given, adjust dn and dt to match design population


# Time period of interest (Used in precipitation data extraction & mask when applying model) -------------------------------------

date_begin <- "2021-01-01"
date_end <- "2024-12-31"


time_period_of_interest <- "2021_2024" # either "2010_2016" (for data from 2015 in imp and pop) or "2021_2024" (for data from 2021 used in imp and pop)

# get years between begin and end date
yr_begin <- year(date_begin)
yr_end <- year(date_end)
current_years_num <- seq(yr_begin, yr_end)
current_years <- as.character(current_years_num)
nam <- paste(current_years, collapse = "_")

# Area of interest ---------------------------------------------------------------------------------------------------------------

area_of_interest <- "data/AUT_border.shp"
area_of_interest_name <- paste0("AUT_smaller_settlements_", nam)


# relevant for the pre-processing: precipitation, imperviousness, population and CS share data will be extracted for the specified settlements
settlements <- "data/agglo_fixed_geometries.shp"
gridcode_nam <- "gridcode" # the name column of the spatial units, will be used for naming in the preprocessing as well


# finds factor to scale results for validation regions crossing border of AoI
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

    val_data <- setDT(read.xlsx(file.path(path_intermediate_res, "Validation_data.xlsx"), sheet = 1))
    val_value <- val_data[Code == validation_code, Value]
    manual_CS <- val_data[Code == validation_code, CS]

    if (time_period_of_interest == "2021_2024"){
        manual_pop <- val_data[Code == validation_code, Pop2018_2023] / factor_enlarge_validation
    } else {
        manual_pop <- val_data[Code == validation_code, Pop_2015] / factor_enlarge_validation
    }

}else{
    factor_enlarge_validation <- 1
}


# Processed data names -----------------------------------------------------------------------------------------------------------
filenam_prec_data_base <- paste0("precipitation_ts_AUT_WWTP") # the year is added in file 04
filenam_imp_data <- ifelse(time_period_of_interest == "2010_2016", "impervious_area_2015_AUT_FUA.rds", "impervious_area_2021_AUT_WWTP.rds")
filenam_cs_data <- "share_CS_wwtp.rds"
filenam_pop_data <- ifelse(time_period_of_interest == "2010_2016", "population_2015_AUT.rds", "population_2021_AUT_WWTP.rds")


# Gridcode specification (NULL to process all within AoI, else vector of gridcodes to process) -----------------------------------

gridcode_to_process <- NULL

# model params for the gridcodes, either one for all or one per gridcode (vector of length of gridcodes)
# default:              0.3     1.5     7       4       5       2

if (use_default_params){

    k0 <- 0.3
    W0 <- 1.5
    dn <- 7
    dt <- 4
    W1 <- 5
    W2 <- 2

} else { # option to manually change the parameters

    k0 <- 0.3
    W0 <- 1.5
    dn <- 30
    dt <- 4
    W1 <- 5
    W2 <- 2
}

dwf_per_capita <- 0.125


if (!validation_region){ # for validation regions: Cs share and population from data shared and Emreg
    manual_pop <- FALSE # specify number of connected inhabitants or set to FALSE to use Eurostat population data
    manual_CS <- FALSE # specify the CS share or set to FALSE to extract from CS share data
}


# Data processing / calling -------------------------------------------------------------------------------------------------------
if (already_processed){
    urb_4326 <- vect("data/settlements_cropped_epsg4326.gpkg")
    urb_3035 <- vect("data/settlements_cropped_epsg3035.gpkg")

    if (validation_region){
        val_einzug <- vect("C:\\Users\\simulation\\bkroyer\\git_clone\\cso-modell-upper-danube\\data\\Validation_Einzugsgebiete\\Validation_Einzugsgebiete.shp")
        val_3035 <- project(val_einzug, urb_3035)
        urb_3035 <- val_3035
    }

    aoi_buffered_epsg4326 <- vect("data/aoi_buffered_epsg4326.gpkg")
    aoi_epsg3035 <- vect("data/aoi_epsg3035.gpkg")


}else{

    preprocessed <- wrapper_preprocess_data(area_of_interest, settlements)
    list2env(preprocessed, envir = .GlobalEnv)

}



# check AoI & gridcodes  ---------------------------------------------------------------------------------------------------------

if (!validation_region){
    aoi <- vect(area_of_interest)
    if (crs(aoi, describe=T)$code != "3035"){
        aoi_3035 <- project(aoi, crs(urb_3035)) # using urb_3035 from setup as reference as has gridcodes and spatially defined
    } else {
        aoi_3035 <- aoi # already in the correct crs
    }

    urb_3035_cropped_to_aoi <- intersect(urb_3035, aoi_3035)
    gridcode_in_aoi <- unique(unlist(values(urb_3035_cropped_to_aoi[gridcode_nam])))


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

    params <- list(data.table(gridcode = unique_gridcodes, k0 = k0, W0 = W0, dn = dn, dt = dt, W1 = W1, W2 = W2, dwf_per_capita = dwf_per_capita, factor_enlarge_prec = 1, key = "gridcode"))

    # if (!already_processed){ # sets dn to 30 for settlements in AUT/GER, not needed if just AUT used and dn always 30
    #
    #     austria_border <- project(vect("Q:/GIS-Daten/Oesterreich/Verwaltungsgrenzen/Bundeslaender.shp"), urb_3035)
    #     germany_border <- project(vect("Q:\\GIS-Daten\\Other Countries\\Germany\\germany_border\\germany_border.shp"), urb_3035)
    #
    #     aut_ger <- terra::aggregate(rbind(austria_border, germany_border))
    #     gridcodes_in_aut_germ <- unique(intersect(urb_3035, aut_ger)$gridcode)
    #
    #     gridcodes_in_aut_germ <- c(gridcodes_in_aut_germ, LETTERS[1:10]) # adding the validation region gridcodes
    #
    #     saveRDS(gridcodes_in_aut_germ, file.path(path_input, "gridcodes_in_aut_germ.rds"))
    #
    # } else{
    #
    gridcodes_in_aut_germ <- readRDS(file.path(path_input, "gridcodes_in_aut_germ.rds"))
    # }
    #
    params[[1]][gridcode %in% gridcodes_in_aut_germ, dn := 30]


} else {
    if (sensitivity_analysis == TRUE){ # only the default values are used for the sensitivity analysis, but specify manually as dn in AUT/GER difficult

        # specify the varying parameter / data
        # varying_param <- seq(0.1, 1.9, 0.2) # k0
        # varying_param <- seq(0.2, 5, 0.4) # W0
        varying_param <- seq(3, 30, 1) # dn
        # varying_param <- seq(10,50,10) # prec
        # varying_param <- seq(2, 20, 1) # dt
        # varying_param <- seq(0.2, 5, 0.4) # W1
        # varying_param <- seq(0.2, 9.8, 0.8) # W2
        #varying_param <- seq(0.15,0.25,0.01) #dwf

        # list instead of single row
        params <- lapply(varying_param, function(vary_temp) {
            data.table(
                gridcode = gridcode_to_process,
                k0 = k0,
                W0 = W0,
                #dn = dn,
                dn = vary_temp,
                dt = dt,
                W1 = W1,
                W2 = W2,
                dwf_per_capita = dwf_per_capita,
                factor_enlarge_prec = 1,
                key = "gridcode"
            )
        })

        area_of_interest_name <- paste0(area_of_interest_name, "_prec_", varying_param)

    } else {
        params <- list(data.table(gridcode = gridcode_to_process, k0 = k0, W0 = W0, dn = dn, dt = dt, W1 = W1, W2 = W2, dwf_per_capita = dwf_per_capita, factor_enlarge_prec = 1, key = "gridcode"))
    }
}




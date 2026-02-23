identify_needed_nc_files <- function(begin_date, end_date, product){
    require(googledrive)
    # Convert strings to Date objects (ignoring time part if present)
    begin_date <- as.Date(begin_date)
    end_date <- as.Date(end_date)
    # Calculate start and end times (beginning at 00:00:00, ending at 23:59:59)
    start_time <- as.POSIXct(begin_date, tz = "UTC")
    end_time <- as.POSIXct(end_date, tz = "UTC") + 86400 - 1  # 86400 seconds = 1 day For including the whole last day.
    # Generate 3-hour sequence
    time_series <- seq(start_time, end_time, by = "3 hours")
    years <- unique(year(time_series))
    # Connect to GloH20 and identify needed data
    # folder <- fcase(product == "Past", "https://drive.google.com/drive/folders/1DVR90Ud1C444bTOPeENX-3I7tgqPLnao",
    #                 product == "Past_nogauge", "https://drive.google.com/drive/folders/1MnCbZPTCYV1VrNf5eV1YelTeq0EP4HPH",
    #                 product == "NRT","https://drive.google.com/drive/folders/1XBonFS0_t3aSM_C4CYobwsN-spmj29vo",
    #                 default = stop("Product not known, select from 'Past', 'Past_nogauge' or 'NRT'."))

    # V3.16 (2021 to 2025 onwards), fcase did not work correctly
    folder <- switch(product == "Past", "https://drive.google.com/drive/folders/1vBOlufW9NmZ_i2HIyZU2Gs-mZQmmn0P-",
                    product == "Past_nogauge", "https://drive.google.com/drive/folders/1pE3Gak_sYpPa8efIFoUnjVXCYNf5F7N_",
                    product == "NRT","https://drive.google.com/drive/folders/1etXYLvcTS1MAbiab5u-hWr99dwyaf2yk",
                    default = stop("Product not known, select from 'Past', 'Past_nogauge' or 'NRT'."))


    query <- paste0("(", paste(sprintf("name contains '%s'", years), collapse = " or "),")")
    file_list <- drive_ls(path = folder, q = query)
    cat(" ", nrow(file_list), " files found for year(s) ", paste(years, collapse = " and ") ,".\n", sep = "")
    # identify needed files
    needed_files <- format(time_series, format = "%Y%j.%H.nc")
    files_needed <- subset(file_list, name %in% needed_files)
    cat(" ", nrow(files_needed), " files found of needed ", length(needed_files) ,".\n", sep = "")
    return(files_needed)
}

load_and_crop_nc_files <- function(file_row, area_of_interest, overwrite = FALSE){
    files_already_processed <- list.files(path_cropped_nc, pattern=".nc")
    require(googledrive)
    require(terra)
    require(ncdf4)
    file_name <- file_row$name
    if(file_name %in% files_already_processed & overwrite == FALSE){
        cat(" The file", file_name, "is already processed, skipping. \n")
    }else{
        tryCatch({
            file_id <- file_row$id
            file_path <- file.path(path_raw_nc, file_name)
            file_path_out <- file.path(path_cropped_nc, file_name)
            cat(" Downloading:", file_name, "\n")
            drive_download(as_id(file_id), path = file_path, overwrite = TRUE)
            r <- rast(file_path)
            if (crs(r) != "EPSG:4326") {r <- project(r, "EPSG:4326")}
            r_croped <- crop(r, area_of_interest)
            time_step <- as.POSIXlt(file_name, format = "%Y%j.%H.nc", tz = "GMT")
            time(r_croped) <- time_step
            writeCDF(r_croped, file_path_out, overwrite =TRUE)
            file.remove(file_path)
            cat(format(Sys.time(), "%F %R")," File ", file_name, " cropped. \n")},
            error = function(e) {
                message("Error: ", e$message)
                return(NA) # Return NA or NULL to indicate failure
            })
    }
}

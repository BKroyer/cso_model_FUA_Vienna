##########################################################################################################
###                                                                                                    ###
### Hydrological Modeling of Combined Sewer Overflows (CSOs)                                           ###
### An R-Based Implementation of Quaranta et al.`s Approach                                            ###
### by Nina Kleemeyer, B.Sc.                                                                           ###
###                                                                                                    ###
### Supervisor: Univ. Prof. Dipl.-Ing. Dr. techn. Matthias Zessner, TU Wien                            ###
###                                                                                                    ###
##########################################################################################################

.onLoad <- function(libname, pkgname) {
  options("cso_silent" = 1)
  options("cso_dwf_per_capita" = 0.2)
  options("cso_catchment_surface_storage" = 1.5)
  options("cso_rate_constant_surface_storage" = 0.3)
  options("cso_network_dwf_dilution_rate" = 9.1)
  options("cso_tank_dwf_dilution_rate" = 9)
  options("cso_network_storage" = 1)
  options("cso_tank_storage" = 0.45)
  options("cso_tank_volume" = 0.444624746)
}


#' Set default parameters for CSO modelling
#'
#' This function sets or overrides default parameter values for the hydrological model
#' of Combined Sewer Overflows (CSOs) as implemented in this package.
#' The defaults are based on the hydrological model of Quaranta et al. (2022),
#' which was adapted and validated in the corresponding master's thesis.
#'
#' @param silent numeric. If 1 (default), suppresses console messages; if 0, messages are displayed.
#' @param dwf_per_capita numeric. Dry Weather Flow (DWF) per capita [m?/day/person].
#'   Default: 0.2 (corresponding to 200 L/day/person as in Quaranta et al. 2022).
#' @param catchment_surface_storage numeric. Maximum surface storage capacity of the catchment [mm].
#'   Default: 1.5 mm (water retained on impervious surfaces before runoff begins).
#' @param rate_constant_surface_storage numeric. Reservoir constant for surface storage [1/timestep].
#'   Default: 0.3 (represents depletion of surface storage during dry periods).
#' @param network_dwf_dilution_rate numeric. Dilution rate of the sewer network [-].
#'   Defines the maximum network capacity relative to DWF. Default: 9.1.
#' @param tank_dwf_dilution_rate numeric. Dilution rate of the tank [-].
#'   Defines the maximum tank outflow capacity relative to DWF. Default: 9.
#' @param network_storage numeric. Network storage capacity [mm].
#'   Default: 1 mm (storage capacity of the sewer network before overflow).
#' @param tank_storage numeric. Tank storage capacity [mm].
#'   Default: 0.45 mm (capacity of the retention tank before overflow).
#' @param tank_volume numeric. Initial volume in the retention tank [mm].
#'   Default: 0.444624746 mm (initial state variable for tank storage).
#'
#' @details
#' The default values are derived from:
#' * Quaranta, E., Fuchs, S., Liefting, H.J., Schellart, A., & Pistocchi, A. (2022).
#'   A hydrological model to estimate pollution from combined sewer overflows at the regional scale: Application to Europe.
#'   \emph{Journal of Hydrology: Regional Studies, 41}, 101080. https://doi.org/10.1016/j.ejrh.2022.101080
#'
#' * Kleemeyer, N. (2025). Hydrological Modelling of Combined Sewer Overflows (CSOs): An R-Based Implementation of Quaranta et al.'s Approach.
#'   Master's Thesis, University of Vienna.
#'
#' @return No return value. The function updates global package options for CSO modelling.
#'
#' @examples
#' # Use default values:
#' cso_defaults()
#'
#' # Change only specific parameters:
#' cso_defaults(network_storage = 1.1, tank_storage = 0.55)
#'
#' # Enable verbose output:
#' cso_defaults(silent = 0)
#'
#' @export


cso_defaults <- function(silent,
                         dwf_per_capita,
                         catchment_surface_storage,
                         rate_constant_surface_storage,
                         network_dwf_dilution_rate,
                         tank_dwf_dilution_rate,
                         network_storage,
                         tank_storage,
                         tank_volume) {
  if (!missing(silent)) {
    stopifnot("silent must be a numeric value." = is.numeric(silent))
    stopifnot("silent must be 0 or 1." = ((silent == 0) |
                                            (silent == 1)))
    options("cso_silent" = silent)
  }
  if (!missing(dwf_per_capita)) {
    stopifnot("dwf_per_capita must be a numeric value." = is.numeric(dwf_per_capita))
    options("cso_dwf_per_capita" = dwf_per_capita)
  }
  if (!missing(catchment_surface_storage)) {
    stopifnot(
      "catchment_surface_storage must be a numeric value." = is.numeric(catchment_surface_storage)
    )
    options("cso_catchment_surface_storage" = catchment_surface_storage)
  }
  if (!missing(rate_constant_surface_storage)) {
    stopifnot(
      "rate_constant_surface_storage must be a numeric value." = is.numeric(rate_constant_surface_storage)
    )
    options("cso_rate_constant_surface_storage" = rate_constant_surface_storage)
  }
  if (!missing(network_dwf_dilution_rate)) {
    stopifnot(
      "network_dwf_dilution_rate must be a numeric value." = is.numeric(network_dwf_dilution_rate)
    )
    options("cso_network_dwf_dilution_rate" = network_dwf_dilution_rate)
  }
  if (!missing(tank_dwf_dilution_rate)) {
    stopifnot("tank_dwf_dilution_rate must be a numeric value." = is.numeric(tank_dwf_dilution_rate))
    options("cso_tank_dwf_dilution_rate" = tank_dwf_dilution_rate)
  }
  if (!missing(network_storage)) {
    stopifnot("network_storage must be a numeric value." = is.numeric(network_storage))
    options("cso_network_storage" = network_storage)
  }
  if (!missing(tank_storage)) {
    stopifnot("tank_storage must be a numeric value." = is.numeric(tank_storage))
    options("cso_tank_storage" = tank_storage)
  }
  if (!missing(tank_volume)) {
    stopifnot("tank_volume must be a numeric value." = is.numeric(tank_volume))
    options("cso_tank_volume" = tank_volume)
  }
}

#' Display a message in the console
#'
#' This function prints a message to the console if the package option
#' \code{cso_silent} is set to 0. If \code{cso_silent} is 1 (the default),
#' the message will be suppressed.
#'
#' @param msg character. The message to display.
#'
#' @details
#' This helper function is part of the internal messaging system of the CSO
#' hydrological model package. It is mainly used to provide user feedback
#' during model execution or data processing steps.
#'
#' @return No return value. Prints a message to the console if allowed.
#'
#' @examples
#' # Display a message (if silent mode is disabled)
#' cso_defaults(silent = 0)
#' cso_message("Model run started")
#'
#' # No message is displayed if silent mode is enabled
#' cso_defaults(silent = 1)
#' cso_message("This will not be shown")
#'
#' @seealso [cso_defaults()]
#'
#' @export

cso_message <- function(msg) {
  if (getOption("cso_silent") != 1) {
    cat(paste(msg, "\n"))
  }
}


#' Calculate CSO values from an Excel file
#'
#' This function reads precipitation data from an Excel file (or optionally from a URL),
#' combines it with population and area data, and calculates combined sewer overflow (CSO)
#' volumes, durations, and related hydrological parameters using the CSO model described
#' in Quaranta et al. (2022) and adapted in the master's thesis.
#'
#' @param filename character. Path to the Excel file containing the precipitation data.
#'   If empty and `url` is provided, the file will be downloaded first.
#' @param url character. Optional URL to download the Excel file from the internet.
#' @param population numeric. Population of the catchment area. Must be > 0.
#' @param area numeric. Catchment area in km?. Must be > 0.
#' @param sheet character or numeric. The sheet in the Excel file to read from.
#' @param col_timestep numeric. Column number containing the timesteps (e.g. date or datetime). Default: 1.
#' @param col_precipitation numeric. Column number containing precipitation values [mm]. Default: 2.
#'
#' @returns A data frame with approximately 50 columns containing CSO results,
#' including overflow volumes, durations, dry-weather flow shares, and event-based metrics.
#'
#' @details
#'
#' The function automatically handles downloading data if a URL is provided, reads the Excel file,
#' and passes the precipitation data to `cso_data()` for CSO calculations.
#'
#' @examples
#' # Calculate CSO results from a local Excel file
#' data <- cso_xlsx("vienna.xlsx", population = 2000000, area = 948)
#'
#' # Download an Excel file from the internet and run the calculation
#' data <- cso_xlsx(url = "https://example.com/precipitation.xlsx",
#'                  population = 1500000, area = 500)
#'
#' @seealso [cso_data()] for the core calculation function and [cso_defaults()] for default parameter settings.
#'
#' @export

cso_xlsx <- function(filename = "",
                     url = "",
                     population = 0,
                     area = 0,
                     sheet = "",
                     col_timestep = 1,
                     col_precipitation = 2) {
  library(readxl)

  stopifnot("Population must be a numeric value." = is.numeric(population))
  stopifnot("Population must be a value greater 0." = population > 0)
  stopifnot("Area must be a numeric value." = is.numeric(area))
  stopifnot("Area mus be a value greater 0." = area > 0)
  if (is.numeric(sheet)) {
    stopifnot("Sheet must be a numeric value greater 0." = sheet == 0)
  }
  if (is.character(sheet)) {
    stopifnot("Sheet is empty" = (sheet != ""))
  }
  stopifnot("col_timestep must be a numeric value." = is.numeric(col_timestep))
  stopifnot("col_timestep must be a value greater 0." = col_timestep > 0)
  stopifnot("col_precipitation must be a numeric value." = is.numeric(col_precipitation))
  stopifnot("col_precipitation must be a value greater 0." = col_precipitation > 0)

  if (url != "") {
    if (filename == "") {
      filename <- paste(tempdir(), basename(url), sep = "\\")
    }
    download.file(url, filename)
    cso_message(paste("Download:", url, " -> ", filename))
  }

  ### Open Excel File

  cso_message(paste("Open source datafile", filename))

  data <- read_xlsx(
    filename,
    sheet = sheet,
    range = cell_cols(c(col_timestep, col_precipitation)),
    col_types = c("date", "numeric")
  )
  colnames(data) <- c("timestep", "precipitation")

  ### Calculation

  cso_data(data, population, area)
}


#' Calculate CSO values from a CSV file
#'
#' This function reads precipitation data from a CSV file (or optionally from a URL),
#' combines it with population and area data, and calculates combined sewer overflow (CSO)
#' volumes, durations, and related hydrological parameters using the CSO model described
#' in Quaranta et al. (2022) and adapted in the master's thesis.
#'
#' @param filename character. Path to the CSV file containing the precipitation data.
#'   If empty and `url` is provided, the file will be downloaded first.
#' @param url character. Optional URL to download the CSV file from the internet.
#' @param population numeric. Population of the catchment area. Must be > 0.
#' @param area numeric. Catchment area in km?. Must be > 0.
#' @param delim character. Column delimiter in the CSV file. Default: `";"`.
#' @param col_timestep numeric. Column number containing the timesteps (e.g. date or datetime). Default: 1.
#' @param col_precipitation numeric. Column number containing precipitation values [mm]. Default: 2.
#'
#' @returns A data frame with approximately 50 columns containing CSO results,
#' including overflow volumes, durations, dry-weather flow shares, and event-based metrics.
#'
#' @details
#' This function is part of the R-based implementation of the CSO hydrological model:
#' * Quaranta, E., Fuchs, S., Liefting, H.J., Schellart, A., & Pistocchi, A. (2022).
#'   A hydrological model to estimate pollution from combined sewer overflows at the regional scale: Application to Europe.
#'   \emph{Journal of Hydrology: Regional Studies, 41}, 101080. https://doi.org/10.1016/j.ejrh.2022.101080
#'
#' * Kleemeyer, N.(2025). Hydrological Modelling of Combined Sewer Overflows (CSOs): An R-Based Implementation of Quaranta et al.'s Approach.
#'   Master's Thesis, University of Vienna.
#'
#' The function automatically handles downloading data if a URL is provided, reads the CSV file,
#' and passes the precipitation data to `cso_data()` for CSO calculations.
#'
#' @examples
#' # Calculate CSO results from a local CSV file
#' data <- cso_csv("vienna.csv", population = 2000000, area = 146)
#'
#' # Download a CSV file from the internet and run the calculation
#' data <- cso_csv(url = "https://example.com/precipitation.csv",
#'                 population = 1500000, area = 500)
#'
#' @seealso [cso_data()] for the core calculation function and [cso_defaults()] for default parameter settings.
#'
#' @export

cso_csv <- function(filename = "",
                    url = "",
                    population = 0,
                    area = 0,
                    delim = ";",
                    col_timestep = 1,
                    col_precipitation = 2) {
  library(readr)

  stopifnot("Population must be a numeric value." = is.numeric(population))
  stopifnot("Population must be a value greater 0." = population > 0)
  stopifnot("Area must be a numeric value." = is.numeric(area))
  stopifnot("Area mus be a value greater 0." = area > 0)
  stopifnot("delim must be a character value." = is.character(delim))
  stopifnot("col_timestep must be a numeric value." = is.numeric(col_timestep))
  stopifnot("col_timestep must be a value greater 0." = col_timestep > 0)
  stopifnot("col_precipitation must be a numeric value." = is.numeric(col_precipitation))
  stopifnot("col_precipitation must be a value greater 0." = col_precipitation > 0)

  if (url != "") {
    if (filename == "") {
      filename <- paste(tempdir(), basename(url), sep = "\\")
    }
    download.file(url, filename)
    cso_message(paste("Download:", url, " -> ", filename))
  }

  ### Open csv File

  cso_message(paste("Open source datafile", filename))

  data <- read_delim(
    filename,
    delim = delim,
    col_select = c(all_of(col_timestep), all_of(col_precipitation)),
    col_types = "??"
  )
  colnames(data) <- c("timestep", "precipitation")
  data <- transform(
    data,
    timestep = as.POSIXct(timestep, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    precipitation = as.numeric(precipitation)
  )

  ### Calculation

  cso_data(data, population, area)
}


#' Calculate CSO values from precipitation, area and population data
#'
#' This function performs the core calculation of Combined Sewer Overflows (CSOs) using precipitation
#' time series, population data, and catchment characteristics. It applies the hydrological model
#' described in Quaranta et al. (2022) and adapted in the master's thesis, including surface storage,
#' sewer network storage, tank storage, and dry-weather flow contributions.
#'
#' @param data data.frame. Input data with at least the columns:
#'   \itemize{
#'     \item \code{timestep}: Date or datetime of each observation (3-hourly intervals recommended).
#'     \item \code{precipitation}: Precipitation values in mm.
#'   }
#'   Optionally, the data frame can include columns for population, area, or pre-calculated parameters.
#' @param population numeric. Population of the catchment area. Only required if not provided in `data`.
#' @param area numeric. Catchment area in km?. Only required if not provided in `data`.
#'
#' @returns A data frame with the original input data plus approximately 50 additional columns,
#' including:
#' * CSO volumes and durations,
#' * Dry-weather flow shares,
#' * Network and tank flow parameters,
#' * Event-based CSO metrics.
#'
#' @details
#' This is the central computation engine of the CSO model. It:
#' 1. Prepares input data by filling missing parameters with defaults from [cso_defaults()].
#' 2. Calculates surface storage, network flow, tank flow and over 40 further varaibles.
#' 3. Provides data for further calculation of  CSO volumes, overflow frequencies, and durations of CSO Events.
#'
#' The methodology is based on:
#' * Quaranta, E., Fuchs, S., Liefting, H.J., Schellart, A., & Pistocchi, A. (2022).
#'   A hydrological model to estimate pollution from combined sewer overflows at the regional scale: Application to Europe.
#'   \emph{Journal of Hydrology: Regional Studies, 41}, 101080. https://doi.org/10.1016/j.ejrh.2022.101080
#'
#' * Kleemeyer, N. (2025). Hydrological Modelling of Combined Sewer Overflows (CSOs): An R-Based Implementation of Quaranta et al.'s Approach.
#'   Master's Thesis, University of Vienna.
#'
#' @examples
#' # Example: Calculate CSO results using a prepared data frame
#' df <- data.frame(
#'   timestep = seq.POSIXt(as.POSIXct("2020-01-01 00:00"), by = "3 hours", length.out = 10),
#'   precipitation = c(0, 0, 2, 5, 0, 0, 1, 0, 0, 3)
#' )
#' result <- cso_data(df, population = 2000000, area = 150)
#' head(result)
#'
#' @seealso [cso_xlsx()], [cso_csv()] for data import functions.
#'
#' @export

cso_data <- function(data,
                     population = 0,
                     area = 0) {


  #############################################
  # Parameter                                 #
  #############################################

  ### fua

  if (!("fua" %in% names(data))) {
    data$fua <- "unknown"
  }

  ### Precipitation

  suppressWarnings(data$precipitation <- as.numeric(data$precipitation))
  data  <- subset(data, !is.na(precipitation))

  ### Population

  if (!("population" %in% names(data))) {
    stopifnot("Population must be a numeric value." = is.numeric(population))
    stopifnot("Population must be a value greater 0." = population > 0)
    data$population <- population
  } else {
    data$population <- as.numeric(data$population)
    data  <- subset(data, !is.na(population))
  }

  ### Area

  if (!("area" %in% names(data))) {
    stopifnot("Area must be a numeric value." = is.numeric(area))
    stopifnot("Area mus be a value greater 0." = area > 0)
    data$area <- area
  } else {
    data$area <- as.numeric(data$area)
    data  <- subset(data, !is.na(area))
  }

  ### population / area


  if (!("pop_density" %in% names(data))) {
    data$pop_density <- data$population / (data$area * 100 * 0.72) # Personen/ha
  }

  if (!("DWF_per_capita" %in% names(data))) {
    data$DWF_per_capita <- getOption("cso_dwf_per_capita") # m²/Person/Tag
  }

  if (!("catchment_surface_storage" %in% names(data))) {
    data$catchment_surface_storage <- getOption("cso_catchment_surface_storage") # Oberflächenreservoir Kapazität (mm)
  }
  if (!("rate_constant_surface_storage" %in% names(data))) {
    data$rate_constant_surface_storage <- getOption("cso_rate_constant_surface_storage") # Oberflächenreservoir Rate ((3-Stunden)-1)
  }
  if (!("network_DWF_dilution_rate" %in% names(data))) {
    data$network_DWF_dilution_rate <- getOption("cso_network_dwf_dilution_rate")
  }
  if (!("tank_DWF_dilution_rate" %in% names(data))) {
    data$tank_DWF_dilution_rate <- getOption("cso_tank_dwf_dilution_rate")
  }
  if (!("network_storage" %in% names(data))) {
    data$network_storage <- getOption("cso_network_storage") # Netzwerkspeicher Kapazität (mm) (W1)
  }
  if (!("tank_storage" %in% names(data))) {
    data$tank_storage <- getOption("cso_tank_storage") # Tank Kapazität (mm)
  }
  if (!("qdwf" %in% names(data))) {
    data$qdwf <- data$pop_density * data$DWF_per_capita / 8 / 10 # mm/3 Stunden/m²
  }
  if (!("rate_constant_network_storage" %in% names(data))) {
    data$rate_constant_network_storage <- data$network_DWF_dilution_rate * data$qdwf / data$network_storage # Netzwerkspeicher Rate ((3-Stunden)-1) (k1)
  }
  if (!("rate_constant_tank_storage" %in% names(data))) {
    data$rate_constant_tank_storage <- data$tank_DWF_dilution_rate * data$qdwf / data$tank_storage # Tank Rate ((3-Stunden)-1) (k2)
  }
  if (!("network_max_conveyance" %in% names(data))) {
    data$network_max_conveyance <- 12.75098313 # Max. Netzwerk-Kapazität (mm/3 Stunden/m²)
  }
  if (!("tank_volume" %in% names(data))) {
    data$tank_volume <- getOption("cso_tank_volume") # Wert aus Parametertabelle Berlin
  }
  if (!("network_storage_mult_rate" %in% names(data))) {
    data$network_storage_mult_rate <- data$network_storage * data$rate_constant_network_storage
  }


  #############################################
  # Calculation data                          #
  #############################################


  cso_message("Start calculation...")

  ### Surface Storage ###

  cso_message("Surface storage...")

  fun_surface_storage <- function(fua,
                                  precipitation,
                                  catchment_surface_storage,
                                  rate_constant_surface_storage) {
    v <- numeric(length(precipitation))
    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- pmin(precipitation[t], catchment_surface_storage[t])
      } else {
        v[t] <- pmin(
          catchment_surface_storage[t],
          ifelse(
            precipitation[t] > 0,
            precipitation[t] + v[t - 1],
            v[t - 1] * exp(-rate_constant_surface_storage[t])
          )
        )
      }
    }
    v
  }

  data$surface_storage <- fun_surface_storage(
    data$fua,
    data$precipitation,
    data$catchment_surface_storage,
    data$rate_constant_surface_storage
  )

  ### surface_losses ###

  cso_message("Surface losses...")

  fun_surface_losses <- function(fua,
                                 surface_storage,
                                 rate_constant_surface_storage) {
    v <- numeric(length(surface_storage))
    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        v[t] <- surface_storage[t - 1] * (1 - exp(-rate_constant_surface_storage[t]))
      }
    }
    v
  }

  data$surface_losses <- fun_surface_losses(data$fua,
                                            data$surface_storage,
                                            data$rate_constant_surface_storage)

  ### Runoff ###

  cso_message("Runoff...")

  fun_runoff <- function(fua,
                         precipitation,
                         surface_storage,
                         catchment_surface_storage) {
    v <- numeric(length(precipitation))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- pmax(
          0,
          precipitation[t + 1] - catchment_surface_storage[t] + surface_storage[t +
                                                                                  1] - surface_storage[t]
        )
      } else {
        v[t] <- pmax(0,
                     precipitation[t] - catchment_surface_storage[t] + surface_storage[t - 1])
      }
      if (is.na(v[[t]])) {
        v[t] <- 0
      }
    }
    v
  }

  data$runoff <- fun_runoff(
    data$fua,
    data$precipitation,
    data$surface_storage,
    data$catchment_surface_storage
  )


  ### Ft Network Flow ###

  cso_message("Networt flow...")

  fun_network_flow <- function(fua,
                               runoff,
                               qdwf,
                               rate_constant_network_storage) {
    v <- numeric(length(runoff))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- runoff[t] + qdwf[t]
      } else {
        v[t] <- (runoff[t] + qdwf[t]) * (1 - exp(-rate_constant_network_storage[t])) + v[t - 1] * exp(-rate_constant_network_storage[t])
      }
    }
    v
  }

  data$network_flow <- fun_network_flow(data$fua,
                                        data$runoff,
                                        data$qdwf,
                                        data$rate_constant_network_storage)


  ### Worst case overflow ###

  cso_message("Worst case overflow...")

  data$worst_case_overflow <- pmax(data$runoff + data$qdwf - data$network_storage_mult_rate, 0)

  ### Scenario a ###

  cso_message("Scenario a...")

  fun_scenario_a <- function(fua,
                             runoff,
                             network_flow,
                             qdwf,
                             network_storage_mult_rate) {
    v <- numeric(length(network_flow))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        f1 <- runoff[t] + qdwf[t] - network_flow[t - 1]
        f2 <- runoff[t] + qdwf[t] - network_storage_mult_rate[t]
        if (f1 >= 0) {
          if (f2 >= f1) {
            v[t] <- 1
          } else {
            v[t] <- 0
          }
        } else {
          if (f2 >= 0) {
            v[t] <- 1
          } else {
            v[t] <- 0
          }
        }
      }
    }
    v
  }

  data$scenario_a <- fun_scenario_a(
    data$fua,
    data$runoff,
    data$network_flow,
    data$qdwf,
    data$network_storage_mult_rate
  )

  ### Scenario b ###

  cso_message("Scenario b...")

  fun_scenario_b <- function(fua,
                             runoff,
                             network_flow,
                             qdwf,
                             network_storage_mult_rate,
                             rate_constant_network_storage) {
    v <- numeric(length(network_flow))


    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        f1 <- runoff[t] + qdwf[t] - network_flow[t - 1]
        f2 <- runoff[t] + qdwf[t] - network_storage_mult_rate[t]

        if ((f1 >= 0) &
            (f2 < f1) &
            (f2 >= f1 * exp(-rate_constant_network_storage[t]))) {
          v[t] <- 1
        } else {
          v[t] <- 0
        }
      }
    }
    v
  }

  data$scenario_b <- fun_scenario_b(
    data$fua,
    data$runoff,
    data$network_flow,
    data$qdwf,
    data$network_storage_mult_rate,
    data$rate_constant_network_storage
  )


  ### Scenario c ###

  cso_message("Scenario c...")

  fun_scenario_c <- function(fua,
                             runoff,
                             network_flow,
                             qdwf,
                             network_storage_mult_rate,
                             rate_constant_network_storage) {
    v <- numeric(length(network_flow))


    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        f1 <- runoff[t] + qdwf[t] - network_flow[t - 1]
        f2 <- runoff[t] + qdwf[t] - network_storage_mult_rate[t]

        if ((f1 < 0) &
            (f2 >= f1) &
            (f2 < f1 * exp(-rate_constant_network_storage[t]))) {
          v[t] <- 1
        } else {
          v[t] <- 0
        }
      }
    }
    v
  }

  data$scenario_c <- fun_scenario_c(
    data$fua,
    data$runoff,
    data$network_flow,
    data$qdwf,
    data$network_storage_mult_rate,
    data$rate_constant_network_storage
  )

  ### Scenario d ###

  cso_message("Scenario d...")

  data$scenario_d <- (1 - (data$scenario_a + data$scenario_b + data$scenario_c))


  ### Network overflow ###

  cso_message("Network overflow...")

  fun_network_overflow <- function(fua,
                                   runoff,
                                   network_flow,
                                   scenario_a,
                                   scenario_b,
                                   scenario_c,
                                   scenario_d,
                                   qdwf,
                                   network_storage_mult_rate,
                                   rate_constant_network_storage) {
    v <- numeric(length(network_flow))


    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        f1 <- runoff[t] + qdwf[t] - network_flow[t - 1]
        f2 <- runoff[t] + qdwf[t] - network_storage_mult_rate[t]
        f3 <- pmax(0, f2 / f1) # no lo0() with negative value
        f4 <- pmax(0, f1 / f2) # no lo0() with negative value


        v[t] <- ifelse(
          scenario_d[t] == 1,
          0,
          ifelse(
            scenario_a[t] == 1,
            f2 - f1 / rate_constant_network_storage[t] * (1 - exp(
              -rate_constant_network_storage[t]
            )),
            f2 * (
              scenario_b[t] * (1 - (1 + log(f3)) / rate_constant_network_storage[t]) +
                scenario_c[t] * (1 + log(f4)) / rate_constant_network_storage[t]
            ) - f1 / rate_constant_network_storage[t] * (
              -scenario_b[t] * exp(-rate_constant_network_storage[t]) + scenario_c
            )[t]
          )
        )
      }
    }
    v
  }

  data$network_overflow <- fun_network_overflow(
    data$fua,
    data$runoff,
    data$network_flow,
    data$scenario_a,
    data$scenario_b,
    data$scenario_c,
    data$scenario_d,
    data$qdwf,
    data$network_storage_mult_rate,
    data$rate_constant_network_storage
  )


  ### t1 ###

  cso_message("t1...")

  fun_t1 <- function(fua,
                     runoff,
                     network_flow,
                     scenario_a,
                     scenario_d,
                     qdwf,
                     network_storage_mult_rate,
                     rate_constant_network_storage) {
    v <- numeric(length(network_flow))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        f1 <- runoff[t] + qdwf[t] - network_flow[t - 1]
        f2 <- runoff[t] + qdwf[t] - network_storage_mult_rate[t]
        f3 <- pmax(0, f2 / f1)
        if ((scenario_a[t] == 1) | (scenario_d[t] == 1)) {
          v[t] <- 0
        } else {
          v[t] <- -log(f3) / rate_constant_network_storage[t]
        }
      }
    }
    v
  }

  data$t1 <- fun_t1(
    data$fua,
    data$runoff,
    data$network_flow,
    data$scenario_a,
    data$scenario_d,
    data$qdwf,
    data$network_storage_mult_rate,
    data$rate_constant_network_storage
  )


  ### virtual volume and tank volume ###

  cso_message("Virtual volume and tank volume...")

  fun_virtual_volume_and_tank_volume <- function(fua,
                                                 runoff,
                                                 network_flow,
                                                 t1,
                                                 rate_constant_tank_storage,
                                                 qdwf,
                                                 rate_constant_network_storage,
                                                 network_storage,
                                                 network_storage_mult_rate,
                                                 tank_storage) {
    va <- numeric(length(runoff))
    vd <- numeric(length(runoff))
    vb_t1 <- numeric(length(runoff))
    vc_t1 <- numeric(length(runoff))
    vb <- numeric(length(runoff))
    vc <- numeric(length(runoff))
    tv <- numeric(length(runoff))

    f <- "-"
    for (t in 1:length(va)) {
      if (fua[t] != f) {
        f <- fua[t]
        va[t] <- 0
        vd[t] <- 0
        vb_t1[t] <- 0
        vc_t1[t] <- 0
        vb[t] <- 0
        vc[t] <- 0
        tv[t] <- network_flow[1] / rate_constant_tank_storage[1]
      } else {
        va[t] <- network_storage_mult_rate[t] / rate_constant_tank_storage[t] * (1 - exp(-rate_constant_tank_storage[t])) + tv[t - 1] * exp(-rate_constant_tank_storage[t])

        vd[t] <- (runoff[t] + qdwf[t]) / rate_constant_tank_storage[t] * (1 - exp(-rate_constant_tank_storage[t])) - (runoff[t] + qdwf[t] - network_flow[t - 1]) /
          (rate_constant_tank_storage[t] - rate_constant_network_storage[t]) *
          (exp(-rate_constant_network_storage[t]) - exp(-rate_constant_tank_storage[t])) + tv[t - 1] *
          exp(-rate_constant_tank_storage[t])

        vb_t1[t] <- (runoff[t] + qdwf[t]) / rate_constant_tank_storage[t] * (1 - exp(-rate_constant_tank_storage[t] * t1[t])) - (runoff[t] + qdwf[t] - network_flow[t - 1]) /
          (rate_constant_tank_storage[t] - rate_constant_network_storage[t]) * (
            exp(-rate_constant_network_storage[t] * t1[t]) - exp(-rate_constant_tank_storage[t] * t1[t])
          ) + tv[t - 1] *
          exp(-rate_constant_tank_storage[t] * t1[t])

        vc_t1[t] <- rate_constant_network_storage[t] * network_storage[t] / rate_constant_tank_storage[t] * (1 - exp(-rate_constant_tank_storage[t] * 1)) + tv[t - 1] * exp(-rate_constant_tank_storage[t] * 1)
        vb[t] <- vb_t1[t] * exp(-rate_constant_tank_storage[t] * (1 - t1[t])) + rate_constant_network_storage[t] * network_storage[t] / rate_constant_tank_storage[t] * (1 - exp(-rate_constant_tank_storage[t] * (1 - t1[t])))

        vc[t] <- (runoff[t] + qdwf[t]) / rate_constant_tank_storage[t] * (1 - exp(-rate_constant_tank_storage[t] *
                                                                                    (1 - t1[t]))) + (runoff[t] + qdwf[t] - network_flow[t - 1]) / (rate_constant_tank_storage[t] - rate_constant_network_storage[t]) *
          (
            exp(-rate_constant_tank_storage[t] * (1 - t1[t])) - exp(-rate_constant_network_storage[t] * (1 - t1[t]))
          ) + vc_t1[t] * exp(-rate_constant_tank_storage[t] * (1 - t1[t]))

        tv[t] <- pmin(ifelse(
          data$scenario_a[t] == 1,
          va[t],
          ifelse(
            data$scenario_b[t] == 1,
            vb[t],
            ifelse(data$scenario_c[t] == 1, vc[t], vd[t])
          )
        ),
        tank_storage[t])
      }
    }
    data$virtual_volume_a <<- va
    data$virtual_volume_d <<- vd
    data$virtual_volume_b_t1 <<- vb_t1
    data$virtual_volume_c_t1 <<- vc_t1
    data$virtual_volume_b <<- vb
    data$virtual_volume_c <<- vc
    data$tank_volume <<- tv
    tv
  }

  fun_virtual_volume_and_tank_volume(
    data$fua,
    data$runoff,
    data$network_flow,
    data$t1,
    data$rate_constant_tank_storage,
    data$qdwf,
    data$rate_constant_network_storage,
    data$network_storage,
    data$network_storage_mult_rate,
    data$tank_storage
  )

  ### t2 ###

  cso_message("t2...")

  fun_t2 <- function(fua,
                     tank_volume,
                     rate_constant_tank_storage,
                     tank_storage,
                     network_storage_mult_rate) {
    v <- numeric(length(tank_volume))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        f1 <- network_storage_mult_rate[t]
        f2 <- pmax(
          0,
          (f1 - tank_volume[t - 1] * rate_constant_tank_storage[t]) / (f1 - rate_constant_tank_storage[t] * tank_storage[t])
        )
        v[t] <- pmin(1, log(f2) / rate_constant_tank_storage[t])
      }
    }
    v
  }

  data$t2 <- fun_t2(
    data$fua,
    data$tank_volume,
    data$rate_constant_tank_storage,
    data$tank_storage,
    data$network_storage_mult_rate
  )

  ### ea ###

  cso_message("ea...")

  data$ea <- (1 - data$t2) * (
    data$network_storage_mult_rate - (data$rate_constant_tank_storage * data$tank_storage)
  )

  ### t2_1 ###

  cso_message("t2_1...")

  fun_t2_1 <- function(fua,
                       tank_volume,
                       virtual_volume_d,
                       network_storage_mult_rate,
                       rate_constant_tank_storage,
                       tank_storage) {
    v <- numeric(length(tank_volume))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        f1 <- network_storage_mult_rate[t]
        f2 <- rate_constant_tank_storage[t] * tank_storage[t]
        v[t] <- pmin(1,
                     ifelse(
                       tank_volume[t - 1] == tank_storage[t],
                       ifelse(
                         virtual_volume_d[t] >= tank_storage[t],
                         0,
                         (tank_volume[t - 1] - tank_storage[t]) / (tank_volume[t - 1] - virtual_volume_d[t])
                       ),
                       ifelse(
                         tank_volume[t - 1] == virtual_volume_d[t],
                         1,
                         ifelse(
                           virtual_volume_d[t] >= tank_storage[t],
                           (tank_volume[t - 1] - tank_storage[t]) / (tank_volume[t - 1] - virtual_volume_d[t]),
                           1
                         )
                       )
                     ))
      }
    }
    v
  }

  data$t2_1 <- fun_t2_1(
    data$fua,
    data$tank_volume,
    data$virtual_volume_d,
    data$network_storage_mult_rate,
    data$rate_constant_tank_storage,
    data$tank_storage
  )

  ### ed ###

  cso_message("ed...")

  fun_ed <- function(fua,
                     tank_volume,
                     runoff,
                     network_flow,
                     t2_1,
                     network_storage_mult_rate,
                     rate_constant_tank_storage,
                     tank_storage,
                     qdwf,
                     rate_constant_network_storage) {
    v <- numeric(length(tank_volume))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        f1 <- network_storage_mult_rate[t]
        f2 <- rate_constant_tank_storage[t] * tank_storage[t]
        v[t] <- ifelse(
          tank_volume[t - 1] < tank_storage[t],
          (
            runoff[t] + qdwf[t] - rate_constant_tank_storage[t] * tank_storage[t]
          ) * (1 - t2_1[t]) + (runoff[t] + qdwf[t] - network_flow[t - 1]) /
            rate_constant_network_storage[t] * (
              exp(-rate_constant_network_storage[t]) - exp(-rate_constant_network_storage[t] * t2_1[t])
            ),
          (
            runoff[t] + qdwf[t] - rate_constant_tank_storage[t] * tank_storage[t]
          ) * (t2_1[t]) + (t2_1[t] + qdwf[t] - network_flow[t - 1]) /
            rate_constant_network_storage[t] * (-1 + exp(
              -rate_constant_network_storage[t] * t2_1[t]
            ))
        )
      }
    }
    v
  }

  data$ed <- fun_ed(
    data$fua,
    data$tank_volume,
    data$runoff,
    data$network_flow,
    data$t2_1,
    data$network_storage_mult_rate,
    data$rate_constant_tank_storage,
    data$tank_storage,
    data$qdwf,
    data$rate_constant_network_storage
  )

  ### t2_2 ###

  cso_message("t2_2...")

  fun_t2_2 <- function(fua,
                       tank_volume,
                       virtual_volume_b,
                       t1,
                       tank_storage) {
    v <- numeric(length(tank_volume))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        v[t] <- pmin(1,
                     (tank_volume[t - 1] - tank_storage[t]) / (tank_volume[t - 1] - virtual_volume_b[t])) * t1[t]
      }
    }
    v
  }


  data$t2_2 <- fun_t2_2(data$fua,
                        data$tank_volume,
                        data$virtual_volume_b,
                        data$t1,
                        data$tank_storage)

  ### eb ###

  cso_message("eb...")

  fun_eb <- function(fua,
                     runoff,
                     network_flow,
                     t1,
                     t2_2,
                     virtual_volume_b_t1,
                     network_storage_mult_rate,
                     rate_constant_tank_storage,
                     tank_storage,
                     qdwf,
                     rate_constant_network_storage) {
    v <- numeric(length(runoff))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        f1 <- network_storage_mult_rate[t]
        f2 <- rate_constant_tank_storage[t] * tank_storage[t]
        f3 <- (f1 - virtual_volume_b_t1[t] * rate_constant_tank_storage[t]) / (f1 - f2)
        if (f3 >= 0) {
          v[t] <- (runoff[t] + qdwf[t] - f2) * (t1[t] - t2_2[t]) + (runoff[t] + qdwf[t] - network_flow[t - 1]) /
            rate_constant_network_storage[t] * (
              exp(-rate_constant_network_storage[t] * t1[t]) - exp(-rate_constant_network_storage[t] * t2_2[t])
            ) +
            (1 - pmin(1, t1[t] + pmax(
              0, log(f3) / rate_constant_tank_storage[t]
            ))) * (f1 - f2)
        } else {
          # Negative Value for loG() is Undefined
          v[t] <- NA
        }
      }
    }
    v
  }

  data$eb <- fun_eb(
    data$fua,
    data$runoff,
    data$network_flow,
    data$t1,
    data$t2_2,
    data$virtual_volume_b_t1,
    data$network_storage_mult_rate,
    data$rate_constant_tank_storage,
    data$tank_storage,
    data$qdwf,
    data$rate_constant_network_storage
  )

  ### t2_3 ###

  cso_message("t2_3...")

  data$t2_3 <- ifelse(
    data$virtual_volume_c_t1 < data$tank_storage,
    data$t1,
    ifelse(
      data$virtual_volume_c_t1 == data$tank_volume,
      1,
      data$t1 + (1 - data$t1) *
        (data$virtual_volume_c_t1 - data$tank_storage) / (data$virtual_volume_c_t1 - data$tank_volume)
    )
  )

  ### ec ###

  cso_message("ec...")

  data$ec <- pmax(
    0,
    (
      data$runoff + data$qdwf - (data$rate_constant_tank_storage * data$tank_storage)
    ) * (data$t2_3 - data$t1) + (data$runoff + data$qdwf - data$network_flow[3]) /
      data$rate_constant_network_storage * (
        exp(-data$rate_constant_network_storage * data$t2_3) - exp(-data$rate_constant_network_storage * data$t1)
      ) +
      (
        data$network_storage_mult_rate - (data$rate_constant_tank_storage * data$tank_storage)
      ) * (data$t1 - pmin(data$t1, data$t2))
  )


  ### tank_overflow ###

  cso_message("Tank overflow...")

  data$tank_overflow <- pmax(0, ifelse(
    data$scenario_a == 1,
    data$ea,
    ifelse(
      data$scenario_b == 1,
      data$eb,
      ifelse(data$scenario_c == 1, data$ec, data$ed)
    )
  ))

  ### Duration ###

  cso_message("Duration...")

  data$duration <- ifelse(pmax(data$network_overflow, data$tank_overflow) > 0, 1, 0)

  ### Rain_event ###

  cso_message("Rain event...")

  fun_rain_event <- function(fua, precipitation) {
    v <- numeric(length(precipitation))

    a <- 0
    f <- "-"
    start <-0
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        if (start > 0) {
          v[start] <- a / 16
        }
        a <- 0
        start <- t
        f <- fua[t]
      } else {
        v[t] <- ifelse((precipitation[t - 1] == 0) &
                         (precipitation[t] > 0),
                       1,
                       ifelse((precipitation[t - 1] > 0) &
                                (precipitation[t] > 0), 2, 0))
        if (is.na(v[t])) { v[t]<-0 }
        if (v[t] == 1) {
          a <- a + 1
        }
      }
    }
    if (start > 0) {
      v[start] <- a / 16
    }
    v
  }

  data$rain_event <- fun_rain_event(data$fua, data$precipitation)

  ### Frequency overflow ###

  cso_message("Frequency overflow...")

  fun_frequency_overflow <- function(fua, duration) {
    v <- numeric(length(duration))

    a <- 0
    f <- "-"
    start <-0
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        if (start > 0) {
          v[start] <- a / 16
        }
        a <- 0
        start <- t
        f <- fua[t]
      } else {
        v[t] <- ifelse((duration[t - 1] == 1) & (duration[t] == 0), 1, 0)
        if (v[t] == 1) {
          a <- a + 1
        }
      }
    }
    if (start > 0) {
      v[start] <- a / 16
    }
    v
  }

  data$frequency_overflow <- fun_frequency_overflow(data$fua, data$duration)

  ### Total overflow ###

  cso_message("Total overflow...")

  data$total_overflow <- data$tank_overflow + data$network_overflow

  ### Cumulate rain ###

  cso_message("Cumulate rain...")

  fun_cumulate_rain_per_event <- function(fua, precipitation, rain_event) {
    v <- numeric(length(precipitation))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        v[t] <- ifelse(rain_event[t] == 2, v[t - 1] + precipitation[t], 0)
      }
    }
    v
  }

  data$cumulate_rain_per_event <- fun_cumulate_rain_per_event(data$fua, data$precipitation, data$rain_event)

  ### cso_volume_per_event ###

  cso_message("cso volume per event...")

  fun_cso_volume_per_event <- function(fua,
                                       cumulate_rain_per_event,
                                       total_overflow,
                                       rain_event) {
    v <- numeric(length(cumulate_rain_per_event))

    f <- "-"
    for (t in 1:length(v)) {
      if (fua[t] != f) {
        f <- fua[t]
        v[t] <- 0
      } else {
        v[t] <- ifelse((cumulate_rain_per_event[t] > 0) &
                         (cumulate_rain_per_event[t - 1] > 0),
                       total_overflow[t] + v[t - 1],
                       ifelse((cumulate_rain_per_event[t] > 0) &
                                (cumulate_rain_per_event[t - 1] == 0),
                              total_overflow[t],
                              0
                       )
        ) +
          ifelse((total_overflow[t + 1] > 0) &
                   (cumulate_rain_per_event[t + 1] == 0) &
                   (rain_event[t] == 2) &
                   (rain_event[t + 1] == 0),
                 total_overflow[t + 1],
                 0
          ) +
          ifelse((total_overflow[t + 2] > 0) &
                   (cumulate_rain_per_event[t + 2] == 0) &
                   (rain_event[t] == 2) &
                   (rain_event[t + 1] == 0),
                 total_overflow[t + 2],
                 0
          ) +
          ifelse((total_overflow[t + 3] > 0) &
                   (cumulate_rain_per_event[t + 3] == 0) &
                   (rain_event[t] == 2) &
                   (rain_event[t + 1] == 0),
                 total_overflow[t + 3],
                 0
          ) +
          ifelse((total_overflow[t + 4] > 0) &
                   (cumulate_rain_per_event[t + 4] == 0) &
                   (rain_event[t] == 2) &
                   (rain_event[t + 1] == 0),
                 total_overflow[t + 4],
                 0
          )
      }
    }
    v
  }

  data$cso_volume_per_event <- fun_cso_volume_per_event(data$fua,
                                                        data$cumulate_rain_per_event,
                                                        data$total_overflow,
                                                        data$rain_event)


  ### Rain volumen per event ###

  cso_message("Rain volume per event...")

  fun_total_rain_volume_per_event <- function(fua,
                                              rain_event,
                                              cumulate_rain_per_event) {
    v <- numeric(length(fua))

    f <- "-"
    for (t in 1:(length(v) - 1)) {
      if (fua[t] != f) {
        f <- fua[t]
      }
      if (fua[t] != fua[t + 1]) {
        v[t] <- 0
      } else {
        v[t] <- ifelse((rain_event[t] == 2) &
                         (rain_event[t + 1] == 0),
                       cumulate_rain_per_event[t],
                       0)
      }
    }
    v
  }

  data$total_rain_volume_per_event <- fun_total_rain_volume_per_event(data$fua, data$rain_event, data$cumulate_rain_per_event)


  ### Cso total volume per event ###

  cso_message("Cso total volume per event...")

  fun_cso_total_volume_per_event <- function(fua, rain_event, cso_volume_per_event) {
    v <- numeric(length(rain_event))

    f <- "-"
    for (t in 1:(length(v) - 1)) {
      if (fua[t] != f) {
        f <- fua[t]
      }
      if (fua[t] != fua[t + 1]) {
        v[t] <- 0
      } else {
        v[t] <- ifelse((rain_event[t] == 2) &
                         (rain_event[t + 1] == 0),
                       cso_volume_per_event[t],
                       0)
      }
    }
    v
  }

  data$cso_total_volume_per_event <- fun_cso_total_volume_per_event(data$fua, data$rain_event, data$cso_volume_per_event)



  ### ww_network_rejection ###

  # cat("Waste water network rejection...\n")
  #
  # fun_ww_network_rejection <- function(fua, network_rejection, precipitation, surface_storage, qdwf) {
  #   v <- numeric(length(network_rejection))
  #
  #   f <- "-"
  #   for (t in 1:length(v)) {
  #     if (fua[t] != f) {
  #       f <- fua[t]
  #       v[t] <- 0
  #     } else {
  #        v[t] <- network_rejection[t] / (1 + (precipitation[t] - max(0, surface_storage[t] - surface_storage[t - 1])) / qdwf[t])
  #     }
  #   }
  #   v
  # }
  # data$ww_network_rejection <- fun_ww_network_rejection(data$fua, data$network_rejection, data$precipitation, data$surface_storage, data$qdwf)
  #
  #
  #
  # ### ww_worst_case_overflow##
  #
  # cat("Waste water worst case overflow...\n")
  #
  # fun_ww_worst_case_overflow <- function(fua, worst_case_overflow, precipitation, surface_storage) {
  #   v <- numeric(length(worst_case_overflow))
  #
  #   f <- "-"
  #   for (t in 1:length(v)) {
  #     if (fua[t] != f) {
  #       f <- fua[t]
  #       v[t] <- 0
  #     } else {
  #        v[t] <- worst_case_overflow[t] / (1 + (precipitation[t] - max(0, surface_storage[t] - surface_storage[t - 1])) / param_qdwf
  #     )
  #     }
  #   }
  #   v
  # }
  #
  # data$ww_worst_case_overflow <- fun_ww_worst_case_overflow(data$fua, data$worst_case_overflow, data$precipitation, data$surface_storage)
  #
  #
  # ### ww_attenuated_worst_case_overflow ###
  #
  # cat("Waste water attenuated worst case overflow...\n")
  #
  # data$ww_attenuated_worst_case_overflow <- data$attenuated_worst_case_overflow / (1 + data$drainage / data$qdwf)
  #
  # ### ww_tank_overflow ###
  #
  # cat("Waste water tank overflow...\n")
  #
  # data$ww_tank_overflow <- data$tank_overflow / (1 + data$drainage / data$qdwf)

  cso_message("End calculation...")

  data
}

#' Save CSO results as an Excel file
#'
#' This function saves a data frame containing Combined Sewer Overflow (CSO)
#' results to an Excel file (.xlsx).
#'
#' @param data data.frame. A data frame produced by [cso_data()], typically containing
#' around 50 columns with CSO results such as precipitation, overflow volumes, durations,
#' and dry-weather flow shares.
#' @param filename character. The name of the output Excel file, including the ".xlsx" extension.
#'
#' @details
#' This function is a utility for exporting CSO modeling results to Excel for further reporting
#' or analysis. It uses the \pkg{openxlsx} package internally for writing Excel files.
#'
#' @return No return value. The function writes an Excel file to the specified location.
#'
#' @examples
#' # Save CSO results to Excel
#' result <- cso_data(mydata, population = 2000000, area = 150)
#' cso_save_xlsx(result, "CSO_results.xlsx")
#'
#' @seealso [cso_save_csv()] for saving results as a CSV file.
#'
#' @export

cso_save_xlsx <- function(data, filename) {
  library("openxlsx")
  write.xlsx(data, file = filename)
}

#' Save CSO results as a CSV file
#'
#' This function saves a data frame containing Combined Sewer Overflow (CSO)
#' results to a CSV file (.csv).
#'
#' @param data data.frame. A data frame produced by [cso_data()], typically containing
#' around 50 columns with CSO results such as precipitation, overflow volumes, durations,
#' and dry-weather flow shares.
#' @param filename character. The name of the output CSV file, including the ".csv" extension.
#'
#' @details
#' This function is a utility for exporting CSO modeling results to CSV format.
#' It uses `write.csv2()` to generate a UTF-8 encoded CSV file with semicolon-separated columns,
#' which is suitable for use in many European locales.
#'
#' @return No return value. The function writes a CSV file to the specified location.
#'
#' @examples
#' # Save CSO results to CSV
#' result <- cso_data(mydata, population = 2000000, area = 150)
#' cso_save_csv(result, "CSO_results.csv")
#'
#' @seealso [cso_save_xlsx()] for saving results as an Excel file.
#'
#' @export

cso_save_csv <- function(data, filename) {
  library("openxlsx")
  write.csv2(data, file = filename, fileEncoding = "UTF-8")
}

##########################################################################################################
###                                                                                                    ###
### Hydrological Modeling of Combined Sewer Overflows (CSOs)                                           ###
### An R-Based Implementation of Quaranta et al.`s Approach                                            ###
### by Nina Kleemeyer, B.Sc.                                                                           ###
###                                                                                                    ###
### Supervisor: Univ. Prof. Dipl.-Ing. Dr. techn. Matthias Zessner, TU Wien                            ###
###                                                                                                    ###
### 5/5 Create Country Comparison Tables                                                               ###
##########################################################################################################

###########################################################################
# CSO Results Summary and Export Script                                   #
# Purpose:                                                                #
# 1. Load processed CSO result files from the import script               #
# 2. Generate summary tables (absolute and normalized values) per country #
# 3. Apply formatting and color scales for improved readability           #
# 4. Export the results to CSV, Excel, and Word                           #
###########################################################################

#############################################
# Parameters                                #
#############################################

library(data.table)
library(gt)
library(openxlsx)
library(flextable)
library(officer)
library(scales)

# Input directory
input_dir <- "C:/Input_Path"
csv_files <- list.files(input_dir, pattern = "^[[:space:]]*precip_.*_merged_result_cso\\.csv$", full.names = TRUE)

summary_list <- list()

for (file in csv_files) {
  message("Processing file: ", basename(file))
  
  # Extract ISO country code
  country_code <- toupper(gsub("^precip_([A-Z]{3})_merged_result_cso\\.csv$", "\\1", basename(file)))
  
  # Read CSV
  data <- fread(file)
  
  #############################################
  # Data checks and preprocessing             #
  #############################################
  
  # FUA
  if (!("fua" %in% names(data))) {
    data$fua <- "unknown"
  }
  
  # Precipitation
  suppressWarnings(data$precipitation <- as.numeric(data$precipitation))
  data <- subset(data, !is.na(precipitation))
  
  # Population
  if (!("population" %in% names(data))) {
    stop("Population must be provided or present in the dataset.")
  } else {
    data$population <- as.numeric(data$population)
    data <- subset(data, !is.na(population))
  }
  
  # Area
  if (!("area" %in% names(data))) {
    stop("Area must be provided or present in the dataset.")
  }
  
  # Required columns
  required_cols <- c("cso_volume_per_event", "qdwf", "duration", "fua", "area", "population", "Cntry_name")
  if (!all(required_cols %in% names(data))) {
    message("Skipping ", country_code, " – missing columns")
    next
  }
  
  # Filter valid rows
  data <- data[is.finite(cso_volume_per_event) & !is.na(cso_volume_per_event)]
  if (nrow(data) == 0) {
    message("Skipping ", country_code, " – no valid data")
    next
  }
  
  # Calculations
  data[, vdwf := qdwf * 3 * 3600] # Dry Weather Flow in m³
  
  summary_list[[country_code]] <- data.table(
    country_name = unique(data$Cntry_name),
    fua_count = as.integer(uniqueN(data$fua)),
    area_total = sum(unique(data$area), na.rm = TRUE),
    population_total = sum(unique(data$population), na.rm = TRUE),
    cso_event_count = nrow(data),
    Vcso_m3 = sum(data$cso_volume_per_event, na.rm = TRUE),
    Vdwf_m3 = sum(data$vdwf, na.rm = TRUE),
    duration_hours = sum(data$duration, na.rm = TRUE) * 3 # each time step = 3h
  )
}

#############################################
# Summary tables                            #
#############################################

if (length(summary_list) > 0) {
  summary_dt <- rbindlist(summary_list)
  
  # Absolute table
  absolute <- summary_dt[, .(
    Country = country_name,
    FUAs = fua_count,
    `CSO Events (total)` = cso_event_count,
    `Vcso (total, m³)` = Vcso_m3,
    `Vdwf (total, m³)` = Vdwf_m3,
    `Duration (h)` = duration_hours
  )]
  
  # Normalized values
  summary_dt[, `CSO Events per FUA` := cso_event_count / fua_count]
  summary_dt[, `Vcso per FUA (m³)` := Vcso_m3 / fua_count]
  summary_dt[, `Vdwf per FUA (m³)` := Vdwf_m3 / fua_count]
  summary_dt[, `Duration per FUA (h)` := duration_hours / fua_count]
  
  summary_dt[, `CSO Events per km²` := cso_event_count / area_total]
  summary_dt[, `Vcso per km² (m³)` := Vcso_m3 / area_total]
  summary_dt[, `Vdwf per km² (m³)` := Vdwf_m3 / area_total]
  summary_dt[, `Duration per km² (h)` := duration_hours / area_total]
  
  summary_dt[, `CSO Events per 10,000 inhabitants` := cso_event_count / (population_total / 10000)]
  summary_dt[, `Vcso per 10,000 inhabitants (m³)` := Vcso_m3 / (population_total / 10000)]
  summary_dt[, `Vdwf per 10,000 inhabitants (m³)` := Vdwf_m3 / (population_total / 10000)]
  summary_dt[, `Duration per 10,000 inhabitants (h)` := duration_hours / (population_total / 10000)]
  
  normalized <- summary_dt[, .(
    Country = country_name,
    FUAs = fua_count,
    `CSO Events per FUA`,
    `Vcso per FUA (m³)`,
    `Vdwf per FUA (m³)`,
    `Duration per FUA (h)`,
    `CSO Events per km²`,
    `Vcso per km² (m³)`,
    `Vdwf per km² (m³)`,
    `Duration per km² (h)`,
    `CSO Events per 10,000 inhabitants`,
    `Vcso per 10,000 inhabitants (m³)`,
    `Vdwf per 10,000 inhabitants (m³)`,
    `Duration per 10,000 inhabitants (h)`
  )]
  
  # Export CSV
  fwrite(absolute, "absolute_cso_summary.csv")
  fwrite(normalized, "normalized_cso_summary.csv")
  
  # Export Excel
  wb <- createWorkbook()
  addWorksheet(wb, "Absolute")
  writeData(wb, "Absolute", absolute)
  addWorksheet(wb, "Normalized")
  writeData(wb, "Normalized", normalized)
  saveWorkbook(wb, "cso_summary.xlsx", overwrite = TRUE)
  
  #############################################
  # Flextable: formatted Word export         #
  #############################################
  
  format_table <- function(data, title) {
    ft <- flextable(data)
    ft <- color(ft, color = "white", part = "header")
    ft <- bg(ft, bg = "#2E75B6", part = "header")
    ft <- bold(ft, part = "header")
    ft <- fontsize(ft, size = 9, part = "all")
    ft <- padding(ft, padding = 2)
    ft <- align(ft, align = "center", part = "all")
    
    # Wrap column names
    header_labels <- names(data)
    names(header_labels) <- header_labels
    header_labels <- gsub(" ", "\n", header_labels)
    ft <- set_header_labels(ft, values = header_labels)
    
    # Color scale for numeric columns
    numeric_cols <- setdiff(names(data), c("Country", "FUAs"))
    for (col in numeric_cols) {
      ft <- colformat_double(ft, j = col, digits = 2, big.mark = ",")
      ft <- bg(
        ft, j = col,
        bg = col_numeric(
          palette = c("#EAF3FB", "#6FAFE7"), # light to medium blue
          domain = range(data[[col]], na.rm = TRUE)
        )
      )
    }
    
    ft <- autofit(ft)
    ft <- fit_to_width(ft, max_width = 9)
    return(ft)
  }
  
  ft_absolute <- format_table(absolute, "Absolute CSO values")
  ft_normalized <- format_table(normalized, "Normalized CSO values")
  
  doc <- read_docx()
  doc <- body_end_section_landscape(doc)
  doc <- body_add_par(doc, "Absolute CSO values", style = "heading 1")
  doc <- body_add_flextable(doc, value = ft_absolute)
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, "Normalized CSO values", style = "heading 1")
  doc <- body_add_flextable(doc, value = ft_normalized)
  
  print(doc, target = "C:/Users/sim06/Git/cso-modell/CSO_tables.docx")
  message("Word file with formatted tables successfully created.")
  
} else {
  message("No valid data found.")
}

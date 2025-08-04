##########################################################################################################
###                                                                                                    ###
### Hydrological Modeling of Combined Sewer Overflows (CSOs)                                           ###
### An R-Based Implementation of Quaranta et al.`s Approach                                            ###
### by Nina Kleemeyer, B.Sc.                                                                           ###
###                                                                                                    ###
### Supervisor: Univ. Prof. Dipl.-Ing. Dr. techn. Matthias Zessner, TU Wien                            ###
###                                                                                                    ###
### 2/5 Formula Shiny Validation                                                                 ###
##########################################################################################################


#################################################
# Shiny App: Model Data vs. Validation Data     #
# Purpose:                                      #
# 1. Load model and validation data             #
# 2. Allow variable selection for comparison    #
# 3. Visualize time-series comparison via Shiny # 
#################################################

# Libraries
library(shiny)
library(tidyverse)
library(data.table)
library(readxl)

# Parameters
model_file <- "path_model_data"
validation_file <- "path_validation_data"


# Load Data
cat("Loading model and validation data...\n")

# Load model input data
model_data <- as.data.table(read_xlsx(model_file, sheet = model_sheet))

# Load validation input data
validation_data <- as.data.table(read_xlsx(validation_file, sheet = validation_sheet))

# Rename columns 
rename_columns <- c(
  "surface storage St" = "St", "surface losses" = "surface_losses",
  "Runoff Rt" = "Rt", "Network flow Ft" = "Ft", "worst case overflow Et" = "worst_case_overflow",
  "a" = "scenario_a", "b" = "scenario_b", "c" = "scenario_c", "d" = "scenario_d",
  "Network overflow E't" = "E_t", "virtual volume W*(case a)" = "virtual_volume_a",
  "virtual volume W*(case d)" = "virtual_volume_d", "virtual volume W*(t1), case b" = "virtual_volume_b_t1",
  "virtual volume W*(t1), case c" = "virtual_volume_c_t1", "virtual volume W*(case b)" = "virtual_volume_b",
  "virtual volume W*(case c)" = "virtual_volume_c", "tank volume W" = "w",
  "t2'" = "t2_1", "t2''" = "t2_2", "t2''' " = "t2_3", "Tank overflow E''t" = "E_2_t",
  "spill duration (count every time there is a spill and then multuply by the time step)" = "spill_duration",
  "rain event (1= start, 2= rain continues,0 = no rain)" = "rain_event",
  "frequency overflow (count every time it starts)" = "frequency_overflow",
  "cumulate rain per event" = "cumulative_rain_per_event", "cso volume per event" = "cso_volume_per_event",
  "total rain volume per event" = "total_rain_volume_per_event", "CSO total volume per event" = "cso_total_volume_per_event"
)

setnames(validation_data, old = names(rename_columns), new = rename_columns, skip_absent = TRUE)
setnames(model_data, old = names(rename_columns), new = rename_columns, skip_absent = TRUE)

cat("Model data rows:", nrow(model_data), "\n")
cat("Validation data rows:", nrow(validation_data), "\n")

# Define Variables
variables <- intersect(names(model_data), names(validation_data))
variables <- variables[!variables %in% c("DateValue")] # Remove date column from variables

# UI 
ui <- fluidPage(
  titlePanel("Model Data vs. Validation Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable_select", "Select variable:", choices = variables, selected = "Rt"),
      checkboxGroupInput("quelle_select", "Source:", choices = c("Model Data", "Validation Data"), selected = c("Model Data", "Validation Data")),
      sliderInput("time_range", "Select time range:", 
                  min = 1, 
                  max = min(1000, nrow(model_data)), 
                  value = c(1, min(1000, nrow(model_data))),
                  step = 1)
    ),
    mainPanel(plotOutput("compare_plot", height = "800px"))
  )
)

# Server
server <- function(input, output, session) {
  
  observe({
    updateSliderInput(session, "time_range", max = min(1000, nrow(model_data)), value = c(1, min(1000, nrow(model_data))))
  })
  
  output$vergleich_plot <- renderPlot({
    req(input$variable_select, input$quelle_select)
    var <- input$variable_select
    
    # Ensure variable exists in both datasets
    if (!(var %in% names(model_data)) | !(var %in% names(validation_data))) {
      stop(paste("Variable", var, "not found in one of the datasets."))
    }
    
    t_min <- input$time_range[1]
    t_max <- input$time_range[2]
    
    df_plot <- tibble(
      datetime = model_data$DateValue[t_min:t_max],
      `Model Data` = model_data[[var]][t_min:t_max],
      `Validation Data` = validation_data[[var]][t_min:t_max]
    ) %>%
      pivot_longer(-datetime, names_to = "quelle", values_to = "value") %>%
      filter(quelle %in% input$quelle_select)
    
    ggplot(df_plot, aes(x = datetime, y = value, color = quelle)) +
      geom_line(linewidth = 0.8) +
      labs(
        title = paste("Comparison:", var),
        x = "Time",
        y = var,
        color = "Source"
      ) +
      theme_minimal()
  })
}

#Run shiny
shinyApp(ui, server)

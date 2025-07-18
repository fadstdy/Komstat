# Main App File - app.R
# Dashboard Analisis Statistik
# Author: Dashboard Analytics Team

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(officer)
library(car)       # For VIF and ncvTest
library(lmtest)    # For bptest (Breusch-Pagan test)
library(gridExtra) # For combining plots
library(readr)     # For read_csv
library(data.table) # For fread

# Source utility functions
source("utils/helpers.R")
source("utils/data_processing.R")
source("setup_data.R")

# Source module UI and server functions
source("modules/beranda_module.R")
source("modules/manajemen_data_module.R")
source("modules/eksplorasi_data_module.R")
source("modules/uji_asumsi_module.R")
source("modules/statistik_inferensia_module.R")
source("modules/regresi_linear_module.R")

# Data loading
# This will be called once when the app starts
loaded_data <- setup_data()
global_sovi_data <- loaded_data$sovi_data
global_distance_matrix <- loaded_data$distance_matrix

# Prepare initial data (e.g., addREGION variable)
if (!is.null(global_sovi_data)) {
  global_sovi_data <- prepare_data(global_sovi_data)
} else {
  stop("Failed to load main data. Please check setup_data.R and data files.")
}


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Statistik"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen_data", icon = icon("cogs")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi_data", icon = icon("chart-bar")),
      menuItem("Uji Asumsi", tabName = "uji_asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "statistik_inferensia", icon = icon("flask")),
      menuItem("Regresi Linear", tabName = "regresi_linear", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    # Link to custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
    ),
    
    tabItems(
      tabItem(tabName = "beranda",
              berandaUI("beranda_tab")),
      tabItem(tabName = "manajemen_data",
              manajemenDataUI("manajemen_data_tab")),
      tabItem(tabName = "eksplorasi_data",
              eksplorasiDataUI("eksplorasi_data_tab")),
      tabItem(tabName = "uji_asumsi",
              ujiAsumsiUI("uji_asumsi_tab")),
      tabItem(tabName = "statistik_inferensia",
              statistikInferensiaUI("statistik_inferensia_tab")),
      tabItem(tabName = "regresi_linear",
              regresiLinearUI("regresi_linear_tab"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store the current data (can be modified by management module)
  values <- reactiveValues(current_data = global_sovi_data)
  
  # Call module servers
  berandaServer("beranda_tab", values)
  manajemenDataServer("manajemen_data_tab", values)
  eksplorasiDataServer("eksplorasi_data_tab", values)
  ujiAsumsiServer("uji_asumsi_tab", values)
  statistikInferensiaServer("statistik_inferensia_tab", values)
  regresiLinearServer("regresi_linear_tab", values)
  
  # Optional: observer to update current_data if it changes in manajemen_data_module
  # This might already be handled implicitly by passing `values` reactive object.
  # If data transformations need to persist globally, ensure manajemen_data_module
  # updates values$current_data.
}

# Run the application
shinyApp(ui = ui, server = server)
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
library(readr)
library(shinycssloaders)
library(shinyWidgets)
library(officer)
library(flextable)
library(webshot)
library(leaflet)
library(corrplot)
library(car)
library(nortest)
library(lmtest)
library(VIM)
library(tidyr)
library(gridExtra)
library(moments)
library(tibble)

# Source all modules
source("modules/beranda_module.R")
source("modules/manajemen_data_module.R")
source("modules/eksplorasi_data_module.R")
source("modules/uji_asumsi_module.R")
source("modules/statistik_inferensia_module.R")
source("modules/regresi_linear_module.R")
source("utils/data_processing.R")
source("utils/helpers.R")

# Load and prepare data
data_file <- if (file.exists("data/Data1.txt")) {
  "data/Data1.txt"
} else if (file.exists("Data1.txt")) {
  "Data1.txt"
} else {
  "data/sample_data.csv"
}

data_raw <- read_csv(data_file, show_col_types = FALSE)
data_processed <- prepare_data(data_raw)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Potret Sosial Ekonomi Indonesia"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-line")),
      menuItem("Uji Asumsi", tabName = "asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "inferensia", icon = icon("calculator")),
      menuItem("Regresi Linear", tabName = "regresi", icon = icon("line-chart"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      # Beranda Tab
      tabItem(tabName = "beranda",
              berandaUI("beranda")
      ),
      
      # Manajemen Data Tab
      tabItem(tabName = "manajemen",
              manajemenDataUI("manajemen")
      ),
      
      # Eksplorasi Data Tab
      tabItem(tabName = "eksplorasi",
              eksplorasiDataUI("eksplorasi")
      ),
      
      # Uji Asumsi Tab
      tabItem(tabName = "asumsi",
              ujiAsumsiUI("asumsi")
      ),
      
      # Statistik Inferensia Tab
      tabItem(tabName = "inferensia",
              statistikInferensiaUI("inferensia")
      ),
      
      # Regresi Linear Tab
      tabItem(tabName = "regresi",
              regresiLinearUI("regresi")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values for sharing data between modules
  values <- reactiveValues(
    original_data = data_processed,
    processed_data = data_processed,
    current_data = data_processed
  )
  
  # Call module servers
  berandaServer("beranda", values)
  manajemenDataServer("manajemen", values)
  eksplorasiDataServer("eksplorasi", values)
  ujiAsumsiServer("asumsi", values)
  statistikInferensiaServer("inferensia", values)
  regresiLinearServer("regresi", values)
}

# Run the application
shinyApp(ui = ui, server = server)
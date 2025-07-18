# Package Installation Script for Dashboard Analisis Statistik
# install_packages.R

# Function to install packages if not already installed
install_if_missing <- function(pname) {
  if (!require(pname, character.only = TRUE)) {
    install.packages(pname, dependencies = TRUE)
    library(pname, character.only = TRUE)
  }
}

# List of all packages required by the dashboard
packages_to_install <- c(
  "shiny",
  "shinydashboard",
  "DT",
  "plotly",
  "ggplot2",
  "dplyr",
  "officer",
  "car",       # For VIF and ncvTest in linear regression, and Levene's test
  "lmtest",    # For Breusch-Pagan test (alternative to ncvTest)
  "gridExtra", # For combining plots
  "readr",     # For reading data from URL
  "data.table" # For fread (fast data reading)
)

# Install and load all packages
for (package in packages_to_install) {
  install_if_missing(package)
}

# Ensure specific functions are available if they are part of a larger package
# For example, some functions like `case_when` are from `dplyr`, which is loaded.
# `leveneTest` from `car` and `bptest` from `lmtest` will be loaded if the packages are installed.
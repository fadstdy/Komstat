# Package Installation Script for Dashboard Analisis Statistik
# install_packages.R

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# List of required packages
required_packages <- c(
  # Core Shiny packages
  "shiny",
  "shinydashboard",
  "shinycssloaders",
  "shinyWidgets",
  
  # Data manipulation
  "dplyr",
  "tidyr",
  "readr",
  "tibble",
  
  # Visualization
  "ggplot2",
  "plotly",
  "corrplot",
  "gridExtra",
  "leaflet",
  "VIM",
  
  # Tables
  "DT",
  "flextable",
  
  # Statistical tests
  "car",
  "nortest",
  "lmtest",
  "moments",
  
  # Document generation
  "officer",
  "webshot",
  
  # Optional packages (install if available)
  "multcomp",
  "glmnet",
  "tseries"
)

# Install packages
cat("Installing required packages...\n")
install_if_missing(required_packages)

# Install optional packages (ignore if failed)
optional_packages <- c("multcomp", "glmnet", "tseries")
for (pkg in optional_packages) {
  tryCatch({
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }, error = function(e) {
    cat("Optional package", pkg, "could not be installed. Some features may be limited.\n")
  })
}

# Install webshot dependencies if needed
if (require("webshot", quietly = TRUE)) {
  if (!webshot::is_phantomjs_installed()) {
    cat("Installing PhantomJS for webshot...\n")
    webshot::install_phantomjs()
  }
}

cat("Package installation completed!\n")
cat("You can now run the dashboard with: shiny::runApp()\n")
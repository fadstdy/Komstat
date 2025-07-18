# Data Setup Script
# setup_data.R

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Function to setup and load data
setup_data <- function() {
  # URLs to your data files from metadata.pdf
  sovi_data_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv"
  distance_data_url <- "https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv"
  
  # Define local file paths
  sovi_data_path <- "data/sovi_data.csv"
  distance_data_path <- "data/distance.csv"
  
  # Download data if not already present
  if (!file.exists(sovi_data_path)) {
    tryCatch({
      download.file(sovi_data_url, destfile = sovi_data_path, method = "auto")
      message("sovi_data.csv downloaded successfully.")
    }, error = function(e) {
      warning("Failed to download sovi_data.csv: ", e$message)
    })
  } else {
    message("sovi_data.csv already exists.")
  }
  
  if (!file.exists(distance_data_path)) {
    tryCatch({
      download.file(distance_data_url, destfile = distance_data_path, method = "auto")
      message("distance.csv downloaded successfully.")
    }, error = function(e) {
      warning("Failed to download distance.csv: ", e$message)
    })
  } else {
    message("distance.csv already exists.")
  }
  
  # Load data
  if (file.exists(sovi_data_path)) {
    sovi_data <- readr::read_csv(sovi_data_path, show_col_types = FALSE)
    message("sovi_data loaded.")
  } else {
    sovi_data <- NULL
    warning("sovi_data.csv not found locally and could not be downloaded.")
  }
  
  if (file.exists(distance_data_path)) {
    distance_matrix <- data.table::fread(distance_data_path)
    # Remove the first column (index column) if it's unnamed or just an index
    if(names(distance_matrix)[1] == "V1" || names(distance_matrix)[1] == "") { # Check if first column is an unnamed index from fread
      distance_matrix <- distance_matrix[,-1]
    }
    message("distance_matrix loaded.")
  } else {
    distance_matrix <- NULL
    warning("distance.csv not found locally and could not be downloaded.")
  }
  
  # Return loaded data as a list
  list(sovi_data = sovi_data, distance_matrix = distance_matrix)
}

# Run setup
# loaded_data <- setup_data()
# global_sovi_data <- loaded_data$sovi_data
# global_distance_matrix <- loaded_data$distance_matrix

# Note: The actual loading into global environment variables like global_sovi_data
# will typically happen in app.R by sourcing this script.
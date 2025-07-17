# Data Setup Script
# setup_data.R

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# Function to copy your data file
setup_data <- function() {
  # Path to your uploaded data file
  data_file <- "Data1.txt"
  
  # Check if data file exists
  if (!file.exists(data_file)) {
    cat("Data file 'Data1.txt' not found in current directory.\n")
    cat("Please make sure to place your data file in the project directory.\n")
    cat("The file should contain the socio-economic data with the following columns:\n")
    cat("DISTRICTCODE, CHILDREN, FEMALE, ELDERLY, FHEAD, FAMILYSIZE, NOELECTRIC, LOWEDU, GROWTH, POVERTY, ILLITERATE, NOTRAINING, DPRONE, RENTED, NOSEWER, TAPWATER, POPULATION\n")
    
    # Create a sample data file for testing
    sample_data <- data.frame(
      DISTRICTCODE = c("1101", "1102", "1103", "1104", "1105"),
      CHILDREN = c(7.999956086, 13.51717522, 9.43677942, 11.18920899, 11.68376732),
      FEMALE = c(48.77590901, 49.68611677, 50.77554271, 50.09882757, 50.048859),
      ELDERLY = c(2.184700509, 2.296480805, 4.903403386, 2.735836322, 2.756701347),
      FHEAD = c(13.11146752, 13.16721954, 20.73676104, 17.78283418, 19.47374379),
      FAMILYSIZE = c(4.058094984, 4.479568758, 4.236907731, 4.252137015, 4.296572234),
      NOELECTRIC = c(1.425643767, 1.07433655, 0.50242042, 2.01709612, 0.600965249),
      LOWEDU = c(25.65208668, 28.7247149, 29.7794412, 16.7915821, 32.83977649),
      GROWTH = c(1.249634166, 2.287937103, 1.521355784, 2.109057338, 2.022934328),
      POVERTY = c(20.2, 22.11, 14.07, 14.86, 15.25),
      ILLITERATE = c(5.019033186, 10.98267021, 7.717708677, 6.673470052, 6.648225739),
      NOTRAINING = c(92.71585138, 97.90033172, 98.76228546, 99.87905623, 99.75611513),
      DPRONE = c(48.81047848, 73.08504222, 77.13620361, 94.28079454, 82.23653745),
      RENTED = c(4.882829903, 6.67973462, 3.337245122, 4.046491606, 2.324624139),
      NOSEWER = c(22.89049274, 20.01281665, 31.78817662, 43.54590739, 26.79337703),
      TAPWATER = c(5.595651787, 13.40470446, 6.98070999, 20.30420433, 12.98558301),
      POPULATION = c(91372, 119490, 231893, 208481, 419594)
    )
    
    write.csv(sample_data, "data/sample_data.csv", row.names = FALSE)
    cat("Sample data created in data/sample_data.csv for testing purposes.\n")
    
    return(FALSE)
  }
  
  # Copy data file to data directory
  file.copy(data_file, "data/Data1.txt", overwrite = TRUE)
  cat("Data file copied to data/Data1.txt\n")
  
  return(TRUE)
}

# Run data setup
if (setup_data()) {
  cat("Data setup completed successfully!\n")
} else {
  cat("Please place your data file 'Data1.txt' in the project directory and run this script again.\n")
}

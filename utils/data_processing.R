# Data Processing Utilities
# utils/data_processing.R

# Function to prepare and clean data
prepare_data <- function(data) {
  # Convert DISTRICTCODE to factor
  if ("DISTRICTCODE" %in% names(data)) {
    data$DISTRICTCODE <- as.factor(data$DISTRICTCODE)
  }
  
  # Create region variable based on DISTRICTCODE
  if ("DISTRICTCODE" %in% names(data)) {
    data$REGION <- case_when(
      substr(data$DISTRICTCODE, 1, 2) == "11" ~ "Aceh",
      substr(data$DISTRICTCODE, 1, 2) == "12" ~ "Sumatera Utara",
      substr(data$DISTRICTCODE, 1, 2) %in% c("13", "14", "15", "16", "17", "18", "19") ~ "Sumatera",
      substr(data$DISTRICTCODE, 1, 2) %in% c("31", "32", "33", "34", "35", "36") ~ "Jawa, Bali, dan Nusa Tenggara",
      substr(data$DISTRICTCODE, 1, 2) %in% c("51", "52", "53") ~ "Jawa, Bali, dan Nusa Tenggara",
      substr(data$DISTRICTCODE, 1, 2) %in% c("61", "62", "63", "64", "65") ~ "Kalimantan",
      substr(data$DISTRICTCODE, 1, 2) %in% c("71", "72", "73", "74", "75", "76") ~ "Sulawesi",
      substr(data$DISTRICTCODE, 1, 2) %in% c("81", "82") ~ "Maluku dan Papua",
      substr(data$DISTRICTCODE, 1, 2) %in% c("91", "92") ~ "Maluku dan Papua",
      TRUE ~ "Lainnya"
    )
    data$REGION <- as.factor(data$REGION)
  }
  
  return(data)
}

# Function to get numeric variables
get_numeric_vars <- function(data) {
  names(data)[sapply(data, is.numeric)]
}

# Function to get categorical variables
get_categorical_vars <- function(data) {
  names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
}

# Function to calculate basic descriptive statistics for a numeric variable
calculate_basic_stats <- function(data, var_name) {
  if (!is.numeric(data[[var_name]])) {
    return(NULL) # Only for numeric variables
  }
  
  vec <- na.omit(data[[var_name]])
  if (length(vec) == 0) {
    return(list(
      Mean = NA, Median = NA, StdDev = NA, Min = NA, Max = NA,
      Q1 = NA, Q3 = NA, IQR = NA, Skewness = NA, Kurtosis = NA, N = 0, Missing = sum(is.na(data[[var_name]]))
    ))
  }
  
  # Ensure moments package is installed for skewness/kurtosis
  if (!requireNamespace("moments", quietly = TRUE)) {
    warning("Package 'moments' needed for skewness and kurtosis. Please install it.")
    skew <- NA
    kurt <- NA
  } else {
    skew <- moments::skewness(vec)
    kurt <- moments::kurtosis(vec)
  }
  
  qs <- quantile(vec, probs = c(0.25, 0.75), na.rm = TRUE)
  
  list(
    Mean = mean(vec),
    Median = median(vec),
    StdDev = sd(vec),
    Min = min(vec),
    Max = max(vec),
    Q1 = qs[1],
    Q3 = qs[2],
    IQR = IQR(vec),
    Skewness = skew,
    Kurtosis = kurt,
    N = length(vec),
    Missing = sum(is.na(data[[var_name]]))
  )
}
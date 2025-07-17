# Data Processing Utilities
# utils/data_processing.R

# Function to prepare and clean data
prepare_data <- function(data) {
  # Convert DISTRICTCODE to factor
  data$DISTRICTCODE <- as.factor(data$DISTRICTCODE)
  
  # Create region variable based on DISTRICTCODE
  data$REGION <- case_when(
    substr(data$DISTRICTCODE, 1, 2) == "11" ~ "Aceh",
    substr(data$DISTRICTCODE, 1, 2) == "12" ~ "Sumatera Utara",
    substr(data$DISTRICTCODE, 1, 2) == "13" ~ "Sumatera Barat",
    substr(data$DISTRICTCODE, 1, 2) == "14" ~ "Riau",
    substr(data$DISTRICTCODE, 1, 2) == "15" ~ "Jambi",
    substr(data$DISTRICTCODE, 1, 2) == "16" ~ "Sumatera Selatan",
    substr(data$DISTRICTCODE, 1, 2) == "17" ~ "Bengkulu",
    substr(data$DISTRICTCODE, 1, 2) == "18" ~ "Lampung",
    substr(data$DISTRICTCODE, 1, 2) == "19" ~ "Kepulauan Bangka Belitung",
    substr(data$DISTRICTCODE, 1, 2) == "21" ~ "Kepulauan Riau",
    substr(data$DISTRICTCODE, 1, 2) == "31" ~ "DKI Jakarta",
    substr(data$DISTRICTCODE, 1, 2) == "32" ~ "Jawa Barat",
    substr(data$DISTRICTCODE, 1, 2) == "33" ~ "Jawa Tengah",
    substr(data$DISTRICTCODE, 1, 2) == "34" ~ "DI Yogyakarta",
    substr(data$DISTRICTCODE, 1, 2) == "35" ~ "Jawa Timur",
    substr(data$DISTRICTCODE, 1, 2) == "36" ~ "Banten",
    substr(data$DISTRICTCODE, 1, 2) == "51" ~ "Bali",
    substr(data$DISTRICTCODE, 1, 2) == "52" ~ "Nusa Tenggara Barat",
    substr(data$DISTRICTCODE, 1, 2) == "53" ~ "Nusa Tenggara Timur",
    substr(data$DISTRICTCODE, 1, 2) == "61" ~ "Kalimantan Barat",
    substr(data$DISTRICTCODE, 1, 2) == "62" ~ "Kalimantan Tengah",
    substr(data$DISTRICTCODE, 1, 2) == "63" ~ "Kalimantan Selatan",
    substr(data$DISTRICTCODE, 1, 2) == "64" ~ "Kalimantan Timur",
    substr(data$DISTRICTCODE, 1, 2) == "65" ~ "Kalimantan Utara",
    substr(data$DISTRICTCODE, 1, 2) == "71" ~ "Sulawesi Utara",
    substr(data$DISTRICTCODE, 1, 2) == "72" ~ "Sulawesi Tengah",
    substr(data$DISTRICTCODE, 1, 2) == "73" ~ "Sulawesi Selatan",
    substr(data$DISTRICTCODE, 1, 2) == "74" ~ "Sulawesi Tenggara",
    substr(data$DISTRICTCODE, 1, 2) == "75" ~ "Gorontalo",
    substr(data$DISTRICTCODE, 1, 2) == "76" ~ "Sulawesi Barat",
    substr(data$DISTRICTCODE, 1, 2) == "81" ~ "Maluku",
    substr(data$DISTRICTCODE, 1, 2) == "82" ~ "Maluku Utara",
    substr(data$DISTRICTCODE, 1, 2) == "91" ~ "Papua Barat",
    substr(data$DISTRICTCODE, 1, 2) == "94" ~ "Papua",
    TRUE ~ "Unknown"
  )
  
  # Create island grouping
  data$ISLAND <- case_when(
    substr(data$DISTRICTCODE, 1, 2) %in% c("11", "12", "13", "14", "15", "16", "17", "18", "19", "21") ~ "Sumatera",
    substr(data$DISTRICTCODE, 1, 2) %in% c("31", "32", "33", "34", "35", "36") ~ "Jawa",
    substr(data$DISTRICTCODE, 1, 2) %in% c("51", "52", "53") ~ "Nusa Tenggara",
    substr(data$DISTRICTCODE, 1, 2) %in% c("61", "62", "63", "64", "65") ~ "Kalimantan",
    substr(data$DISTRICTCODE, 1, 2) %in% c("71", "72", "73", "74", "75", "76") ~ "Sulawesi",
    substr(data$DISTRICTCODE, 1, 2) %in% c("81", "82") ~ "Maluku",
    substr(data$DISTRICTCODE, 1, 2) %in% c("91", "94") ~ "Papua",
    TRUE ~ "Unknown"
  )
  
  # Convert categorical variables to factors
  data$REGION <- as.factor(data$REGION)
  data$ISLAND <- as.factor(data$ISLAND)
  
  return(data)
}

# Function to create categorical variables
create_categorical_var <- function(data, var_name, breaks, labels) {
  if (var_name %in% names(data)) {
    if (length(breaks) == length(labels) + 1) {
      data[[paste0(var_name, "_CAT")]] <- cut(data[[var_name]], 
                                              breaks = breaks, 
                                              labels = labels,
                                              include.lowest = TRUE)
    } else {
      # For quantile-based categorization
      data[[paste0(var_name, "_CAT")]] <- cut(data[[var_name]], 
                                              breaks = quantile(data[[var_name]], 
                                                                probs = seq(0, 1, length.out = length(labels) + 1),
                                                                na.rm = TRUE),
                                              labels = labels,
                                              include.lowest = TRUE)
    }
  }
  return(data)
}

# Function to get numeric variables
get_numeric_vars <- function(data) {
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  return(numeric_vars[!numeric_vars %in% c("DISTRICTCODE")])
}

# Function to get categorical variables
get_categorical_vars <- function(data) {
  categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  return(categorical_vars)
}

# Function to calculate basic statistics
calculate_basic_stats <- function(data, var_name) {
  if (var_name %in% names(data) && is.numeric(data[[var_name]])) {
    stats <- data %>%
      summarise(
        Mean = mean(.data[[var_name]], na.rm = TRUE),
        Median = median(.data[[var_name]], na.rm = TRUE),
        StdDev = sd(.data[[var_name]], na.rm = TRUE),
        Min = min(.data[[var_name]], na.rm = TRUE),
        Max = max(.data[[var_name]], na.rm = TRUE),
        Q1 = quantile(.data[[var_name]], 0.25, na.rm = TRUE),
        Q3 = quantile(.data[[var_name]], 0.75, na.rm = TRUE),
        IQR = IQR(.data[[var_name]], na.rm = TRUE),
        Skewness = moments::skewness(.data[[var_name]], na.rm = TRUE),
        Kurtosis = moments::kurtosis(.data[[var_name]], na.rm = TRUE),
        N = sum(!is.na(.data[[var_name]])),
        Missing = sum(is.na(.data[[var_name]]))
      )
    return(stats)
  }
  return(NULL)
}

# Function to detect outliers using IQR method
detect_outliers <- function(data, var_name) {
  if (var_name %in% names(data) && is.numeric(data[[var_name]])) {
    Q1 <- quantile(data[[var_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[var_name]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    outliers <- data[[var_name]] < lower_bound | data[[var_name]] > upper_bound
    return(sum(outliers, na.rm = TRUE))
  }
  return(0)
}
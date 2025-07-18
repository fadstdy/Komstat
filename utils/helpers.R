# Helper Functions
# utils/helpers.R

# Function to show safe notification
show_notification <- function(message, type = "default") {
  # Valid types for showNotification: "default", "message", "warning", "error"
  valid_types <- c("default", "message", "warning", "error")
  
  # Convert common types to valid ones
  if (type == "success") type <- "message"
  if (type == "info") type <- "message"
  if (type == "danger") type <- "error"
  
  # Ensure type is valid
  if (!type %in% valid_types) type <- "default"
  
  showNotification(message, type = type)
}

# Function to create download handler for plots
create_plot_download <- function(plot_obj, filename) {
  downloadHandler(
    filename = function() {
      paste0(filename, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot_obj, width = 10, height = 6, dpi = 300)
    }
  )
}

# Function to create download handler for tables
create_table_download <- function(table_data, filename) {
  downloadHandler(
    filename = function() {
      paste0(filename, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(table_data, file, row.names = FALSE)
    }
  )
}

# Function to create Word document with results
create_word_report <- function(title, content_list, filename) {
  doc <- officer::read_docx()
  
  # Add title
  doc <- doc %>%
    officer::body_add_par(title, style = "heading 1") %>%
    officer::body_add_par(" ")
  
  # Add content
  for (item in content_list) {
    if (item$type == "text") {
      doc <- doc %>%
        officer::body_add_par(item$content, style = "Normal")
    } else if (item$type == "table") {
      doc <- doc %>%
        officer::body_add_flextable(item$content)
    } else if (item$type == "plot") {
      temp_file <- tempfile(fileext = ".png")
      ggsave(temp_file, item$content, width = 8, height = 6, dpi = 300)
      doc <- doc %>%
        officer::body_add_img(temp_file, width = 6, height = 4)
      unlink(temp_file)
    }
    doc <- doc %>% officer::body_add_par(" ")
  }
  
  print(doc, target = filename)
}

# Function to interpret correlation coefficient
interpret_correlation <- function(r) {
  abs_r <- abs(r)
  if (abs_r >= 0.8) {
    strength <- "sangat kuat"
  } else if (abs_r >= 0.6) {
    strength <- "kuat"
  } else if (abs_r >= 0.4) {
    strength <- "sedang"
  } else if (abs_r >= 0.2) {
    strength <- "lemah"
  } else {
    strength <- "sangat lemah"
  }
  
  direction <- ifelse(r > 0, "positif", "negatif")
  return(paste("Korelasi", direction, strength))
}

# Function to interpret normality test
interpret_normality <- function(p_value, alpha = 0.05) {
  if (p_value > alpha) {
    return(paste0("Data berdistribusi normal (p-value = ", 
                  round(p_value, 4), " > ", alpha, ")"))
  } else {
    return(paste0("Data tidak berdistribusi normal (p-value = ", 
                  round(p_value, 4), " < ", alpha, ")"))
  }
}

# Function to interpret homogeneity test
interpret_homogeneity <- function(p_value, alpha = 0.05) {
  if (p_value > alpha) {
    return(paste0("Variansi homogen (p-value = ", 
                  round(p_value, 4), " > ", alpha, ")"))
  } else {
    return(paste0("Variansi tidak homogen (p-value = ", 
                  round(p_value, 4), " < ", alpha, ")"))
  }
}

# Function to interpret t-test
interpret_t_test <- function(p_value, alternative, alpha = 0.05) {
  if (p_value < alpha) {
    if (alternative == "two.sided") {
      return(paste0("Terdapat perbedaan yang signifikan (p-value = ", 
                    round(p_value, 4), " < ", alpha, ")"))
    } else if (alternative == "greater") {
      return(paste0("Rata-rata kelompok pertama signifikan lebih besar (p-value = ", 
                    round(p_value, 4), " < ", alpha, ")"))
    } else {
      return(paste0("Rata-rata kelompok pertama signifikan lebih kecil (p-value = ", 
                    round(p_value, 4), " < ", alpha, ")"))
    }
  } else {
    return(paste0("Tidak terdapat perbedaan yang signifikan (p-value = ", 
                  round(p_value, 4), " > ", alpha, ")"))
  }
}

# Function to interpret proportion test
interpret_prop_test <- function(p_value, alternative, alpha = 0.05) {
  if (p_value < alpha) {
    if (alternative == "two.sided") {
      return(paste0("Terdapat perbedaan proporsi yang signifikan (p-value = ", 
                    round(p_value, 4), " < ", alpha, ")"))
    } else if (alternative == "greater") {
      return(paste0("Proporsi kelompok pertama signifikan lebih besar (p-value = ", 
                    round(p_value, 4), " < ", alpha, ")"))
    } else {
      return(paste0("Proporsi kelompok pertama signifikan lebih kecil (p-value = ", 
                    round(p_value, 4), " < ", alpha, ")"))
    }
  } else {
    return(paste0("Tidak terdapat perbedaan proporsi yang signifikan (p-value = ", 
                  round(p_value, 4), " > ", alpha, ")"))
  }
}


# Function to interpret ANOVA
interpret_anova <- function(p_value, alpha = 0.05) {
  if (p_value < alpha) {
    return(paste0("Terdapat perbedaan rata-rata yang signifikan antar kelompok (p-value = ", 
                  round(p_value, 4), " < ", alpha, ")"))
  } else {
    return(paste0("Tidak terdapat perbedaan rata-rata yang signifikan antar kelompok (p-value = ", 
                  round(p_value, 4), " > ", alpha, ")"))
  }
}

# Function to interpret R-squared
interpret_r_squared <- function(r_squared) {
  if (r_squared >= 0.8) {
    return(paste0("Model sangat baik (R² = ", round(r_squared, 4), ")"))
  } else if (r_squared >= 0.6) {
    return(paste0("Model baik (R² = ", round(r_squared, 4), ")"))
  } else if (r_squared >= 0.4) {
    return(paste0("Model sedang (R² = ", round(r_squared, 4), ")"))
  } else if (r_squared >= 0.2) {
    return(paste0("Model lemah (R² = ", round(r_squared, 4), ")"))
  } else {
    return(paste0("Model sangat lemah (R² = ", round(r_squared, 4), ")"))
  }
}

# Function to format p-value
format_p_value <- function(p_value) {
  if (p_value < 0.001) {
    return("< 0.001")
  } else {
    return(sprintf("%.3f", p_value))
  }
}

# Function to create custom theme for plots
theme_custom <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
    )
}

# Function to create summary statistics table
create_summary_table <- function(data, var_name) {
  if (is.numeric(data[[var_name]])) {
    # Assuming calculate_basic_stats is defined elsewhere (e.g., data_processing.R)
    # If not, you might need to define it here or ensure it's sourced.
    stats <- calculate_basic_stats(data, var_name) 
    return(
      data.frame(
        Statistik = c("Mean", "Median", "Std Dev", "Min", "Max", "Q1", "Q3", "IQR", "Skewness", "Kurtosis", "N", "Missing"),
        Nilai = c(
          round(stats$Mean, 3),
          round(stats$Median, 3),
          round(stats$StdDev, 3),
          round(stats$Min, 3),
          round(stats$Max, 3),
          round(stats$Q1, 3),
          round(stats$Q3, 3),
          round(stats$IQR, 3),
          round(stats$Skewness, 3),
          round(stats$Kurtosis, 3),
          stats$N,
          stats$Missing
        )
      )
    )
  }
  return(NULL)
}

# NEW FUNCTION: interpret_regression_summary
interpret_regression_summary <- function(model_summary, alpha = 0.05) {
  f_p_value <- pf(model_summary$fstatistic[1], 
                  model_summary$fstatistic[2], 
                  model_summary$fstatistic[3], 
                  lower.tail = FALSE)
  adj_r_squared <- model_summary$adj.r.squared
  
  overall_significance <- ""
  if (f_p_value < alpha) {
    overall_significance <- paste0("Model regresi secara keseluruhan **signifikan secara statistik** (p-value F-test = ", 
                                   format_p_value(f_p_value), " < ", alpha, "), menunjukkan bahwa setidaknya satu variabel independen secara signifikan memprediksi variabel dependen.")
  } else {
    overall_significance <- paste0("Model regresi secara keseluruhan **tidak signifikan secara statistik** (p-value F-test = ", 
                                   format_p_value(f_p_value), " > ", alpha, "), menunjukkan bahwa variabel independen yang digunakan tidak secara signifikan memprediksi variabel dependen.")
  }
  
  r_squared_interpretation <- interpret_r_squared(adj_r_squared)
  
  return(paste0(overall_significance, "\n\n",
                "Model menjelaskan ", round(adj_r_squared * 100, 2), "% variasi dalam variabel dependen (Adjusted R-squared). Ini menunjukkan ", r_squared_interpretation, " dalam menjelaskan variabilitas variabel dependen."))
}

# Assuming get_numeric_vars and get_categorical_vars are defined elsewhere
# (e.g., in data_processing.R or sourced globally).
# Also assuming calculate_basic_stats is available.
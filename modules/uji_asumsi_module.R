# Uji Asumsi Module
# modules/uji_asumsi_module.R

# UI function for Uji Asumsi
ujiAsumsiUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Control Panel
    column(3,
           box(
             title = "Panel Kontrol Uji Asumsi",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Test type selection
             selectInput(ns("test_type"), 
                         "Jenis Uji Asumsi:",
                         choices = list(
                           "Uji Normalitas" = "normality",
                           "Uji Homogenitas" = "homogeneity",
                           "Uji Linearitas" = "linearity",
                           "Uji Independensi" = "independence"
                         )),
             
             # Variable selection
             selectInput(ns("test_variable"), 
                         "Pilih Variabel:",
                         choices = NULL),
             
             # Conditional inputs for homogeneity test
             conditionalPanel(
               condition = "input.test_type == 'homogeneity'",
               ns = ns,
               selectInput(ns("group_variable"), 
                           "Variabel Pengelompokan:",
                           choices = NULL)
             ),
             
             # Conditional inputs for linearity test
             conditionalPanel(
               condition = "input.test_type == 'linearity'",
               ns = ns,
               selectInput(ns("x_variable"), 
                           "Variabel X:",
                           choices = NULL),
               selectInput(ns("y_variable"), 
                           "Variabel Y:",
                           choices = NULL)
             ),
             
             # Significance level
             numericInput(ns("alpha"), 
                          "Tingkat Signifikansi (α):",
                          value = 0.05,
                          min = 0.01,
                          max = 0.1,
                          step = 0.01),
             
             # Normality test method
             conditionalPanel(
               condition = "input.test_type == 'normality'",
               ns = ns,
               selectInput(ns("normality_method"), 
                           "Metode Uji Normalitas:",
                           choices = list(
                             "Shapiro-Wilk Test" = "shapiro",
                             "Kolmogorov-Smirnov Test" = "ks",
                             "Anderson-Darling Test" = "ad",
                             "Jarque-Bera Test" = "jb"
                           ))
             ),
             
             # Homogeneity test method
             conditionalPanel(
               condition = "input.test_type == 'homogeneity'",
               ns = ns,
               selectInput(ns("homogeneity_method"), 
                           "Metode Uji Homogenitas:",
                           choices = list(
                             "Levene Test" = "levene",
                             "Bartlett Test" = "bartlett",
                             "Fligner-Killeen Test" = "fligner"
                           ))
             ),
             
             # Action buttons
             br(),
             actionButton(ns("run_test"), 
                          "Jalankan Uji", 
                          class = "btn-success"),
             br(), br(),
             
             # Download options
             h5("Download Hasil:"),
             downloadButton(ns("download_plot"), 
                            "Download Plot", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_results"), 
                            "Download Hasil", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_report"), 
                            "Download Laporan", 
                            class = "btn-info")
           )
    ),
    
    # Main Content
    column(9,
           # Test Results
           box(
             title = "Hasil Uji Statistik",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("test_results")))
           ),
           
           # Visualizations
           box(
             title = "Visualisasi Diagnostik",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             tabsetPanel(
               tabPanel("Plot Utama",
                        br(),
                        withSpinner(plotOutput(ns("main_plot"), height = "400px"))),
               
               tabPanel("Plot Diagnostik",
                        br(),
                        withSpinner(plotOutput(ns("diagnostic_plot"), height = "400px"))),
               
               tabPanel("Histogram & Q-Q Plot",
                        br(),
                        withSpinner(plotOutput(ns("distribution_plot"), height = "400px")))
             )
           ),
           
           # Summary Table
           box(
             title = "Ringkasan Statistik",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             withSpinner(tableOutput(ns("summary_table")))
           )
    ),
    
    # Interpretation Section
    column(12,
           box(
             title = "Interpretasi dan Rekomendasi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("interpretation")))
           )
    )
  )
}

# Server function for Uji Asumsi
ujiAsumsiServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update variable choices
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data)
      
      updateSelectInput(session, "test_variable", 
                        choices = setNames(numeric_vars, numeric_vars))
      updateSelectInput(session, "group_variable", 
                        choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "x_variable", 
                        choices = setNames(numeric_vars, numeric_vars))
      updateSelectInput(session, "y_variable", 
                        choices = setNames(numeric_vars, numeric_vars))
    })
    
    # Reactive values for test results
    test_results <- reactiveValues(
      results = NULL,
      plot = NULL,
      interpretation = NULL,
      summary_stats = NULL
    )
    
    # Run statistical test
    observeEvent(input$run_test, {
      req(input$test_variable)
      
      data <- values$current_data
      
      if (input$test_type == "normality") {
        # Normality tests
        variable <- data[[input$test_variable]]
        variable <- variable[!is.na(variable)]
        
        if (input$normality_method == "shapiro") {
          if (length(variable) > 5000) {
            # Sample for large datasets
            variable <- sample(variable, 5000)
          }
          test_result <- shapiro.test(variable)
          test_results$results <- test_result
          
        } else if (input$normality_method == "ks") {
          test_result <- ks.test(variable, "pnorm", mean(variable), sd(variable))
          test_results$results <- test_result
          
        } else if (input$normality_method == "ad") {
          if (requireNamespace("nortest", quietly = TRUE)) {
            test_result <- nortest::ad.test(variable)
            test_results$results <- test_result
          }
          
        } else if (input$normality_method == "jb") {
          if (requireNamespace("tseries", quietly = TRUE)) {
            test_result <- tseries::jarque.bera.test(variable)
            test_results$results <- test_result
          }
        }
        
        # Create Q-Q plot
        test_results$plot <- ggplot(data, aes_string(sample = input$test_variable)) +
          stat_qq() + stat_qq_line() +
          labs(title = paste("Q-Q Plot:", input$test_variable),
               x = "Theoretical Quantiles", y = "Sample Quantiles") +
          theme_custom()
        
        # Generate interpretation
        p_value <- test_results$results$p.value
        interpretation <- paste(
          "INTERPRETASI UJI NORMALITAS:\n",
          "============================\n\n",
          "Metode:", input$normality_method, "\n",
          "Variabel:", input$test_variable, "\n",
          "H0: Data berdistribusi normal\n",
          "H1: Data tidak berdistribusi normal\n\n",
          "Hasil:\n",
          "- Statistik uji:", round(test_results$results$statistic, 4), "\n",
          "- P-value:", format_p_value(p_value), "\n",
          "- Tingkat signifikansi:", input$alpha, "\n\n",
          "Kesimpulan:\n",
          interpret_normality(p_value, input$alpha), "\n\n",
          "Rekomendasi:\n",
          if (p_value > input$alpha) {
            "- Data dapat digunakan untuk uji parametrik\n- Asumsi normalitas terpenuhi\n- Dapat menggunakan uji t, ANOVA, regresi linear"
          } else {
            "- Pertimbangkan transformasi data\n- Gunakan uji non-parametrik\n- Periksa outliers dan distribusi data"
          }
        )
        
      } else if (input$test_type == "homogeneity") {
        req(input$group_variable)
        
        # Homogeneity tests
        formula_str <- paste(input$test_variable, "~", input$group_variable)
        
        if (input$homogeneity_method == "levene") {
          if (requireNamespace("car", quietly = TRUE)) {
            test_result <- car::leveneTest(as.formula(formula_str), data = data)
            test_results$results <- test_result
          }
          
        } else if (input$homogeneity_method == "bartlett") {
          test_result <- bartlett.test(as.formula(formula_str), data = data)
          test_results$results <- test_result
          
        } else if (input$homogeneity_method == "fligner") {
          test_result <- fligner.test(as.formula(formula_str), data = data)
          test_results$results <- test_result
        }
        
        # Create boxplot for visualization
        test_results$plot <- ggplot(data, aes_string(x = input$group_variable, 
                                                     y = input$test_variable)) +
          geom_boxplot(fill = "lightblue", alpha = 0.7) +
          labs(title = paste("Boxplot:", input$test_variable, "by", input$group_variable),
               x = input$group_variable, y = input$test_variable) +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Generate interpretation
        p_value <- test_results$results$p.value
        interpretation <- paste(
          "INTERPRETASI UJI HOMOGENITAS:\n",
          "=============================\n\n",
          "Metode:", input$homogeneity_method, "\n",
          "Variabel:", input$test_variable, "\n",
          "Kelompok:", input$group_variable, "\n",
          "H0: Variansi homogen antar kelompok\n",
          "H1: Variansi tidak homogen antar kelompok\n\n",
          "Hasil:\n",
          "- Statistik uji:", round(test_results$results$statistic, 4), "\n",
          "- P-value:", format_p_value(p_value), "\n",
          "- Tingkat signifikansi:", input$alpha, "\n\n",
          "Kesimpulan:\n",
          interpret_homogeneity(p_value, input$alpha), "\n\n",
          "Rekomendasi:\n",
          if (p_value > input$alpha) {
            "- Asumsi homogenitas terpenuhi\n- Dapat menggunakan ANOVA klasik\n- Uji t dengan equal variances"
          } else {
            "- Gunakan Welch's ANOVA\n- Uji t dengan unequal variances\n- Pertimbangkan transformasi data"
          }
        )
        
      } else if (input$test_type == "linearity") {
        req(input$x_variable, input$y_variable)
        
        # Linearity test using correlation and residuals
        x_var <- data[[input$x_variable]]
        y_var <- data[[input$y_variable]]
        
        # Fit linear model
        model <- lm(y_var ~ x_var)
        
        # Create scatter plot with regression line
        test_results$plot <- ggplot(data, aes_string(x = input$x_variable, 
                                                     y = input$y_variable)) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", se = TRUE, color = "red") +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          labs(title = paste("Linearity Test:", input$y_variable, "vs", input$x_variable),
               subtitle = "Red: Linear fit, Blue: LOESS fit") +
          theme_custom()
        
        # Linearity assessment
        correlation <- cor(x_var, y_var, use = "complete.obs")
        r_squared <- summary(model)$r.squared
        
        interpretation <- paste(
          "INTERPRETASI UJI LINEARITAS:\n",
          "============================\n\n",
          "Variabel X:", input$x_variable, "\n",
          "Variabel Y:", input$y_variable, "\n\n",
          "Hasil:\n",
          "- Korelasi Pearson:", round(correlation, 4), "\n",
          "- R-squared:", round(r_squared, 4), "\n",
          "- F-statistic:", round(summary(model)$fstatistic[1], 4), "\n",
          "- P-value:", format_p_value(summary(model)$coefficients[2, 4]), "\n\n",
          "Interpretasi:\n",
          "- Hubungan linear:", interpret_correlation(correlation), "\n",
          "- Kekuatan model:", interpret_r_squared(r_squared), "\n\n",
          "Rekomendasi:\n",
          if (abs(correlation) > 0.3) {
            "- Hubungan linear cukup kuat\n- Model regresi linear dapat diterapkan\n- Periksa residual untuk validasi"
          } else {
            "- Hubungan linear lemah\n- Pertimbangkan transformasi variabel\n- Eksplorasi hubungan non-linear"
          }
        )
        
      } else if (input$test_type == "independence") {
        # Independence test using runs test or Durbin-Watson
        variable <- data[[input$test_variable]]
        variable <- variable[!is.na(variable)]
        
        # Simple runs test approximation
        n <- length(variable)
        median_val <- median(variable)
        runs <- rle(variable > median_val)$lengths
        n_runs <- length(runs)
        
        # Expected runs and variance
        n1 <- sum(variable > median_val)
        n2 <- sum(variable <= median_val)
        expected_runs <- (2 * n1 * n2) / n + 1
        var_runs <- (2 * n1 * n2 * (2 * n1 * n2 - n)) / (n^2 * (n - 1))
        
        # Z-score
        z_score <- (n_runs - expected_runs) / sqrt(var_runs)
        p_value <- 2 * (1 - pnorm(abs(z_score)))
        
        # Create sequence plot
        test_results$plot <- ggplot(data.frame(Index = 1:n, Value = variable), 
                                    aes(x = Index, y = Value)) +
          geom_line() +
          geom_point(alpha = 0.6) +
          geom_hline(yintercept = median_val, color = "red", linetype = "dashed") +
          labs(title = paste("Sequence Plot:", input$test_variable),
               x = "Index", y = input$test_variable) +
          theme_custom()
        
        # Store results
        test_results$results <- list(
          statistic = z_score,
          p.value = p_value,
          runs = n_runs,
          expected = expected_runs
        )
        
        interpretation <- paste(
          "INTERPRETASI UJI INDEPENDENSI:\n",
          "==============================\n\n",
          "Variabel:", input$test_variable, "\n",
          "Metode: Runs Test\n",
          "H0: Data independen (tidak ada pola)\n",
          "H1: Data tidak independen (ada pola)\n\n",
          "Hasil:\n",
          "- Jumlah runs:", n_runs, "\n",
          "- Expected runs:", round(expected_runs, 2), "\n",
          "- Z-score:", round(z_score, 4), "\n",
          "- P-value:", format_p_value(p_value), "\n\n",
          "Kesimpulan:\n",
          if (p_value > input$alpha) {
            "- Data menunjukkan independensi\n- Tidak ada pola yang signifikan"
          } else {
            "- Data menunjukkan ketergantungan\n- Terdapat pola dalam data"
          }, "\n\n",
          "Rekomendasi:\n",
          if (p_value > input$alpha) {
            "- Asumsi independensi terpenuhi\n- Dapat menggunakan uji statistik standar"
          } else {
            "- Pertimbangkan analisis time series\n- Gunakan metode yang memperhitungkan autokorelasi"
          }
        )
      }
      
      test_results$interpretation <- interpretation
      
      # Calculate summary statistics
      if (input$test_type %in% c("normality", "independence")) {
        test_results$summary_stats <- create_summary_table(data, input$test_variable)
      } else if (input$test_type == "homogeneity") {
        test_results$summary_stats <- data %>%
          group_by(.data[[input$group_variable]]) %>%
          summarise(
            N = n(),
            Mean = round(mean(.data[[input$test_variable]], na.rm = TRUE), 3),
            SD = round(sd(.data[[input$test_variable]], na.rm = TRUE), 3),
            Variance = round(var(.data[[input$test_variable]], na.rm = TRUE), 3),
            Min = round(min(.data[[input$test_variable]], na.rm = TRUE), 3),
            Max = round(max(.data[[input$test_variable]], na.rm = TRUE), 3),
            .groups = 'drop'
          )
      } else if (input$test_type == "linearity") {
        model <- lm(data[[input$y_variable]] ~ data[[input$x_variable]])
        test_results$summary_stats <- data.frame(
          Statistik = c("Correlation", "R-squared", "Adj R-squared", "F-statistic", "P-value"),
          Nilai = c(
            round(cor(data[[input$x_variable]], data[[input$y_variable]], use = "complete.obs"), 4),
            round(summary(model)$r.squared, 4),
            round(summary(model)$adj.r.squared, 4),
            round(summary(model)$fstatistic[1], 4),
            round(summary(model)$coefficients[2, 4], 4)
          )
        )
      }
    })
    
    # Display test results
    output$test_results <- renderText({
      req(test_results$results)
      
      if (input$test_type == "independence") {
        paste(
          "HASIL UJI INDEPENDENSI (RUNS TEST)\n",
          "==================================\n\n",
          "Statistik Uji (Z-score):", round(test_results$results$statistic, 4), "\n",
          "P-value:", format_p_value(test_results$results$p.value), "\n",
          "Jumlah Runs:", test_results$results$runs, "\n",
          "Expected Runs:", round(test_results$results$expected, 2), "\n",
          "Tingkat Signifikansi:", input$alpha, "\n\n",
          "Kesimpulan:", 
          if (test_results$results$p.value > input$alpha) "H0 diterima" else "H0 ditolak"
        )
      } else {
        capture.output(print(test_results$results))
      }
    })
    
    # Display main plot
    output$main_plot <- renderPlot({
      req(test_results$plot)
      test_results$plot
    })
    
    # Display diagnostic plot
    output$diagnostic_plot <- renderPlot({
      req(input$test_variable)
      
      if (input$test_type == "normality") {
        # Create histogram with normal curve overlay
        data <- values$current_data
        variable <- data[[input$test_variable]]
        
        ggplot(data, aes_string(x = input$test_variable)) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.7) +
          stat_function(fun = dnorm, 
                        args = list(mean = mean(variable, na.rm = TRUE), 
                                    sd = sd(variable, na.rm = TRUE)),
                        color = "red", size = 1) +
          labs(title = paste("Histogram with Normal Curve:", input$test_variable),
               x = input$test_variable, y = "Density") +
          theme_custom()
        
      } else if (input$test_type == "homogeneity") {
        # Create variance plot
        data <- values$current_data
        
        variance_data <- data %>%
          group_by(.data[[input$group_variable]]) %>%
          summarise(Variance = var(.data[[input$test_variable]], na.rm = TRUE), .groups = 'drop')
        
        ggplot(variance_data, aes_string(x = input$group_variable, y = "Variance")) +
          geom_col(fill = "lightcoral", alpha = 0.7) +
          labs(title = paste("Variance by Group:", input$group_variable),
               x = input$group_variable, y = "Variance") +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      } else if (input$test_type == "linearity") {
        # Create residual plot
        data <- values$current_data
        model <- lm(data[[input$y_variable]] ~ data[[input$x_variable]])
        
        residual_data <- data.frame(
          Fitted = fitted(model),
          Residuals = residuals(model)
        )
        
        ggplot(residual_data, aes(x = Fitted, y = Residuals)) +
          geom_point(alpha = 0.6) +
          geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
          geom_smooth(method = "loess", se = FALSE, color = "blue") +
          labs(title = "Residual Plot",
               x = "Fitted Values", y = "Residuals") +
          theme_custom()
        
      } else if (input$test_type == "independence") {
        # Create autocorrelation plot
        data <- values$current_data
        variable <- data[[input$test_variable]]
        
        # Simple lag plot
        lag_data <- data.frame(
          X = variable[-length(variable)],
          Y = variable[-1]
        )
        
        ggplot(lag_data, aes(x = X, y = Y)) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", se = TRUE) +
          labs(title = "Lag Plot (Xt vs Xt+1)",
               x = "Xt", y = "Xt+1") +
          theme_custom()
      }
    })
    
    # Display distribution plot
    output$distribution_plot <- renderPlot({
      req(input$test_variable)
      
      data <- values$current_data
      variable <- data[[input$test_variable]]
      
      # Create combined histogram and Q-Q plot
      p1 <- ggplot(data, aes_string(x = input$test_variable)) +
        geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
        labs(title = paste("Histogram:", input$test_variable),
             x = input$test_variable, y = "Frequency") +
        theme_custom()
      
      p2 <- ggplot(data, aes_string(sample = input$test_variable)) +
        stat_qq() + stat_qq_line() +
        labs(title = paste("Q-Q Plot:", input$test_variable),
             x = "Theoretical Quantiles", y = "Sample Quantiles") +
        theme_custom()
      
      gridExtra::grid.arrange(p1, p2, ncol = 2)
    })
    
    # Display summary table
    output$summary_table <- renderTable({
      req(test_results$summary_stats)
      test_results$summary_stats
    })
    
    # Display interpretation
    output$interpretation <- renderText({
      req(test_results$interpretation)
      test_results$interpretation
    })
    
    # Download handlers
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("plot_uji_asumsi_", input$test_type, "_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        if (!is.null(test_results$plot)) {
          ggsave(file, test_results$plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
    
    output$download_results <- downloadHandler(
      filename = function() {
        paste("hasil_uji_asumsi_", input$test_type, "_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        if (!is.null(test_results$results)) {
          writeLines(capture.output(print(test_results$results)), file)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste("laporan_uji_asumsi_", input$test_type, "_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        if (!is.null(test_results$interpretation)) {
          # Create comprehensive report
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Uji Asumsi", style = "heading 1") %>%
            officer::body_add_par(paste("Jenis Uji:", input$test_type)) %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Variabel:", input$test_variable)) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil dan Interpretasi:", style = "heading 2") %>%
            officer::body_add_par(test_results$interpretation)
          
          # Add plot if available
          if (!is.null(test_results$plot)) {
            temp_file <- tempfile(fileext = ".png")
            ggsave(temp_file, test_results$plot, width = 8, height = 6, dpi = 300)
            doc <- doc %>%
              officer::body_add_par("Visualisasi:", style = "heading 2") %>%
              officer::body_add_img(temp_file, width = 6, height = 4)
            unlink(temp_file)
          }
          
          print(doc, target = file)
        }
      }
    )
  })
}
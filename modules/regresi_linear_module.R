# Regresi Linear Module
# modules/regresi_linear_module.R

# UI function for Regresi Linear
regresiLinearUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Control Panel
    column(3,
           box(
             title = "Panel Kontrol Regresi Linear", # Changed title
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Variable selection
             selectInput(ns("dependent_variable"), # Changed from dependent_var
                         "Variabel Dependen (Y):",
                         choices = NULL),
             
             selectInput(ns("independent_variables"), # Changed from independent_vars
                         "Variabel Independen (X):",
                         choices = NULL,
                         multiple = TRUE),
             
             # Model type (only Linear Regression as per request)
             selectInput(ns("model_type"), # Kept for future expandability, but only one option
                         "Jenis Model:",
                         choices = list(
                           "Regresi Linear Berganda" = "linear" # Only linear regression
                         )),
             
             # Removed conditional panels for other regression types as per request
             
             # Include interactions (Kept as it applies to multiple linear regression)
             checkboxInput(ns("include_interactions"), 
                           "Sertakan Interaksi", 
                           value = FALSE),
             
             # Standardize variables (Kept as it applies to multiple linear regression)
             checkboxInput(ns("standardize"), 
                           "Standardisasi Variabel", 
                           value = FALSE),
             
             # Confidence level
             numericInput(ns("conf_level"), 
                          "Tingkat Kepercayaan Interval (0-1):",
                          value = 0.95,
                          min = 0.8,
                          max = 0.99,
                          step = 0.01),
             
             # Action buttons
             br(),
             actionButton(ns("run_regression"), 
                          "Jalankan Regresi", 
                          class = "btn-success"),
             br(), br(),
             
             # Download options
             h5("Download Hasil:"),
             downloadButton(ns("download_regression_summary"), 
                            "Download Ringkasan Model", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_residual_plots"), 
                            "Download Plot Asumsi Residual", # Changed name for clarity
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_report"), 
                            "Download Laporan Lengkap", # Changed name
                            class = "btn-info")
           )
    ),
    
    # Main Content
    column(9,
           # Regression Summary
           box(
             title = "Ringkasan Model Regresi",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("regression_summary")))
           ),
           
           # Assumption Checks (New Section)
           box(
             title = "Hasil Uji Asumsi Regresi",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             tabsetPanel(
               tabPanel("Normalitas Residual",
                        br(),
                        h4("Histogram Residual"),
                        withSpinner(plotOutput(ns("residual_histogram"))),
                        h4("Q-Q Plot Residual"),
                        withSpinner(plotOutput(ns("residual_qqplot"))),
                        h4("Uji Shapiro-Wilk Residual"),
                        withSpinner(verbatimTextOutput(ns("shapiro_test_result")))
               ),
               tabPanel("Multikolinearitas (VIF)",
                        br(),
                        withSpinner(tableOutput(ns("vif_results")))
               ),
               tabPanel("Heteroskedastisitas", # NEW TAB PANEL for Heteroskedastisitas
                        br(),
                        h4("Uji Heteroskedastisitas (Non-Constant Variance Test / Breusch-Pagan)"),
                        withSpinner(verbatimTextOutput(ns("heteroscedasticity_test_result"))), # Changed output name
                        br(),
                        p("Periksa juga 'Plot Residual vs Fitted' di bagian 'Visualisasi Diagnostik Model' untuk inspeksi visual heteroskedastisitas.")
               )
             )
           ),
           
           # Model Plots
           box(
             title = "Visualisasi Diagnostik Model", # Changed title
             status = "success",
             solidHeader = TRUE,
             width = 12,
             tabsetPanel(
               tabPanel("Plot Residual vs Fitted", 
                        br(),
                        withSpinner(plotOutput(ns("residuals_vs_fitted")))),
               tabPanel("Scale-Location Plot",
                        br(),
                        withSpinner(plotOutput(ns("scale_location")))),
               tabPanel("Cook's Distance Plot",
                        br(),
                        withSpinner(plotOutput(ns("cooks_distance"))))
             )
           )
    ),
    
    # Interpretation Section
    column(12,
           box(
             title = "Interpretasi Hasil Regresi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("regression_interpretation")))
           )
    ),
    
    # Assumption Interpretation Section (New Section)
    column(12,
           box(
             title = "Interpretasi Uji Asumsi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("assumption_interpretation")))
           )
    )
  )
}

# Server function for Regresi Linear
regresiLinearServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for regression results
    regression_results <- reactiveValues(
      model = NULL,
      summary = NULL,
      interpretation = NULL,
      residuals = NULL,
      shapiro_test = NULL,
      vif = NULL,
      ncv_test = NULL, # For Heteroskedastisitas (using ncvTest from car package)
      bptest_result = NULL, # For Breusch-Pagan test (using bptest from lmtest package)
      assumption_interpretation = NULL,
      residual_hist_plot = NULL,
      residual_qq_plot = NULL
    )
    
    # Update variable choices
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      
      updateSelectInput(session, "dependent_variable", 
                        choices = setNames(numeric_vars, numeric_vars))
      updateSelectInput(session, "independent_variables", 
                        choices = setNames(numeric_vars, numeric_vars))
    })
    
    # Run regression
    observeEvent(input$run_regression, {
      req(input$dependent_variable)
      req(input$independent_variables)
      
      data <- values$current_data
      
      # Data standardization if checked
      if (input$standardize) {
        # Select only independent_variables for standardization
        # dependent_variable should not be standardized for interpretation of original scale
        data_to_standardize <- data %>%
          select(all_of(input$independent_variables)) %>%
          mutate_all(~scale(.)[,1]) # scale returns a matrix, convert to vector
        
        # Combine standardized independent vars with original dependent var and other non-numeric
        data_model <- data %>%
          select(-all_of(input$independent_variables)) %>% # Remove original independent vars
          bind_cols(data_to_standardize) # Add standardized ones back
        
        # Ensure dependent variable is in the data_model
        if (!input$dependent_variable %in% names(data_model)) {
          data_model <- data_model %>%
            bind_cols(data %>% select(all_of(input$dependent_variable)))
        }
        
      } else {
        data_model <- data
      }
      
      # Handle interactions if checked
      if (input$include_interactions && length(input$independent_variables) >= 2) {
        interaction_terms <- combn(input$independent_variables, 2, simplify = FALSE)
        interaction_formula_parts <- sapply(interaction_terms, function(pair) paste(pair, collapse = "*"))
        formula_str <- paste(input$dependent_variable, "~", 
                             paste(c(input$independent_variables, interaction_formula_parts), collapse = "+"))
      } else {
        formula_str <- paste(input$dependent_variable, "~", 
                             paste(input$independent_variables, collapse = "+"))
      }
      
      # Fit model
      model_results <- lm(as.formula(formula_str), data = data_model)
      regression_results$model <- model_results
      regression_results$summary <- summary(model_results)
      
      # Calculate residuals
      residuals_val <- residuals(model_results)
      regression_results$residuals <- residuals_val
      
      # --- Uji Asumsi ---
      
      # Normalitas Residual (Shapiro-Wilk)
      # Shapiro test requires at least 3 distinct values and no NA
      if (length(residuals_val) > 3 && length(unique(na.omit(residuals_val))) > 1 && !any(is.na(residuals_val))) { 
        regression_results$shapiro_test <- shapiro.test(residuals_val)
      } else {
        regression_results$shapiro_test <- "Tidak dapat menjalankan uji Shapiro-Wilk (data residual tidak cukup, tidak bervariasi, atau mengandung NA)."
      }
      
      # Multikolinearitas (VIF)
      if (length(input$independent_variables) > 1 && all(input$independent_variables %in% names(data_model))) {
        if (requireNamespace("car", quietly = TRUE)) {
          tryCatch({
            regression_results$vif <- car::vif(model_results)
          }, error = function(e) {
            regression_results$vif <- paste("Error menghitung VIF:", e$message, ". Pastikan variabel independen tidak linear bergantung.")
          })
        } else {
          regression_results$vif <- "Paket 'car' diperlukan untuk VIF. Mohon install: install.packages('car')"
        }
      } else {
        regression_results$vif <- "VIF hanya relevan untuk model dengan lebih dari satu variabel independen."
      }
      
      # Heteroskedastisitas (Non-Constant Variance Test / ncvTest or Breusch-Pagan)
      if (requireNamespace("car", quietly = TRUE)) {
        tryCatch({
          regression_results$ncv_test <- car::ncvTest(model_results)
        }, error = function(e) {
          regression_results$ncv_test <- paste("Error menghitung Uji Heteroskedastisitas (ncvTest):", e$message)
        })
      } else {
        regression_results$ncv_test <- "Paket 'car' diperlukan untuk Uji Heteroskedastisitas (ncvTest). Mohon install: install.packages('car')"
      }
      
      # You can also add Breusch-Pagan test from lmtest
      if (requireNamespace("lmtest", quietly = TRUE)) {
        tryCatch({
          regression_results$bptest_result <- lmtest::bptest(model_results)
        }, error = function(e) {
          regression_results$bptest_result <- paste("Error menghitung Uji Breusch-Pagan:", e$message)
        })
      } else {
        regression_results$bptest_result <- "Paket 'lmtest' diperlukan untuk Uji Breusch-Pagan. Mohon install: install.packages('lmtest')"
      }
      
      
      # --- Plot untuk Uji Asumsi ---
      # Histogram Residual
      residual_hist_data <- data.frame(Residuals = residuals_val)
      regression_results$residual_hist_plot <- ggplot(residual_hist_data, aes(x = Residuals)) +
        geom_histogram(bins = 30, fill = "lightblue", color = "black") +
        labs(title = "Histogram Residual", x = "Residual", y = "Frekuensi") +
        theme_minimal()
      
      # Q-Q Plot Residual
      qq_data <- as.data.frame(qqnorm(residuals_val, plot.it = FALSE))
      regression_results$residual_qq_plot <- ggplot(qq_data, aes(x = x, y = y)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + 
        labs(title = "Q-Q Plot Residual", x = "Kuartil Teoritis", y = "Kuartil Sampel") +
        theme_minimal()
      
      # --- Interpretasi Hasil Regresi ---
      regression_results$interpretation <- interpret_regression_summary(regression_results$summary)
      
      # --- Interpretasi Uji Asumsi ---
      assumption_interpret_text <- "INTERPRETASI UJI ASUMSI REGRESI:\n==============================\n\n"
      
      # Normalitas Residual Interpretation
      assumption_interpret_text <- paste(assumption_interpret_text, "1. Normalitas Residual (Uji Shapiro-Wilk & Plot):\n")
      if (is.character(regression_results$shapiro_test)) {
        assumption_interpret_text <- paste(assumption_interpret_text, regression_results$shapiro_test, "\n")
      } else if (!is.null(regression_results$shapiro_test)) {
        shapiro_p_value <- regression_results$shapiro_test$p.value
        assumption_interpret_text <- paste(assumption_interpret_text, 
                                           "- Hasil Uji Shapiro-Wilk: W =", round(regression_results$shapiro_test$statistic, 3), 
                                           ", p-value =", format_p_value(shapiro_p_value), "\n",
                                           if (shapiro_p_value < 0.05) {
                                             "  Kesimpulan: Residual **tidak terdistribusi normal** (p-value < 0.05). Hal ini dapat mempengaruhi validitas interval kepercayaan dan uji hipotesis, terutama pada ukuran sampel kecil. Regresi mungkin masih kuat jika ukuran sampel besar karena Teorema Batas Pusat.\n"
                                           } else {
                                             "  Kesimpulan: Residual **terdistribusi normal** (p-value >= 0.05). Asumsi normalitas residual terpenuhi.\n"
                                           },
                                           "  Rekomendasi: Periksa juga histogram dan Q-Q plot residual secara visual untuk konfirmasi. Jika tidak normal, pertimbangkan transformasi variabel dependen atau penggunaan metode regresi non-parametrik/robust.\n\n"
        )
      } else {
        assumption_interpret_text <- paste(assumption_interpret_text, "Tidak ada hasil uji normalitas residual.\n\n")
      }
      
      # Multikolinearitas (VIF) Interpretation
      assumption_interpret_text <- paste(assumption_interpret_text, "2. Multikolinearitas (Variance Inflation Factor - VIF):\n")
      if (is.character(regression_results$vif)) {
        assumption_interpret_text <- paste(assumption_interpret_text, regression_results$vif, "\n")
      } else if (!is.null(regression_results$vif)) {
        vif_values <- regression_results$vif
        if (length(vif_values) > 0) {
          max_vif <- max(vif_values)
          assumption_interpret_text <- paste(assumption_interpret_text,
                                             paste(capture.output(print(vif_values)), collapse = "\n"), "\n",
                                             "  Nilai VIF > 5 atau > 10 umumnya mengindikasikan multikolinearitas yang signifikan.\n",
                                             "  Nilai VIF maksimum:", round(max_vif, 2), "\n",
                                             if (max_vif >= 10) {
                                               "  Kesimpulan: Terdapat **multikolinearitas yang tinggi** (VIF maks >= 10). Ini bisa menyebabkan koefisien regresi menjadi tidak stabil, estimasi *standard error* membesar, dan uji signifikansi tidak valid. \n"
                                             } else if (max_vif >= 5) {
                                               "  Kesimpulan: Terdapat **multikolinearitas moderat** (VIF maks >= 5). Perlu diwaspadai potensi masalah interpretasi.\n"
                                             } else {
                                               "  Kesimpulan: Tidak ada multikolinearitas yang signifikan (VIF maks < 5). Asumsi tidak ada multikolinearitas terpenuhi.\n"
                                             },
                                             "  Rekomendasi: Jika multikolinearitas tinggi, pertimbangkan untuk menghilangkan salah satu variabel yang berkorelasi tinggi, menggabungkan variabel, atau menggunakan metode regresi regularisasi (misalnya Ridge atau Lasso).\n\n"
          )
        } else {
          assumption_interpret_text <- paste(assumption_interpret_text, "VIF tidak dapat dihitung (mungkin hanya ada satu variabel independen).\n\n")
        }
      } else {
        assumption_interpret_text <- paste(assumption_interpret_text, "Tidak ada hasil uji multikolinearitas.\n\n")
      }
      
      # Heteroskedastisitas Interpretation (ncvTest or bptest)
      assumption_interpret_text <- paste(assumption_interpret_text, "3. Heteroskedastisitas:\n")
      if (is.character(regression_results$ncv_test)) {
        assumption_interpret_text <- paste(assumption_interpret_text, "- Uji Non-Constant Variance (ncvTest): ", regression_results$ncv_test, "\n")
      } else if (!is.null(regression_results$ncv_test)) {
        # FIX: Directly access p, ChiSquare, Df from ncv_test object
        ncv_p_value <- regression_results$ncv_test$p
        ncv_chisq <- regression_results$ncv_test$ChiSquare
        ncv_df <- regression_results$ncv_test$Df
        
        assumption_interpret_text <- paste(assumption_interpret_text,
                                           "- Hasil Uji Non-Constant Variance (ncvTest): Chisq =", round(ncv_chisq, 3),
                                           ", Df =", ncv_df,
                                           ", p-value =", format_p_value(ncv_p_value), "\n",
                                           if (ncv_p_value < 0.05) {
                                             "  Kesimpulan: Terdapat **heteroskedastisitas** (p-value < 0.05). Varians residual tidak konstan, melanggar asumsi homoskedastisitas. Ini dapat menyebabkan estimasi *standard error* menjadi bias dan uji signifikansi tidak valid.\n"
                                           } else {
                                             "  Kesimpulan: Tidak ada bukti heteroskedastisitas yang signifikan (p-value >= 0.05). Asumsi homoskedastisitas terpenuhi.\n"
                                           },
                                           "  Rekomendasi: Periksa 'Plot Residual vs Fitted' secara visual. Jika ada heteroskedastisitas, pertimbangkan transformasi variabel dependen, menggunakan *robust standard errors* (misalnya dengan paket `sandwich`), atau regresi kuantil.\n"
        )
      } else {
        assumption_interpret_text <- paste(assumption_interpret_text, "- Tidak ada hasil uji ncvTest.\n")
      }
      
      if (is.character(regression_results$bptest_result)) {
        assumption_interpret_text <- paste(assumption_interpret_text, "- Uji Breusch-Pagan: ", regression_results$bptest_result, "\n\n")
      } else if (!is.null(regression_results$bptest_result)) {
        bp_p_value <- regression_results$bptest_result$p.value 
        assumption_interpret_text <- paste(assumption_interpret_text,
                                           "- Hasil Uji Breusch-Pagan: BP =", round(regression_results$bptest_result$statistic, 3),
                                           ", Df =", regression_results$bptest_result$parameter,
                                           ", p-value =", format_p_value(bp_p_value), "\n",
                                           if (bp_p_value < 0.05) {
                                             "  Kesimpulan: Terdapat **heteroskedastisitas** (p-value < 0.05). Varians residual tidak konstan. \n"
                                           } else {
                                             "  Kesimpulan: Tidak ada bukti heteroskedastisitas yang signifikan (p-value >= 0.05).\n"
                                           },
                                           "  Catatan: Kedua uji (ncvTest dan Breusch-Pagan) memeriksa heteroskedastisitas dan umumnya memberikan kesimpulan yang serupa. Pilih salah satu untuk interpretasi utama.\n\n"
        )
      } else {
        assumption_interpret_text <- paste(assumption_interpret_text, "- Tidak ada hasil uji Breusch-Pagan.\n\n")
      }
      
      regression_results$assumption_interpretation <- assumption_interpret_text
    })
    
    # Display regression summary
    output$regression_summary <- renderPrint({
      req(regression_results$summary)
      regression_results$summary
    })
    
    # Display assumption check plots
    output$residual_histogram <- renderPlot({
      req(regression_results$residual_hist_plot)
      regression_results$residual_hist_plot
    })
    
    output$residual_qqplot <- renderPlot({
      req(regression_results$residual_qq_plot)
      regression_results$residual_qq_plot
    })
    
    output$shapiro_test_result <- renderPrint({
      req(regression_results$shapiro_test)
      regression_results$shapiro_test
    })
    
    output$vif_results <- renderTable({
      req(regression_results$vif)
      if (is.character(regression_results$vif)) {
        data.frame(Info = regression_results$vif)
      } else {
        data.frame(Variabel = names(regression_results$vif), VIF_Value = as.numeric(regression_results$vif))
      }
    })
    
    # Display heteroscedasticity test result (ncvTest and/or bptest)
    output$heteroscedasticity_test_result <- renderPrint({
      req(regression_results$ncv_test, regression_results$bptest_result)
      cat("Uji Non-Constant Variance (ncvTest) dari paket 'car':\n")
      print(regression_results$ncv_test)
      cat("\nUji Breusch-Pagan dari paket 'lmtest':\n")
      print(regression_results$bptest_result)
    })
    
    # Display model diagnostic plots (base R plots)
    output$residuals_vs_fitted <- renderPlot({
      req(regression_results$model)
      plot(regression_results$model, which = 1) # Residuals vs Fitted
    })
    
    output$scale_location <- renderPlot({
      req(regression_results$model)
      plot(regression_results$model, which = 3) # Scale-Location
    })
    
    output$cooks_distance <- renderPlot({
      req(regression_results$model)
      plot(regression_results$model, which = 4) # Cook's Distance
    })
    
    # Display interpretation
    output$regression_interpretation <- renderText({
      req(regression_results$interpretation)
      regression_results$interpretation
    })
    
    # Display assumption interpretation
    output$assumption_interpretation <- renderText({
      req(regression_results$assumption_interpretation)
      regression_results$assumption_interpretation
    })
    
    # Download handlers
    output$download_regression_summary <- downloadHandler(
      filename = function() {
        paste("ringkasan_regresi_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        if (!is.null(regression_results$summary)) {
          writeLines(capture.output(print(regression_results$summary)), file)
        }
      }
    )
    
    output$download_residual_plots <- downloadHandler(
      filename = function() {
        paste("plot_residual_asumsi_", Sys.Date(), ".png", sep = "") 
      },
      content = function(file) {
        req(regression_results$residual_hist_plot, regression_results$residual_qq_plot)
        
        # Combine ggplot objects
        plots_combined_ggplot <- gridExtra::grid.arrange(
          regression_results$residual_hist_plot,
          regression_results$residual_qq_plot,
          ncol = 2
        )
        ggsave(file, plots_combined_ggplot, width = 12, height = 6, dpi = 300)
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste("laporan_regresi_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        if (!is.null(regression_results$interpretation) && !is.null(regression_results$assumption_interpretation)) {
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Regresi Linear Berganda", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Ringkasan Model Regresi:", style = "heading 2") %>%
            officer::body_add_par(capture.output(print(regression_results$summary))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Interpretasi Model:", style = "heading 2") %>%
            officer::body_add_par(regression_results$interpretation) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil dan Interpretasi Uji Asumsi Regresi:", style = "heading 2") %>%
            officer::body_add_par(regression_results$assumption_interpretation)
          
          # Add ggplot residual plots to report
          if (!is.null(regression_results$residual_hist_plot) && !is.null(regression_results$residual_qq_plot)) {
            temp_hist_file <- tempfile(fileext = ".png")
            ggsave(temp_hist_file, regression_results$residual_hist_plot, width = 6, height = 4, dpi = 300)
            
            temp_qq_file <- tempfile(fileext = ".png")
            ggsave(temp_qq_file, regression_results$residual_qq_plot, width = 6, height = 4, dpi = 300)
            
            doc <- doc %>%
              officer::body_add_par("Histogram Residual:", style = "Normal") %>%
              officer::body_add_img(temp_hist_file, width = 6, height = 4) %>%
              officer::body_add_par("Q-Q Plot Residual:", style = "Normal") %>%
              officer::body_add_img(temp_qq_file, width = 6, height = 4)
            
            unlink(temp_hist_file)
            unlink(temp_qq_file)
          }
          
          # Add VIF table if available and not an error message
          if (!is.null(regression_results$vif) && !is.character(regression_results$vif)) {
            vif_df_for_report <- data.frame(Variabel = names(regression_results$vif), VIF_Value = as.numeric(regression_results$vif))
            doc <- doc %>%
              officer::body_add_par("Hasil VIF:", style = "Normal") %>%
              officer::body_add_table(vif_df_for_report, style = "Table Grid")
          }
          
          # Add ncvTest result if available and not an error message
          if (!is.null(regression_results$ncv_test) && !is.character(regression_results$ncv_test)) {
            doc <- doc %>%
              officer::body_add_par("Hasil Uji Heteroskedastisitas (ncvTest):", style = "Normal") %>%
              officer::body_add_par(capture.output(print(regression_results$ncv_test)))
          }
          
          # Add Breusch-Pagan test result if available and not an error message
          if (!is.null(regression_results$bptest_result) && !is.character(regression_results$bptest_result)) {
            doc <- doc %>%
              officer::body_add_par("Hasil Uji Breusch-Pagan:", style = "Normal") %>%
              officer::body_add_par(capture.output(print(regression_results$bptest_result)))
          }
          
          # For base R plots (Residuals vs Fitted, Scale-Location, Cook's Distance), 
          # save them to a temporary file using png()/pdf() and then add to the doc.
          if (!is.null(regression_results$model)) {
            temp_plot_file_1 <- tempfile(fileext = ".png")
            png(temp_plot_file_1, width = 800, height = 600, res = 100)
            plot(regression_results$model, which = 1, main = "Residuals vs Fitted") 
            dev.off()
            doc <- doc %>%
              officer::body_add_par("Residuals vs Fitted Plot (untuk Homoskedastisitas):", style = "Normal") %>%
              officer::body_add_img(temp_plot_file_1, width = 6, height = 4)
            unlink(temp_plot_file_1)
            
            temp_plot_file_2 <- tempfile(fileext = ".png")
            png(temp_plot_file_2, width = 800, height = 600, res = 100)
            plot(regression_results$model, which = 3, main = "Scale-Location Plot") 
            dev.off()
            doc <- doc %>%
              officer::body_add_par("Scale-Location Plot (untuk Homoskedastisitas):", style = "Normal") %>%
              officer::body_add_img(temp_plot_file_2, width = 6, height = 4)
            unlink(temp_plot_file_2)
            
            temp_plot_file_3 <- tempfile(fileext = ".png")
            png(temp_plot_file_3, width = 800, height = 600, res = 100)
            plot(regression_results$model, which = 4, main = "Cook's Distance Plot") 
            dev.off()
            doc <- doc %>%
              officer::body_add_par("Cook's Distance Plot (untuk Outlier/Pengaruh):", style = "Normal") %>%
              officer::body_add_img(temp_plot_file_3, width = 6, height = 4)
            unlink(temp_plot_file_3)
          }
          
          print(doc, target = file)
        }
      }
    )
  })
}
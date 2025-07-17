# Regresi Linear Module
# modules/regresi_linear_module.R

# UI function for Regresi Linear
regresiLinearUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Control Panel
    column(3,
           box(
             title = "Panel Kontrol Regresi",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Dependent variable
             selectInput(ns("dependent_var"), 
                         "Variabel Dependen (Y):",
                         choices = NULL),
             
             # Independent variables
             selectInput(ns("independent_vars"), 
                         "Variabel Independen (X):",
                         choices = NULL,
                         multiple = TRUE),
             
             # Model type
             selectInput(ns("model_type"), 
                         "Jenis Model:",
                         choices = list(
                           "Linear Regression" = "linear",
                           "Stepwise Regression" = "stepwise",
                           "Ridge Regression" = "ridge",
                           "Polynomial Regression" = "polynomial"
                         )),
             
             # Conditional inputs
             conditionalPanel(
               condition = "input.model_type == 'stepwise'",
               ns = ns,
               selectInput(ns("step_direction"), 
                           "Arah Stepwise:",
                           choices = list(
                             "Forward" = "forward",
                             "Backward" = "backward",
                             "Both" = "both"
                           ))
             ),
             
             conditionalPanel(
               condition = "input.model_type == 'polynomial'",
               ns = ns,
               numericInput(ns("poly_degree"), 
                            "Derajat Polinomial:",
                            value = 2,
                            min = 2,
                            max = 5)
             ),
             
             # Include interactions
             checkboxInput(ns("include_interactions"), 
                           "Sertakan Interaksi", 
                           value = FALSE),
             
             # Standardize variables
             checkboxInput(ns("standardize"), 
                           "Standardisasi Variabel", 
                           value = FALSE),
             
             # Confidence level
             numericInput(ns("conf_level"), 
                          "Tingkat Kepercayaan:",
                          value = 0.95,
                          min = 0.8,
                          max = 0.99,
                          step = 0.01),
             
             # Action buttons
             br(),
             actionButton(ns("build_model"), 
                          "Bangun Model", 
                          class = "btn-success"),
             br(), br(),
             actionButton(ns("run_diagnostics"), 
                          "Jalankan Diagnostik", 
                          class = "btn-warning"),
             br(), br(),
             
             # Download options
             h5("Download Hasil:"),
             downloadButton(ns("download_model"), 
                            "Download Model", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_diagnostics"), 
                            "Download Diagnostik", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_report"), 
                            "Download Laporan", 
                            class = "btn-info")
           )
    ),
    
    # Main Content
    column(9,
           # Model Summary
           box(
             title = "Ringkasan Model",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             withSpinner(verbatimTextOutput(ns("model_summary")))
           ),
           
           # Model Performance
           box(
             title = "Performa Model",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             withSpinner(tableOutput(ns("model_performance")))
           ),
           
           # Coefficients Table
           box(
             title = "Koefisien Model",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             withSpinner(DT::dataTableOutput(ns("coefficients_table")))
           ),
           
           # Diagnostic Plots
           box(
             title = "Plot Diagnostik",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             tabsetPanel(
               tabPanel("Residual Plots",
                        br(),
                        withSpinner(plotOutput(ns("residual_plots"), height = "500px"))),
               
               tabPanel("Normality Tests",
                        br(),
                        withSpinner(plotOutput(ns("normality_plots"), height = "400px")),
                        br(),
                        withSpinner(verbatimTextOutput(ns("normality_tests")))),
               
               tabPanel("Multicollinearity",
                        br(),
                        withSpinner(tableOutput(ns("vif_table"))),
                        br(),
                        withSpinner(plotOutput(ns("correlation_matrix"), height = "400px"))),
               
               tabPanel("Outliers & Influence",
                        br(),
                        withSpinner(plotOutput(ns("influence_plots"), height = "400px")),
                        br(),
                        withSpinner(tableOutput(ns("outliers_table"))))
             )
           )
    ),
    
    # Assumptions Check
    column(12,
           box(
             title = "Pemeriksaan Asumsi Regresi",
             status = "danger",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("assumptions_check")))
           )
    ),
    
    # Model Interpretation
    column(12,
           box(
             title = "Interpretasi Model dan Rekomendasi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("model_interpretation")))
           )
    )
  )
}

# Server function for Regresi Linear
regresiLinearServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update variable choices
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      
      updateSelectInput(session, "dependent_var", 
                        choices = setNames(numeric_vars, numeric_vars))
      updateSelectInput(session, "independent_vars", 
                        choices = setNames(numeric_vars, numeric_vars))
    })
    
    # Reactive values for model results
    model_results <- reactiveValues(
      model = NULL,
      data = NULL,
      diagnostics = NULL,
      interpretation = NULL,
      assumptions = NULL
    )
    
    # Build regression model
    observeEvent(input$build_model, {
      req(input$dependent_var, input$independent_vars)
      
      data <- values$current_data
      
      # Prepare data
      if (input$standardize) {
        data[input$independent_vars] <- scale(data[input$independent_vars])
        data[[input$dependent_var]] <- scale(data[[input$dependent_var]])[,1]
      }
      
      # Remove missing values
      complete_vars <- c(input$dependent_var, input$independent_vars)
      data <- data[complete.cases(data[complete_vars]), ]
      
      # Build formula
      if (input$include_interactions && length(input$independent_vars) > 1) {
        formula_str <- paste(input$dependent_var, "~", paste(input$independent_vars, collapse = " * "))
      } else {
        formula_str <- paste(input$dependent_var, "~", paste(input$independent_vars, collapse = " + "))
      }
      
      if (input$model_type == "linear") {
        model <- lm(as.formula(formula_str), data = data)
        
      } else if (input$model_type == "stepwise") {
        # Full model
        full_model <- lm(as.formula(formula_str), data = data)
        # Null model
        null_model <- lm(as.formula(paste(input$dependent_var, "~ 1")), data = data)
        
        if (input$step_direction == "forward") {
          model <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                        direction = "forward", trace = FALSE)
        } else if (input$step_direction == "backward") {
          model <- step(full_model, direction = "backward", trace = FALSE)
        } else {
          model <- step(null_model, scope = list(lower = null_model, upper = full_model), 
                        direction = "both", trace = FALSE)
        }
        
      } else if (input$model_type == "polynomial") {
        # For simplicity, create polynomial terms for the first independent variable
        main_var <- input$independent_vars[1]
        poly_terms <- paste0("I(", main_var, "^", 2:input$poly_degree, ")", collapse = " + ")
        
        if (length(input$independent_vars) > 1) {
          other_vars <- paste(input$independent_vars[-1], collapse = " + ")
          formula_str <- paste(input$dependent_var, "~", main_var, "+", poly_terms, "+", other_vars)
        } else {
          formula_str <- paste(input$dependent_var, "~", main_var, "+", poly_terms)
        }
        
        model <- lm(as.formula(formula_str), data = data)
        
      } else if (input$model_type == "ridge") {
        # Ridge regression using glmnet
        if (requireNamespace("glmnet", quietly = TRUE)) {
          X <- model.matrix(as.formula(formula_str), data = data)[, -1]
          y <- data[[input$dependent_var]]
          
          cv_fit <- glmnet::cv.glmnet(X, y, alpha = 0)
          model <- glmnet::glmnet(X, y, alpha = 0, lambda = cv_fit$lambda.min)
          
          # Convert to lm-like object for consistency
          coeffs <- as.matrix(coef(model))
          model_lm <- lm(as.formula(formula_str), data = data)
          model_lm$coefficients <- coeffs[,1]
          model <- model_lm
        } else {
          model <- lm(as.formula(formula_str), data = data)
        }
      }
      
      model_results$model <- model
      model_results$data <- data
      
      showNotification("Model berhasil dibangun!", type = "message")
    })
    
    # Run diagnostics
    observeEvent(input$run_diagnostics, {
      req(model_results$model)
      
      model <- model_results$model
      
      # Residual analysis
      residuals <- residuals(model)
      fitted_values <- fitted(model)
      
      # Normality tests
      shapiro_test <- shapiro.test(residuals)
      if (requireNamespace("nortest", quietly = TRUE)) {
        ad_test <- nortest::ad.test(residuals)
      } else {
        ad_test <- list(statistic = NA, p.value = NA)
      }
      
      # Heteroscedasticity test
      if (requireNamespace("lmtest", quietly = TRUE)) {
        bp_test <- lmtest::bptest(model)
      } else {
        bp_test <- list(statistic = NA, p.value = NA)
      }
      
      # Durbin-Watson test for autocorrelation
      if (requireNamespace("lmtest", quietly = TRUE)) {
        dw_test <- lmtest::dwtest(model)
      } else {
        dw_test <- list(statistic = NA, p.value = NA)
      }
      
      # VIF for multicollinearity
      if (requireNamespace("car", quietly = TRUE) && length(input$independent_vars) > 1) {
        vif_values <- car::vif(model)
      } else {
        vif_values <- NULL
      }
      
      model_results$diagnostics <- list(
        shapiro = shapiro_test,
        ad = ad_test,
        bp = bp_test,
        dw = dw_test,
        vif = vif_values
      )
      
      # Generate assumptions check
      assumptions <- paste(
        "PEMERIKSAAN ASUMSI REGRESI LINEAR:\n",
        "==================================\n\n",
        "1. NORMALITAS RESIDUAL:\n",
        "   - Shapiro-Wilk test: p-value =", format_p_value(shapiro_test$p.value), "\n",
        "   - Interpretasi:", interpret_normality(shapiro_test$p.value), "\n",
        if (!is.na(ad_test$p.value)) {
          paste("   - Anderson-Darling test: p-value =", format_p_value(ad_test$p.value), "\n")
        } else {
          ""
        },
        "\n2. HOMOSKEDASTISITAS:\n",
        if (!is.na(bp_test$p.value)) {
          paste("   - Breusch-Pagan test: p-value =", format_p_value(bp_test$p.value), "\n",
                "   - Interpretasi:", interpret_homogeneity(bp_test$p.value), "\n")
        } else {
          "   - Test tidak tersedia\n"
        },
        "\n3. INDEPENDENSI:\n",
        if (!is.na(dw_test$p.value)) {
          paste("   - Durbin-Watson test: DW =", round(dw_test$statistic, 3), 
                ", p-value =", format_p_value(dw_test$p.value), "\n")
        } else {
          "   - Test tidak tersedia\n"
        },
        "\n4. MULTIKOLINEARITAS:\n",
        if (!is.null(vif_values)) {
          paste("   - VIF values:\n", 
                paste(paste("     ", names(vif_values), ":", round(vif_values, 2)), collapse = "\n"), "\n",
                "   - Interpretasi:", 
                if (any(vif_values > 10)) "Ada multikolinearitas serius" 
                else if (any(vif_values > 5)) "Ada multikolinearitas moderat" 
                else "Tidak ada multikolinearitas")
        } else {
          "   - VIF tidak dapat dihitung (model dengan satu prediktor)"
        }
      )
      
      model_results$assumptions <- assumptions
      
      showNotification("Diagnostik selesai!", type = "message")
    })
    
    # Display model summary
    output$model_summary <- renderText({
      req(model_results$model)
      capture.output(summary(model_results$model))
    })
    
    # Display model performance
    output$model_performance <- renderTable({
      req(model_results$model)
      
      model <- model_results$model
      summary_stats <- summary(model)
      
      data.frame(
        Metrik = c("R-squared", "Adjusted R-squared", "F-statistic", "p-value", "AIC", "BIC", "RMSE", "MAE"),
        Nilai = c(
          round(summary_stats$r.squared, 4),
          round(summary_stats$adj.r.squared, 4),
          round(summary_stats$fstatistic[1], 4),
          format_p_value(pf(summary_stats$fstatistic[1], summary_stats$fstatistic[2], summary_stats$fstatistic[3], lower.tail = FALSE)),
          round(AIC(model), 2),
          round(BIC(model), 2),
          round(sqrt(mean(residuals(model)^2)), 4),
          round(mean(abs(residuals(model))), 4)
        )
      )
    })
    
    # Display coefficients table
    output$coefficients_table <- DT::renderDataTable({
      req(model_results$model)
      
      model <- model_results$model
      coef_summary <- summary(model)$coefficients
      
      coef_df <- data.frame(
        Variable = rownames(coef_summary),
        Estimate = round(coef_summary[, 1], 4),
        Std_Error = round(coef_summary[, 2], 4),
        t_value = round(coef_summary[, 3], 4),
        p_value = format_p_value(coef_summary[, 4]),
        Significance = case_when(
          coef_summary[, 4] < 0.001 ~ "***",
          coef_summary[, 4] < 0.01 ~ "**",
          coef_summary[, 4] < 0.05 ~ "*",
          coef_summary[, 4] < 0.1 ~ ".",
          TRUE ~ ""
        )
      )
      
      DT::datatable(
        coef_df,
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Significance",
          backgroundColor = DT::styleEqual(c("***", "**", "*", "."), 
                                           c("darkred", "red", "orange", "yellow"))
        )
    })
    
    # Display residual plots
    output$residual_plots <- renderPlot({
      req(model_results$model)
      
      model <- model_results$model
      
      # Create diagnostic plots
      par(mfrow = c(2, 2))
      plot(model, which = 1:4)
      par(mfrow = c(1, 1))
    })
    
    # Display normality plots
    output$normality_plots <- renderPlot({
      req(model_results$model)
      
      model <- model_results$model
      residuals <- residuals(model)
      
      # Create normality plots
      par(mfrow = c(1, 2))
      
      # Histogram with normal curve
      hist(residuals, breaks = 30, freq = FALSE, main = "Histogram of Residuals",
           xlab = "Residuals", col = "lightblue", border = "black")
      curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), 
            add = TRUE, col = "red", lwd = 2)
      
      # Q-Q plot
      qqnorm(residuals, main = "Q-Q Plot of Residuals")
      qqline(residuals, col = "red", lwd = 2)
      
      par(mfrow = c(1, 1))
    })
    
    # Display normality tests
    output$normality_tests <- renderText({
      req(model_results$diagnostics)
      
      diag <- model_results$diagnostics
      
      paste(
        "HASIL UJI NORMALITAS RESIDUAL:\n",
        "==============================\n\n",
        "Shapiro-Wilk Test:\n",
        "- W =", round(diag$shapiro$statistic, 4), "\n",
        "- p-value =", format_p_value(diag$shapiro$p.value), "\n",
        "- Interpretasi:", interpret_normality(diag$shapiro$p.value), "\n\n",
        if (!is.na(diag$ad$p.value)) {
          paste("Anderson-Darling Test:\n",
                "- A =", round(diag$ad$statistic, 4), "\n",
                "- p-value =", format_p_value(diag$ad$p.value), "\n",
                "- Interpretasi:", interpret_normality(diag$ad$p.value), "\n")
        } else {
          ""
        }
      )
    })
    
    # Display VIF table
    output$vif_table <- renderTable({
      req(model_results$diagnostics)
      
      if (!is.null(model_results$diagnostics$vif)) {
        vif_values <- model_results$diagnostics$vif
        
        data.frame(
          Variable = names(vif_values),
          VIF = round(vif_values, 3),
          Interpretation = case_when(
            vif_values > 10 ~ "Multikolinearitas Serius",
            vif_values > 5 ~ "Multikolinearitas Moderat",
            vif_values > 2 ~ "Multikolinearitas Ringan",
            TRUE ~ "Tidak Ada Multikolinearitas"
          )
        )
      } else {
        data.frame(
          Info = "VIF tidak dapat dihitung (model dengan satu prediktor atau library tidak tersedia)"
        )
      }
    })
    
    # Display correlation matrix
    output$correlation_matrix <- renderPlot({
      req(model_results$data, input$independent_vars)
      
      if (length(input$independent_vars) > 1) {
        cor_matrix <- cor(model_results$data[input$independent_vars], use = "complete.obs")
        
        if (requireNamespace("corrplot", quietly = TRUE)) {
          corrplot::corrplot(cor_matrix, method = "color", type = "upper", 
                             order = "hclust", tl.cex = 0.8, tl.col = "black",
                             addCoef.col = "black", number.cex = 0.7)
        } else {
          # Alternative using base R
          heatmap(cor_matrix, main = "Correlation Matrix", 
                  col = colorRampPalette(c("blue", "white", "red"))(100))
        }
      } else {
        plot(1, type = "n", main = "Correlation Matrix tidak tersedia untuk satu variabel")
      }
    })
    
    # Display influence plots
    output$influence_plots <- renderPlot({
      req(model_results$model)
      
      model <- model_results$model
      
      # Cook's distance and leverage plots
      par(mfrow = c(2, 2))
      
      # Cook's distance
      cooks_d <- cooks.distance(model)
      plot(cooks_d, type = "h", main = "Cook's Distance", 
           ylab = "Cook's Distance", xlab = "Observation")
      abline(h = 4/length(cooks_d), col = "red", lty = 2)
      
      # Leverage
      leverage <- hatvalues(model)
      plot(leverage, type = "h", main = "Leverage", 
           ylab = "Leverage", xlab = "Observation")
      abline(h = 2*length(coef(model))/length(leverage), col = "red", lty = 2)
      
      # Residuals vs Leverage
      plot(leverage, residuals(model), main = "Residuals vs Leverage",
           xlab = "Leverage", ylab = "Residuals")
      abline(h = 0, col = "red", lty = 2)
      
      # DFFITS
      if (requireNamespace("car", quietly = TRUE)) {
        dffits_vals <- dffits(model)
        plot(dffits_vals, type = "h", main = "DFFITS", 
             ylab = "DFFITS", xlab = "Observation")
        abline(h = c(-2, 2) * sqrt(length(coef(model))/length(dffits_vals)), 
               col = "red", lty = 2)
      } else {
        plot(1, type = "n", main = "DFFITS tidak tersedia")
      }
      
      par(mfrow = c(1, 1))
    })
    
    # Display outliers table
    output$outliers_table <- renderTable({
      req(model_results$model)
      
      model <- model_results$model
      
      # Identify outliers
      cooks_d <- cooks.distance(model)
      leverage <- hatvalues(model)
      standardized_residuals <- rstandard(model)
      
      # Thresholds
      cooks_threshold <- 4/length(cooks_d)
      leverage_threshold <- 2*length(coef(model))/length(leverage)
      residual_threshold <- 2
      
      # Find outliers
      outliers <- which(
        cooks_d > cooks_threshold | 
          leverage > leverage_threshold | 
          abs(standardized_residuals) > residual_threshold
      )
      
      if (length(outliers) > 0) {
        outlier_df <- data.frame(
          Observation = outliers,
          Cooks_Distance = round(cooks_d[outliers], 4),
          Leverage = round(leverage[outliers], 4),
          Std_Residual = round(standardized_residuals[outliers], 4),
          Outlier_Type = case_when(
            cooks_d[outliers] > cooks_threshold ~ "Influential",
            leverage[outliers] > leverage_threshold ~ "High Leverage",
            abs(standardized_residuals[outliers]) > residual_threshold ~ "Outlier",
            TRUE ~ "Multiple Issues"
          )
        )
        
        outlier_df[order(outlier_df$Cooks_Distance, decreasing = TRUE), ]
      } else {
        data.frame(Info = "Tidak ada outlier yang terdeteksi")
      }
    })
    
    # Display assumptions check
    output$assumptions_check <- renderText({
      req(model_results$assumptions)
      model_results$assumptions
    })
    
    # Display model interpretation
    output$model_interpretation <- renderText({
      req(model_results$model)
      
      model <- model_results$model
      summary_stats <- summary(model)
      
      # Generate comprehensive interpretation
      interpretation <- paste(
        "INTERPRETASI MODEL REGRESI LINEAR:\n",
        "==================================\n\n",
        "1. KEKUATAN MODEL:\n",
        "   - R-squared:", round(summary_stats$r.squared, 4), "\n",
        "   - Interpretasi:", interpret_r_squared(summary_stats$r.squared), "\n",
        "   - Adjusted R-squared:", round(summary_stats$adj.r.squared, 4), "\n",
        "   - Model menjelaskan", round(summary_stats$r.squared * 100, 1), 
        "% variasi dalam", input$dependent_var, "\n\n",
        
        "2. SIGNIFIKANSI MODEL:\n",
        "   - F-statistic:", round(summary_stats$fstatistic[1], 4), "\n",
        "   - p-value:", format_p_value(pf(summary_stats$fstatistic[1], 
                                           summary_stats$fstatistic[2], 
                                           summary_stats$fstatistic[3], 
                                           lower.tail = FALSE)), "\n",
        "   - Kesimpulan:", 
        if (pf(summary_stats$fstatistic[1], summary_stats$fstatistic[2], 
               summary_stats$fstatistic[3], lower.tail = FALSE) < 0.05) {
          "Model secara keseluruhan signifikan"
        } else {
          "Model secara keseluruhan tidak signifikan"
        }, "\n\n",
        
        "3. INTERPRETASI KOEFISIEN:\n",
        paste(sapply(2:length(coef(model)), function(i) {
          coef_name <- names(coef(model))[i]
          coef_value <- coef(model)[i]
          p_value <- summary_stats$coefficients[i, 4]
          
          paste("   -", coef_name, ":", round(coef_value, 4), "\n",
                "     Interpretasi: Setiap peningkatan 1 unit dalam", coef_name, 
                "akan mengubah", input$dependent_var, "sebesar", round(coef_value, 4), "\n",
                "     Signifikansi:", if (p_value < 0.05) "Signifikan" else "Tidak signifikan",
                "(p =", format_p_value(p_value), ")\n")
        }), collapse = "\n"), "\n",
        
        "4. KUALITAS PREDIKSI:\n",
        "   - RMSE:", round(sqrt(mean(residuals(model)^2)), 4), "\n",
        "   - MAE:", round(mean(abs(residuals(model))), 4), "\n",
        "   - Interpretasi: Model memiliki error rata-rata", 
        round(sqrt(mean(residuals(model)^2)), 4), "unit\n\n",
        
        "5. REKOMENDASI:\n",
        if (summary_stats$r.squared > 0.7) {
          "   - Model memiliki daya prediksi yang baik\n"
        } else if (summary_stats$r.squared > 0.5) {
          "   - Model memiliki daya prediksi yang cukup\n"
        } else {
          "   - Model memiliki daya prediksi yang lemah\n"
        },
        
        "   - Periksa asumsi regresi sebelum interpretasi final\n",
        "   - Pertimbangkan transformasi variabel jika asumsi tidak terpenuhi\n",
        "   - Validasi model dengan data baru jika memungkinkan\n",
        
        if (!is.null(model_results$diagnostics$vif) && any(model_results$diagnostics$vif > 5)) {
          "   - Atasi masalah multikolinearitas\n"
        } else {
          ""
        },
        
        if (!is.null(model_results$diagnostics$shapiro) && model_results$diagnostics$shapiro$p.value < 0.05) {
          "   - Pertimbangkan transformasi untuk mengatasi non-normalitas\n"
        } else {
          ""
        }
      )
      
      model_results$interpretation <- interpretation
      return(interpretation)
    })
    
    # Download handlers
    output$download_model <- downloadHandler(
      filename = function() {
        paste("model_regresi_", Sys.Date(), ".rds", sep = "")
      },
      content = function(file) {
        if (!is.null(model_results$model)) {
          saveRDS(model_results$model, file)
        }
      }
    )
    
    output$download_diagnostics <- downloadHandler(
      filename = function() {
        paste("diagnostik_regresi_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        if (!is.null(model_results$assumptions)) {
          writeLines(model_results$assumptions, file)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste("laporan_regresi_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        if (!is.null(model_results$model)) {
          # Create comprehensive report
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Analisis Regresi Linear", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Variabel Dependen:", input$dependent_var)) %>%
            officer::body_add_par(paste("Variabel Independen:", paste(input$independent_vars, collapse = ", "))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Ringkasan Model:", style = "heading 2") %>%
            officer::body_add_par(capture.output(summary(model_results$model)))
          
          if (!is.null(model_results$interpretation)) {
            doc <- doc %>%
              officer::body_add_par("Interpretasi:", style = "heading 2") %>%
              officer::body_add_par(model_results$interpretation)
          }
          
          if (!is.null(model_results$assumptions)) {
            doc <- doc %>%
              officer::body_add_par("Pemeriksaan Asumsi:", style = "heading 2") %>%
              officer::body_add_par(model_results$assumptions)
          }
          
          print(doc, target = file)
        }
      }
    )
  })
}
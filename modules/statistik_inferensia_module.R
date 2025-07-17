# Statistik Inferensia Module
# modules/statistik_inferensia_module.R

# UI function for Statistik Inferensia
statistikInferensiaUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Control Panel
    column(3,
           box(
             title = "Panel Kontrol Uji Statistik",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Test type selection
             selectInput(ns("test_type"), 
                        "Jenis Uji Statistik:",
                        choices = list(
                          "Uji t Satu Sampel" = "one_sample_t",
                          "Uji t Dua Sampel" = "two_sample_t",
                          "Uji Proporsi Satu Sampel" = "one_sample_prop",
                          "Uji Proporsi Dua Sampel" = "two_sample_prop",
                          "Uji Variansi" = "variance_test",
                          "ANOVA Satu Arah" = "anova_one_way",
                          "ANOVA Dua Arah" = "anova_two_way",
                          "Uji Chi-Square" = "chi_square"
                        )),
             
             # Variable selection
             selectInput(ns("test_variable"), 
                        "Variabel Uji:",
                        choices = NULL),
             
             # Conditional inputs for different tests
             conditionalPanel(
               condition = "input.test_type == 'one_sample_t'",
               ns = ns,
               numericInput(ns("mu0"), 
                           "Nilai Hipotesis (μ₀):",
                           value = 0)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'two_sample_t'",
               ns = ns,
               selectInput(ns("group_variable"), 
                          "Variabel Pengelompokan:",
                          choices = NULL),
               checkboxInput(ns("equal_var"), 
                            "Asumsi Variansi Sama", 
                            value = TRUE)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'one_sample_prop'",
               ns = ns,
               numericInput(ns("p0"), 
                           "Proporsi Hipotesis (p₀):",
                           value = 0.5,
                           min = 0,
                           max = 1,
                           step = 0.01)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'two_sample_prop'",
               ns = ns,
               selectInput(ns("group_variable_prop"), 
                          "Variabel Pengelompokan:",
                          choices = NULL)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'variance_test'",
               ns = ns,
               selectInput(ns("group_variable_var"), 
                          "Variabel Pengelompokan:",
                          choices = NULL)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'anova_one_way'",
               ns = ns,
               selectInput(ns("group_variable_anova"), 
                          "Variabel Pengelompokan:",
                          choices = NULL)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'anova_two_way'",
               ns = ns,
               selectInput(ns("factor1"), 
                          "Faktor 1:",
                          choices = NULL),
               selectInput(ns("factor2"), 
                          "Faktor 2:",
                          choices = NULL),
               checkboxInput(ns("interaction"), 
                            "Sertakan Interaksi", 
                            value = TRUE)
             ),
             
             conditionalPanel(
               condition = "input.test_type == 'chi_square'",
               ns = ns,
               selectInput(ns("var1_chi"), 
                          "Variabel 1:",
                          choices = NULL),
               selectInput(ns("var2_chi"), 
                          "Variabel 2:",
                          choices = NULL)
             ),
             
             # Alternative hypothesis
             conditionalPanel(
               condition = "input.test_type == 'one_sample_t' || input.test_type == 'two_sample_t'",
               ns = ns,
               selectInput(ns("alternative"), 
                          "Hipotesis Alternatif:",
                          choices = list(
                            "Dua arah (≠)" = "two.sided",
                            "Lebih besar (>)" = "greater",
                            "Lebih kecil (<)" = "less"
                          ))
             ),
             
             # Significance level
             numericInput(ns("alpha"), 
                         "Tingkat Signifikansi (α):",
                         value = 0.05,
                         min = 0.01,
                         max = 0.1,
                         step = 0.01),
             
             # Confidence level
             numericInput(ns("conf_level"), 
                         "Tingkat Kepercayaan:",
                         value = 0.95,
                         min = 0.8,
                         max = 0.99,
                         step = 0.01),
             
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
             title = "Visualisasi Hasil",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             tabsetPanel(
               tabPanel("Plot Utama",
                       br(),
                       withSpinner(plotOutput(ns("main_plot"), height = "400px"))),
               
               tabPanel("Distribusi",
                       br(),
                       withSpinner(plotOutput(ns("distribution_plot"), height = "400px"))),
               
               tabPanel("Confidence Interval",
                       br(),
                       withSpinner(plotOutput(ns("ci_plot"), height = "400px")))
             )
           ),
           
           # Summary Statistics
           box(
             title = "Statistik Deskriptif",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             withSpinner(tableOutput(ns("descriptive_stats")))
           ),
           
           # Additional Analysis
           box(
             title = "Analisis Tambahan",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             tabsetPanel(
               tabPanel("Effect Size",
                       br(),
                       withSpinner(tableOutput(ns("effect_size")))),
               
               tabPanel("Power Analysis",
                       br(),
                       withSpinner(verbatimTextOutput(ns("power_analysis")))),
               
               tabPanel("Post-hoc Tests",
                       br(),
                       withSpinner(tableOutput(ns("posthoc_tests"))))
             )
           )
    ),
    
    # Interpretation Section
    column(12,
           box(
             title = "Interpretasi dan Kesimpulan",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("interpretation")))
           )
    )
  )
}

# Server function for Statistik Inferensia
statistikInferensiaServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update variable choices
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data)
      
      updateSelectInput(session, "test_variable", 
                       choices = setNames(numeric_vars, numeric_vars))
      updateSelectInput(session, "group_variable", 
                       choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "group_variable_prop", 
                       choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "group_variable_var", 
                       choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "group_variable_anova", 
                       choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "factor1", 
                       choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "factor2", 
                       choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "var1_chi", 
                       choices = setNames(categorical_vars, categorical_vars))
      updateSelectInput(session, "var2_chi", 
                       choices = setNames(categorical_vars, categorical_vars))
    })
    
    # Reactive values for test results
    test_results <- reactiveValues(
      results = NULL,
      plot = NULL,
      interpretation = NULL,
      descriptive_stats = NULL,
      effect_size = NULL,
      power_analysis = NULL,
      posthoc = NULL
    )
    
    # Run statistical test
    observeEvent(input$run_test, {
      req(input$test_variable)
      
      data <- values$current_data
      
      if (input$test_type == "one_sample_t") {
        # One-sample t-test
        variable <- data[[input$test_variable]]
        test_result <- t.test(variable, mu = input$mu0, 
                             alternative = input$alternative,
                             conf.level = input$conf_level)
        test_results$results <- test_result
        
        # Create visualization
        test_results$plot <- ggplot(data, aes(x = .data[[input$test_variable]])) +
          geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7, color = "black") +
          geom_vline(xintercept = mean(variable, na.rm = TRUE), color = "red", linetype = "dashed", linewidth = 1) +
          geom_vline(xintercept = input$mu0, color = "blue", linetype = "dashed", linewidth = 1) +
          labs(title = paste("One-Sample t-test:", input$test_variable),
               subtitle = paste("Red: Sample mean, Blue: Hypothesized mean =", input$mu0),
               x = input$test_variable, y = "Frequency") +
          theme_custom()
        
        # Calculate effect size (Cohen's d)
        effect_size <- abs(mean(variable, na.rm = TRUE) - input$mu0) / sd(variable, na.rm = TRUE)
        test_results$effect_size <- data.frame(
          Measure = "Cohen's d",
          Value = round(effect_size, 3),
          Interpretation = case_when(
            effect_size < 0.2 ~ "Small effect",
            effect_size < 0.5 ~ "Medium effect",
            effect_size < 0.8 ~ "Large effect",
            TRUE ~ "Very large effect"
          )
        )
        
        # Generate interpretation
        interpretation <- paste(
          "INTERPRETASI UJI t SATU SAMPEL:\n",
          "===============================\n\n",
          "Hipotesis:\n",
          "H0: μ =", input$mu0, "\n",
          "H1: μ", switch(input$alternative, 
                         "two.sided" = "≠", 
                         "greater" = ">", 
                         "less" = "<"), input$mu0, "\n\n",
          "Hasil:\n",
          "- t-statistik:", round(test_result$statistic, 4), "\n",
          "- df:", test_result$parameter, "\n",
          "- p-value:", format_p_value(test_result$p.value), "\n",
          "- Confidence Interval (", input$conf_level * 100, "%):", 
          paste(round(test_result$conf.int, 3), collapse = " - "), "\n",
          "- Sample mean:", round(test_result$estimate, 3), "\n\n",
          "Kesimpulan:\n",
          interpret_t_test(test_result$p.value, input$alternative, input$alpha), "\n\n",
          "Effect Size:\n",
          "- Cohen's d:", round(effect_size, 3), "\n",
          "- Interpretasi:", test_results$effect_size$Interpretation
        )
        
      } else if (input$test_type == "two_sample_t") {
        req(input$group_variable)
        
        # Two-sample t-test
        formula_str <- paste(input$test_variable, "~", input$group_variable)
        test_result <- t.test(as.formula(formula_str), data = data,
                             var.equal = input$equal_var,
                             alternative = input$alternative,
                             conf.level = input$conf_level)
        test_results$results <- test_result
        
        # Create visualization
        test_results$plot <- ggplot(data, aes(x = .data[[input$group_variable]], 
                                             y = .data[[input$test_variable]])) +
          geom_boxplot(fill = "lightblue", alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
          labs(title = paste("Two-Sample t-test:", input$test_variable, "by", input$group_variable),
               x = input$group_variable, y = input$test_variable) +
          theme_custom()
        
        # Calculate effect size (Cohen's d)
        group_stats <- data %>%
          group_by(.data[[input$group_variable]]) %>%
          summarise(mean = mean(.data[[input$test_variable]], na.rm = TRUE),
                   sd = sd(.data[[input$test_variable]], na.rm = TRUE),
                   n = n(),
                   .groups = 'drop')
        
        if (nrow(group_stats) == 2) {
          pooled_sd <- sqrt(((group_stats$n[1] - 1) * group_stats$sd[1]^2 + 
                            (group_stats$n[2] - 1) * group_stats$sd[2]^2) / 
                           (group_stats$n[1] + group_stats$n[2] - 2))
          effect_size <- abs(group_stats$mean[1] - group_stats$mean[2]) / pooled_sd
          
          test_results$effect_size <- data.frame(
            Measure = "Cohen's d",
            Value = round(effect_size, 3),
            Interpretation = case_when(
              effect_size < 0.2 ~ "Small effect",
              effect_size < 0.5 ~ "Medium effect",
              effect_size < 0.8 ~ "Large effect",
              TRUE ~ "Very large effect"
            )
          )
        }
        
        # Generate interpretation
        interpretation <- paste(
          "INTERPRETASI UJI t DUA SAMPEL:\n",
          "==============================\n\n",
          "Hipotesis:\n",
          "H0: μ₁ = μ₂\n",
          "H1: μ₁", switch(input$alternative, 
                         "two.sided" = "≠", 
                         "greater" = ">", 
                         "less" = "<"), "μ₂\n\n",
          "Hasil:\n",
          "- t-statistik:", round(test_result$statistic, 4), "\n",
          "- df:", test_result$parameter, "\n",
          "- p-value:", format_p_value(test_result$p.value), "\n",
          "- Confidence Interval (", input$conf_level * 100, "%):", 
          paste(round(test_result$conf.int, 3), collapse = " - "), "\n",
          "- Mean difference:", round(diff(test_result$estimate), 3), "\n\n",
          "Kesimpulan:\n",
          interpret_t_test(test_result$p.value, input$alternative, input$alpha), "\n\n",
          "Effect Size:\n",
          if (!is.null(test_results$effect_size)) {
            paste("- Cohen's d:", test_results$effect_size$Value, "\n",
                  "- Interpretasi:", test_results$effect_size$Interpretation)
          } else {
            "- Tidak dapat dihitung"
          }
        )
        
      } else if (input$test_type == "anova_one_way") {
        req(input$group_variable_anova)
        
        # Validate data before ANOVA
        if (!input$group_variable_anova %in% names(data)) {
          showNotification("Variabel pengelompokan tidak ditemukan", type = "error")
          return()
        }
        
        # Remove missing values
        complete_data <- data[complete.cases(data[[input$test_variable]], data[[input$group_variable_anova]]), ]
        
        if (nrow(complete_data) < 3) {
          showNotification("Data tidak cukup untuk ANOVA", type = "error")
          return()
        }
        
        # Check if there are at least 2 groups
        groups <- unique(complete_data[[input$group_variable_anova]])
        if (length(groups) < 2) {
          showNotification("Minimal harus ada 2 kelompok untuk ANOVA", type = "error")
          return()
        }
        
        # One-way ANOVA
        formula_str <- paste(input$test_variable, "~", input$group_variable_anova)
        anova_result <- aov(as.formula(formula_str), data = complete_data)
        anova_summary <- summary(anova_result)
        test_results$results <- anova_summary
        
        # Create visualization
        test_results$plot <- ggplot(complete_data, aes(x = .data[[input$group_variable_anova]], 
                                                      y = .data[[input$test_variable]])) +
          geom_boxplot(fill = "lightgreen", alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
          labs(title = paste("One-Way ANOVA:", input$test_variable, "by", input$group_variable_anova),
               x = input$group_variable_anova, y = input$test_variable) +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Calculate eta-squared (effect size) - with error checking
        tryCatch({
          ss_total <- sum((complete_data[[input$test_variable]] - mean(complete_data[[input$test_variable]], na.rm = TRUE))^2, na.rm = TRUE)
          
          # Check if ANOVA results are valid
          if (length(anova_summary) > 0 && "Sum Sq" %in% names(anova_summary[[1]])) {
            ss_between <- anova_summary[[1]][["Sum Sq"]][1]
            eta_squared <- ss_between / ss_total
            
            test_results$effect_size <- data.frame(
              Measure = "Eta-squared",
              Value = round(eta_squared, 3),
              Interpretation = case_when(
                eta_squared < 0.01 ~ "Small effect",
                eta_squared < 0.06 ~ "Medium effect",
                eta_squared < 0.14 ~ "Large effect",
                TRUE ~ "Very large effect"
              )
            )
          } else {
            test_results$effect_size <- data.frame(
              Measure = "Eta-squared",
              Value = NA,
              Interpretation = "Cannot be calculated"
            )
          }
        }, error = function(e) {
          test_results$effect_size <- data.frame(
            Measure = "Eta-squared",
            Value = NA,
            Interpretation = "Cannot be calculated"
          )
        })
        
        # Post-hoc test if significant - with error checking
        tryCatch({
          if (length(anova_summary) > 0 && "Pr(>F)" %in% names(anova_summary[[1]])) {
            p_value <- anova_summary[[1]][["Pr(>F)"]][1]
            if (!is.na(p_value) && length(p_value) > 0 && p_value < input$alpha) {
              posthoc_result <- pairwise.t.test(complete_data[[input$test_variable]], 
                                               complete_data[[input$group_variable_anova]], 
                                               p.adjust.method = "bonferroni")
              test_results$posthoc <- posthoc_result
            }
          }
        }, error = function(e) {
          # Post-hoc test failed, continue without it
        })
        
        # Generate interpretation - with error checking
        tryCatch({
          if (length(anova_summary) > 0 && all(c("F value", "Df", "Pr(>F)") %in% names(anova_summary[[1]]))) {
            f_value <- anova_summary[[1]][["F value"]][1]
            df1 <- anova_summary[[1]][["Df"]][1]
            df2 <- anova_summary[[1]][["Df"]][2]
            p_value <- anova_summary[[1]][["Pr(>F)"]][1]
            
            interpretation <- paste(
              "INTERPRETASI ANOVA SATU ARAH:\n",
              "=============================\n\n",
              "Hipotesis:\n",
              "H0: μ₁ = μ₂ = μ₃ = ... (semua rata-rata sama)\n",
              "H1: Minimal ada satu rata-rata yang berbeda\n\n",
              "Hasil:\n",
              "- F-statistik:", round(f_value, 4), "\n",
              "- df:", df1, ",", df2, "\n",
              "- p-value:", format_p_value(p_value), "\n\n",
              "Kesimpulan:\n",
              interpret_anova(p_value, input$alpha), "\n\n",
              "Effect Size:\n",
              "- Eta-squared:", ifelse(is.na(test_results$effect_size$Value), "Tidak dapat dihitung", test_results$effect_size$Value), "\n",
              "- Interpretasi:", test_results$effect_size$Interpretation, "\n\n",
              if (!is.na(p_value) && p_value < input$alpha) {
                "Rekomendasi:\n- Lakukan uji post-hoc untuk mengetahui kelompok mana yang berbeda\n- Periksa asumsi normalitas dan homogenitas"
              } else {
                "Rekomendasi:\n- Tidak ada perbedaan signifikan antar kelompok\n- Pertimbangkan faktor lain yang mungkin berpengaruh"
              }
            )
          } else {
            interpretation <- "Error: Tidak dapat memproses hasil ANOVA"
          }
        }, error = function(e) {
          interpretation <- paste("Error dalam interpretasi ANOVA:", e$message)
        })
        
      } else if (input$test_type == "chi_square") {
        req(input$var1_chi, input$var2_chi)
        
        # Chi-square test of independence
        cont_table <- table(data[[input$var1_chi]], data[[input$var2_chi]])
        test_result <- chisq.test(cont_table)
        test_results$results <- test_result
        
        # Create visualization
        cont_df <- as.data.frame(cont_table)
        names(cont_df) <- c("Var1", "Var2", "Freq")
        
        test_results$plot <- ggplot(cont_df, aes(x = Var1, y = Var2, fill = Freq)) +
          geom_tile() +
          geom_text(aes(label = Freq), color = "white", size = 4) +
          scale_fill_gradient(low = "lightblue", high = "darkblue") +
          labs(title = paste("Chi-Square Test:", input$var1_chi, "vs", input$var2_chi),
               x = input$var1_chi, y = input$var2_chi) +
          theme_custom() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # Calculate Cramer's V (effect size)
        n <- sum(cont_table)
        cramers_v <- sqrt(test_result$statistic / (n * (min(nrow(cont_table), ncol(cont_table)) - 1)))
        
        test_results$effect_size <- data.frame(
          Measure = "Cramer's V",
          Value = round(cramers_v, 3),
          Interpretation = case_when(
            cramers_v < 0.1 ~ "Small effect",
            cramers_v < 0.3 ~ "Medium effect",
            cramers_v < 0.5 ~ "Large effect",
            TRUE ~ "Very large effect"
          )
        )
        
        # Generate interpretation
        interpretation <- paste(
          "INTERPRETASI UJI CHI-SQUARE:\n",
          "============================\n\n",
          "Hipotesis:\n",
          "H0: Tidak ada asosiasi antara", input$var1_chi, "dan", input$var2_chi, "\n",
          "H1: Ada asosiasi antara", input$var1_chi, "dan", input$var2_chi, "\n\n",
          "Hasil:\n",
          "- Chi-square statistik:", round(test_result$statistic, 4), "\n",
          "- df:", test_result$parameter, "\n",
          "- p-value:", format_p_value(test_result$p.value), "\n\n",
          "Kesimpulan:\n",
          if (test_result$p.value < input$alpha) {
            paste("Terdapat asosiasi yang signifikan (p-value =", 
                  format_p_value(test_result$p.value), "< α =", input$alpha, ")")
          } else {
            paste("Tidak terdapat asosiasi yang signifikan (p-value =", 
                  format_p_value(test_result$p.value), "> α =", input$alpha, ")")
          }, "\n\n",
          "Effect Size:\n",
          "- Cramer's V:", test_results$effect_size$Value, "\n",
          "- Interpretasi:", test_results$effect_size$Interpretation
        )
      }
      
      test_results$interpretation <- interpretation
      
      # Calculate descriptive statistics
      if (input$test_type %in% c("one_sample_t", "two_sample_t")) {
        if (input$test_type == "one_sample_t") {
          test_results$descriptive_stats <- create_summary_table(data, input$test_variable)
        } else {
          test_results$descriptive_stats <- data %>%
            group_by(.data[[input$group_variable]]) %>%
            summarise(
              N = n(),
              Mean = round(mean(.data[[input$test_variable]], na.rm = TRUE), 3),
              SD = round(sd(.data[[input$test_variable]], na.rm = TRUE), 3),
              SE = round(sd(.data[[input$test_variable]], na.rm = TRUE) / sqrt(n()), 3),
              Min = round(min(.data[[input$test_variable]], na.rm = TRUE), 3),
              Max = round(max(.data[[input$test_variable]], na.rm = TRUE), 3),
              .groups = 'drop'
            )
        }
      } else if (input$test_type == "anova_one_way") {
        test_results$descriptive_stats <- data %>%
          group_by(.data[[input$group_variable_anova]]) %>%
          summarise(
            N = n(),
            Mean = round(mean(.data[[input$test_variable]], na.rm = TRUE), 3),
            SD = round(sd(.data[[input$test_variable]], na.rm = TRUE), 3),
            SE = round(sd(.data[[input$test_variable]], na.rm = TRUE) / sqrt(n()), 3),
            .groups = 'drop'
          )
      }
    })
    
    # Display test results
    output$test_results <- renderText({
      req(test_results$results)
      capture.output(print(test_results$results))
    })
    
    # Display main plot
    output$main_plot <- renderPlot({
      req(test_results$plot)
      test_results$plot
    })
    
    # Display descriptive statistics
    output$descriptive_stats <- renderTable({
      req(test_results$descriptive_stats)
      test_results$descriptive_stats
    })
    
    # Display effect size
    output$effect_size <- renderTable({
      req(test_results$effect_size)
      test_results$effect_size
    })
    
    # Display interpretation
    output$interpretation <- renderText({
      req(test_results$interpretation)
      test_results$interpretation
    })
    
    # Display post-hoc tests
    output$posthoc_tests <- renderTable({
      if (!is.null(test_results$posthoc)) {
        if (input$test_type == "anova_one_way") {
          # Format pairwise t-test results
          posthoc_matrix <- test_results$posthoc$p.value
          posthoc_df <- data.frame(
            Comparison = paste(rownames(posthoc_matrix), "vs", rep(colnames(posthoc_matrix), each = nrow(posthoc_matrix))),
            P_value = as.vector(posthoc_matrix),
            Significant = ifelse(as.vector(posthoc_matrix) < input$alpha, "Yes", "No")
          )
          posthoc_df <- posthoc_df[!is.na(posthoc_df$P_value), ]
          return(posthoc_df)
        }
      } else {
        return(data.frame(Info = "Tidak ada uji post-hoc yang dilakukan"))
      }
    })
    
    # Download handlers
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("plot_statistik_inferensia_", input$test_type, "_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        if (!is.null(test_results$plot)) {
          ggsave(file, test_results$plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
    
    output$download_results <- downloadHandler(
      filename = function() {
        paste("hasil_statistik_inferensia_", input$test_type, "_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        if (!is.null(test_results$results)) {
          writeLines(capture.output(print(test_results$results)), file)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste("laporan_statistik_inferensia_", input$test_type, "_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        if (!is.null(test_results$interpretation)) {
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Statistik Inferensia", style = "heading 1") %>%
            officer::body_add_par(paste("Jenis Uji:", input$test_type)) %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Hasil dan Interpretasi:", style = "heading 2") %>%
            officer::body_add_par(test_results$interpretation)
          
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
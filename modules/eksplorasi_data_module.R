# Eksplorasi Data Module
# modules/eksplorasi_data_module.R

# UI function for Eksplorasi Data
eksplorasiDataUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Control Panel
    column(3,
           box(
             title = "Panel Kontrol Eksplorasi",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Variable selection
             selectInput(ns("primary_var"), 
                         "Variabel Utama:",
                         choices = NULL),
             
             selectInput(ns("secondary_var"), 
                         "Variabel Pembanding (Opsional):",
                         choices = NULL),
             
             # Analysis type
             radioButtons(ns("analysis_type"), 
                          "Jenis Analisis:",
                          choices = list(
                            "Univariat" = "univariate",
                            "Bivariate" = "bivariate",
                            "Korelasi" = "correlation",
                            "Geografis" = "geographic"
                          )),
             
             # Plot type for univariate
             conditionalPanel(
               condition = "input.analysis_type == 'univariate'",
               ns = ns,
               selectInput(ns("plot_type_uni"), 
                           "Jenis Visualisasi:",
                           choices = list(
                             "Histogram" = "histogram",
                             "Boxplot" = "boxplot",
                             "Density Plot" = "density",
                             "Q-Q Plot" = "qqplot"
                           ))
             ),
             
             # Plot type for bivariate
             conditionalPanel(
               condition = "input.analysis_type == 'bivariate'",
               ns = ns,
               selectInput(ns("plot_type_bi"), 
                           "Jenis Visualisasi:",
                           choices = list(
                             "Scatter Plot" = "scatter",
                             "Box Plot Grouped" = "boxplot_grouped",
                             "Bar Chart" = "bar",
                             "Violin Plot" = "violin"
                           ))
             ),
             
             # Group variable
             conditionalPanel(
               condition = "input.analysis_type == 'bivariate' || input.analysis_type == 'geographic'",
               ns = ns,
               selectInput(ns("group_var"), 
                           "Variabel Pengelompokan:",
                           choices = NULL)
             ),
             
             # Action buttons
             br(),
             actionButton(ns("generate_analysis"), 
                          "Generate Analisis", 
                          class = "btn-success"),
             br(), br(),
             
             # Download options
             h5("Download Hasil:"),
             downloadButton(ns("download_plot"), 
                            "Download Plot", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_table"), 
                            "Download Tabel", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_report"), 
                            "Download Laporan", 
                            class = "btn-info")
           )
    ),
    
    # Main Content
    column(9,
           # Statistics Summary
           box(
             title = "Statistik Deskriptif",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(tableOutput(ns("descriptive_stats")))
           ),
           
           # Main Visualization
           box(
             title = "Visualisasi Data",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             withSpinner(plotOutput(ns("main_plot"), height = "500px"))
           ),
           
           # Additional Analysis
           box(
             title = "Analisis Tambahan",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             tabsetPanel(
               tabPanel("Tabel Ringkasan",
                        br(),
                        withSpinner(DT::dataTableOutput(ns("summary_table")))),
               
               tabPanel("Outlier Analysis",
                        br(),
                        withSpinner(tableOutput(ns("outlier_table"))),
                        br(),
                        withSpinner(plotOutput(ns("outlier_plot")))),
               
               tabPanel("Missing Values",
                        br(),
                        withSpinner(tableOutput(ns("missing_table"))),
                        br(),
                        withSpinner(plotOutput(ns("missing_plot")))),
               
               tabPanel("Correlation Matrix",
                        br(),
                        withSpinner(plotOutput(ns("correlation_plot"))),
                        br(),
                        withSpinner(tableOutput(ns("correlation_table"))))
             )
           )
    ),
    
    # Interpretation Section
    column(12,
           box(
             title = "Interpretasi Hasil Eksplorasi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("interpretation")))
           )
    )
  )
}

# Server function for Eksplorasi Data
eksplorasiDataServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update variable choices
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      categorical_vars <- get_categorical_vars(values$current_data)
      all_vars <- c(numeric_vars, categorical_vars)
      
      updateSelectInput(session, "primary_var", 
                        choices = setNames(all_vars, all_vars))
      updateSelectInput(session, "secondary_var", 
                        choices = c("Tidak ada" = "", setNames(all_vars, all_vars)))
      updateSelectInput(session, "group_var", 
                        choices = c("Tidak ada" = "", setNames(categorical_vars, categorical_vars)))
    })
    
    # Reactive values for analysis results
    analysis_results <- reactiveValues(
      stats = NULL,
      plot = NULL,
      interpretation = NULL
    )
    
    # Generate analysis
    observeEvent(input$generate_analysis, {
      req(input$primary_var)
      
      data <- values$current_data
      
      if (input$analysis_type == "univariate") {
        # Univariate analysis
        analysis_results$stats <- calculate_basic_stats(data, input$primary_var)
        
        if (input$plot_type_uni == "histogram") {
          analysis_results$plot <- ggplot(data, aes(x = .data[[input$primary_var]])) +
            geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7, color = "black") +
            labs(title = paste("Histogram", input$primary_var),
                 x = input$primary_var, y = "Frekuensi") +
            theme_custom()
        } else if (input$plot_type_uni == "boxplot") {
          analysis_results$plot <- ggplot(data, aes(x = "", y = .data[[input$primary_var]])) +
            geom_boxplot(fill = "lightcoral", alpha = 0.7) +
            labs(title = paste("Boxplot", input$primary_var),
                 x = "", y = input$primary_var) +
            theme_custom()
        } else if (input$plot_type_uni == "density") {
          analysis_results$plot <- ggplot(data, aes(x = .data[[input$primary_var]])) +
            geom_density(fill = "lightgreen", alpha = 0.7) +
            labs(title = paste("Density Plot", input$primary_var),
                 x = input$primary_var, y = "Density") +
            theme_custom()
        } else if (input$plot_type_uni == "qqplot") {
          analysis_results$plot <- ggplot(data, aes(sample = .data[[input$primary_var]])) +
            stat_qq() + stat_qq_line() +
            labs(title = paste("Q-Q Plot", input$primary_var)) +
            theme_custom()
        }
        
        # Generate interpretation
        if (is.numeric(data[[input$primary_var]])) {
          stats <- analysis_results$stats
          interpretation <- paste(
            "INTERPRETASI ANALISIS UNIVARIAT:\n",
            "=================================\n\n",
            "Variabel:", input$primary_var, "\n\n",
            "Statistik Deskriptif:\n",
            "- Mean:", round(stats$Mean, 3), "\n",
            "- Median:", round(stats$Median, 3), "\n",
            "- Standard Deviation:", round(stats$StdDev, 3), "\n",
            "- Minimum:", round(stats$Min, 3), "\n",
            "- Maximum:", round(stats$Max, 3), "\n",
            "- Skewness:", round(stats$Skewness, 3), "\n",
            "- Kurtosis:", round(stats$Kurtosis, 3), "\n\n",
            "Interpretasi Skewness:\n",
            ifelse(abs(stats$Skewness) < 0.5, "- Distribusi relatif simetris",
                   ifelse(stats$Skewness > 0, "- Distribusi menceng ke kanan (right-skewed)",
                          "- Distribusi menceng ke kiri (left-skewed)")), "\n\n",
            "Interpretasi Kurtosis:\n",
            ifelse(abs(stats$Kurtosis - 3) < 0.5, "- Distribusi normal (mesokurtic)",
                   ifelse(stats$Kurtosis > 3, "- Distribusi leptokurtic (runcing)",
                          "- Distribusi platykurtic (datar)")), "\n\n",
            "Outliers terdeteksi:", detect_outliers(data, input$primary_var), "observasi\n\n",
            "Rekomendasi:\n",
            "- Periksa outliers untuk validitas data\n",
            "- Pertimbangkan transformasi jika distribusi tidak normal\n",
            "- Gunakan median jika data menceng"
          )
        }
        
      } else if (input$analysis_type == "bivariate") {
        req(input$secondary_var)
        
        if (input$plot_type_bi == "scatter") {
          analysis_results$plot <- ggplot(data, aes(x = .data[[input$primary_var]], y = .data[[input$secondary_var]])) +
            geom_point(alpha = 0.6) +
            geom_smooth(method = "lm", se = TRUE) +
            labs(title = paste("Scatter Plot:", input$primary_var, "vs", input$secondary_var)) +
            theme_custom()
        } else if (input$plot_type_bi == "boxplot_grouped") {
          analysis_results$plot <- ggplot(data, aes(x = .data[[input$primary_var]], y = .data[[input$secondary_var]])) +
            geom_boxplot(fill = "lightblue", alpha = 0.7) +
            labs(title = paste("Boxplot Grouped:", input$secondary_var, "by", input$primary_var)) +
            theme_custom()
        }
        
        # Calculate correlation if both variables are numeric
        if (is.numeric(data[[input$primary_var]]) && is.numeric(data[[input$secondary_var]])) {
          correlation <- cor(data[[input$primary_var]], data[[input$secondary_var]], use = "complete.obs")
          interpretation <- paste(
            "INTERPRETASI ANALISIS BIVARIATE:\n",
            "=================================\n\n",
            "Variabel 1:", input$primary_var, "\n",
            "Variabel 2:", input$secondary_var, "\n\n",
            "Korelasi Pearson:", round(correlation, 3), "\n",
            "Interpretasi:", interpret_correlation(correlation), "\n\n",
            "Signifikansi korelasi:\n",
            ifelse(abs(correlation) > 0.3, "- Korelasi cukup signifikan untuk dianalisis lebih lanjut",
                   "- Korelasi lemah, hubungan mungkin tidak linear"), "\n\n",
            "Rekomendasi:\n",
            "- Periksa asumsi linearitas\n",
            "- Pertimbangkan faktor confounding\n",
            "- Lakukan uji signifikansi korelasi"
          )
        }
      }
      
      analysis_results$interpretation <- interpretation
    })
    
    # Display descriptive statistics
    output$descriptive_stats <- renderTable({
      req(analysis_results$stats)
      
      if (is.numeric(values$current_data[[input$primary_var]])) {
        create_summary_table(values$current_data, input$primary_var)
      } else {
        # For categorical variables
        table_data <- table(values$current_data[[input$primary_var]])
        prop_data <- prop.table(table_data) * 100
        
        data.frame(
          Kategori = names(table_data),
          Frekuensi = as.numeric(table_data),
          Persentase = round(as.numeric(prop_data), 2)
        )
      }
    })
    
    # Display main plot
    output$main_plot <- renderPlot({
      req(analysis_results$plot)
      analysis_results$plot
    })
    
    # Display summary table
    output$summary_table <- DT::renderDataTable({
      req(input$primary_var)
      
      if (input$analysis_type == "univariate") {
        summary_data <- values$current_data %>%
          select(all_of(input$primary_var)) %>%
          summary()
        
        DT::datatable(
          as.data.frame(summary_data),
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE
        )
      } else if (input$analysis_type == "bivariate" && input$secondary_var != "") {
        summary_data <- values$current_data %>%
          select(all_of(c(input$primary_var, input$secondary_var))) %>%
          summary()
        
        DT::datatable(
          as.data.frame(summary_data),
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE
        )
      }
    })
    
    # Display outlier analysis
    output$outlier_table <- renderTable({
      req(input$primary_var)
      
      if (is.numeric(values$current_data[[input$primary_var]])) {
        data <- values$current_data
        Q1 <- quantile(data[[input$primary_var]], 0.25, na.rm = TRUE)
        Q3 <- quantile(data[[input$primary_var]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        
        outliers <- data[data[[input$primary_var]] < lower_bound | data[[input$primary_var]] > upper_bound, ]
        
        if (nrow(outliers) > 0) {
          outliers[, c("DISTRICTCODE", input$primary_var)]
        } else {
          data.frame(Info = "Tidak ada outlier terdeteksi")
        }
      }
    })
    
    # Display outlier plot
    output$outlier_plot <- renderPlot({
      req(input$primary_var)
      
      if (is.numeric(values$current_data[[input$primary_var]])) {
        ggplot(values$current_data, aes(x = "", y = .data[[input$primary_var]])) +
          geom_boxplot(fill = "lightcoral", alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.5) +
          labs(title = paste("Outlier Detection:", input$primary_var),
               x = "", y = input$primary_var) +
          theme_custom()
      }
    })
    
    # Display missing values table
    output$missing_table <- renderTable({
      missing_summary <- values$current_data %>%
        summarise_all(~sum(is.na(.))) %>%
        gather(key = "Variable", value = "Missing_Count") %>%
        mutate(Missing_Percentage = round(Missing_Count / nrow(values$current_data) * 100, 2)) %>%
        arrange(desc(Missing_Count))
      
      missing_summary
    })
    
    # Display missing values plot
    output$missing_plot <- renderPlot({
      if (requireNamespace("VIM", quietly = TRUE)) {
        VIM::aggr(values$current_data, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE)
      } else {
        # Alternative plot if VIM is not available
        missing_data <- values$current_data %>%
          summarise_all(~sum(is.na(.))) %>%
          gather(key = "Variable", value = "Missing_Count") %>%
          filter(Missing_Count > 0)
        
        ggplot(missing_data, aes(x = reorder(Variable, Missing_Count), y = Missing_Count)) +
          geom_col(fill = "red", alpha = 0.7) +
          coord_flip() +
          labs(title = "Missing Values by Variable",
               x = "Variable", y = "Missing Count") +
          theme_custom()
      }
    })
    
    # Display correlation plot
    output$correlation_plot <- renderPlot({
      numeric_vars <- get_numeric_vars(values$current_data)
      if (length(numeric_vars) > 1) {
        cor_matrix <- cor(values$current_data[numeric_vars], use = "complete.obs")
        corrplot::corrplot(cor_matrix, method = "circle", type = "upper", 
                           order = "hclust", tl.cex = 0.8, tl.col = "black")
      }
    })
    
    # Display correlation table
    output$correlation_table <- renderTable({
      numeric_vars <- get_numeric_vars(values$current_data)
      if (length(numeric_vars) > 1) {
        cor_matrix <- cor(values$current_data[numeric_vars], use = "complete.obs")
        cor_matrix[upper.tri(cor_matrix)] <- NA
        diag(cor_matrix) <- NA
        
        cor_df <- as.data.frame(cor_matrix) %>%
          rownames_to_column("Variable1") %>%
          gather(key = "Variable2", value = "Correlation", -Variable1) %>%
          filter(!is.na(Correlation)) %>%
          arrange(desc(abs(Correlation)))
        
        cor_df$Correlation <- round(cor_df$Correlation, 3)
        cor_df$Interpretation <- sapply(cor_df$Correlation, interpret_correlation)
        
        cor_df
      }
    })
    
    # Display interpretation
    output$interpretation <- renderText({
      req(analysis_results$interpretation)
      analysis_results$interpretation
    })
    
    # Download handlers
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("plot_eksplorasi_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        if (!is.null(analysis_results$plot)) {
          ggsave(file, analysis_results$plot, width = 10, height = 6, dpi = 300)
        }
      }
    )
    
    output$download_table <- downloadHandler(
      filename = function() {
        paste("tabel_eksplorasi_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        if (!is.null(analysis_results$stats)) {
          write.csv(create_summary_table(values$current_data, input$primary_var), 
                    file, row.names = FALSE)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste("laporan_eksplorasi_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        # Create comprehensive report
        doc <- officer::read_docx()
        
        doc <- doc %>%
          officer::body_add_par("Laporan Eksplorasi Data", style = "heading 1") %>%
          officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
          officer::body_add_par(paste("Variabel Utama:", input$primary_var)) %>%
          officer::body_add_par(" ") %>%
          officer::body_add_par("Interpretasi:", style = "heading 2") %>%
          officer::body_add_par(analysis_results$interpretation)
        
        print(doc, target = file)
      }
    )
  })
}
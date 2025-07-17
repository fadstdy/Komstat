# Manajemen Data Module
# modules/manajemen_data_module.R

# UI function for Manajemen Data
manajemenDataUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Control panel
    column(4,
           box(
             title = "Panel Kontrol Manajemen Data",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             
             # Variable selection
             selectInput(ns("select_variable"), 
                         "Pilih Variabel:",
                         choices = NULL),
             
             # Transformation type
             radioButtons(ns("transformation_type"), 
                          "Jenis Transformasi:",
                          choices = list(
                            "Kategorisasi Manual" = "manual",
                            "Kategorisasi Otomatis" = "auto",
                            "Transformasi Logaritma" = "log",
                            "Transformasi Akar Kuadrat" = "sqrt",
                            "Standardisasi (Z-score)" = "zscore"
                          )),
             
             # Conditional inputs based on transformation type
             conditionalPanel(
               condition = "input.transformation_type == 'manual'",
               ns = ns,
               textInput(ns("custom_breaks"), 
                         "Titik Potong (pisahkan dengan koma):",
                         value = ""),
               textInput(ns("custom_labels"), 
                         "Label Kategori (pisahkan dengan koma):",
                         value = "")
             ),
             
             conditionalPanel(
               condition = "input.transformation_type == 'auto'",
               ns = ns,
               numericInput(ns("n_categories"), 
                            "Jumlah Kategori:", 
                            value = 3, 
                            min = 2, 
                            max = 10),
               selectInput(ns("auto_method"), 
                           "Metode Kategorisasi:",
                           choices = list(
                             "Kuartil" = "quartile",
                             "Interval Sama" = "equal",
                             "Jenks Natural Breaks" = "jenks"
                           ))
             ),
             
             # Action buttons
             br(),
             actionButton(ns("apply_transformation"), 
                          "Terapkan Transformasi", 
                          class = "btn-success"),
             br(), br(),
             actionButton(ns("reset_data"), 
                          "Reset ke Data Asli", 
                          class = "btn-warning"),
             
             # Download options
             hr(),
             h5("Download Hasil:"),
             downloadButton(ns("download_transformed"), 
                            "Download Data Transformasi", 
                            class = "btn-primary"),
             br(), br(),
             downloadButton(ns("download_report"), 
                            "Download Laporan", 
                            class = "btn-info")
           )
    ),
    
    # Main content area
    column(8,
           # Summary statistics before transformation
           box(
             title = "Statistik Deskriptif Sebelum Transformasi",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(tableOutput(ns("before_stats")))
           ),
           
           # Visualization before transformation
           box(
             title = "Visualisasi Sebelum Transformasi",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(plotOutput(ns("before_plot")))
           )
    ),
    
    # Results after transformation
    column(12,
           box(
             title = "Hasil Transformasi",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             
             # Tabs for different views
             tabsetPanel(
               tabPanel("Statistik Deskriptif",
                        br(),
                        withSpinner(tableOutput(ns("after_stats")))),
               
               tabPanel("Visualisasi",
                        br(),
                        withSpinner(plotOutput(ns("after_plot")))),
               
               tabPanel("Tabel Kontingensi",
                        br(),
                        withSpinner(tableOutput(ns("contingency_table")))),
               
               tabPanel("Data Preview",
                        br(),
                        withSpinner(DT::dataTableOutput(ns("data_preview"))))
             )
           )
    ),
    
    # Interpretation section
    column(12,
           box(
             title = "Interpretasi Hasil Transformasi",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             withSpinner(verbatimTextOutput(ns("interpretation")))
           )
    )
  )
}

# Server function for Manajemen Data
manajemenDataServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Update variable choices
    observe({
      numeric_vars <- get_numeric_vars(values$current_data)
      updateSelectInput(session, "select_variable", 
                        choices = setNames(numeric_vars, numeric_vars))
    })
    
    # Reactive values for transformation results
    transformation_results <- reactiveValues(
      before_data = NULL,
      after_data = NULL,
      transformation_info = NULL
    )
    
    # Display statistics before transformation
    output$before_stats <- renderTable({
      req(input$select_variable)
      if (input$select_variable %in% names(values$current_data)) {
        create_summary_table(values$current_data, input$select_variable)
      }
    })
    
    # Display plot before transformation
    output$before_plot <- renderPlot({
      req(input$select_variable)
      if (input$select_variable %in% names(values$current_data)) {
        data <- values$current_data
        var_name <- input$select_variable
        
        p1 <- ggplot(data, aes(x = .data[[var_name]])) +
          geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7, color = "black") +
          labs(title = paste("Histogram", var_name),
               x = var_name, y = "Frekuensi") +
          theme_custom()
        
        p2 <- ggplot(data, aes(x = "", y = .data[[var_name]])) +
          geom_boxplot(fill = "lightcoral", alpha = 0.7) +
          labs(title = paste("Boxplot", var_name),
               x = "", y = var_name) +
          theme_custom()
        
        gridExtra::grid.arrange(p1, p2, ncol = 2)
      }
    })
    
    # Apply transformation
    observeEvent(input$apply_transformation, {
      req(input$select_variable)
      
      data <- values$current_data
      var_name <- input$select_variable
      
      # Store original data
      transformation_results$before_data <- data[[var_name]]
      
      # Apply transformation based on type
      if (input$transformation_type == "manual") {
        req(input$custom_breaks, input$custom_labels)
        
        breaks <- as.numeric(unlist(strsplit(input$custom_breaks, ",")))
        labels <- trimws(unlist(strsplit(input$custom_labels, ",")))
        
        if (length(breaks) == length(labels) + 1) {
          data <- create_categorical_var(data, var_name, breaks, labels)
          transformation_results$transformation_info <- list(
            type = "Manual Categorization",
            breaks = breaks,
            labels = labels
          )
        } else {
          showNotification("Jumlah breaks harus sama dengan jumlah labels + 1", 
                           type = "error")
          return()
        }
        
      } else if (input$transformation_type == "auto") {
        req(input$n_categories, input$auto_method)
        
        n_cat <- input$n_categories
        method <- input$auto_method
        
        if (method == "quartile") {
          breaks <- quantile(data[[var_name]], 
                             probs = seq(0, 1, length.out = n_cat + 1), 
                             na.rm = TRUE)
          labels <- paste("Q", 1:n_cat, sep = "")
        } else if (method == "equal") {
          breaks <- seq(min(data[[var_name]], na.rm = TRUE), 
                        max(data[[var_name]], na.rm = TRUE), 
                        length.out = n_cat + 1)
          labels <- paste("Cat", 1:n_cat, sep = "")
        } else if (method == "jenks") {
          # Simplified jenks - using equal intervals for now
          breaks <- seq(min(data[[var_name]], na.rm = TRUE), 
                        max(data[[var_name]], na.rm = TRUE), 
                        length.out = n_cat + 1)
          labels <- paste("Group", 1:n_cat, sep = "")
        }
        
        data <- create_categorical_var(data, var_name, breaks, labels)
        transformation_results$transformation_info <- list(
          type = paste("Auto Categorization -", method),
          breaks = breaks,
          labels = labels,
          n_categories = n_cat
        )
        
      } else if (input$transformation_type == "log") {
        new_var <- paste0(var_name, "_log")
        data[[new_var]] <- log(data[[var_name]] + 1)  # Adding 1 to avoid log(0)
        transformation_results$transformation_info <- list(
          type = "Log Transformation",
          new_variable = new_var
        )
        
      } else if (input$transformation_type == "sqrt") {
        new_var <- paste0(var_name, "_sqrt")
        data[[new_var]] <- sqrt(data[[var_name]])
        transformation_results$transformation_info <- list(
          type = "Square Root Transformation",
          new_variable = new_var
        )
        
      } else if (input$transformation_type == "zscore") {
        new_var <- paste0(var_name, "_zscore")
        data[[new_var]] <- scale(data[[var_name]])[,1]
        transformation_results$transformation_info <- list(
          type = "Z-Score Standardization",
          new_variable = new_var
        )
      }
      
      # Update processed data
      values$processed_data <- data
      transformation_results$after_data <- data
      
      showNotification("Transformasi berhasil diterapkan!", type = "message")
    })
    
    # Reset data
    observeEvent(input$reset_data, {
      values$processed_data <- values$original_data
      transformation_results$before_data <- NULL
      transformation_results$after_data <- NULL
      transformation_results$transformation_info <- NULL
      showNotification("Data berhasil direset!", type = "message")
    })
    
    # Display statistics after transformation
    output$after_stats <- renderTable({
      req(transformation_results$after_data)
      req(input$select_variable)
      
      data <- transformation_results$after_data
      
      if (input$transformation_type %in% c("manual", "auto")) {
        cat_var <- paste0(input$select_variable, "_CAT")
        if (cat_var %in% names(data)) {
          table_data <- table(data[[cat_var]])
          prop_data <- prop.table(table_data) * 100
          
          result <- data.frame(
            Kategori = names(table_data),
            Frekuensi = as.numeric(table_data),
            Persentase = round(as.numeric(prop_data), 2)
          )
          return(result)
        }
      } else {
        new_var <- transformation_results$transformation_info$new_variable
        if (new_var %in% names(data)) {
          return(create_summary_table(data, new_var))
        }
      }
      
      return(data.frame(Info = "Tidak ada data transformasi untuk ditampilkan"))
    })
    
    # Display plot after transformation
    output$after_plot <- renderPlot({
      req(transformation_results$after_data)
      req(input$select_variable)
      
      data <- transformation_results$after_data
      
      if (input$transformation_type %in% c("manual", "auto")) {
        cat_var <- paste0(input$select_variable, "_CAT")
        if (cat_var %in% names(data)) {
          p1 <- ggplot(data, aes(x = .data[[cat_var]])) + # Changed aes_string to aes with .data[[]]
            geom_bar(fill = "lightgreen", alpha = 0.7, color = "black") +
            labs(title = paste("Distribusi", cat_var),
                 x = cat_var, y = "Frekuensi") +
            theme_custom() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          p2 <- ggplot(data, aes(x = .data[[cat_var]], y = .data[[input$select_variable]])) + # Changed aes_string to aes with .data[[]]
            geom_boxplot(fill = "lightyellow", alpha = 0.7) +
            labs(title = paste("Boxplot per Kategori"),
                 x = cat_var, y = input$select_variable) +
            theme_custom() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          gridExtra::grid.arrange(p1, p2, ncol = 2)
        }
      } else {
        new_var <- transformation_results$transformation_info$new_variable
        if (new_var %in% names(data)) {
          p1 <- ggplot(data, aes(x = .data[[new_var]])) + # Changed aes_string to aes with .data[[]]
            geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
            labs(title = paste("Histogram", new_var),
                 x = new_var, y = "Frekuensi") +
            theme_custom()
          
          p2 <- ggplot(data, aes(x = "", y = .data[[new_var]])) + # Changed aes_string to aes with .data[[]]
            geom_boxplot(fill = "lightcoral", alpha = 0.7) +
            labs(title = paste("Boxplot", new_var),
                 x = "", y = new_var) +
            theme_custom()
          
          gridExtra::grid.arrange(p1, p2, ncol = 2)
        }
      }
    })
    
    # Display contingency table
    output$contingency_table <- renderTable({
      req(transformation_results$after_data)
      req(input$select_variable)
      
      if (input$transformation_type %in% c("manual", "auto")) {
        cat_var <- paste0(input$select_variable, "_CAT")
        if (cat_var %in% names(transformation_results$after_data)) {
          # Create contingency table with another categorical variable
          categorical_vars <- get_categorical_vars(transformation_results$after_data)
          if (length(categorical_vars) > 1) {
            other_var <- categorical_vars[categorical_vars != cat_var][1]
            if (!is.null(other_var) && other_var %in% names(transformation_results$after_data)) {
              cont_table <- table(transformation_results$after_data[[cat_var]], 
                                  transformation_results$after_data[[other_var]])
              return(addmargins(cont_table))
            }
          }
        }
      }
      return(data.frame(Info = "Tabel kontingensi hanya tersedia untuk variabel kategorikal"))
    })
    
    # Display data preview
    output$data_preview <- DT::renderDataTable({
      req(transformation_results$after_data)
      
      DT::datatable(
        transformation_results$after_data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(10, 25, 50)
        ),
        class = 'cell-border stripe',
        rownames = FALSE
      )
    })
    
    # Generate interpretation
    output$interpretation <- renderText({
      req(transformation_results$transformation_info)
      
      info <- transformation_results$transformation_info
      interpretation <- ""
      
      if (info$type == "Manual Categorization") {
        interpretation <- paste(
          "INTERPRETASI KATEGORISASI MANUAL:\n",
          "=====================================\n\n",
          "Variabel", input$select_variable, "telah dikategorikan secara manual dengan:\n",
          "- Titik potong:", paste(info$breaks, collapse = ", "), "\n",
          "- Label kategori:", paste(info$labels, collapse = ", "), "\n\n",
          "Kategorisasi ini memungkinkan analisis yang lebih fokus pada kelompok-kelompok tertentu",
          "berdasarkan rentang nilai yang telah ditentukan.\n\n",
          "Keuntungan:\n",
          "- Kontrol penuh terhadap pembagian kategori\n",
          "- Sesuai dengan pengetahuan domain\n",
          "- Interpretasi yang lebih mudah\n\n",
          "Saran Analisis Lanjutan:\n",
          "- Uji chi-square untuk asosiasi antar kategori\n",
          "- Analisis deskriptif per kategori\n",
          "- Visualisasi distribusi kategori"
        )
      } else if (grepl("Auto Categorization", info$type)) {
        interpretation <- paste(
          "INTERPRETASI KATEGORISASI OTOMATIS:\n",
          "===================================\n\n",
          "Variabel", input$select_variable, "telah dikategorikan secara otomatis dengan:\n",
          "- Metode:", info$type, "\n",
          "- Jumlah kategori:", info$n_categories, "\n",
          "- Titik potong:", paste(round(info$breaks, 3), collapse = ", "), "\n\n",
          "Metode ini memberikan pembagian yang objektif berdasarkan distribusi data.\n\n",
          "Karakteristik:\n",
          "- Pembagian berdasarkan distribusi empiris\n",
          "- Setiap kategori memiliki proporsi yang relatif seimbang\n",
          "- Mengurangi bias subjektif\n\n",
          "Saran Analisis Lanjutan:\n",
          "- Validasi kategori dengan pengetahuan domain\n",
          "- Analisis perbedaan antar kategori\n",
          "- Uji statistik untuk validasi pembagian"
        )
      } else if (info$type == "Log Transformation") {
        interpretation <- paste(
          "INTERPRETASI TRANSFORMASI LOGARITMA:\n",
          "====================================\n\n",
          "Variabel", input$select_variable, "telah ditransformasi menggunakan logaritma natural.\n",
          "Variabel baru:", info$new_variable, "\n\n",
          "Tujuan transformasi:\n",
          "- Mengurangi skewness (kemencengan) data\n",
          "- Stabilisasi varians\n",
          "- Menormalkan distribusi\n",
          "- Mengurangi pengaruh outlier\n\n",
          "Kapan digunakan:\n",
          "- Data dengan distribusi right-skewed\n",
          "- Data dengan rentang yang sangat lebar\n",
          "- Persiapan untuk analisis yang mengasumsikan normalitas\n\n",
          "Catatan penting:\n",
          "- Interpretasi berubah menjadi dalam skala logaritma\n",
          "- Nilai 0 atau negatif memerlukan penyesuaian\n",
          "- Transformasi balik diperlukan untuk interpretasi asli"
        )
      } else if (info$type == "Square Root Transformation") {
        interpretation <- paste(
          "INTERPRETASI TRANSFORMASI AKAR KUADRAT:\n",
          "======================================\n\n",
          "Variabel", input$select_variable, "telah ditransformasi menggunakan akar kuadrat.\n",
          "Variabel baru:", info$new_variable, "\n\n",
          "Tujuan transformasi:\n",
          "- Mengurangi skewness ringan sampai sedang\n",
          "- Stabilisasi varians (terutama untuk data count)\n",
          "- Menormalkan distribusi\n",
          "- Alternatif yang lebih mild dibanding log\n\n",
          "Kapan digunakan:\n",
          "- Data count atau frekuensi\n",
          "- Distribusi Poisson\n",
          "- Data dengan varians proporsional terhadap mean\n\n",
          "Keuntungan:\n",
          "- Lebih mudah diinterpretasi dibanding log\n",
          "- Cocok untuk data non-negatif\n",
          "- Transformasi yang relatif gentle"
        )
      } else if (info$type == "Z-Score Standardization") {
        interpretation <- paste(
          "INTERPRETASI STANDARDISASI Z-SCORE:\n",
          "==================================\n\n",
          "Variabel", input$select_variable, "telah distandarisasi menjadi z-score.\n",
          "Variabel baru:", info$new_variable, "\n\n",
          "Karakteristik hasil:\n",
          "- Mean = 0\n",
          "- Standard deviation = 1\n",
          "- Skala yang uniform\n\n",
          "Tujuan standardisasi:\n",
          "- Membuat variabel comparable\n",
          "- Persiapan untuk analisis multivariate\n",
          "- Identifikasi outlier (|z| > 3)\n",
          "- Interpretasi dalam unit standard deviation\n\n",
          "Interpretasi z-score:\n",
          "- z = 0: nilai sama dengan rata-rata\n",
          "- z = 1: nilai 1 SD di atas rata-rata\n",
          "- z = -1: nilai 1 SD di bawah rata-rata\n",
          "- |z| > 2: nilai ekstrim (outlier potensial)"
        )
      }
      
      return(interpretation)
    })
    
    # Download handlers
    output$download_transformed <- downloadHandler(
      filename = function() {
        paste("data_transformed_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        if (!is.null(transformation_results$after_data)) {
          write.csv(transformation_results$after_data, file, row.names = FALSE)
        }
      }
    )
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste("laporan_transformasi_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        if (!is.null(transformation_results$transformation_info)) {
          # Create Word document with transformation report
          doc <- officer::read_docx()
          
          doc <- doc %>%
            officer::body_add_par("Laporan Transformasi Data", style = "heading 1") %>%
            officer::body_add_par(paste("Tanggal:", Sys.Date())) %>%
            officer::body_add_par(paste("Variabel:", input$select_variable)) %>%
            officer::body_add_par(paste("Jenis Transformasi:", transformation_results$transformation_info$type)) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Interpretasi:", style = "heading 2") %>%
            officer::body_add_par(capture.output(cat(renderText({
              req(transformation_results$transformation_info)
              # Return interpretation text here
            })()))) %>%
            officer::body_add_par(" ") %>%
            officer::body_add_par("Statistik Deskriptif:", style = "heading 2")
          
          print(doc, target = file)
        }
      }
    )
  })
}
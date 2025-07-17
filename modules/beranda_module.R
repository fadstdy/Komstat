# Beranda Module
# modules/beranda_module.R

# UI function for Beranda
berandaUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Header section
    column(12,
           box(
             title = "Selamat Datang di Dashboard Analisis Statistik",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             h3("Dashboard Analisis Data Sosial-Ekonomi Indonesia", 
                style = "text-align: center; margin-bottom: 20px;"),
             p("Dashboard ini dikembangkan untuk memfasilitasi analisis statistik komprehensif terhadap data sosial-ekonomi Indonesia. 
               Dengan menggunakan framework R Shiny, dashboard ini menyediakan antarmuka yang intuitif untuk eksplorasi data, 
               pengujian hipotesis, dan pemodelan statistik.",
               style = "text-align: justify; font-size: 14px;")
           )
    ),
    
    # Data overview section
    column(6,
           box(
             title = "Informasi Dataset",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             height = 400,
             div(
               h4("📊 Dataset: Data Sosial-Ekonomi Indonesia"),
               hr(),
               tags$ul(
                 tags$li(strong("Jumlah Observasi: "), textOutput(ns("n_obs"), inline = TRUE)),
                 tags$li(strong("Jumlah Variabel: "), textOutput(ns("n_vars"), inline = TRUE)),
                 tags$li(strong("Periode Data: "), "Data Cross-Section"),
                 tags$li(strong("Unit Analisis: "), "Kabupaten/Kota"),
                 tags$li(strong("Cakupan Geografis: "), "Seluruh Indonesia")
               ),
               br(),
               h5("📈 Variabel Utama:"),
               tags$ul(
                 tags$li("CHILDREN: Persentase anak-anak"),
                 tags$li("FEMALE: Persentase perempuan"),
                 tags$li("ELDERLY: Persentase lansia"),
                 tags$li("POVERTY: Tingkat kemiskinan"),
                 tags$li("GROWTH: Pertumbuhan ekonomi"),
                 tags$li("POPULATION: Jumlah penduduk")
               )
             )
           )
    ),
    
    # Features section
    column(6,
           box(
             title = "Fitur Dashboard",
             status = "success",
             solidHeader = TRUE,
             width = 12,
             height = 400,
             div(
               h4("🔧 Fitur Analisis:"),
               hr(),
               tags$ul(
                 tags$li(strong("Manajemen Data: "), "Transformasi dan kategorisasi variabel"),
                 tags$li(strong("Eksplorasi Data: "), "Statistik deskriptif dan visualisasi"),
                 tags$li(strong("Uji Asumsi: "), "Normalitas dan homogenitas"),
                 tags$li(strong("Statistik Inferensia: "), "Uji t, ANOVA, dan uji proporsi"),
                 tags$li(strong("Regresi Linear: "), "Analisis regresi berganda")
               ),
               br(),
               h5("📥 Fitur Ekspor:"),
               tags$ul(
                 tags$li("Download grafik (PNG/PDF)"),
                 tags$li("Export tabel (CSV/Excel)"),
                 tags$li("Laporan lengkap (Word/PDF)")
               )
             )
           )
    ),
    
    # Instructions section
    column(12,
           box(
             title = "Panduan Penggunaan Dashboard",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             div(
               h4("📋 Langkah-langkah Analisis:"),
               hr(),
               tags$ol(
                 tags$li(strong("Beranda: "), "Memahami overview dataset dan fitur dashboard"),
                 tags$li(strong("Manajemen Data: "), "Melakukan transformasi data sesuai kebutuhan analisis"),
                 tags$li(strong("Eksplorasi Data: "), "Mengeksplorasi karakteristik data melalui statistik deskriptif"),
                 tags$li(strong("Uji Asumsi: "), "Memeriksa asumsi-asumsi yang diperlukan untuk analisis"),
                 tags$li(strong("Statistik Inferensia: "), "Melakukan pengujian hipotesis sesuai tujuan penelitian"),
                 tags$li(strong("Regresi Linear: "), "Membangun model prediktif dan interpretasi hasil")
               ),
               br(),
               div(
                 class = "alert alert-info",
                 h5("💡 Tips Penggunaan:"),
                 tags$ul(
                   tags$li("Selalu periksa asumsi sebelum melakukan uji statistik"),
                   tags$li("Gunakan fitur download untuk menyimpan hasil analisis"),
                   tags$li("Interpretasi hasil tersedia di setiap output analisis"),
                   tags$li("Data dapat di-filter dan dimanipulasi sesuai kebutuhan")
                 )
               )
             )
           )
    ),
    
    # Data preview section
    column(12,
           box(
             title = "Preview Data",
             status = "primary",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             collapsed = TRUE,
             div(
               h4("📋 Struktur Data:"),
               withSpinner(DT::dataTableOutput(ns("data_preview"))),
               br(),
               downloadButton(ns("download_data"), "Download Data Lengkap",
                              class = "btn-primary")
             )
           )
    ),
    
    # Footer section
    column(12,
           box(
             title = "Informasi Teknis",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             div(
               fluidRow(
                 column(4,
                        h5("📊 Teknologi:"),
                        tags$ul(
                          tags$li("R Shiny"),
                          tags$li("ggplot2"),
                          tags$li("DT"),
                          tags$li("plotly")
                        )
                 ),
                 column(4,
                        h5("📈 Metode Statistik:"),
                        tags$ul(
                          tags$li("Analisis Deskriptif"),
                          tags$li("Uji Normalitas"),
                          tags$li("ANOVA"),
                          tags$li("Regresi Linear")
                        )
                 ),
                 column(4,
                        h5("🔗 Kontak:"),
                        tags$ul(
                          tags$li("Tim Analisis Data"),
                          tags$li("dashboard@analytics.com"),
                          tags$li("Versi: 1.0.0"),
                          tags$li(paste("Build:", Sys.Date()))
                        )
                 )
               )
             )
           )
    )
  )
}

# Server function for Beranda
berandaServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Data overview outputs
    output$n_obs <- renderText({
      nrow(values$current_data)
    })
    
    output$n_vars <- renderText({
      ncol(values$current_data)
    })
    
    # Data preview table
    output$data_preview <- DT::renderDataTable({
      DT::datatable(
        values$current_data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        class = 'cell-border stripe',
        rownames = FALSE
      )
    })
    
    # Download handler for complete data
    output$download_data <- downloadHandler(
      filename = function() {
        paste("data_lengkap_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(values$current_data, file, row.names = FALSE)
      }
    )
  })
}
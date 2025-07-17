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
             title = "Informasi Dataset Utama",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             height = "auto", # Mengubah tinggi menjadi auto agar konten bisa menyesuaikan
             div(
               h4("📊 Dataset: Data Kerentanan Sosial di Indonesia"),
               hr(),
               tags$ul(
                 tags$li(strong("Sumber Data Utama: "), "Survei Sosial Ekonomi Nasional (SUSENAS) 2017 oleh BPS-Statistics Indonesia"),
                 tags$li(strong("Jumlah Observasi: "), textOutput(ns("n_obs"), inline = TRUE), " (Kabupaten/Kota)"),
                 tags$li(strong("Jumlah Variabel: "), textOutput(ns("n_vars"), inline = TRUE)),
                 tags$li(strong("Periode Data: "), "Cross-Sectional (2017)"),
                 tags$li(strong("Unit Analisis: "), "Kabupaten/Kota di Indonesia"),
                 tags$li(strong("Cakupan Geografis: "), "511 Kabupaten/Kota di Seluruh Indonesia")
               ),
               br(),
               h5("📈 Variabel Utama yang Tersedia:"),
               tags$ul(
                 tags$li("CHILDREN: Persentase anak-anak di bawah lima tahun"),
                 tags$li("FEMALE: Persentase populasi perempuan"),
                 tags$li("ELDERLY: Persentase lansia (65 tahun ke atas)"),
                 tags$li("POVERTY: Tingkat kemiskinan"),
                 tags$li("GROWTH: Pertumbuhan ekonomi (persentase perubahan populasi)"),
                 tags$li("POPULATION: Jumlah penduduk"),
                 tags$li("FHEAD: Persentase rumah tangga dengan kepala rumah tangga perempuan"),
                 tags$li("FAMILYSIZE: Rata-rata jumlah anggota rumah tangga"),
                 tags$li("NOELECTRIC: Persentase rumah tangga yang tidak menggunakan listrik"),
                 tags$li("LOWEDU: Persentase populasi 15 tahun ke atas dengan pendidikan rendah"),
                 tags$li("ILLITERATE: Persentase populasi buta huruf"),
                 tags$li("NOTRAINING: Persentase rumah tangga yang tidak mendapatkan pelatihan bencana"),
                 tags$li("DPRONE: Persentase rumah tangga yang tinggal di daerah rawan bencana"),
                 tags$li("RENTED: Persentase rumah tangga yang menyewa rumah"),
                 tags$li("NOSEWER: Persentase rumah tangga yang tidak memiliki sistem drainase"),
                 tags$li("TAPWATER: Persentase rumah tangga yang menggunakan air pipa")
               ),
               br(),
               h5("🔗 Ketersediaan Data:"),
               tags$ul(
                 tags$li("Data Kerentanan Sosial: ", tags$a(href="https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv", "sovi_data.csv", target="_blank"), ""),
                 tags$li("Matriks Jarak Antar-Distrik: ", tags$a(href="https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv", "distance.csv", target="_blank"), "")
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
             height = "auto",
             div(
               h4("🔧 Fitur Analisis Utama:"),
               hr(),
               tags$ul(
                 tags$li(strong("Manajemen Data: "), "Transformasi dan kategorisasi variabel, termasuk interpretasi output."),
                 tags$li(strong("Eksplorasi Data: "), "Statistik deskriptif, visualisasi (histogram, boxplot, barplot), tabel ringkasan, dan peta (jika memungkinkan), disertai interpretasi."),
                 tags$li(strong("Uji Asumsi: "), "Pengujian normalitas (Shapiro-Wilk, Q-Q plot) dan homogenitas varians (Levene, Bartlett) dengan interpretasi."),
                 tags$li(strong("Statistik Inferensia: "), "Uji beda rata-rata (satu/dua kelompok), uji proporsi (satu/dua kelompok), uji variansi, ANOVA (satu/dua arah), dan penjelasan hasil."),
                 tags$li(strong("Regresi Linear Berganda: "), "Analisis regresi lengkap dengan uji asumsi (normalitas residual, multikolinearitas), serta penjelasan parameter dan kesimpulan model.")
               ),
               br(),
               h5("📥 Fitur Ekspor Hasil:"),
               tags$ul(
                 tags$li("Download grafik (PNG/PDF)"),
                 tags$li("Export tabel (CSV/Excel)"),
                 tags$li("Laporan lengkap (Word/PDF)"),
                 tags$li("Tombol unduh gabungan untuk seluruh isi tiap menu.")
               )
             )
           )
    ),
    
    # Interesting Facts section
    column(12,
           box(
             title = "Fakta Menarik dari Dataset",
             status = "warning",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             div(
               h4("💡 Insight Utama:"),
               hr(),
               tags$ul(
                 tags$li("Indonesia merupakan negara yang rentan terhadap berbagai bencana alam karena letaknya di Cincin Api Pasifik dan pertemuan tiga lempeng tektonik utama dunia."),
                 tags$li("Analisis kerentanan sosial sangat penting untuk memahami dampak bencana terhadap komunitas dan kemampuannya untuk pulih."),
                 tags$li("Dataset ini mencakup 511 kabupaten/kota di Indonesia, yang telah dikalibrasi dengan peta geografis Indonesia tahun 2013 untuk tujuan analisis spasial."),
                 tags$li("Hampir semua variabel dalam dataset menunjukkan keberadaan *outlier*, mengindikasikan adanya ketidaksetaraan antar wilayah di Indonesia dalam konteks kerentanan sosial."),
                 tags$li("Wilayah Timur Indonesia, khususnya Maluku dan Papua, menunjukkan karakteristik demografi yang berbeda dengan persentase anak-anak dan ukuran keluarga yang relatif lebih tinggi, meskipun populasi perempuan dan lansia cenderung rendah."),
                 tags$li("Maluku dan Papua menghadapi tantangan signifikan terkait akses listrik, tingkat kemiskinan, buta huruf, dan sistem drainase yang kurang memadai dibandingkan wilayah lain."),
                 tags$li("Mayoritas rumah tangga di berbagai wilayah memiliki persentase tinggi yang tidak mendapatkan pelatihan kesiapsiagaan bencana, ironisnya, banyak dari wilayah ini juga merupakan daerah rawan bencana.")
               )
             )
           )
    ),
    
    # Metadata Article section
    column(12,
           box(
             title = "Metadata Artikel Pendukung",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             collapsible = TRUE,
             collapsed = TRUE, # Agar collapsed secara default
             div(
               h4("📄 Detail Publikasi:"),
               hr(),
               tags$ul(
                 tags$li(strong("Judul Artikel: "), "Revisiting social vulnerability analysis in Indonesia data"),
                 tags$li(strong("Penulis: "), "Robert Kurniawan, Bahrul Ilmi Nasution, Neli Agustina, Budi Yuniarto"),
                 tags$li(strong("Afiliasi: "), "Department of Statistical Computing, Polytechnic Statistics STIS, Jakarta; Department of Statistics, Polytechnic Statistics STIS, Jakarta; Jakarta Smart City, Department of Communications, Informatics, and Statistics, Jakarta"),
                 tags$li(strong("Jurnal: "), "Data in Brief 40 (2022) 107743"),
                 tags$li(strong("Tanggal Publikasi: "), "Received: 6 Oktober 2021; Revised: 14 Desember 2021; Accepted: 20 Desember 2021; Available online: 23 Desember 2021"),
                 tags$li(strong("DOI Artikel: "), tags$a(href="https://doi.org/10.1016/j.dib.2021.107743", "10.1016/j.dib.2021.107743", target="_blank"), ""),
                 tags$li(strong("Kata Kunci: "), "Manajemen Bencana, Mitigasi Bencana, Klasterisasi Berbobot Geografis Fuzzy, Sampling Multistage, Kerentanan Sosial")
               ),
               br(),
               h4("📝 Abstrak (Ringkasan Dataset dari Artikel):"),
               hr(),
               p("Makalah ini menyajikan dataset tentang kerentanan sosial di Indonesia. Dataset ini berisi beberapa dimensi yang didasarkan pada studi sebelumnya. Data dikompilasi terutama dari Survei Sosial Ekonomi Nasional (SUSENAS) 2017 yang dilakukan oleh BPS-Statistics Indonesia. Kami menggunakan bobot untuk mendapatkan estimasi berdasarkan sampling multistage. Kami juga menerima informasi tambahan tentang populasi, jumlah, dan pertumbuhan populasi dari proyeksi populasi BPS-Statistics Indonesia tahun 2017. Selanjutnya, kami menyediakan matriks jarak sebagai informasi pelengkap dan jumlah populasi untuk melakukan Fuzzy Geographically Weighted Clustering (FGWC). Data ini dapat digunakan untuk analisis lebih lanjut tentang kerentanan sosial untuk mempromosikan manajemen bencana.", style = "text-align: justify;")
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
                 tags$li(strong("Beranda: "), "Memahami *overview* dataset, metadata, fakta menarik, dan fitur *dashboard*."),
                 tags$li(strong("Manajemen Data: "), "Melakukan transformasi data sesuai kebutuhan analisis, dengan interpretasi setiap output."),
                 tags$li(strong("Eksplorasi Data: "), "Mengeksplorasi karakteristik data melalui statistik deskriptif dan visualisasi interaktif, disertai interpretasi."),
                 tags$li(strong("Uji Asumsi: "), "Memeriksa asumsi-asumsi yang diperlukan untuk analisis statistik inferensial, dengan interpretasi hasil uji."),
                 tags$li(strong("Statistik Inferensia: "), "Melakukan pengujian hipotesis (uji t, ANOVA, uji proporsi, uji variansi) sesuai tujuan penelitian, disertai penjelasan hasil."),
                 tags$li(strong("Regresi Linear: "), "Membangun model prediktif regresi linear berganda, melakukan uji asumsi model, dan interpretasi setiap parameter serta kesimpulan dari model regresi.")
               ),
               br(),
               div(
                 class = "alert alert-info",
                 h5("💡 Tips Penggunaan:"),
                 tags$ul(
                   tags$li("Selalu periksa asumsi sebelum melakukan uji statistik inferensial atau pemodelan regresi."),
                   tags$li("Gunakan fitur *download* yang tersedia di setiap menu untuk menyimpan hasil analisis (grafik, tabel, laporan)."),
                   tags$li("Interpretasi hasil disediakan di setiap *output* analisis untuk membantu pemahaman Anda."),
                   tags$li("Data dapat di-*filter* dan dimanipulasi sesuai kebutuhan analisis Anda di menu Manajemen Data.")
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
                          tags$li("plotly"),
                          tags$li("dplyr"),
                          tags$li("officer")
                        )
                 ),
                 column(4,
                        h5("📈 Metode Statistik:"),
                        tags$ul(
                          tags$li("Analisis Deskriptif"),
                          tags$li("Uji Normalitas & Homogenitas"),
                          tags$li("Uji t, ANOVA, Uji Proporsi, Uji Variansi"),
                          tags$li("Regresi Linear Berganda")
                        )
                 ),
                 column(4,
                        h5("🔗 Kontak & Versi:"),
                        tags$ul(
                          tags$li("Tim Analisis Data"),
                          tags$li("Email: dashboard@analytics.com"),
                          tags$li("Versi: 1.0.0"),
                          tags$li(paste("Build Date:", Sys.Date()))
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
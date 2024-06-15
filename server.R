library(ggplot2)
library(MASS)
library(dplyr)
library(sf)
library(viridis)
library(DT)
library(leaflet)
library(cluster)

# Set working directory
#setwd("C:/Users/mhafd/OneDrive/Documents/GitHub/crzn")

# Import Data
data_adopsi <- read.csv("dataSEC - new.csv")
colnames(data_adopsi)[colnames(data_adopsi) == 'ID'] <- 'ADM2_PCODE'

Kabupaten.shp <- read_sf(dsn = "File SHP Indonesia", layer = "idn_admbnda_adm2_bps_20200401")

dat_map <- inner_join(data_adopsi, Kabupaten.shp, by = 'ADM2_PCODE')
dat_map <- st_as_sf(dat_map)

# Ensure selected columns are numeric
data_adopsi <- data_adopsi %>%
  mutate(
    Adopsi.TIK = as.numeric(Adopsi.TIK),
    Infrastruktur = as.numeric(Infrastruktur),
    Kapabilitas.Inovasi = as.numeric(`Kapabilitas.Inovasi`),
    Keterampilan = as.numeric(Keterampilan),
    Pasar.Tenaga.Kerja = as.numeric(`Pasar.Tenaga.Kerja`),
    Stabilitas.Ekonomi = as.numeric(`Stabilitas.Ekonomi`),
    IRBI = as.numeric(`IRBI`)
  )

# Run K-Means
clustering_data <- data_adopsi[, c("Adopsi.TIK","Infrastruktur", "Kapabilitas.Inovasi", "Keterampilan", "Pasar.Tenaga.Kerja", "Stabilitas.Ekonomi", "IRBI")]

# Select variables for clustering
cluster_vars <- data_adopsi[, c("Infrastruktur", "Kapabilitas.Inovasi", "Keterampilan", "Pasar.Tenaga.Kerja", "Stabilitas.Ekonomi", "IRBI")]

# Run K-Means
set.seed(20)
kmeans_result <- kmeans(cluster_vars, centers = 3, nstart = 25)

# Menambahkan Hasil Klaster ke Data
clustering_data$cluster <- as.factor(kmeans_result$cluster)

# Menambahkan variabel Adopsi.TIK yang sebelumnya dihapus
clustering_data <- data_adopsi %>%
  dplyr::select(-Adopsi.TIK) %>%
  na.omit() %>%
  mutate(cluster = as.factor(kmeans_result$cluster)) %>%
  bind_cols(data_adopsi %>% dplyr::select(Adopsi.TIK))

# Memastikan semua kolom selain 'cluster' adalah numerik
numeric_cols <- clustering_data %>%
  dplyr::select(-cluster) %>%
  select_if(is.numeric) %>%
  colnames()

#CLUSTERWISE REGRESSION
# Filter data untuk setiap klaster
cluster1_data <- clustering_data %>% filter(cluster == 1)
cluster2_data <- clustering_data %>% filter(cluster == 2)
cluster3_data <- clustering_data %>% filter(cluster == 3)

cluster1_data = cluster1_data[, c("Adopsi.TIK","Infrastruktur", "Kapabilitas.Inovasi",
                                  "Keterampilan", "Pasar.Tenaga.Kerja", "Stabilitas.Ekonomi", "IRBI")]
cluster2_data = cluster2_data[, c("Adopsi.TIK","Infrastruktur", "Kapabilitas.Inovasi",
                                  "Keterampilan", "Pasar.Tenaga.Kerja", "Stabilitas.Ekonomi", "IRBI")]
cluster3_data = cluster3_data[, c("Adopsi.TIK","Infrastruktur", "Kapabilitas.Inovasi",
                                  "Keterampilan", "Pasar.Tenaga.Kerja", "Stabilitas.Ekonomi", "IRBI")]
Metpen1 = cluster1_data
Metpen2 = cluster2_data
Metpen3 = cluster3_data

# Run K-Means
clustering_data <- data_adopsi %>%
  dplyr::select(-Adopsi.TIK) %>%
  na.omit()

# Select variables for clustering
cluster_vars <- clustering_data[, c("Infrastruktur", "Kapabilitas.Inovasi", "Keterampilan", "Pasar.Tenaga.Kerja", "Stabilitas.Ekonomi", "IRBI")]

# Run K-Means
set.seed(20)
kmeans_result <- kmeans(cluster_vars, centers = 3)  # Misal, 3 klaster
dat_map$cluster <- factor(kmeans_result$cluster)

server <- function(input, output, session) {
  
  # Filtered Data
  data_filtered <- reactive({
    req(input$cluster_selection)
    
    if (input$cluster_selection == "all") {
      filtered_data <- dat_map
    } else {
      filtered_data <- dat_map[dat_map$cluster == as.integer(substring(input$cluster_selection, 9)), ]
    }
    
    filtered_data
  })
  
  # Display Cluster Title
  # Run K-Means
  clustering_data <- data_adopsi[, c("Adopsi.TIK","Infrastruktur", "Kapabilitas.Inovasi", "Keterampilan", "Pasar.Tenaga.Kerja", "Stabilitas.Ekonomi", "IRBI")]
  
  # Menambahkan variabel Adopsi.TIK yang sebelumnya dihapus
  clustering_data <- data_adopsi %>%
    dplyr::select(-Adopsi.TIK) %>%
    na.omit() %>%
    mutate(cluster = as.factor(kmeans_result$cluster)) %>%
    bind_cols(data_adopsi %>% dplyr::select(Adopsi.TIK))
  
  clustering_data = clustering_data[, c("name","cluster","Infrastruktur", "Kapabilitas.Inovasi","Keterampilan","Pasar.Tenaga.Kerja","Stabilitas.Ekonomi","IRBI","Adopsi.TIK")]
  
  output$cluster_title_ui <- renderUI({
    if (input$cluster_selection != "all") {
      tags$div(class = "cluster-characteristics-title", "Karakteristik Klaster (Rata-Rata)")
    } else {
      tags$div(class = "cluster-characteristics-title", "Karakteristik Data (Rata-Rata)")
    }
  })
  
  # Leaflet Map
  output$cluster_map <- renderLeaflet({
    req(input$cluster_selection)
    
    filtered_data <- data_filtered()
    
    # Create a color palette
    palette <- colorFactor(viridis::viridis_pal(option = "plasma")(length(unique(filtered_data$cluster))),
                           domain = filtered_data$cluster)
    
    # Create the leaflet map
    leaflet(data = filtered_data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~palette(cluster),
                  color = "white",
                  weight = 2,
                  opacity = 1,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  popup = ~paste("Kabupaten:", name, "; Adopsi TIK: ", Adopsi.TIK),
                  label = ~paste("Kabupaten:", name, "; Adopsi TIK: ", Adopsi.TIK),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = palette, values = ~cluster, opacity = 0.7, title = "Cluster", position = "bottomright") %>%
      setView(lng = 120, lat = -2, zoom = 5)  # Sesuaikan dengan koordinat dan zoom peta yang diinginkan
  })
  
  # Render DataTable
  output$cluster_table <- renderDT({
    if (input$cluster_selection == "all") {
      datatable(clustering_data, options = list(pageLength = 10))
    } else {
      filtered_data <- clustering_data[clustering_data$cluster == as.integer(substring(input$cluster_selection, 9)), ]
      datatable(filtered_data, options = list(pageLength = 10))
    }
  })
  
  # Bar Chart
  output$plot_output <- renderPlot({
    req(input$cluster_selection)
    
    filtered_data <- data_filtered()
    
    # Mengambil 10 kabupaten dengan Adopsi TIK tertinggi
    top25 <- filtered_data %>%
      arrange(Adopsi.TIK) %>%
      head(25)
    
    ggplot(top25, aes(x = reorder(name, Adopsi.TIK), y = Adopsi.TIK, fill = Adopsi.TIK)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Top 25 Kabupaten dengan Adopsi TIK Tertinggi",
           x = "Kabupaten",
           y = "Adopsi TIK") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 12, face = "bold"))
  })
  
  # Characteristic clusters
  output$cluster_characteristics <- renderUI({
    req(input$cluster_selection)
    
    if (input$cluster_selection == "all") {
      cluster_data <- data_filtered()
    } else {
      cluster_id <- as.integer(substring(input$cluster_selection, 9))
      cluster_data <- data_filtered()[data_filtered()$cluster == cluster_id, ]
    }
    
    # Select numeric variables except Adopsi.TIK and geometry
    numeric_vars <- c("Infrastruktur", "Kapabilitas.Inovasi", "Keterampilan", "Pasar.Tenaga.Kerja", "Stabilitas.Ekonomi", "IRBI")
    
    # Calculate mean of selected variables for the selected cluster
    cluster_means <- sapply(cluster_data[, numeric_vars], mean, na.rm = TRUE)
    
    # Add Adopsi.TIK to the cluster_means
    cluster_means <- c(cluster_means, Adopsi.TIK = mean(cluster_data$Adopsi.TIK, na.rm = TRUE))
    
    # Create scorecard UI using infoBox
    fluidRow(
      column(
        width = 4,
        infoBox(
          "Infrastruktur",
          round(cluster_means["Infrastruktur"], 2),
          icon = icon("cogs"),
          width = 12
        )
      ),
      column(
        width = 4,
        infoBox(
          "Kapabilitas Inovasi",
          round(cluster_means["Kapabilitas.Inovasi"], 2),
          icon = icon("lightbulb"),
          width = 12
        )
      ),
      column(
        width = 4,
        infoBox(
          "Keterampilan",
          round(cluster_means["Keterampilan"], 2),
          icon = icon("hand-paper"),
          width = 12
        )
      ),
      column(
        width = 4,
        infoBox(
          "Pasar Tenaga Kerja",
          round(cluster_means["Pasar.Tenaga.Kerja"], 2),
          icon = icon("users"),
          width = 12
        )
      ),
      column(
        width = 4,
        infoBox(
          "Stabilitas Ekonomi",
          round(cluster_means["Stabilitas.Ekonomi"], 2),
          icon = icon("line-chart"),
          width = 12
        )
      ),
      column(
        width = 4,
        infoBox(
          "Indeks Risiko Bencana",
          round(cluster_means["IRBI"], 2),
          icon = icon("mountain"),
          width = 12
        )
      )
    )
  })
  
  output$cluster_regression <- renderUI({
    req(input$cluster_selection)
    
    if (input$cluster_selection == "all") {
      ui <- fluidRow(
        tags$div(
          style = "font-family: Verdana; font-size: 11px; font-weight: 200; text-align: justify; padding: 12px; 
               background-color: #d3d3d3; margin-right: 30px;",
          tags$h3("Pengenalan Smart City"),
          tags$p(
            "Smart City adalah konsep kota yang menggunakan teknologi informasi dan komunikasi (TIK) untuk meningkatkan efisiensi operasional, berbagi informasi dengan publik, dan meningkatkan kualitas layanan pemerintah serta kesejahteraan warga. Berikut adalah enam komponen dasar dari Smart City:"
          ),
          tags$ul(
            tags$li(tags$b("Smart Mobility:"), " Infrastruktur yang mendukung mobilitas cerdas, termasuk transportasi publik yang efisien dan infrastruktur jalan yang terintegrasi."),
            tags$li(tags$b("Smart Living:"), " Kapabilitas inovasi yang mendukung kualitas hidup warga, termasuk akses ke layanan kesehatan, pendidikan, dan perumahan yang berkualitas."),
            tags$li(tags$b("Smart People:"), " Peningkatan keterampilan dan edukasi warga untuk menciptakan masyarakat yang berpengetahuan dan adaptif terhadap perubahan."),
            tags$li(tags$b("Smart Economy:"), " Ekonomi yang stabil untuk mendorong pertumbuhan ekonomi yang berkelanjutan."),
            tags$li(tags$b("Smart Environment:"), " Manajemen lingkungan yang efektif, termasuk pengelolaan risiko bencana dan pelestarian sumber daya alam."),
            tags$li(tags$b("Smart Governance:"), " Pemerintahan yang transparan dan partisipatif, memanfaatkan teknologi untuk meningkatkan keterlibatan warga dalam proses pengambilan keputusan.")
          )
        )
      )
    } else {
      cluster_id <- as.integer(substring(input$cluster_selection, 9))
      cluster_data <- data_filtered()[data_filtered()$cluster == cluster_id, ]
      
      # Tambahkan penjelasan fokus pengembangan smart city
      focus_areas <- list(
        X1 = "Infrastruktur yang mendukung Smart Mobility.",
        X2 = "Kapabilitas inovasi untuk mendukung Smart Living.",
        X3 = "Keterampilan untuk mendukung Smart People.",
        X4 = "Pasar Tenaga Kerja untuk mendukung Smart People",
        X5 = "Stabilitas Ekonomi Makro untuk mewujudkan Smart Economy.",
        X6 = "Manajemen risiko bencana yang lebih baik untuk Mewujudkan Smart Environment."
      )
      
      # Generate HTML for focus areas
      significant_variables <- character()
      if (cluster_id == 1) {
        splinecluster1 <- uji12(0)
        coefficients <- splinecluster1$Parameters
        ujiT <- splinecluster1$P_Value
        
        for (i in seq_along(coefficients)) {
          # Skip first row (intercept), then process every 3 rows
          if (i %% 3 != 1 && ujiT[i] <= 0.05) {
            variable_index <- ceiling((i - 1) / 3)
            
            if (!(paste0("X", variable_index) %in% significant_variables)) {
              significant_variables <- c(significant_variables, paste0("X", variable_index))
            }
          }
        }
      } else if (cluster_id == 2) {
        splinecluster2 <- uji23(0)
        coefficients <- splinecluster2$Parameters
        ujiT <- splinecluster2$P_Value
        
        for (i in seq_along(coefficients)) {
          # Skip first row (intercept), then process every 4 rows
          if (i %% 4 != 1 && ujiT[i] <= 0.05) {
            variable_index <- ceiling((i - 1) / 4)
            
            if (!(paste0("X", variable_index) %in% significant_variables)) {
              significant_variables <- c(significant_variables, paste0("X", variable_index))
            }
          }
        }
      } else if (cluster_id == 3) {
        splinecluster3 <- uji33(0)
        coefficients <- splinecluster3$Parameters
        ujiT <- splinecluster3$P_Value
        
        for (i in seq_along(coefficients)) {
          # Skip first row (intercept), then process every 4 rows
          if (i %% 4 != 1 && ujiT[i] <= 0.05) {
            variable_index <- ceiling((i - 1) / 4)
            
            if (!(paste0("X", variable_index) %in% significant_variables)) {
              significant_variables <- c(significant_variables, paste0("X", variable_index))
            }
          }
        }
      }
      
      focus_text <- lapply(significant_variables, function(var) {
        focus_areas[[var]]
      })
      
      # Render UI dengan box dan highlight CSS
      ui <- box(
        title = paste("Hasil Regresi pada Klaster", cluster_id),
        status = "info",
        solidHeader = TRUE,
        width = 12,
        div(
          style = "padding: 10px; border-radius: 5px; background-color: #f0f0f0;",
          h3("Fokus Pengembangan untuk Smart City"),
          tags$ul(
            lapply(focus_text, function(text) {
              tags$li(text)
            })
          )
        )
      )
      
      return(ui)
    }
  })
  
  # Download Data
  output$download_cluster <- downloadHandler(
    filename = function() {
      paste("cluster_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(st_drop_geometry(dat_map), file)
    }
  )
}
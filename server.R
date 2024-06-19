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
    
    top25 <- filtered_data %>%
      arrange(Adopsi.TIK) %>%
      head(25)
    
    ggplot(top25, aes(x = reorder(name, Adopsi.TIK), y = Adopsi.TIK, fill = Adopsi.TIK)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "#f7f4f9", high = "#6e016b") +  
      coord_flip() +
      labs(title = "25 Kabupaten/Kota dengan Nilai Adopsi TIK Terendah",
           x = "Kabupaten/Kota",
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
    
    numeric_vars <- c("Infrastruktur", "Kapabilitas.Inovasi", "Keterampilan", "Pasar.Tenaga.Kerja", "Stabilitas.Ekonomi", "IRBI")
    cluster_means <- sapply(cluster_data[, numeric_vars], mean, na.rm = TRUE)
    cluster_means <- c(cluster_means, Adopsi.TIK = mean(cluster_data$Adopsi.TIK, na.rm = TRUE))
    
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
          style = "font-family: Verdana; font-size: 13px; font-weight: 400; text-align: justify; padding: 16px; 
            background-color: #f0f0f0; border-radius: 10px; box-shadow: 0px 0px 5px rgba(0,0,0,0.3);
            margin-right: 24px",
          tags$h3("Pengenalan Smart City"),
          tags$p(
            "Smart City adalah konsep kota yang menggunakan teknologi informasi dan komunikasi (TIK) untuk meningkatkan efisiensi operasional, berbagi informasi dengan publik, dan meningkatkan kualitas layanan pemerintah serta kesejahteraan warga. Berikut adalah enam komponen dasar dari Smart City:"
          ),
          tags$ul(
            style = "list-style-type: none; padding-left: 0;",
            tags$li(
              tags$b("Smart Mobility:"), 
              " Infrastruktur yang mendukung mobilitas cerdas, termasuk transportasi publik yang efisien dan infrastruktur jalan yang terintegrasi."
            ),
            tags$li(
              tags$b("Smart Living:"), 
              " Kapabilitas inovasi yang mendukung kualitas hidup warga, termasuk akses ke layanan kesehatan, pendidikan, dan perumahan yang berkualitas."
            ),
            tags$li(
              tags$b("Smart People:"), 
              " Peningkatan keterampilan dan edukasi warga untuk menciptakan masyarakat yang berpengetahuan dan adaptif terhadap perubahan."
            ),
            tags$li(
              tags$b("Smart Economy:"), 
              " Ekonomi yang stabil untuk mendorong pertumbuhan ekonomi yang berkelanjutan."
            ),
            tags$li(
              tags$b("Smart Environment:"), 
              " Manajemen lingkungan yang efektif, termasuk pengelolaan risiko bencana dan pelestarian sumber daya alam."
            ),
            tags$li(
              tags$b("Smart Governance:"), 
              " Pemerintahan yang transparan dan partisipatif, memanfaatkan teknologi untuk meningkatkan keterlibatan warga dalam proses pengambilan keputusan."
            )
          )
        )
      )
    } else {
      cluster_id <- as.integer(substring(input$cluster_selection, 9))
      cluster_data <- data_filtered()[data_filtered()$cluster == cluster_id, ]
      
      focus_areas <- list(
        X1 = "Peningkatan Infrastruktur yang mendukung Smart Mobility.",
        X2 = "Peningkatan Kapabilitas Inovasi untuk mendukung Smart Living.",
        X3 = "Peningkatan Keterampilan untuk mendukung Smart People.",
        X4 = "Perluasan Pasar Tenaga Kerja untuk mendukung Smart People",
        X5 = "Peningkatan Stabilitas Ekonomi Makro untuk mewujudkan Smart Economy.",
        X6 = "Manajemen Risiko Bencana yang lebih baik untuk Mewujudkan Smart Environment."
      )
      
      significant_variables <- character()
      if (cluster_id == 1) {
        splinecluster1 <- uji12(0)
        coefficients <- splinecluster1$Parameters
        ujiT <- splinecluster1$P_Value
        
        for (i in seq_along(coefficients)) {
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
      
      ui <- box(
        title = paste("Hasil Regresi untuk Klaster", cluster_id),
        status = "info",
        solidHeader = TRUE,
        width = 12,
        div(
          style = "padding: 10px; border-radius: 5px; background-color: #f0f0f0;",
          h3("Fokus pengembangan yang dapat dilakukan untuk mewujudkan Smart City:"),
          tags$ul(
            style="margin-top: 0; margin-bottom: 10px;  margin-right: 40px; text-align: center;",
            lapply(focus_text, function(text) {
              tags$li(
                style = "background: #c1b7ff; font-size: 15px; padding: 6px; 
                margin-bottom: 10px; border-radius: 10px; list-style: none;",
                text
              )
            })
          )
        )
      )
      
      return(ui)
    }
  })
  
  # Render Recommendation Actions Table
  output$recommendations <- renderDT({
    req(input$cluster_selection)
    
    if (input$cluster_selection == "all") {
      recommendation_data <- cluster_recommendations
    } else {
      cluster_id <- as.integer(substring(input$cluster_selection, 9))
      recommendation_data <- cluster_recommendations[cluster_recommendations$cluster == cluster_id, ]
    }
    
    datatable(recommendation_data, options = list(pageLength = 10))
  })
}

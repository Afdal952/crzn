library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggcorrplot)
library(MASS)
library(dplyr)
library(sf)
library(viridis)
library(DT)
library(leaflet)
library(cluster)
library(shinyWidgets)  # Add shinyWidgets library for boxPlus
library(robustbase)

# Import Data
data_adopsi <- read.csv("C:/Users/mhafd/Documents/SEC - repository/dataSEC - new.csv")
colnames(data_adopsi)[colnames(data_adopsi) == 'ID'] <- 'ADM2_PCODE'

Kabupaten.shp <- read_sf(dsn = "C:/Users/mhafd/Documents/SEC - repository/idn_adm_bps_20200401_shp", 
                         layer = "idn_admbnda_adm2_bps_20200401")

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

t_test <- function(B, mx, data, n1) {
  yhat <- mx %*% B
  residuals <- data[, 1] - yhat
  SSE <- sum(residuals^2)
  MSE <- SSE / (nrow(data) - n1)
  var_cov_matrix <- MSE * ginv(t(mx) %*% mx)
  
  # Perform T-test
  t_statistics <- B / sqrt(diag(var_cov_matrix))
  p_values <- 2 * (1 - pt(abs(t_statistics), nrow(data) - n1))
  
  results <- data.frame(Parameters = colnames(mx), Estimate = B, 
                        Std.Error = sqrt(diag(var_cov_matrix)), 
                        t_value = t_statistics, P_Value = p_values)
  return(results)
}

# Run K-Means
trun <- function(data,a,power)
{
  data[data<a] <- a
  (data-a)^power
}

#INVERS MATRIX
MPL<-function(x,eps=1e-20)
{
  x<-as.matrix(x)
  xsvd<-svd(x)
  diago<-xsvd$d[xsvd$d>eps]
  if(length(diago)==1)
  {
    xplus<-as.matrix(xsvd$v[,1])%*%t(as.matrix(xsvd$u[,1])/diago)
  }
  else
  {
    xplus<-
      xsvd$v[,1:length(diago)]%*%diag(1/diago)%*%t(xsvd$u[,1:length(diago)])
  }
  return(xplus)
}

uji12=function(para){
  library(readxl) 
  data=Metpen1
  knot=read_excel("C:/Users/mhafd/Documents/SEC - repository/cluster12.xlsx")
  knot=knot[,-13:-14]
  data=as.matrix(data)  
  knot=as.matrix(knot)  
  ybar=mean(data[,1])  
  m=para+2  
  p=nrow(data)  
  q=ncol(data)  
  dataA=cbind(data[,m],data[,m],data[,m+1],data[,m+1],data[,m+2],data[,m+2],data[,m+3],data[,m+3],data[,m+4],data[,m+4],data[,m+5],data[,m+5])  
  dataA=as.matrix(dataA)  
  satu=rep(1,p)  
  n1=ncol(knot)  
  data.knot=matrix(ncol=n1,nrow=p)  
  for(i in 1:n1)  
  {  
    for(j in 1:p)  
    {  
      if (dataA[j,i]<knot[1,i])  
        data.knot[j,i]=0  
      else data.knot[j,i]=dataA[j,i]-knot[1,i]  
    }  
  }  
  mx=cbind(satu,data[,2],data.knot[,1:2],data[,3],data.knot[,3:4],data[,4],data.knot[,5:6],data[,5],data.knot[,7:8],data[,5],data.knot[,9:10],data[,6],data.knot[,11:12])  
  mx=as.matrix(mx) 
  B=(ginv(t(mx)%*%mx))%*%t(mx)%*%data[,1]  
  cat("=======================================","\n")  
  cat("Estimasi Parameter","\n")  
  cat("=======================================","\n")  
  print(B)  
  n1=nrow(B)  
  yhat=mx%*%B  
  res=data[,1]-yhat  
  SSE=sum((data[,1]-yhat)^2)  
  SSR=sum((yhat-ybar)^2)  
  SST=SSR+SSE 
  MSE=SSE/(p-n1) ;  MSR=SSR/(n1-1)  
  Rsq=(SSR/(SSR+SSE))*100 
  print(Rsq)
  
  # Perhitungan AIC
  L = -(p/2) * log(2*pi) - (p/2) * log(SSE/p) - (1/(2*MSE)) * SSE
  k = ncol(mx)  # Jumlah parameter yang diestimasi
  AIC = 2*k - 2*L
  cat("=======================================","\n")
  cat("Nilai AIC dari model","\n")
  cat("=======================================","\n")
  cat("AIC =", AIC, "\n")
  
  # Model tanpa knots (model regresi linear)
  fit_reduced <- lm(data[,1] ~ data[,2] + data[,3] + data[,4] + data[,5] + data[,6] + data[,7])
  yhat_reduced <- predict(fit_reduced)
  SSE_reduced <- sum((data[,1] - yhat_reduced)^2)
  MSE_reduced <- SSE_reduced / (p - length(coef(fit_reduced)))
  
  # Uji F
  F_statistic <- (((SSE_reduced - SSE)/6) / (SSE/(p-6)))
  p_value_f <- 1 - pf(F_statistic, 6, p - 6)
  print(paste("F-statistic:", F_statistic))
  print(paste("P-value (F):", p_value_f))
  
  # Uji t-statistik
  t_test_result <- t_test(B, mx, data, n1)
  print(t_test_result)
} 
uji23=function(para){
  data=Metpen2
  knot=read_excel("C:/Users/mhafd/Documents/SEC - repositorycluster23.xlsx")
  knot=knot[,-19:-20]
  data=as.matrix(data)  
  knot=as.matrix(knot)  
  ybar=mean(data[,1])  
  m=para+2  
  p=nrow(data)  
  q=ncol(data)  
  dataA=cbind(data[,m],data[,m],data[,m],data[,m+1],data[,m+1],data[,m+1],data[,m+2],data[,m+2],data[,m+2],
              data[,m+3],data[,m+3],data[,m+3],data[,m+4],data[,m+4],data[,m+4],data[,m+5],data[,m+5],data[,m+5])  
  dataA=as.matrix(dataA)  
  satu=rep(1,p)  
  n1=ncol(knot)  
  data.knot=matrix(ncol=n1,nrow=p)  
  for(i in 1:n1)  
  {  
    for(j in 1:p)  
    {  
      if (dataA[j,i]<knot[1,i])  
        data.knot[j,i]=0  
      else data.knot[j,i]=dataA[j,i]-knot[1,i]  
    }  
  }  
  mx=cbind(satu,data[,2],data.knot[,1:3],data[,3],data.knot[,4:6],data[,4],data.knot[,7:9],data[,5],data.knot[,10:12],data[,6],data.knot[,13:15],data[,7],data.knot[,16:18])  
  mx=as.matrix(mx) 
  B=(ginv(t(mx)%*%mx))%*%t(mx)%*%data[,1]  
  cat("=======================================","\n")  
  cat("Estimasi Parameter","\n")  
  cat("=======================================","\n")  
  print(B)  
  n1=nrow(B)  
  yhat=mx%*%B  
  res=data[,1]-yhat  
  SSE=sum((data[,1]-yhat)^2)  
  SSR=sum((yhat-ybar)^2)  
  SST=SSR+SSE 
  MSE=SSE/(p-n1) ;  MSR=SSR/(n1-1)  
  Rsq=(SSR/(SSR+SSE))*100 
  print(Rsq)   
  
  # Perhitungan AIC
  L = -(p/2) * log(2*pi) - (p/2) * log(SSE/p) - (1/(2*MSE)) * SSE
  k = ncol(mx)  # Jumlah parameter yang diestimasi
  AIC = 2*k - 2*L
  cat("=======================================","\n")
  cat("Nilai AIC dari model","\n")
  cat("=======================================","\n")
  cat("AIC =", AIC, "\n")
  
  # Model tanpa knots (model regresi linear)
  fit_reduced <- lm(data[,1] ~ data[,2] + data[,3] + data[,4] + data[,5] + data[,6] + data[,7])
  yhat_reduced <- predict(fit_reduced)
  SSE_reduced <- sum((data[,1] - yhat_reduced)^2)
  MSE_reduced <- SSE_reduced / (p - length(coef(fit_reduced)))
  
  # Uji F
  F_statistic <- (((SSE_reduced - SSE)/6) / (SSE/(p-6)))
  p_value_f <- 1 - pf(F_statistic, 6, p - 6)
  print(paste("F-statistic:", F_statistic))
  print(paste("P-value (F):", p_value_f))
  
  # Uji t-statistik
  t_test_result <- t_test(B, mx, data, n1)
  print(t_test_result)
}
uji33=function(para){
  library(readxl) 
  data=Metpen3
  knot=read_excel("C:/Users/mhafd/Documents/SEC - repositorycluster33.xlsx")
  knot=knot[,-19:-20]
  data=as.matrix(data)  
  knot=as.matrix(knot)  
  ybar=mean(data[,1])  
  m=para+2  
  p=nrow(data)  
  q=ncol(data)  
  dataA=cbind(data[,m],data[,m],data[,m],data[,m+1],data[,m+1],data[,m+1],data[,m+2],data[,m+2],data[,m+2],
              data[,m+3],data[,m+3],data[,m+3],data[,m+4],data[,m+4],data[,m+4],data[,m+5],data[,m+5],data[,m+5])  
  dataA=as.matrix(dataA)  
  satu=rep(1,p)  
  n1=ncol(knot)  
  data.knot=matrix(ncol=n1,nrow=p)  
  for(i in 1:n1)  
  {  
    for(j in 1:p)  
    {  
      if (dataA[j,i]<knot[1,i])  
        data.knot[j,i]=0  
      else data.knot[j,i]=dataA[j,i]-knot[1,i]  
    }  
  }  
  mx=cbind(satu,data[,2],data.knot[,1:3],data[,3],data.knot[,4:6],data[,4],data.knot[,7:9],data[,5],data.knot[,10:12],data[,6],data.knot[,13:15],data[,7],data.knot[,16:18])  
  mx=as.matrix(mx) 
  B=(ginv(t(mx)%*%mx))%*%t(mx)%*%data[,1]  
  cat("=======================================","\n")  
  cat("Estimasi Parameter","\n")  
  cat("=======================================","\n")  
  print(B)  
  n1=nrow(B)  
  yhat=mx%*%B  
  res=data[,1]-yhat  
  SSE=sum((data[,1]-yhat)^2)  
  SSR=sum((yhat-ybar)^2)  
  SST=SSR+SSE 
  MSE=SSE/(p-n1) ;  MSR=SSR/(n1-1)  
  Rsq=(SSR/(SSR+SSE))*100 
  print(Rsq)   
  
  # Perhitungan AIC
  L = -(p/2) * log(2*pi) - (p/2) * log(SSE/p) - (1/(2*MSE)) * SSE
  k = ncol(mx)  # Jumlah parameter yang diestimasi
  AIC = 2*k - 2*L
  cat("=======================================","\n")
  cat("Nilai AIC dari model","\n")
  cat("=======================================","\n")
  cat("AIC =", AIC, "\n")
  
  # Model tanpa knots (model regresi linear)
  fit_reduced <- lm(data[,1] ~ data[,2] + data[,3] + data[,4] + data[,5] + data[,6] + data[,7])
  yhat_reduced <- predict(fit_reduced)
  SSE_reduced <- sum((data[,1] - yhat_reduced)^2)
  MSE_reduced <- SSE_reduced / (p - length(coef(fit_reduced)))
  
  # Uji F
  F_statistic <- (((SSE_reduced - SSE)/6) / (SSE/(p-6)))
  p_value_f <- 1 - pf(F_statistic, 6, p - 6)
  print(paste("F-statistic:", F_statistic))
  print(paste("P-value (F):", p_value_f))
  
  # Uji t-statistik
  t_test_result <- t_test(B, mx, data, n1)
  print(t_test_result)
}

# UI Shiny dengan tema warna yang disesuaikan
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(
      title = tags$span(
        "Strategy Forge",
        style = "height: 100%; display: flex; align-items: center; font-size: 20px; font-weight: bold;"
      ),
      titleWidth = 200,
      tags$li(class = "dropdown",
              div(
                style = "margin-top: 10px; margin-right: 15px;",
                selectInput("cluster_selection", label = NULL,
                            choices = c("Semua Klaster" = "all", paste("Klaster", unique(dat_map$cluster))),
                            width = "200px")
              )
      )
    ),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      # Set background color and font
      tags$head(
        tags$style(HTML("
          body {
            font-family: Verdana, serif;
            background-color: #f0f0f5; /* Warna background */
          }
          .main-header .logo {
            font-family: Georgia, serif;
            height: 58.77px;
          }
          .sidebar {
            background-color: #4b0082 !important; /* Warna sidebar */
            color: #ffffff; /* Warna teks sidebar */
          }
          #cluster_map {
            height: 1000px; /* Atur tinggi peta sesuai preferensi */
            border-radius: 10px; /* Atur border radius peta */
          }
          .main-header .logo {
            background-color: #4b0082 !important; /* Warna background logo header */
          }
          .main-header .logo:hover {
            background-color: #2e0854 !important; /* Warna background logo header saat hover */
          }
          .main-header .navbar {
            background-color: #6a5acd !important; /* Warna background navbar header */
          }
          .content-wrapper, .right-side {
            background-color: #ffffff; /* Warna background konten utama */
          }
          .box.box-primary {
            border-top-color: #6a5acd; /* Warna border box */
          }
          .box.box-solid.box-primary>.box-header {
            color: #fff;
            background: #3c8dbc;
            background-color: #6a5acd;
          }
          .info-box-icon.bg-ungu {
            background: linear-gradient(to right, #6a5acd, #836FFF); /* Gradient ungu */
          }
          .alert-info, .bg-aqua, .callout.callout-info, .label-info, .modal-info .modal-body {
            background-color: #9b28b7!important;
            border-radius: 5px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          }
          .box.box-solid.box-info>.box-header {
            color: #fff;
            background: purple;
            background-color: slateblue;
          }
          .cluster-characteristics-title {
            background-color: lightgray;
            padding: 10px;
            margin-bottom: 20px;
            border-radius: 10px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
            font-size: 18px;
            font-weight: bold;
            width: fit-content;
            margin-left: 25%;
          }
          .info-box-text, .progress-description {
            display: block;
            font-size: 13px;
            overflow: hidden;
            text-overflow: ellipsis;
            text-wrap: balance;
          }
          .col-sm-6 {
            width: 50%;
          }
          .dataTables_wrapper {
            position: relative;
            clear: both;
            margin-top: 10px;
          }
          .h3, h3{
            font-size: 20px;
            font-weight: 800;
            text-align: center;
            margin-top: 0px
          }
          .info-box-content {
            padding: 20px 10px 0px 10px;
            margin-left: 90px;
          }
        "))
      ),
      fluidRow(
        box(
          width = 12,
          title = "Data Exploration",
          status = "primary",
          solidHeader = TRUE,
          tabsetPanel(
            id = "data_exploration",
            tabPanel("Map", leafletOutput(outputId = "cluster_map")),
            tabPanel("Table", DTOutput(outputId = "cluster_table")),
            tabPanel("Chart", plotOutput(outputId = "plot_output"))
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          uiOutput("cluster_title_ui"),
          uiOutput("cluster_characteristics")
        ),
        column(
          width = 6,
          uiOutput("cluster_regression")
        )
      ),
      fluidRow(
        downloadButton("download_cluster", "Download Cluster Data")
      )
    )
  )
)

# Server Logic
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

# Run Shiny App
shinyApp(ui = ui, server = server)

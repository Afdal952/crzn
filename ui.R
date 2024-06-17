library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(leaflet)
library(ggplot2)
library(MASS)
library(dplyr)
library(sf)
library(viridis)
library(readxl)

# Set working directory
#setwd("C:/Users/mhafd/OneDrive/Documents/GitHub/EtherealSEC")

# Import Data
data_adopsi <- read.csv("dataSEC - new.csv")
colnames(data_adopsi)[colnames(data_adopsi) == 'ID'] <- 'ADM2_PCODE'

Kabupaten.shp <- read_sf(dsn = ".", layer = "idn_admbnda_adm2_bps_20200401")

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
clustering_data <- data_adopsi[, c("Adopsi.TIK","Infrastruktur", 
                                   "Kapabilitas.Inovasi", "Keterampilan", 
                                   "Pasar.Tenaga.Kerja", "Stabilitas.Ekonomi", "IRBI")]

# Select variables for clustering
cluster_vars <- data_adopsi[, c("Infrastruktur", "Kapabilitas.Inovasi", 
                                "Keterampilan", "Pasar.Tenaga.Kerja", 
                                "Stabilitas.Ekonomi", "IRBI")]

# Run K-Means
clustering_data <- data_adopsi %>%
  dplyr::select(-Adopsi.TIK) %>%
  na.omit()

# Select variables for clustering
cluster_vars <- clustering_data[, c("Infrastruktur", "Kapabilitas.Inovasi", 
                                    "Keterampilan", "Pasar.Tenaga.Kerja", 
                                    "Stabilitas.Ekonomi", "IRBI")]

# Run K-Means
set.seed(20)
kmeans_result <- kmeans(cluster_vars, centers = 3)  # Misal, 3 klaster
dat_map$cluster <- factor(kmeans_result$cluster)

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
  #return(results)
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
  data=Metpen1
  knot=read_excel("cluster12.xlsx")
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
  
  # Perhitungan AIC
  L = -(p/2) * log(2*pi) - (p/2) * log(SSE/p) - (1/(2*MSE)) * SSE
  k = ncol(mx)  # Jumlah parameter yang diestimasi
  
  # Model tanpa knots (model regresi linear)
  fit_reduced <- lm(data[,1] ~ data[,2] + data[,3] + data[,4] + data[,5] + data[,6] + data[,7])
  yhat_reduced <- predict(fit_reduced)
  SSE_reduced <- sum((data[,1] - yhat_reduced)^2)
  MSE_reduced <- SSE_reduced / (p - length(coef(fit_reduced)))
  
  # Uji F
  F_statistic <- (((SSE_reduced - SSE)/6) / (SSE/(p-6)))
  p_value_f <- 1 - pf(F_statistic, 6, p - 6)
  
  # Uji t-statistik
  t_test_result <- t_test(B, mx, data, n1)
} 
uji23=function(para){
  data=Metpen2
  knot=read_excel("cluster23.xlsx")
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
  
  # Perhitungan AIC
  L = -(p/2) * log(2*pi) - (p/2) * log(SSE/p) - (1/(2*MSE)) * SSE
  k = ncol(mx)  # Jumlah parameter yang diestimasi
  AIC = 2*k - 2*L
  
  # Model tanpa knots (model regresi linear)
  fit_reduced <- lm(data[,1] ~ data[,2] + data[,3] + data[,4] + data[,5] + data[,6] + data[,7])
  yhat_reduced <- predict(fit_reduced)
  SSE_reduced <- sum((data[,1] - yhat_reduced)^2)
  MSE_reduced <- SSE_reduced / (p - length(coef(fit_reduced)))
  
  # Uji F
  F_statistic <- (((SSE_reduced - SSE)/6) / (SSE/(p-6)))
  p_value_f <- 1 - pf(F_statistic, 6, p - 6)
  
  # Uji t-statistik
  t_test_result <- t_test(B, mx, data, n1)
}
uji33=function(para){
  data=Metpen3
  knot=read_excel("cluster33.xlsx")
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
  
  # Perhitungan AIC
  L = -(p/2) * log(2*pi) - (p/2) * log(SSE/p) - (1/(2*MSE)) * SSE
  k = ncol(mx)  # Jumlah parameter yang diestimasi
  AIC = 2*k - 2*L
  
  # Model tanpa knots (model regresi linear)
  fit_reduced <- lm(data[,1] ~ data[,2] + data[,3] + data[,4] + data[,5] + data[,6] + data[,7])
  yhat_reduced <- predict(fit_reduced)
  SSE_reduced <- sum((data[,1] - yhat_reduced)^2)
  MSE_reduced <- SSE_reduced / (p - length(coef(fit_reduced)))
  
  # Uji F
  F_statistic <- (((SSE_reduced - SSE)/6) / (SSE/(p-6)))
  p_value_f <- 1 - pf(F_statistic, 6, p - 6)
  
  # Uji t-statistik
  t_test_result <- t_test(B, mx, data, n1)
}

library(shiny)
library(shinydashboard)

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(
      title = tags$span(
        tags$img(src = "logo.png", height = "50px", style = "margin-right: 10px;"),
        "Strategy Forge",
        style = "height: 100%; display: flex; align-items: center; font-size: 26px; font-weight: bold;"
      ),
      titleWidth = 300,  # Adjusted to accommodate the logo
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
            background-color: #f0f0f5;
          }
          .main-header .logo {
            font-family: Georgia, serif;
            height: 70px;
          }
          .sidebar {
            background-color: #4b0082 !important;
            color: #ffffff;
          }
          #cluster_map {
            height: 1000px; 
            border-radius: 10px; 
          }
          .main-header .logo {
            background-color: #4b0082 !important; 
          }
          .main-header .logo:hover {
            background-color: #2e0854 !important; 
          }
          .logo{
          width: auto;
          }
          .main-header .navbar {
            background-color: #6a5acd !important; 
            height: 70px; 
          }
          .main-header .navbar-custom-menu, .main-header .navbar-right {
            float: right;
            margin-top: 6px;
          }
          .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
            margin-top: 20px;
          }
          .content-wrapper, .right-side {
            background-color: #ffffff;
          }
          .box.box-primary {
            border-top-color: #6a5acd;
          }
          .box.box-solid.box-info {
            border: 1px solid #d2d6de;
          }
          .box.box-solid.box-primary {
            border: 1px solid #ccc;
            border-radius: 5px;
          }
          .box.box-solid.box-primary>.box-header {
            color: #fff;
            background: #3c8dbc;
            background-color: #6a5acd;
            text-align: center;
            border-radius: 5px 5px 0px 0px
          }
          .box-header .box-title, .box-header>.fa, .box-header>.glyphicon, .box-header>.ion {
            display: flex;
            font-size: 18px;
            margin: 0;
            font-family: Verdana;
          }
          .info-box-icon.bg-ungu {
            background: linear-gradient(to right, #6a5acd, #836FFF);
          }
          .alert-info, .bg-aqua, .callout.callout-info, .label-info, .modal-info .modal-body {
            background-color: #9b28b7!important;
            border-radius: 5px;
            box-shadow: 2px 2px 5px rgba(0,0,0,0.2);
          }
          .box.box-solid.box-info>.box-header {
            color: #fff;
            background: purple;
            background-color: slateblue;
          }
          .cluster-characteristics-title {
            background-color: #f0f0f0;
            padding: 12px;
            margin-bottom: 20px;
            border-radius: 10px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.5);
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
          .h3, h3 {
            font-size: 20px;
            font-weight: 800;
            text-align: center;
            margin: 0px 0px 10px;
          }
          .info-box-content {
            padding: 20px 10px 0px 10px;
            margin-left: 90px;
          }
          .cluster-regression-info {
            font-family: Verdana;
            font-size: 11px;
            font-weight: 200;
            text-align: justify;
            padding: 12px;
            background-color: #d3d3d3;
            margin-right: 30px;
          }
          div#cluster_characteristics {
            margin-top: 25px;
          }
          .info-box {
            display: block;
            min-height: 90px;
            background: #fff;
            width: 100%;
            box-shadow: 0 1px 1px rgba(0,0,0,.1);
            border-radius: 2px;
            margin-bottom: 10px;
            margin-top: 10px;
          }
          .row {
            margin-right: -15px;
            margin-left: -15px;
            margin-top: 10px;
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
      )
    )
  )
)
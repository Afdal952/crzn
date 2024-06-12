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

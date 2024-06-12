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
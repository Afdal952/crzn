# Use the official R image as a base
FROM rocker/r-ver:4.1.0

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gdal-bin \
    libsqlite-dev \
    libglpk-dev

# Install R packages
RUN R -e "install.packages(c('dplyr', 'ggplot2', 'shiny', 'DT', 'leaflet', 'readxl', 'viridis', 'sf', 'MASS', 'shinydashboard', 'shinyWidgets'), repos='http://cran.rstudio.com/')"

# Copy app files
COPY . /srv/shiny-server/

# Set working directory
WORKDIR /srv/shiny-server/

# Expose the port the app runs on
EXPOSE 3838

# Run the application
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server')"]



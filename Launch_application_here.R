# Short script to easily launch the shiny application with multiple cores for faster processing
# Sam Siljee
# 20 November 2023

# List of required packages
required_packages <- c(
  "BiocManager",
  "shiny",
  "MSstats",
  "MSstatsTMT",
  "vroom",
  "janitor",
  "DT",
  "ggplot2",
  "tidyr",
  "dplyr",
  "stringr",
  "tibble",
  "rmarkdown",
  "tinytex",
  "shinycssloaders",
  "shinyjs",
  "ComplexHeatmap",
  "clusterProfiler",
  "STRINGdb"
)

# Load or check and install missing packages
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    if (package %in% c("ComplexHeatmap", "clusterProfiler", "STRINGdb", "MSstats", "MSstatsTMT")) {
      # Bioconductor packages
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        install.packages("BiocManager")
      }
      BiocManager::install(package)
    } else {
      # CRAN packages
      install.packages(package)
    }
  }
  library(package, character.only = TRUE)
}

# Get the number of available cores, reserve 20% for other tasks
available_cores <- floor(0.8 * parallel::detectCores())

# Set the number of CPU cores for Shiny app for faster processing, and maximum file size
options(shiny.num_procs = available_cores, shiny.maxRequestSize = 30 * 1024^3)

# Load your Shiny app
shiny::runApp("MS_proteomics_shiny/")

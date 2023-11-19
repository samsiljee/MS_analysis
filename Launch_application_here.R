# Short script to easily launch the shiny application with multiple cores for faster processing
# Sam Siljee
# 20 November 2023

# Libraries
library(parallel)

# Get the number of available cores, reserve 20% for other tasks
available_cores <- floor(0.8 * detectCores())

# Set the number of CPU cores for Shiny app
options(shiny.num_procs = available_cores)

# Load your Shiny app
shiny::runApp("MS_proteomics_shiny/")

# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages ----

library(shiny)

source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)

# Shiny app to run proteomics analysis in R
# Created by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages ----

# Load required packages

library(shiny)
library(MSstats)
library(tidyverse)

# UI ----

# Display the UI, with tabs for each section

ui <- navbarPage("Proteomics analysis pipeline",
                 
  # Welcome ----
  tabPanel("Welcome",
           p("Welcome to my proteomics analysis pipeline."),
           p("Please move sequentially through the tabs to complete the analysis.")
           ),
  
  # Input ----
  tabPanel("Input",
           p("Please upload your dataset as exported from Proteome discoverer and the corresponding annotations table here:"),
           fileInput(inputId = "PSMs",
                     label = "PSMs file",
                     buttonLabel = "Browse",
                     placeholder = "Upload PSMs file here"),
           fileInput(inputId = "annotations",
                     label = "Annotations file",
                     buttonLabel = "Browse",
                     placeholder = "Upload annotations file here")
           ),
  
  # MSstats ----
  tabPanel("MSstats",
           p("This section will be where MSstats is computed. There will be drop down options here too for the settings.")
           ),
  tabPanel("Comparisons",
           p("This section will be where the comparisons will be defined. There will be an input panel to set up the comparion matrix")
           ),
  tabPanel("Visualisation",
           p("This section will be for making the graphs. Again a sidebar panel to select the types of graphs.")
           ),
  navbarMenu("Menu placeholder",
             tabPanel("First option",
                      
                      ),
             tabPanel("Second option",
                      
                      )
             )
)

# Server ----

server <- function(input, output, session) {
  # MSstats ----
  output$table <- renderTable(
    read.table(input$annotations,
               sep = ",")
  )
  
}



# Run the application 
shinyApp(ui = ui, server = server)

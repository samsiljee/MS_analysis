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

ui <- navbarPage(
  title = "Proteomics analysis pipeline",
                 
# Instructions ----

  tabPanel("Instructions",
    
    "Welcome to my proteomics analysis pipeline.",
    
    "Please move sequentially through the tabs to complete the analysis."
    ),
  
# Input ----

  tabPanel("Input",
    
    "Please upload your dataset as exported from Proteome discoverer and the corresponding annotations table here:",
    
    sidebarPanel(
      
      fileInput("annotations", "Annotations file",
        buttonLabel = "Browse",
        placeholder = "Upload annotations file here"
        ),
      
      radioButtons("annotations_sep", "Separator",
        choices = c(
          Tab = "\t",
          Comma = ",",
          Semicolon = ";"
          ),
        selected = ","
        ),
      
      tags$hr(),
      
      fileInput("PSMs", "PSMs file",
                buttonLabel = "Browse",
                placeholder = "Upload PSMs file here"
      ),
      
      radioButtons("PSMs_sep", "Separator",
                   choices = c(
                     Tab = "\t",
                     Comma = ",",
                     Semicolon = ";"
                   ),
                   selected = "\t"
      )
      
      ),
    
    mainPanel(
              
      "Annotations",
      dataTableOutput("annotation_tab"),
      
      tags$hr(),
      
      "PSM data",
      dataTableOutput("head_PSMs_tab")
      
    )
    ),
  
# MSstats ----

  tabPanel("MSstats",
    
    "This section will be where MSstats is computed. There will be drop down options here too for the settings."
    
    ),

# Comparisons ----

  tabPanel("Comparisons",
    
    "This section will be where the comparisons will be defined. There will be an input panel to set up the comparion matrix"
    
    ),

# Visualisations ----

  tabPanel("Visualisation",
    
    "This section will be for making the graphs. Again a sidebar panel to select the types of graphs."
    
    ),

)

# Server ----

# Setting option to increase allowed file size to 30MB, I will probably have to increase this further
options(shiny.maxRequestSize=30*1024^2)

# Set up the server to run the calculations

server <- function(input, output, session){

# Input ----
# First set some values using `reactive`
  
# Generate the output
  
  output$annotation_tab <- renderDataTable({
    req(input$PSMs)
    
    annot_col <- read.table(
      input$annotations$datapath,
      header = TRUE,
      sep = input$annotations_sep
    )
    
    return(annot_col)
    
  })
    
  output$head_PSMs_tab <- renderDataTable({
    req(input$PSMs)
    
    raw <- read.table(
      input$PSMs$datapath,
      header = TRUE,
      sep = input$PSMs_sep
      )
    
    return(raw)
    
  })
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

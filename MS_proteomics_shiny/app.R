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
  title = "MS analysis",
                 
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
        placeholder = "Upload annotations file here"),
      
      radioButtons("annotations_sep", "Separator",
        choices = c(
          Tab = "\t",
          Comma = ",",
          Semicolon = ";"),
        selected = ","),
      
      tags$hr(),
      
      fileInput("PSMs", "PSMs file",
        buttonLabel = "Browse",
        placeholder = "Upload PSMs file here"),
      
      radioButtons("PSMs_sep", "Separator",
        choices = c(
          Tab = "\t",
          Comma = ",",
          Semicolon = ";"),
                   selected = "\t"
      )
      
      ),
    
    mainPanel(
              
      h3("Annotations"),
      dataTableOutput("annotation_tab"),
      
      tags$hr(),
      
      h3("PSM data"),
      dataTableOutput("PSMs_tab")
      
    )
    ),
  
# Format ----
tabPanel("Format", "Process your data to work in MSstats, options can be changed on the left.",
  sidebarPanel(h4("MSstats formating options"),
    checkboxInput("useNumProteinsColumn",
                  "Remove peptides with more than one in \"number of proteins\" column of PD output",
                  value = FALSE),
    checkboxInput("useUniquePeptide",
                  "Remove peptides assigned to more than one protein",
                  value = TRUE),
    checkboxInput("removeFewMeasurements",
                  "Remove features with one or two measurements across runs",
                  value = TRUE),
    checkboxInput("removeOxidationMpeptides",
                  "Remove peptides with methionine oxidation",
                  value = FALSE),
    checkboxInput("removeProtein_with1Peptide",
                  "Remove proteins with only one peptide and charge",
                  value = TRUE),
    radioButtons("summaryforMultipleRows",
                 "Summary method for multiple rows",
                 choiceNames = c("Max", "Sum"),
                 choiceValues = c("max", "sum")),
    radioButtons("which.quantification",
                 "Column to be used for quantification",
                 choiceNames = c("Precursor area", "Intensity", "Area"),
                 choiceValues = c("Precursor.Area", "Intensity", "Area")),
    radioButtons("which.proteinid",
                 "Column to be used for protein names",
                 choiceNames = c("Protein accessions", "Master protein accessions"),
                 choiceValues = c("Protein.Accessions", "Master.Protein.Accessions")),
    radioButtons("which.sequence",
                 "Column to be used for peptide sequences",
                 choiceNames = c("Sequence", "Annotated sequence"),
                 choiceValues = c("Sequence", "Annotated.Sequence")),
    actionButton("go_format",
                 "Format!")
    ),
  
  mainPanel("Put some text here")
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
    
    )

)

# Server ----

# Setting option to increase allowed file size to 30MB, I will probably have to increase this further
options(shiny.maxRequestSize=30*1024^2)

# Set up the server to run the calculations

server <- function(input, output, session){

# Input ----
# Set reactive values
  
  annot_col <- reactive({
    read.table(
      input$annotations$datapath,
      header = TRUE,
      sep = input$annotations_sep
      )
    })
  
  raw <- reactive({
    read.table(
      input$PSMs$datapath,
      header = TRUE,
      sep = input$PSMs_sep
      ) %>%
      # rename columns as required by MSstats
      mutate(
        ProteinGroupAccessions = .$Master.Protein.Accessions,
        PrecursorArea = .$Precursor.Abundance,
        Run = .$Spectrum.File)
    })
  
# Generate output

  output$annotation_tab <- renderDataTable({
    annot_col()
  })
    
  output$PSMs_tab <- renderDataTable({
    raw()
  })
  
# Format ----
# Set reactive values

  
# Generate output
  
  }

# Run the application 
shinyApp(ui = ui, server = server)

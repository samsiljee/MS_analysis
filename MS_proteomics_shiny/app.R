# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages ----

library(shiny)
library(MSstats)
library(tidyverse)

# UI ----

ui <- navbarPage(
  title = "MS analysis",

# Instructions ----

  tabPanel("Instructions",
    
    "Welcome to my proteomics analysis pipeline.",
    
    "Please move sequentially through the tabs to complete the analysis."),
  
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
                   selected = "\t")),
    
    mainPanel(
              
      h3("Annotations"),
      dataTableOutput("annotation_tab"),
      
      tags$hr(),
      
      h3("PSM data"),
      dataTableOutput("PSMs_tab"))),
  
# Format ----
tabPanel("Format", "Format and pre-filter your data to work in MSstats, options can be changed on the left.",
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
    actionButton("go_format", "Format!")),
  
  mainPanel("Preview of formatted input data",
            "I still need to add functionality to download as .rda, .csv, or .tsv format, and a way to save the log file.",
    dataTableOutput("MSstats_input_tab"))),

# Process ----

tabPanel("Process",
  sidebarPanel(h4("MSstats processing options"),
    radioButtons("logTrans",
      "Base of log transformation",
      choices = c(2, 10),
      selected = 2),
    radioButtons("normalization",
      "Normalisation method used to remove bias between runs",
      choiceNames = c("Equalize medians", "Quantile", "Global standards", "None"),
      choiceValues = c("equalizeMedians", "quantile", "globalStandards", FALSE)),
    textInput("nameStandards",
      "Named vector for global standard peptides (Global standards"),
    radioButtons("featureSubset",
      "Subset features to use",
      choiceNames = c("All", "Top 3", "Top N", "High quality"),
      choiceValues = c("all", "top3", "topN", "highQuality")),
    numericInput("n_top_feature",
      "Number of top features to use (Top N)",
      value = 3),
    checkboxInput("remove_uninformative_feature_outlier",
      "Remove noisy features and outliers before run-level summarisation (High quality)",
      value = FALSE),
    numericInput("min_feature_count",
      "Minimum features required to be considered in feature selection algorythm (High quality)",
      value = 2),
    radioButtons("summaryMethod",
      "Method used to summarise features",
      choiceNames = c("Tukey's median polish", "Linear mixed model"),
      choiceValues = c("TMP", "linear")),
    checkboxInput("equalFeatureVar",
      "Account for heterogeneous variation among intensities from different features (Linear)",
      value = TRUE),
    checkboxInput("MBimpute",
      "Impute censored values by Accelated failure model (TMP)",
      value = TRUE),
    checkboxInput("remove50missing",
      "Remove runs with >50% missing values (TMP)",
      value = FALSE),
    numericInput("maxQuantileforCensored",
      "Maximum quantile for deciding censored missing values",
      value = 0.999),
    actionButton("go_process", "Process!")),
  
  mainPanel("This section will be where MSstats processing happens. There will be drop down options here too for the settings.",
            "Note that currently I've not got the \"Global standards\" method working as it takes a named vector as input",
            "Overview of processed protein level data",
            dataTableOutput("MSstats_processed_protein_tab")
            )),

# Comparison ----

  tabPanel("Comparison",
    "This section will be where MSstats is computed. There will be drop down options here too for the settings."),

# Visualisation ----

  tabPanel("Visualisation",
    "This section will be for making the graphs. Again a sidebar panel to select the types of graphs.")
# Close UI
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
      sep = input$annotations_sep)
    })
  
  raw <- reactive({
    read.table(
      input$PSMs$datapath,
      header = TRUE,
      sep = input$PSMs_sep) %>%
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
# Reactive values
  MSstats_input <- eventReactive(input$go_format, {
    PDtoMSstatsFormat(
      input = raw(),
      annotation = annot_col(),
      useNumProteinsColumn = input$useNumProteinsColumn,
      useUniquePeptide = input$useUniquePeptide,
      summaryforMultipleRows = ifelse(input$summaryforMultipleRows == "max", max, sum),
      removeFewMeasurements = input$removeFewMeasurements,
      removeOxidationMpeptides = input$removeOxidationMpeptides,
      removeProtein_with1Peptide = input$removeProtein_with1Peptide,
      which.quantification = input$which.quantification,
      which.proteinid = input$which.proteinid,
      which.sequence = input$which.sequence,
      use_log_file = FALSE
    )
  })

# Output
  output$MSstats_input_tab <- renderDataTable({
    MSstats_input()
  })

# Process ----
  # Reactive values
  
  MSstats_processed <- eventReactive(input$go_process, {
    dataProcess(
      MSstats_input(),
      logTrans = as.numeric(input$logTrans),
      normalization = input$normalization,
      nameStandards = input$nameStandards,
      featureSubset = input$featureSubset,
      remove_uninformative_feature_outlier = input$remove_uninformative_feature_outlier,
      min_feature_count = input$min_feature_count,
      n_top_feature = input$n_top_feature,
      summaryMethod = input$summaryMethod,
      equalFeatureVar = input$equalFeatureVar,
      censoredInt = input$censoredInt,
      MBimpute = input$MBimpute,
      remove50missing = input$remove50missing,
      fix_missing = input$fix_missing,
      maxQuantileforCensored = input$maxQuantileforCensored,
      use_log_file = FALSE)
  })
  
  # Output
  output$MSstats_processed_protein_tab <- renderDataTable({
    MSstats_processed()$ProteinLevelData
  })
 
# Comparison ----
  
# Visualisation ----
     
# Testing ----
  
  # Close the server function
  }

# Run the application 
shinyApp(ui = ui, server = server)

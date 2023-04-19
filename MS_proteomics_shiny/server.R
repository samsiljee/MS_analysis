# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages ----
library(shiny)
library(MSstats)
library(tidyverse)

# Setting option to increase allowed file size to 30MB, I will probably have to increase this further
options(shiny.maxRequestSize=30*1024^2)

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

  output$annotation_tab <- renderDataTable(annot_col())
    
  output$PSMs_tab <- renderDataTable(raw())
  
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
  output$MSstats_input_tab <- renderDataTable(MSstats_input())

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
      censoredInt = ifelse(input$censoredInt == "NULL", NULL, input$censoredInt),
      MBimpute = input$MBimpute,
      remove50missing = input$remove50missing,
      fix_missing = ifelse(input$fix_missing == "NULL", NULL, input$fix_missing),
      maxQuantileforCensored = input$maxQuantileforCensored,
      use_log_file = FALSE)
  })
  
  # Output
  output$MSstats_processed_tab <- renderDataTable({
    switch(input$processed_tab_view,
           ProteinLevelData = {
             MSstats_processed()$ProteinLevelData
           },
           FeatureLevelData = {
             MSstats_processed()$FeatureLevelData
           })
  })
 
# Comparison ----
  #Making UI input with selection of variables for comparison
  output$select_numerator <- renderUI({
    selectInput("numerator", "Numerator/s",
                choices = conditions(),
                multiple = TRUE)
  })
  
  output$select_denominator <- renderUI({
    selectInput("denominator", "Denominator/s",
                choices = setdiff(conditions(), input$numerator),
                multiple = TRUE)
  })
  
  #Reactive variables
  conditions <- reactive(sort(unique(annot_col()$Condition)))
  
  num_conditions <- reactive(length(unique(annot_col()$Condition)))
  
  comparison <- eventReactive(input$add_comparison, {
    ifelse(conditions() %in% input$numerator,
           1,
           ifelse(conditions() %in% input$denominator,
                  -1,
                  0))
  })
  
  # Generate Reactive Text Data
  blank_matrix <- reactiveValues(data = {
    df <- data.frame(matrix(nrow = 0, ncol = 1))
    df
    })
  
  # Add rows equal to n conditions
  comparison_matrix <- reactive({
    cbind(blank_matrix$data, data.frame(matrix(nrow = 0, ncol = num_conditions())))
  })
  
  # Add New Text
  comparison_matrix <- eventReactive(input$add_comparison, {
    rbind(comparison_matrix(), comparison())
  })
  
  #Output
  output$comparison_matrix_tab <- renderTable(comparison_matrix())
  
  output$comparison_text <- renderText(comparison())
  
# Visualisation ----
  
#Testing ----
  
# Downloads ----
  #Formatted data tables
  output$formatted_csv <- downloadHandler(
    filename = function() {
      paste0("MSstats_formatted_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(MSstats_input(), file)
    }
  )
  
  output$formatted_rda <- downloadHandler(
    filename = function() {
      paste0("MSstats_formatted_", Sys.Date(), ".rda")
    },
    content = function(file) {
      saveRDS(MSstats_input(), file = file)
    }
  )
  
  #Processed data
  
  output$processed_protein_csv <- downloadHandler(
    filename = function() {
      paste0("Processed_protein_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(MSstats_processed()$ProteinLevelData, file)
    }
  )
  
  output$processed_feature_csv <- downloadHandler(
    filename = function() {
      paste0("Processed_feature_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(MSstats_processed()$FeatureLevelData, file)
    }
  )
  
  output$processed_rda <- downloadHandler(
    filename = function() {
      paste0("MSstats_processed_", Sys.Date(), ".rda")
    },
    content = function(file) {
      saveRDS(MSstats_processed(), file = file)
    }
  )
  
  # Close the server
  }

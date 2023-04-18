# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages ----

library(shiny)
library(MSstats)
library(tidyverse)
library(shinycssloaders)

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
  
# Downloads
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
  
  # Download
  
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
 
# Comparison ----
  
  #Making input for UI side with selection of variables for comparison
  output$select_numerator <- renderUI({
    selectInput("numerator", "Numerator/s",
                choices = conditions(),
                multiple = TRUE)
  })
  
  output$select_denominator <- renderUI({
    selectInput("denominator", "Denominator/s",
                choices = conditions(),
                multiple = TRUE)
  })
  
  #Blank comparison_matrix
  comparison_matrix <- NULL
  
  #Reactive variables
  conditions <- reactive({
    sort(unique(annot_col()$Condition))
  })
  
  comparison <- reactive({
    c(input$comparison_name, c(1,-1, 0))
  })
  
  comparison_matrix <- eventReactive(input$go_comparison, {
    rbind(comparison_matrix, comparison)
  })
  
  #Output
  output$comparison_matrix_tab <- renderDataTable({
    comparison_matrix()
  })
  
# Visualisation ----
     
# Testing ----
  
  # Close the server
  }

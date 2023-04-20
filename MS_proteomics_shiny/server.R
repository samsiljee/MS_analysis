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
    if (!is.null(input$annotations)) {
      read.table(input$annotations$datapath,
                 header = TRUE,
                 sep = input$annotations_sep)
    } else {
      data.frame()
    }
  })
  
  raw <- reactive({
    if (!is.null(input$PSMs)) {
    read.table(
      input$PSMs$datapath,
      header = TRUE,
      sep = input$PSMs_sep) %>%
      # rename columns as required by MSstats
      mutate(
        ProteinGroupAccessions = .$Master.Protein.Accessions,
        PrecursorArea = .$Precursor.Abundance,
        Run = .$Spectrum.File)
      } else {
        data.frame()
        }
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

  
  # Reactive UI
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
  
  # Reactive variables
  conditions <- reactive({
    annot_col()$Condition %>% unique() %>% sort()
  })
  
  num_conditions <- reactive({
    length(unique(annot_col()$Condition))
  })
  
  #Attempt to make "comparison_matrix" available outside of the observe event
  comparison_matrix <- NULL
  makeReactiveBinding("comparison_matrix")
  
  # Making the comparison matrix
  observeEvent(input$annotations, {

    # Generate empty matrix
    comparison_values <- reactiveValues(
      matrix = data.frame(matrix(nrow = 0, ncol = num_conditions() + 1)),
      num_rows = 0)
    
    # Add row to matrix
    comparison_matrix <<- eventReactive(input$add_comparison, {
      row <- c(input$comparison_name,
               ifelse(
                 conditions() %in% input$numerator,
                 1,
                 ifelse(
                   conditions() %in% input$denominator,
                   -1,
                   0)))
      comparison_values$num_rows <- comparison_values$num_rows + 1
      comparison_values$matrix[comparison_values$num_rows, ] <- row
      colnames(comparison_values$matrix) <- c("Comparison", conditions())
      comparison_values$matrix
    })

  # End the observeEvent here
})
  
  # Run the comparison function
  MSstats_test <- eventReactive(input$go_compare, {
    groupComparison(
      contrast.matrix = comparison_matrix()[,-1],
      data = MSstats_processed(),
      save_fitted_models = input$save_fitted_models,
      log_base = input$logTrans,
      use_log_file = FALSE
    )
  })
  
# Output
  output$comparison_matrix_tab <- renderTable(comparison_matrix())
  
  output$results_tab <- renderDataTable({
    switch(input$results_tab_view,
           ComparisonResult = MSstats_test()$ComparisonResult,
           ModelQC = MSstats_test()$ModelQC)
  })

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
  
  # Comparison
  output$results_csv <- downloadHandler(
    filename = function() {
      paste0("MSstats_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(MSstats_test()$ComparisonResult, file)
    }
  )
  
  output$model_qc_csv <- downloadHandler(
    filename = function() {
      paste0("MSstats_model_QC_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(MSstats_test()$ModelQC, file)
    }
  )  
  
  output$comparisons_rda <- downloadHandler(
    filename = function() {
      paste0("MSstats_test_results_", Sys.Date(), ".rda")
    },
    content = function(file) {
      saveRDS(MSstats_test(), file = file)
    }
  )
  
  # Close the server
  }

# Process
# Produce named standards
standards <- reactive({
  if (!is.null(input$nameStandards)) {
    fasta <- readLines(input$nameStandards$datapath)
    standards <- str_extract(fasta[grep(">", fasta)], "(?<=sp\\|)[[:alnum:]]+")
    return(standards)
  } else {
    NULL
  }
})

# Process input
MSstats_processed <- eventReactive(input$go_process, {
  process_log_file_path <- file.path("logs", "process_log.txt")

  switch(input$quant_method,
    LFQ = {
      dataProcess(
        MSstats_input(),
        logTrans = as.numeric(input$logTrans),
        normalization = input$normalization,
        nameStandards = standards(),
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
        use_log_file = TRUE,
        log_file_path = process_log_file_path
      )
    },
    TMT = {
      proteinSummarization(
        MSstats_input(),
        method = input$TMTProtSumMethod,
        global_norm = input$global_norm,
        reference_norm = input$reference_norm,
        remove_norm_channel = input$remove_norm_channel,
        remove_empty_channel = TRUE,
        MBimpute = input$MBimpute,
        maxQuantileforCensored = input$maxQuantileforCensored,
        use_log_file = TRUE,
        log_file_path = process_log_file_path
      )
    }
  )
})

# Output
output$MSstats_processed_tab <- renderDataTable({
  switch(input$processed_tab_view,
    ProteinLevelData = {
      MSstats_processed()$ProteinLevelData
    },
    FeatureLevelData = {
      MSstats_processed()$FeatureLevelData
    }
  )
})

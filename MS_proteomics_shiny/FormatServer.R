# Format
# Reactive UI
output$select_summary_method <- renderUI({
  radioButtons(
    "summaryforMultipleRows",
    "Summary method for multiple rows",
    choiceNames = c("Sum", "Max"),
    choiceValues = c("sum", "max"),
    selected = switch(input$quant_method,
      LFQ = {
        "max"
      },
      TMT = {
        "sum"
      }
    )
  )
})

# Generate input
MSstats_input <- eventReactive(input$go_format, {
  # Path for log files
  format_log_file_path <- file.path("logs", "format_log.txt")

  # Create the directory if it doesn't exist
  if (!dir.exists("logs")) {
    dir.create("logs")
  }

  switch(input$quant_method,
    ## LFQ ----
    LFQ = {
      switch(input$platform,
        PD = {
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
            use_log_file = TRUE,
            log_file_path = format_log_file_path
          )
        }, # switch = PD (LFQ)
        MQ = {
          MaxQtoMSstatsFormat(
            evidence = raw(),
            annotation = annot_col(),
            proteinGroups = protein_groups(),
            proteinID = input$MQLFQproteinID,
            useUniquePeptide = input$useUniquePeptide,
            summaryforMultipleRows = ifelse(input$summaryforMultipleRows == "max", max, sum),
            removeFewMeasurements = input$removeFewMeasurements,
            removeMpeptides = input$removeMpeptides,
            removeOxidationMpeptides = input$removeOxidationMpeptides,
            removeProtein_with1Peptide = input$removeProtein_with1Peptide,
            use_log_file = TRUE,
            log_file_path = format_log_file_path
          )
        }, # switch = MQ (LFQ)
        DIANN = {
          DIANNtoMSstatsFormat(
            input = raw(),
            annotation = annot_col(),
            global_qvalue_cutoff = 0.01,
            qvalue_cutoff = 0.01,
            pg_qvalue_cutoff = 0.01,
            useUniquePeptide = TRUE,
            removeFewMeasurements = TRUE,
            removeOxidationMpeptides = TRUE,
            removeProtein_with1Feature = TRUE,
            use_log_file = TRUE,
            append = FALSE,
            verbose = TRUE,
            log_file_path = NULL,
            MBR = TRUE,
          )
        } # swich = DIANN (LFQ)
      )
    }, # switch = LFQ

    ## TMT ----
    TMT = {
      switch(input$platform,
        PD = {
          PDtoMSstatsTMTFormat(
            input = raw(),
            annotation = annot_col(),
            which.proteinid = input$which.proteinid,
            useNumProteinsColumn = input$useNumProteinsColumn,
            useUniquePeptide = input$useUniquePeptide,
            rmPSM_withfewMea_withinRun = input$removeFewMeasurements,
            rmProtein_with1Feature = input$rmProtein_with1Feature,
            summaryforMultipleRows = ifelse(input$summaryforMultipleRows == "max", max, sum),
            use_log_file = TRUE,
            log_file_path = format_log_file_path
          )
        }, # close PD (TMT)
        MQ = {
          MaxQtoMSstatsTMTFormat(
            evidence = raw(),
            annotation = annot_col(),
            proteinGroups = protein_groups(),
            which.proteinid = input$MQTMTproteinID,
            rmProt_Only.identified.by.site = input$rmProt_Only.identified.by.site,
            useUniquePeptide = input$useUniquePeptide,
            rmPSM_withfewMea_withinRun = input$removeFewMeasurements,
            rmProtein_with1Feature = input$rmProtein_with1Feature,
            summaryforMultipleRows = ifelse(input$summaryforMultipleRows == "max", max, sum),
            use_log_file = TRUE,
            log_file_path = format_log_file_path
          )
        } # close MQ (TMT)
      ) # close switch platform
    } # Close TMT
  ) # close switch quant method
}) # eventReactive

# Output
output$MSstats_input_tab <- renderDataTable(MSstats_input())

# Format

# Generate input
MSstats_input <- eventReactive(input$go_format, {
  # Path for log files
  format_log_file_path <- file.path("logs", "format_log.txt")

  # Create the directory if it doesn't exist
  if (!dir.exists("logs")) {
    dir.create("logs")
  }

  switch(
    input$quant_method,
    LFQ = {
      switch(
        input$platform,
        # LFQ PD ----
        PD = {
          PDtoMSstatsFormat(
            input = raw(),
            annotation = annot_col(),
            useNumProteinsColumn = input$LFQPDuseNumProteinsColumn,
            useUniquePeptide = input$LFQPDuseUniquePeptide,
            summaryforMultipleRows = ifelse(input$LFQPDsummaryforMultipleRows == "max", max, sum),
            removeFewMeasurements = input$LFQPDremoveFewMeasurements,
            removeOxidationMpeptides = input$LFQPDremoveOxidationMpeptides,
            removeProtein_with1Peptide = input$LFQPDremoveProtein_with1Peptide,
            which.quantification = input$LFQPDwhich.quantification,
            which.proteinid = input$LFQPDwhich.proteinid,
            which.sequence = input$LFQPDwhich.sequence,
            use_log_file = TRUE,
            log_file_path = format_log_file_path
          )
        }, # switch = PD (LFQ)
        # LFQ MQ ----
        MQ = {
          MaxQtoMSstatsFormat(
            evidence = raw(),
            annotation = annot_col(),
            proteinGroups = protein_groups(),
            proteinID = input$LFQMQproteinID,
            useUniquePeptide = input$LFQMQuseUniquePeptide,
            summaryforMultipleRows = ifelse(input$LFQMQsummaryforMultipleRows == "max", max, sum),
            removeFewMeasurements = input$LFQMQremoveFewMeasurements,
            removeMpeptides = input$LFQMQremoveMpeptides,
            removeOxidationMpeptides = input$LFQMQremoveOxidationMpeptides,
            removeProtein_with1Peptide = input$LFQMQremoveProtein_with1Peptide,
            use_log_file = TRUE,
            log_file_path = format_log_file_path
          )
        }, # switch = MQ (LFQ)
        # LFQ DIA-NN ----
        DIANN = {
          DIANNtoMSstatsFormat(
            input = raw(),
            annotation = annot_col(),
            global_qvalue_cutoff = input$global_qvalue_cutoff,
            qvalue_cutoff = input$qvalue_cutoff,
            pg_qvalue_cutoff = input$pg_qvalue_cutoff,
            useUniquePeptide = input$useUniquePeptide,
            removeFewMeasurements = input$removeFewMeasurements,
            removeOxidationMpeptides = input$removeOxidationMpeptides,
            removeProtein_with1Feature = input$removeProtein_with1Feature,
            use_log_file = TRUE,
            log_file_path = format_log_file_path,
            MBR = input$MBR,
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
            which.proteinid = input$TMTPDwhich.proteinid,
            useNumProteinsColumn = input$TMTPDuseNumProteinsColumn,
            useUniquePeptide = input$TMTPDuseUniquePeptide,
            rmPSM_withfewMea_withinRun = input$TMTPDremoveFewMeasurements,
            rmProtein_with1Feature = input$TMTPDrmProtein_with1Feature,
            summaryforMultipleRows = ifelse(input$TMTPDsummaryforMultipleRows == "max", max, sum),
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

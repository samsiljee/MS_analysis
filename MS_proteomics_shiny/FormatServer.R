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
            global_qvalue_cutoff = input$LFQDIANNglobal_qvalue_cutoff,
            qvalue_cutoff = input$LFQDIANNqvalue_cutoff,
            pg_qvalue_cutoff = input$LFQDIANNpg_qvalue_cutoff,
            useUniquePeptide = input$LFQDIANNuseUniquePeptide,
            removeFewMeasurements = input$LFQDIANNremoveFewMeasurements,
            removeOxidationMpeptides = input$LFQDIANNremoveOxidationMpeptides,
            removeProtein_with1Feature = input$LFQDIANNremoveProtein_with1Feature,
            MBR = input$LFQDIANNMBR,
            use_log_file = TRUE,
            log_file_path = format_log_file_path
          )
        } # swich = DIANN (LFQ)
      )
    }, # switch = LFQ

    TMT = {
      switch(
        input$platform,
        # TMT PD ----
        PD = {
          PDtoMSstatsTMTFormat(
            input = raw(),
            annotation = annot_col(),
            which.proteinid = input$TMTPDwhich.proteinid,
            useNumProteinsColumn = input$TMTPDuseNumProteinsColumn,
            useUniquePeptide = input$TMTPDuseUniquePeptide,
            rmPSM_withfewMea_withinRun = input$TMTPDrmPSM_withfewMea_withinRun,
            rmProtein_with1Feature = input$TMTPDrmProtein_with1Feature,
            summaryforMultipleRows = ifelse(input$TMTPDsummaryforMultipleRows == "max", max, sum),
            use_log_file = TRUE,
            log_file_path = format_log_file_path
          )
        }, # close PD (TMT)
        
        # TMT MQ ----
        MQ = {
          MaxQtoMSstatsTMTFormat(
            evidence = raw(),
            annotation = annot_col(),
            proteinGroups = protein_groups(),
            which.proteinid = input$TMTMQwhich.proteinid,
            rmProt_Only.identified.by.site = input$TMTMQrmProt_Only.identified.by.site,
            useUniquePeptide = input$TMTMQuseUniquePeptide,
            rmPSM_withfewMea_withinRun = input$TMTMQrmPSM_withfewMea_withinRun,
            rmProtein_with1Feature = input$TMTMQrmProtein_with1Feature,
            summaryforMultipleRows = ifelse(input$TMTMQsummaryforMultipleRows == "max", max, sum),
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

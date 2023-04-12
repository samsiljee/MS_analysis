# UI page


# Server side code for formating

# Format ----
# Set reactive values
input <- eventReactive(input$go_format, {
  PDtoMSstatsFormat(
    input = raw(),
    annotation = annot_col(),
    useNumProteinsColumn = input$useNumProteinsColumn,
    useUniquePeptide = input$useUniquePeptide,
    summaryforMultipleRows = input$summaryforMultipleRows,
    removeFewMeasurements = input$removeFewMeasurements,
    removeOxidationMpeptides = input$removeOxidationMpeptides,
    removeProtein_with1Peptide = input$removeProtein_with1Peptide,
    which.quantification = input$which.quantification,
    which.proteinid = input$which.proteinid,
    which.sequence = input$which.sequence,
    use_log_file = FALSE
  )
})
# Generate output
output$input_tab <- renderDataTable({
  input()
})
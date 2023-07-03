# Input
# read in files
annot_col <- reactive({
    if (!is.null(input$annotations)){
        df <- vroom(input$annotations$datapath)
        df$PcaRef <- str_trim(as.character(df$Run))
        df$PcaRef <- gsub(".", "", df$PcaRef, fixed = TRUE)
        df
    } else {
        data.frame()
    }
})

raw <- reactive({
    if (!is.null(input$PSMs)) {
        switch(input$platform,
               PD = {
                   df <- vroom(input$PSMs$datapath)
                   df <- clean_names(df, case = "upper_camel")
                   # rename columns as required by `MSstats
                   df <- switch(input$quant_method,
                                LFQ = {
                                    mutate(df,
                                           ProteinGroupAccessions = MasterProteinAccessions,
                                           PrecursorArea = PrecursorAbundance,
                                           Run = SpectrumFile)},
                                TMT = {
                                    mutate(df,
                                           ProteinGroupAccessions = MasterProteinAccessions,
                                           Run = SpectrumFile)
                                })
                   df},
               
               MQ = {
                   df <- vroom(input$PSMs$datapath)
                   if(input$keep_contaminants) {
                       df$`Potential contaminant` <- NA
                   }
                   df}
        )
    } else {
        data.frame()
    }
})

protein_groups <- reactive({
    if (!is.null(input$proteinGroups)) {
        vroom(input$proteinGroups$datapath)
    } else{
        data.frame()
    }
})

# Generate output
output$annotation_tab <- renderDataTable(annot_col())

output$PSMs_tab <- renderDataTable(raw())

output$proteinGroups_tab <- renderDataTable(protein_groups())
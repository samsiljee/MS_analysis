# Input
# reactive UI
output$psm_input <- renderUI({
    fileInput("PSMs",
              switch(input$platform,
                     PD = {"PSMs file"},
                     MQ = {"MQ evidence file"}),
              buttonLabel = "Browse",
              placeholder = switch(input$platform,
                                   PD = {"Upload PSMs.txt"},
                                   MQ = {"Upload evidence.txt"}))
})

# read in annotations
annot_col <- reactive({
    switch(input$quant_method,
           LFQ = {
               if (!is.null(input$annotations)){
                   df <- vroom(input$annotations$datapath)
                   df$PcaRef <- str_trim(as.character(df$Run))
                   df$PcaRef <- gsub(".", "", df$PcaRef, fixed = TRUE)
                   
                   # Add "Experiment' column for PCA and heatmap labeling
                   df <- df %>%
                     group_by(Condition) %>%
                     mutate(Experiment = paste0(Condition, "_", row_number())) %>%
                     ungroup()
                   
                   df
               } else {
                   data.frame()
               }},
           TMT = {
               if (!is.null(input$channel_annotations) & !is.null(input$run_annotations)){
                   # Channel annotations
                   channel_df <- vroom(input$channel_annotations$datapath)
                   
                   # Run annotations
                   run_df <- vroom(input$run_annotations$datapath)
                   
                   # Combine
                   df <- full_join(run_df, channel_df)
                   
                   df$PcaRef <- str_trim(as.character(df$Run))
                   df$PcaRef <- gsub(".", "", df$PcaRef, fixed = TRUE)
                   
                   # Add "Experiment' column for PCA and heatmap labeling
                   df <- df %>%
                     group_by(Condition) %>%
                     mutate(Experiment = paste0(Condition, "_", row_number())) %>%
                     ungroup()
                   
                   df
               } else {
                   data.frame()
               }
           })
})

# Read in raw PSM data
raw <- reactive({
    if (!is.null(input$PSMs)) {
        switch(
            input$platform,
            PD = {
                df <- vroom(input$PSMs$datapath)
                df <- clean_names(df, case = "upper_camel")
                # rename columns as required by `MSstats
                df <- switch(
                    input$quant_method,
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

# Read in proteinGroups for MQ experiments
protein_groups <- reactive({
    if (!is.null(input$proteinGroups)) {
        vroom(input$proteinGroups$datapath)
    } else{
        data.frame()
    }
})

# Generate output
output$annotation_tab <- renderDataTable(annot_col() %>% dplyr::select(!(PcaRef)))

output$PSMs_tab <- renderDataTable(raw())

output$proteinGroups_tab <- renderDataTable(protein_groups())
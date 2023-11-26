# Reactive UI ----
output$plot_title_input_qc <- renderUI({
    textInput("plot_title_qc",
              "Plot title",
              value = switch(
                  input$plot_type_qc,
                  Abundance = "Abundance"
              )
    )
})

# Plot x lab
output$plot_x_lab_input_qc <- renderUI({
    textInput("plot_x_lab_qc",
              "X label",
              value = switch(
                  input$plot_type_qc,
                  Abundance = "Run"
              )
    )
})

# Plot y lab
output$plot_y_lab_input_qc <- renderUI({
    textInput("plot_y_lab_qc",
              "Y label",
              value = switch(
                  input$plot_type_qc,
                  Abundance = "Log intensity"
              )
    )
})

# Reactive variables ----

# Selected theme
selected_theme_qc <- reactive({
    switch(input$select_theme_qc,
           "B&W" = theme_bw(),
           "Gray" = theme_gray(),
           "Classic" = theme_classic(),
           "Minimal" = theme_minimal(),
           "Void" = theme_void()
    )
})

# Make plots ----
## Abundance plot ----
# Get data for un-normalised data
un_normalised_data <- reactiveVal(NULL)
observeEvent(input$go_process, {
    un_normalised_data({
        dataProcess(
            MSstats_input(),
            logTrans = as.numeric(input$logTrans),
            normalization = FALSE,
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
            use_log_file = FALSE
        ) %>% .$ProteinLevelData
    })
})

# Combine to make data for abundance plot
abundance_plot_data <- reactive({
    # Clean normalised data
   norm_dat <- merge(
       x = MSstats_processed()$ProteinLevelData %>%
           mutate(originalRUN = as.character(originalRUN)),
       y = annot_col(),
       by.x = "originalRUN",
       by.y = "PcaRef",
       all.x = TRUE
   ) %>%
      mutate(Normalisation = "Normalised") %>%
       dplyr::select(originalRUN, LogIntensities, Normalisation)
   
   # Clean un-normalised data
   un_norm_dat <- merge(
       x = un_normalised_data() %>%
           mutate(originalRUN = as.character(originalRUN)),
       y = annot_col(),
       by.x = "originalRUN",
       by.y = "PcaRef",
       all.x = TRUE
   ) %>%
       mutate(Normalisation = "Un-normalised") %>%
       dplyr::select(originalRUN, LogIntensities, Normalisation)
   
   # Return combined datasets
   return(rbind(norm_dat, un_norm_dat))
})


# Create plot
abundance_plot <- reactive({
    abundance_plot_data() %>%
        ggplot(aes(x = originalRUN, y = LogIntensities, fill = Normalisation)) +
        geom_boxplot() +
        ylab(input$plot_y_lab_qc) +
        xlab(input$plot_x_lab_qc) +
        ggtitle(input$plot_title_qc) +
        selected_theme_qc()
})

# Render plot ----
output$plot_qc <- renderPlot({
    plot_obj <- switch(
        input$plot_type_qc,
        Abundance = abundance_plot()
    )
    return(plot_obj)
})

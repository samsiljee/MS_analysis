# Downloads
# Formatted data tables
output$formatted_tsv <- downloadHandler(
  filename = function() {
    paste0("MSstats_formatted_", Sys.Date(), ".tsv")
  },
  content = function(file) {
    vroom_write(MSstats_input(), file, delim = "\t")
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

# Processed data
output$processed_protein_tsv <- downloadHandler(
  filename = function() {
    paste0("Processed_protein_data_", Sys.Date(), ".tsv")
  },
  content = function(file) {
    vroom_write(MSstats_processed()$ProteinLevelData, file, delim = "\t")
  }
)

output$processed_feature_tsv <- downloadHandler(
  filename = function() {
    paste0("Processed_feature_data_", Sys.Date(), ".tsv")
  },
  content = function(file) {
    vroom_write(MSstats_processed()$FeatureLevelData, file, delim = "\t")
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
output$results_tsv <- downloadHandler(
  filename = function() {
    paste0("MSstats_results_", Sys.Date(), ".tsv")
  },
  content = function(file) {
    vroom_write(MSstats_comparison_results(), file, delim = "\t")
  }
)

output$model_qc_tsv <- downloadHandler(
  filename = function() {
    paste0("MSstats_model_QC_", Sys.Date(), ".tsv")
  },
  content = function(file) {
    vroom_write(MSstats_test()$ModelQC, file, delim = "\t")
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

## Analysis ----
output$go_results_tsv <- downloadHandler(
  filename = function() {
    paste0("GO_analysis_results_", Sys.Date(), ".tsv")
  },
  content = function(file) {
    vroom_write(go_results(), file, delim = "\t")
  }
)

output$STRING_dataset_tsv <- downloadHandler(
  filename = function() {
    paste0("STRING_dataset_", Sys.Date(), ".tsv")
  },
  content = function(file) {
    vroom_write(STRING_dataset(), file, delim = "\t")
  }
)

output$STRING_enrichment_tsv <- downloadHandler(
  filename = function() {
    paste0("STRING_enrichment_", Sys.Date(), ".tsv")
  },
  content = function(file) {
    vroom_write(STRING_enrichment(), file, delim = "\t")
  }
)

## Download plots ----
output$plot_download <- downloadHandler(
  filename = function() {
    switch(input$plot_type,
      Volcano = paste0(input$comparison_selected, "_Volcano_", Sys.Date(), ".png"),
      PCA = paste0("PCA_", Sys.Date(), ".png"),
      Heatmap = paste0("Heatmap_", Sys.Date(), ".png"),
      `GO enrichment` = paste0(
        "GO_enrichment_",
        input$go_comparison_selected, "_",
        input$go_direction_selected, "_",
        input$go_ont_selected, "_",
        Sys.Date(), ".png"
      ),
      `STRING network` = paste0("STRING_network_", Sys.Date(), ".png")
    )
  },
  content = function(file) {
    switch(input$plot_type,
      Volcano = {
        ggsave(file,
          plot = volcano_plot(),
          width = input$plot_width,
          height = input$plot_height,
          dpi = input$plot_dpi,
          units = "mm"
        )
      },
      PCA = {
        ggsave(file,
          plot = pca_plot(),
          width = input$plot_width,
          height = input$plot_height,
          dpi = input$plot_dpi,
          units = "mm"
        )
      },
      Heatmap = {
        png(file,
          height = input$plot_height,
          width = input$plot_width,
          res = input$plot_dpi,
          units = "mm"
        )
        draw(heatmap_plot())
        dev.off()
      },
      `GO enrichment` = {
        ggsave(file,
          plot = go_enrichment_plot(),
          width = input$plot_width,
          height = input$plot_height,
          dpi = input$plot_dpi,
          units = "mm"
        )
      },
      `STRING network` = {
        png(file,
          height = input$plot_height,
          width = input$plot_width,
          res = input$plot_dpi,
          units = "mm"
        )
        draw(STRING_network_plot())
        dev.off()
      }
    ) # Switch
  } # Content
) # Download handler

# Methods summary
output$downloadReport <- downloadHandler(
  filename = paste("Analysis_methods_summary_", Sys.Date(), ".pdf"),
  content = function(file) {
    # Copy to temp directory in case writing permission not given
    tempReport <- file.path(tempdir(), "MethodsSummary.Rmd")
    file.copy("MethodsSummary.Rmd", tempReport, overwrite = TRUE)
    
    # Set up parameters to pass to the methods summary
    params <- list(
      input = reactiveValuesToList(input),
      contrast_matrix = ifelse(input$contrast_method == "custom",
                               comparison_matrix_updated()[-1, ],
                               data.frame()),
      working_directory = getwd()
      )

    # Knit the document, with params
    rmarkdown::render(tempReport,
      output_file = file,
      params = params,
      envir = new.env(parent = globalenv())
    )
  }
)

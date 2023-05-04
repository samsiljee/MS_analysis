# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages ----
library(shiny)
library(MSstats)
library(ComplexHeatmap)

 

# Setting option to increase allowed file size to 30MB, I will probably have to increase this further
options(shiny.maxRequestSize=30*1024^3)

server <- function(input, output, session){
 # Packages
  if (!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
  }
  
  if (!require(tidyr)) {
    install.packages("tidyr")
    library(tidyr)
  }
  
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  
  if (!require(tibble)) {
    install.packages("tibble")
    library(tibble)
  }
  
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
      # rename columns as required by `MSstats`
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
  conditions <- reactive(sort(unique(annot_col()$Condition)))

  # Define comparison_matrix as a reactiveValues object
  c_vals <- reactiveValues(matrix = NULL, comparison_names = character())
  
  # Initialising the comparison matrix on uploading annotations file
  observeEvent(input$annotations, {
    add_comparison()
  })
  
  # Define function to add a row
  add_comparison <- function() {
    
    # Read the values used to create the row
    row <- ifelse(
      conditions() %in% input$numerator,
      1,
      ifelse(
        conditions() %in% input$denominator,
        -1,
        0))
    
    # Update the row names
    c_vals$comparison_names <- if (is.null(c_vals$matrix)) {
      "First_name_to_be_removed"
    } else {
      c(c_vals$comparison_names, input$comparison_name)
    }
    
    # Update the matrix
    c_vals$matrix <- if (is.null(c_vals$matrix)) {
      c_vals$matrix <- matrix(NA,  nrow = 1, ncol = length(row))
    } else {
      c_vals$matrix <- rbind(c_vals$matrix, matrix(row, nrow = 1, ncol = length(row)))
    }
    
    # Update the column names
    colnames(c_vals$matrix) <- conditions()
    
    #Update the row names
    rownames(c_vals$matrix) <- c_vals$comparison_names
    
    return(c_vals$matrix)
  }
  
  # Update the comparison matrix when add_comparison is clicked
  comparison_matrix_updated <- eventReactive(input$add_comparison, {
    add_comparison()
  })
  
  # Run the comparison function
  MSstats_test <- eventReactive(input$go_compare, {
    groupComparison(
      contrast.matrix = if (input$pairwise) {
          "pairwise"
        } else {
          comparison_matrix_updated()[-1, , drop = FALSE]
        },
      data = MSstats_processed(),
      save_fitted_models = input$save_fitted_models,
      log_base = input$logTrans,
      use_log_file = FALSE)
  })
  
  # Results of comparison, and some further processing
  MSstats_comparison_results <- reactive({
      MSstats_test()$ComparisonResult %>%
      mutate(Dif = ifelse(
        MSstats_test()$ComparisonResult$log2FC > 1 & MSstats_test()$ComparisonResult$adj.pvalue < 0.05,
        "Upregulated",
        ifelse(
          MSstats_test()$ComparisonResult$log2FC < -1 & MSstats_test()$ComparisonResult$adj.pvalue < 0.05,
          "Downregulated",
          "Not significant")))
    })
  
  MSstats_results <- reactive({
    if(input$filter_results) {
      MSstats_comparison_results()[-which(MSstats_comparison_results()$log2FC == Inf | MSstats_comparison_results()$log2FC == -Inf),]
    } else {
      MSstats_comparison_results()
    }
  })
      
  # Output
  output$comparison_matrix_tab <- renderTable(comparison_matrix_updated()[-1, , drop = FALSE], rownames = TRUE)
  
  output$results_tab <- renderDataTable({
    switch(input$results_tab_view,
           ComparisonResult = MSstats_results(),
           ModelQC = MSstats_test()$ModelQC)
  })
  
output$outliers <- renderText(paste("There are", length(which(MSstats_comparison_results()$log2FC == Inf | MSstats_comparison_results()$log2FC == -Inf)), "results with infinite fold-change.", sep = " "))

# Visualisation ----
  # Reactive UI
  output$select_comparison <- renderUI({
    selectInput("comparison_selected", "Comparison to plot",
                choices = sort(unique(MSstats_results()$Label)),
                multiple = FALSE)
  })
  
# Reactive variables
plot_height <- reactive(input$plot_height)
plot_width <- reactive(input$plot_width)
selected_theme <- reactive({
  switch(input$select_theme,
         "B&W" = theme_bw(),
         "Gray" = theme_gray(),
         "Classic" = theme_classic(),
         "Minimal" = theme_minimal(),
         "Void" = theme_void())
})

# create a matrix of protein abundance for use in heatmap
prot_mat <- reactive({
  df <- MSstats_processed()$ProteinLevelData %>%
    select(Protein, originalRUN, LogIntensities) %>%
    pivot_wider(names_from = originalRUN, values_from = LogIntensities)
  rownames(df) <- df$Protein
  df <- df[,-1] %>% as.matrix() %>% na.omit()
  df
  })

#create annotations for sample type
column_ha <- reactive(HeatmapAnnotation(Condition = annot_col()$Condition))

  # Set colours as a named vector - for use in volcano plot
  colours <- c("red", "blue", "black") 
  names(colours) <- c("Upregulated", "Downregulated", "Not significant")
  
  # Make volcano plot
  volcano_plot <-  eventReactive(input$go_plot, {
    MSstats_results() %>%
    filter(Label == input$comparison_selected) %>%
    ggplot(aes(x = log2FC, y = -log10(adj.pvalue), col = Dif)) +
    geom_vline(xintercept = c(-1, 1), linetype = "dashed", colour = "black") +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "red") +
    geom_point(alpha = 0.25, show.legend = FALSE) +
    scale_color_manual(values = colours) +
    ylab("-Log10(adjusted p-value)") +
    xlab("Log2 fold change") +
    ggtitle(input$comparison_selected)
  })
  
  # Make heatmap
  heatmap_plot <- eventReactive(input$go_plot, {
    #create heatmap of gene expression, scaled rows (genes)
    t(scale(t(prot_mat()))) %>% 
      Heatmap(
        row_title = "Proteins",
        column_title = "Unfiltered proteome heatmap",
        show_row_dend = FALSE,
        show_column_dend = TRUE, 
        column_names_gp = gpar(fontsize = 8),
        bottom_annotation = column_ha(),
        show_row_names = FALSE,
        show_heatmap_legend = FALSE)
  })
  
  # Make PCA
  pca_plot <- eventReactive(input$go_plot, {
    pca <- prcomp(t(prot_mat()), center = TRUE, scale. = TRUE)
    pca_dat <- merge(pca$x, annot_col(), by.x = "row.names", by.y = "Run")

    #plot eigen values
    eigen_plot <- fviz_eig(pca) + ggtitle("Eigen value plot of Rosalind data")

  #  eigen_plot

    #save plot
 #   ggsave(plot = eigen_plot, filename = paste0(dir, "/", "Rosalind_Eigen_value_plot.png"), device = "png")

    #plot first two PCs
    pca_plot <- ggplot(pca_dat, aes(x = PC1, y = PC2, colour = Condition)) +
      geom_point() +
      ggtitle("PCA plot")

    pca_plot
  })
  
  # Output
  output$plot <- renderPlot({
    plot_obj <- switch(input$plot_type,
                       Volcano = volcano_plot(),
                       PCA = pca_plot(),
                       Heatmap = heatmap_plot()) +
      selected_theme()
    return(plot_obj)
  })
  
#Testing ----
  
  output$test <- renderTable(prot_mat())
  
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
      write.csv(MSstats_results(), file)
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
  
  # Calculate width and height in pixels
  width_in <- reactive(input$plot_width/25.4)
  height_in <- reactive(input$plot_height/25.4)
  
  # Download the plot
  # Download the plot
  output$plot_download <- downloadHandler(
    filename = switch(input$plot_type,
                      Volcano = paste0(input$comparison_selected, " volcano plot.png"),
                      PCA = "PCA plot.png",
                      Heatmap = "Heatmap.png"),
    
    content = function(file) {
      # Define the device function
      device <- function(filename) {
        grDevices::png(
          filename = filename,
          width = width_in(),
          height = height_in(),
          res = 600,
          bg = "white"
        )
      }
      
      # Create the plot object
      plot_obj <- switch(input$plot_type,
                         Volcano = volcano_plot(),
                         PCA = pca_plot(),
                         Heatmap = heatmap_plot()) +
        selected_theme()
      
      # Save the plot object to the PNG file
      ggplot2::ggsave(
        file = file,
        plot = plot_obj,
        device = device,
        bg = "white",
        width = width_in(),
        height = height_in()
      )
    },
    
    contentType = "image/png"
    
  )
  
  
  
  
  # Close the server
  }

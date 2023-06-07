# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages ----
library(shiny)
library(MSstats)
library(ComplexHeatmap)
library(vroom)
library(janitor)
library(clusterProfiler) # may be replaced with topGO, or a GO tool via API
library(STRINGdb)
library(DT)

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
  
  if (!require(stringr)) {
    install.packages("stringr")
    library(stringr)
  }
  
  if (!require(tibble)) {
    install.packages("tibble")
    library(tibble)
  }
  
  observeEvent(input$species, {
    selected_species <- input$species
    
    # Load the selected package
    switch(selected_species,
           "Human" = library(org.Hs.eg.db),
           "Rat" = library(org.Rn.eg.db))
  })
  
  output$test_text <- renderText({
    paste("Species", input$species, "selected.")
  })
  
# Input ----
# Set reactive values
  
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
          df <-  mutate(df,
                        ProteinGroupAccessions = MasterProteinAccessions,
                        PrecursorArea = PrecursorAbundance,
                        Run = SpectrumFile)
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
  
# Format ----
  # Reactive values
  MSstats_input <- eventReactive(input$go_format, {
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
          use_log_file = FALSE)
      }, # switch = PD
      
    MQ = {
      MaxQtoMSstatsFormat(
        evidence = raw(),
        annotation = annot_col(),
        proteinGroups = protein_groups(),
        proteinID = input$proteinID,
        useUniquePeptide = input$useUniquePeptide,
        summaryforMultipleRows = ifelse(input$summaryforMultipleRows == "max", max, sum),
        removeFewMeasurements = input$removeFewMeasurements,
        removeMpeptides = input$removeMpeptides,
        removeOxidationMpeptides = input$removeOxidationMpeptides,
        removeProtein_with1Peptide = input$removeProtein_with1Peptide,
        use_log_file = FALSE)
    } # switch = MQ
    ) # switch
  }) # eventReactive

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
  
  # # Removing the last comparison from the matrix
  # observeEvent(input$reset_comparison, {
  #   c_vals <- reactiveValues(matrix = NULL, comparison_names = character())
  #   observeEvent(input$annotations, {
  #     add_comparison()
  #   })
  # })
  
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
  
  # Results of comparison, and adding up/downregulation
  MSstats_comparison_results <- reactive({
      MSstats_test()$ComparisonResult %>%
      mutate(Dif = ifelse(
        MSstats_test()$ComparisonResult$log2FC > input$FC_threshold & MSstats_test()$ComparisonResult$adj.pvalue < input$pvalue_threshold,
        "Upregulated",
        ifelse(
          MSstats_test()$ComparisonResult$log2FC < -input$FC_threshold & MSstats_test()$ComparisonResult$adj.pvalue < input$pvalue_threshold,
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
  
output$outliers <- renderText(paste("There are",
                                    length(which(MSstats_comparison_results()$log2FC == Inf | MSstats_comparison_results()$log2FC == -Inf)),
                                    "results with infinite fold-change.",
                                    sep = " "))

# Analysis ----
## GO enrichment analysis ----

# Reactive UI
output$select_go_comparison <- renderUI({
  selectInput("go_comparison_selected", "Comparison/s to use",
              choices = sort(unique(MSstats_results()$Label)),
              multiple = TRUE)
})

# Set up value for results
go_results <- reactiveVal(data.frame())

# Run GO enrichment analysis
observeEvent(input$go_go, {
  results_added <- go_results()  # Initialize results_added outside the loop
  
  for (comparison in input$go_comparison_selected) {
    for (ont in input$go_ont) {
      for (directions in input$go_direction){
      
        go_proteins <- MSstats_results() %>%
          filter(Label == comparison & Dif == switch(
            directions,
            Combined = c("Upregulated", "Downregulated"),
            Upregulated = "Upregulated",
            Downregulated = "Downregulated")) %>%
          .$Protein %>%
          as.character() %>%
          unique()
        
        results <- enrichGO(
          gene = go_proteins,
          OrgDb = switch(input$species,
            Human = org.Hs.eg.db,
            Rat = org.Rn.eg.db),
          keyType = "UNIPROT",
          pAdjustMethod = "BH",
          universe = unique(MSstats_input()$ProteinName),
          ont = ont,
          pvalueCutoff = input$go_pvalueCutoff,
          qvalueCutoff = input$go_qvalueCutoff) %>%
          as.data.frame() %>%
          mutate(Comparison = comparison, Direction = directions, Subontology = ont)
        
        results_added <- rbind(results_added, results)
      } # Direction for loop
    } # Sub-ontology for loop
  } # Comparison for loop
  
  go_results(results_added)  # Update go_results after all iterations
})

# Output
output$go_results_tab <- renderDataTable(go_results())

## STRING analysis ----
# Interactive UI
output$select_STRING_comparison <- renderUI({
  selectInput("STRING_comparison_selected", "Comparison to use",
              choices = sort(unique(MSstats_results()$Label)),
              multiple = FALSE)
})

# Initialise database, as human currently
string_db <- reactive({STRINGdb$new(
  version="11.5",
  species=switch(input$species,
    Human = 9606,
    Rat = 10116),
  score_threshold=input$STRING_score_threshold,
  network_type="full",
  input_directory="")
})

# Run STRING analysis
STRING_dataset <- eventReactive(input$go_STRING, {
  MSstats_results() %>%
    filter(Label == input$STRING_comparison_selected) %>%
    dplyr::select(Protein, pvalue, log2FC) %>%
    arrange(pvalue) %>%
    string_db()$map(
      "Protein",
      removeUnmappedRows = TRUE)
})

# STRING enrichment
STRING_enrichment <- eventReactive(input$go_STRING, {
  string_db()$get_enrichment(STRING_dataset()$STRING_id)
})

# output
output$STRING_tab <- renderDataTable({
  datatable(STRING_enrichment(),
            options = list(
              columnDefs = list(
                list(targets = c(6, 7), render = JS("function(data, type, row, meta) {
                  return type === 'display' && data.length > 20 ?
                    data.substr(0, 20) + '...' :
                    data;
                }"))
              )
            ))
})

# Visualisation ----
  # Reactive UI
  output$select_comparison <- renderUI({
    selectInput("comparison_selected", "Comparison to plot",
                choices = sort(unique(MSstats_results()$Label)),
                multiple = FALSE)
  })

output$select_heatmap_filter <- renderUI({
  selectInput("heatmap_filter", "Filter by differentially expressed proteins",
              choices = c("Include all", sort(unique(MSstats_results()$Label))),
              multiple = FALSE)
})

output$go_select_comparison <- renderUI({
  selectInput("go_comparison_selected", "Comparison",
              choices = sort(unique(go_results()$Comparison)),
              multiple = FALSE)
})

output$go_select_direction <- renderUI({
  selectInput("go_direction_selected", "Direction",
              choices = sort(unique(go_results()$Direction)),
              multiple = FALSE)
})

output$go_select_ont <- renderUI({
  selectInput("go_ont_selected", "Subontology",
              choices = sort(unique(go_results()$Subontology)),
              multiple = FALSE)
})
  
# Reactive variables
selected_theme <- reactive({
  switch(input$select_theme,
         "B&W" = theme_bw(),
         "Gray" = theme_gray(),
         "Classic" = theme_classic(),
         "Minimal" = theme_minimal(),
         "Void" = theme_void())
})

heatmap_dif_proteins <- reactive({
  MSstats_results() %>%
    filter(Label == input$heatmap_filter & !Dif == "Not significant") %>%
    .$Protein %>%
    as.character()
})

go_enrichment_plot_dataset <- reactive({
  go_results() %>%
    filter(Comparison == input$go_comparison_selected &
             Direction == input$go_direction_selected &
             Subontology == input$go_ont_selected) %>%
    mutate(GeneRatio =eval(parse(text = GeneRatio))) %>%
    arrange(GeneRatio) %>%
    .[1:input$go_top_n,]
})

# create a matrix of protein abundance for heatmap and PCA
prot_mat <- reactive({
  df <- merge(
    x = MSstats_processed()$ProteinLevelData,
    y = annot_col(),
    by.x = "originalRUN",
    by.y = "PcaRef",
    all.x = TRUE) %>%
    dplyr::select(Protein, Experiment, LogIntensities) %>%
    pivot_wider(names_from = Experiment, values_from = LogIntensities)
  df_mat <- as.matrix(df[,-1])
  row.names(df_mat) <- df$Protein
  df_mat
  })

## Heatmap ----
heatmap_input <- reactive({
  df <- prot_mat() %>%
    na.omit() %>% t() %>% scale() %>% t()
  if(input$heatmap_filter == "Include all"){
    df
  } else {
    df[row.names(df) %in% heatmap_dif_proteins(),]}
})

#create heatmap annotations for sample type
column_ha <- reactive(HeatmapAnnotation(Condition = annot_col()$Condition))

  # Set colours as a named vector - for use in volcano plot
  colours <- c("red", "blue", "black") 
  names(colours) <- c("Upregulated", "Downregulated", "Not significant")
  
  # Make heatmap
  heatmap_plot <- eventReactive(input$go_plot, {
    #create heatmap of gene expression, row scaling removed
      Heatmap(
        matrix = heatmap_input(),
        row_title = "Proteins",
        column_title = "Unfiltered proteome heatmap",
        show_row_dend = FALSE,
        show_column_dend = TRUE, 
        column_names_gp = gpar(fontsize = 8),
        bottom_annotation = column_ha(),
        show_row_names = FALSE,
        show_heatmap_legend = FALSE)
  })
  
  ## PCA plot ----
  
  # Do PCA analysis
  pca <- reactive(prcomp(t(na.omit(prot_mat())), center = TRUE, scale. = TRUE))
  pca_dat <- reactive(merge(pca()$x, annot_col(), by.x = "row.names", by.y = "Experiment"))
  
  pca_plot <- eventReactive(input$go_plot, {
    #plot eigen values - add in later if desired
 #   eigen_plot <- fviz_eig(pca) + ggtitle("Eigen value plot of Rosalind data")

    #plot first two PCs
    pca_plot <- ggplot(pca_dat(), aes(x = PC1, y = PC2, colour = Condition)) +
      geom_point() +
      ggtitle("PCA plot")
    pca_plot + selected_theme()
  })
  
  ## Volcano plot ----
  volcano_plot <-  eventReactive(input$go_plot, {
    MSstats_results() %>%
      filter(Label == input$comparison_selected) %>%
      ggplot(aes(x = log2FC, y = -log10(adj.pvalue), col = Dif)) +
      geom_vline(xintercept = c(-input$FC_threshold, input$FC_threshold), linetype = "dashed", colour = "black") +
      geom_hline(yintercept = -log10(input$pvalue_threshold), linetype = "dashed", colour = "red") +
      geom_point(alpha = 0.25, show.legend = FALSE) +
      scale_color_manual(values = colours) +
      ylab("-Log10(adjusted p-value)") +
      xlab("Log2 fold change") +
      ggtitle(input$comparison_selected) +
      selected_theme()
  })
  
  ## GO enrichment plot ----
   go_enrichment_plot <- eventReactive(input$go_plot, {
     go_enrichment_plot_dataset() %>%
       ggplot(aes(x = GeneRatio, y = 1:input$go_top_n, col = p.adjust, size = Count)) +
       geom_point() +
       scale_y_continuous(breaks = 1:input$go_top_n,
                          labels = go_enrichment_plot_dataset()$Description) +
       labs(y = NULL) +
       ggtitle(paste0("GO enrichment ",
                      input$go_comparison_selected, " ",
                      input$go_direction_selected, " ",
                      input$go_ont_selected)) +
       selected_theme()
   })
  
  ## STRING network plot ----
  STRING_network_plot <- eventReactive(input$go_plot, {
    string_db()$get_png(STRING_dataset()$STRING_id[1:input$STRING_n])
  })
  
  # Output
  output$plot <- renderPlot({
    plot_obj <- switch(input$plot_type,
                       Volcano = volcano_plot(),
                       PCA = pca_plot(),
                       Heatmap = heatmap_plot(),
                       `GO enrichment` = go_enrichment_plot(),
                       `STRING network` = STRING_network_plot())
    return(plot_obj)
  })

  #Testing ----
  
  # output$test_text <- renderText(switch(
  #   input$go_direction,
  #   Both = c("Upregulated", "Downregulated"),
  #   Upregulated = "Upregulated",
  #   Downregulated = "Downregulated"
  # ))
  output$test_table <- renderDataTable(STRING_dataset())
  
  # Downloads ----
  #Formatted data tables
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
  
  #Processed data
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
      vroom_write(MSstats_results(), file, delim = "\t")
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
    
    filename = function(){
      switch(input$plot_type,
        Volcano = paste0(input$comparison_selected, "_Volcano_", Sys.Date(), ".png"),
        PCA = paste0("PCA_",Sys.Date(), ".png"),
        Heatmap = paste0("Heatmap_", Sys.Date(), ".png"),
        `GO enrichment` = paste0("GO_enrichment_",
                                 input$go_comparison_selected, "_",
                                 input$go_direction_selected, "_",
                                 input$go_ont_selected, "_",
                                 Sys.Date(), ".png"),
        `STRING network` = paste0("STRING_network_", Sys.Date(), ".png"))
      },
    
    content = function(file){
      switch(input$plot_type,
        Volcano = {
          ggsave(file,
                 plot = volcano_plot(),
                 width = input$plot_width,
                 height = input$plot_height,
                 dpi = input$plot_dpi,
                 units = "mm")
        },
        
        PCA = {
          ggsave(file,
                 plot = pca_plot(),
                 width = input$plot_width,
                 height = input$plot_height,
                 dpi = input$plot_dpi,
                 units = "mm")
        },
        
        Heatmap = {
          png(file,
              height = input$plot_height,
              width = input$plot_width,
              res = input$plot_dpi,
              units= "mm")
          draw(heatmap_plot())
          dev.off()
        },
        
        `GO enrichment` = {
          ggsave(file,
                 plot = go_enrichment_plot(),
                 width = input$plot_width,
                 height = input$plot_height,
                 dpi = input$plot_dpi,
                 units = "mm")
        },
        
        `STRING network` = {
            png(file,
                height = input$plot_height,
                width = input$plot_width,
                res = input$plot_dpi,
                units= "mm")
            draw(STRING_network_plot())
            dev.off()
        }
      ) # Switch
    } # Content
  ) #Download handler
  
  }# Close the server

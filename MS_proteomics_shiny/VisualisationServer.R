# Visualisation
# Reactive UI ----
# Plot title
output$plot_title_input <- renderUI({
  textInput("plot_title",
    "Plot title",
    value = switch(input$plot_type,
      Volcano = {
        input$comparison_selected
      },
      PCA = {
        "PCA plot"
      },
      `GO enrichment` = {
        paste0(
          "GO enrichment ",
          input$go_comparison_selected, " ",
          input$go_direction_selected, " ",
          input$go_ont_selected
        )
      },
      Heatmap = {
        "Heatmap"
      }
    )
  )
})

# Plot x lab
output$plot_x_lab_input <- renderUI({
  textInput("plot_x_lab",
    "X label",
    value = switch(input$plot_type,
      Volcano = {
        "Log2 Fold Change"
      },
      PCA = {
        "PC1"
      },
      `GO enrichment` = {
        paste0(
          "GO enrichment ",
          input$go_comparison_selected, " ",
          input$go_direction_selected, " ",
          input$go_ont_selected
        )
      }
    )
  )
})

# Plot y lab
output$plot_y_lab_input <- renderUI({
  textInput("plot_y_lab",
    "Y label",
    value = switch(input$plot_type,
      Volcano = {
        "-Log10(adjusted p-value)"
      },
      PCA = {
        "PC2"
      },
      `GO enrichment` = {
        paste0(
          "GO enrichment ",
          input$go_comparison_selected, " ",
          input$go_direction_selected, " ",
          input$go_ont_selected
        )
      },
      Heatmap = {
        "Proteins"
      }
    )
  )
})

output$select_comparison <- renderUI({
  selectInput("comparison_selected", "Comparison to plot",
    choices = sort(unique(MSstats_results()$Label)),
    multiple = FALSE
  )
})

output$select_heatmap_filter <- renderUI({
  selectInput("heatmap_filter", "Filter by differentially expressed proteins",
    choices = c("Include all", sort(unique(MSstats_results()$Label))),
    multiple = FALSE
  )
})

output$go_select_comparison <- renderUI({
  selectInput("go_comparison_selected", "Comparison",
    choices = sort(unique(go_results()$Comparison)),
    multiple = FALSE
  )
})

output$go_select_direction <- renderUI({
  selectInput("go_direction_selected", "Direction",
    choices = sort(unique(go_results()$Direction)),
    multiple = FALSE
  )
})

output$go_select_ont <- renderUI({
  selectInput("go_ont_selected", "Subontology",
    choices = sort(unique(go_results()$Subontology)),
    multiple = FALSE
  )
})

# Reactive variables ----
selected_theme <- reactive({
  switch(input$select_theme,
    "B&W" = theme_bw(),
    "Gray" = theme_gray(),
    "Classic" = theme_classic(),
    "Minimal" = theme_minimal(),
    "Void" = theme_void()
  )
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
    mutate(GeneRatio = eval(parse(text = GeneRatio))) %>%
    arrange(GeneRatio) %>%
    .[1:input$go_top_n, ]
})

# create a matrix of protein abundance for heatmap and PCA
prot_mat <- reactive({
  df <- merge(
    x = switch(input$quant_method,
      LFQ = MSstats_processed()$ProteinLevelData,
      TMT = {
        MSstats_processed()$ProteinLevelData %>%
          mutate(originalRUN = paste(Run, Channel, sep = "_"))
      }
    ),
    y = annot_col(),
    by.x = "originalRUN",
    by.y = "PcaRef",
    all.x = TRUE
  ) %>%
    dplyr::select(Protein, Experiment, switch(input$quant_method,
      LFQ = "LogIntensities",
      TMT = "Abundance"
    )) %>%
    pivot_wider(names_from = Experiment, values_from = switch(input$quant_method,
      LFQ = "LogIntensities",
      TMT = "Abundance"
    ))
  df_mat <- as.matrix(df[, -1])
  row.names(df_mat) <- df$Protein
  df_mat
})

## Heatmap ----
heatmap_input <- reactive({
  df <- prot_mat() %>%
    na.omit() %>%
    t() %>%
    scale() %>%
    t()
  if (input$heatmap_filter == "Include all") {
    df
  } else {
    df[row.names(df) %in% heatmap_dif_proteins(), ]
  }
})

# create heatmap annotations for sample type
column_ha <- reactive(HeatmapAnnotation(Condition = if (input$remove_norm_channel) {
  annot_col() %>%
    filter(Condition != "Norm") %>%
    .$Condition
} else {
  annot_col()$Condition
}))

# Set colours as a named vector - for use in volcano plot
colours <- c("red", "blue", "black")
names(colours) <- c("Upregulated", "Downregulated", "Not significant")

# Make heatmap
heatmap_plot <- reactive({
  # create heatmap of gene expression, row scaling removed
  Heatmap(
    matrix = heatmap_input(),
    row_title = input$plot_y_lab,
    column_title = input$plot_title,
    show_row_dend = FALSE,
    show_column_dend = TRUE,
    column_names_gp = gpar(fontsize = 8),
    bottom_annotation = column_ha(),
    show_row_names = FALSE,
    show_heatmap_legend = FALSE
  )
})

## PCA plot ----

# Do PCA analysis
pca <- reactive(prcomp(t(na.omit(prot_mat())), center = TRUE, scale. = TRUE))
pca_dat <- reactive(merge(pca()$x, annot_col(), by.x = "row.names", by.y = "Experiment"))

pca_plot <- reactive({
  # plot eigen values - add in later if desired
  #   eigen_plot <- fviz_eig(pca) + ggtitle("Eigen value plot of Rosalind data")

  # plot first two PCs
  pca_plot <- ggplot(pca_dat(), aes(x = PC1, y = PC2, colour = Condition)) +
    geom_point() +
    ggtitle(input$plot_title) +
    ylab(input$plot_y_lab) +
    xlab(input$plot_x_lab)
  pca_plot + selected_theme()
})

## Volcano plot ----
volcano_plot <- reactive({
  MSstats_results() %>%
    filter(Label == input$comparison_selected) %>%
    ggplot(aes(x = log2FC, y = -log10(adj.pvalue), col = Dif)) +
    geom_vline(xintercept = c(-input$FC_threshold, input$FC_threshold), linetype = "dashed", colour = "black") +
    geom_hline(yintercept = -log10(input$pvalue_threshold), linetype = "dashed", colour = "red") +
    geom_point(alpha = 0.25, show.legend = FALSE) +
    scale_color_manual(values = colours) +
    ylab(input$plot_y_lab) +
    xlab(input$plot_x_lab) +
    ggtitle(input$plot_title) +
    selected_theme()
})

## GO enrichment plot ----
go_enrichment_plot <- reactive({
  go_enrichment_plot_dataset() %>%
    ggplot(aes(x = GeneRatio, y = 1:input$go_top_n, col = p.adjust, size = Count)) +
    geom_point() +
    scale_y_continuous(
      breaks = 1:input$go_top_n,
      labels = go_enrichment_plot_dataset()$Description
    ) +
    ylab(input$plot_y_lab) +
    xlab(input$plot_x_lab) +
    ggtitle(input$plot_title) +
    selected_theme()
})

## STRING network plot ----
STRING_network_plot <- reactive({
  string_db()$get_png(STRING_dataset()$STRING_id[1:input$STRING_n])
})

# Output
output$plot <- renderPlot({
  plot_obj <- switch(input$plot_type,
                     Volcano = volcano_plot(),
                     PCA = pca_plot(),
                     Heatmap = heatmap_plot(),
                     `GO enrichment` = go_enrichment_plot(),
                     `STRING network` = STRING_network_plot()
  )
  return(plot_obj)
})

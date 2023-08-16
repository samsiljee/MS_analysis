# Analysis
## GO enrichment analysis ----

# Reactive UI
output$select_go_comparison <- renderUI({
  selectInput("go_comparison_selected", "Comparison/s to use",
    choices = sort(unique(MSstats_results()$Label)),
    multiple = TRUE
  )
})

# Set up value for results
go_results <- reactiveVal(data.frame())

# Run GO enrichment analysis
observeEvent(input$go_go, {
  results_added <- go_results() # Initialize results_added outside the loop

  for (comparison in input$go_comparison_selected) {
    for (ont in input$go_ont) {
      for (directions in input$go_direction) {
        go_proteins <- MSstats_results() %>%
          filter(Label == comparison & Dif == switch(directions,
            Combined = c("Upregulated", "Downregulated"),
            Upregulated = "Upregulated",
            Downregulated = "Downregulated"
          )) %>%
          .$Protein %>%
          as.character() %>%
          unique()

        results <- enrichGO(
          gene = go_proteins,
          OrgDb = switch(input$species,
            Human = org.Hs.eg.db,
            Rat = org.Rn.eg.db
          ),
          keyType = "UNIPROT",
          pAdjustMethod = "BH",
          universe = unique(MSstats_input()$ProteinName),
          ont = ont,
          pvalueCutoff = input$go_pvalueCutoff,
          qvalueCutoff = input$go_qvalueCutoff
        ) %>%
          as.data.frame() %>%
          mutate(Comparison = comparison, Direction = directions, Subontology = ont)

        results_added <- rbind(results_added, results)
      } # Direction for loop
    } # Sub-ontology for loop
  } # Comparison for loop

  go_results(results_added) # Update go_results after all iterations
})

# Output
output$go_results_tab <- renderDataTable(go_results())

## STRING analysis ----
# Interactive UI
output$select_STRING_comparison <- renderUI({
  selectInput("STRING_comparison_selected", "Comparison to use",
    choices = sort(unique(MSstats_results()$Label)),
    multiple = FALSE
  )
})

# Create database
string_db <- reactive({
  # First make uncorrected database
  string_db <- STRINGdb$new(
    version = "11.5",
    species = switch(input$species,
      Human = 9606,
      Rat = 10116
    ),
    score_threshold = input$STRING_score_threshold,
    network_type = "full",
    input_directory = ""
  )

  # Apply background if selected
  if (input$set_STRING_background == "all_proteins") {
    # Map whole dataset and use to set background
    string_background <- string_db$map(as.data.frame(MSstats_input()),
      "ProteinName",
      removeUnmappedRows = TRUE
    ) %>%
      .$STRING_id %>%
      unique()
    string_db$set_background(string_background)

    # New database using background
    string_db <- STRINGdb$new(
      version = "11.5",
      species = 9606,
      score_threshold = 700,
      network_type = "full",
      input_directory = "",
      backgroundV = string_background
    )
    return(string_db)
  } else {
    return(string_db) # Whole genome
  }
}) # database reactive

# Run STRING analysis
STRING_dataset <- eventReactive(input$go_STRING, {
  MSstats_results() %>%
    filter(Label == input$STRING_comparison_selected) %>%
    dplyr::select(Protein, pvalue, log2FC) %>%
    arrange(pvalue) %>%
    string_db()$map(
      "Protein",
      removeUnmappedRows = TRUE
    )
})

# STRING enrichment
STRING_enrichment <- eventReactive(input$go_STRING, {
  string_db()$get_enrichment(STRING_dataset()$STRING_id)
})

# STRING annotations

# STRING clustering
STRING_clusters <- eventReactive(input$go_STRING, {
  if (input$cluster_STRING) {
    string_db()$get_clusters(
      STRING_dataset()$STRING_id,
      algorithm = input$STRING_cluster_method
    )
  }
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
    )
  )
})

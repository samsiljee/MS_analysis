AnalysisUI <- tabPanel(
  "Analysis",
  sidebarPanel(
    h4("Analysis type"),
    selectInput("analysis_type", "Analysis to run",
      choices = c("GO enrichment analysis", "STRING"),
      multiple = FALSE
    ),
    selectInput("species", "Species",
      choices = c("Human", "Rat"),
      multiple = FALSE
    ),
    # GO term analysis input
    conditionalPanel(
      condition = "input.analysis_type == 'GO enrichment analysis'",
      uiOutput("select_go_comparison"),
      selectInput("go_direction", "Direction",
        choices = c("Upregulated", "Downregulated", "Combined"),
        multiple = TRUE
      ),
      selectInput("go_ont", "Subontology",
        choices = c(
          "Biological Process" = "BP",
          "Molecular Function" = "MF",
          "Cellular Component" = "CC"
        ),
        multiple = TRUE
      ),
      numericInput("go_pvalueCutoff", "pvalue cutoff",
        min = 0,
        max = 1,
        value = 0.05,
        step = 0.01
      ),
      numericInput("go_qvalueCutoff", "qvalue cutoff",
        min = 0,
        max = 1,
        value = 0.2,
        step = 0.01
      ),
      actionButton("go_go", "Run GO enrichment analysis"),
      hr(style = "border-top: 2px solid #000000;"),
      downloadButton("go_results_tsv", "Save GO analysis as .tsv")
    ), # conditionalPanel; GO enrichment
    # STRING analysis input
    conditionalPanel(
      condition = "input.analysis_type == 'STRING'",
      uiOutput("select_STRING_comparison"),
      numericInput("STRING_score_threshold", "Score threshold",
        value = 400,
        min = 0,
        max = 1000,
        step = 10
      ),
      radioButtons("set_STRING_background", "Background",
        choiceNames = c("All input proteins", "Whole genome"),
        choiceValues = c("all_proteins", "whole_genome")
      ),
      checkboxInput("cluster_STRING", "Cluster string network",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.cluster_STRING == true",
        selectInput("STRING_cluster_method", "Algorithm",
          choices = c(
            "Fastgreedy" = "fastgreedy",
            "Walktrap" = "walktrap",
            "Edge betweenness" = "edge.betweenness"
          )
        )
      ),
      actionButton("go_STRING", "Run STRING analysis"),
      hr(style = "border-top: 2px solid #000000;"),
      downloadButton("STRING_dataset_tsv", "Save STRING dataset as .tsv"),
      downloadButton("STRING_enrichment_tsv", "Save STRING enrichment as .tsv")
    ) # Conditional panel STRING
  ),
  mainPanel(
    # GO analysis table
    conditionalPanel(
      condition = "input.analysis_type == 'GO enrichment analysis'",
      "Note that GO enrichment analysis can be quite slow",
      withSpinner(dataTableOutput("go_results_tab"))
    ),
    # STIGN analysis table
    conditionalPanel(
      condition = "input.analysis_type == 'STRING'",
      "Note that STRING analysis can take some time", br(),
      "STRING enrichment results will be displayed here, please see Visulaisation tab for network plots",
      withSpinner(dataTableOutput("STRING_tab"))
    )
  )
)

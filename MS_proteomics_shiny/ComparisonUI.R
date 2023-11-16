ComparisonUI <- tabPanel(
  "Comparison",
  sidebarPanel(
    h4("Comparisons"),
    # Common options
    radioButtons("contrast_method", "Contrast method",
      choiceNames = c("All combinations (pairwise)", "Custom comparisons"),
      choiceValues = c("pairwise", "custom")
    ),
    conditionalPanel(
      condition = "input.contrast_method == 'custom'",
      h5("Add new comparisons"),
      textInput("comparison_name", "Comparison name"),
      uiOutput("select_numerator"),
      uiOutput("select_denominator"),
      actionButton("add_comparison", "Add comparison")
    ),
    hr(style = "border-top: 2px solid #000000;"),
    numericInput("FC_threshold", "Log 2 fold-change threshold",
      value = 0.58,
      step = 0.1
    ),
    numericInput("pvalue_threshold", "Adjusted p-value threshold",
      min = 0,
      max = 1,
      value = 0.05,
      step = 0.01
    ),

    # TMT options
    conditionalPanel(
      condition = "input.quant_method == 'TMT'",
      checkboxInput(
        "moderated",
        "Use moderated t statistic",
        value = FALSE
      ),
      selectInput(
        "adj.method",
        "Adjustment method",
        choices = c(
          "BH",
          "holm",
          "hochberg",
          "hommel",
          "bonferroni",
          "BY",
          "fdr",
          "none"
        )
      )
    ),

    # More common options
    checkboxInput("save_fitted_models", "Save fitted models to the .rda output", value = FALSE),
    conditionalPanel(
      condition = "input.quant_method == 'LFQ'",
      checkboxInput("filter_results", "Filter out proteins with infinite fold-change", value = TRUE)
    ),
    actionButton("go_compare", "Compare!"),
    hr(style = "border-top: 2px solid #000000;"),
    downloadButton("results_tsv", "Save results as .tsv"),
    conditionalPanel(
      condition = "input.quant_method == 'LFQ'",
      downloadButton("model_qc_tsv", "Save model QC as .tsv")),
    downloadButton("comparisons_rda", "Save as .rda")
  ),
  mainPanel(
    "Comparisons to be made:",
    tableOutput("comparison_matrix_tab"),
    hr(style = "border-top: 2px solid #000000;"),
    textOutput("outliers"),
    conditionalPanel(
      condition = "input.quant_method == 'LFQ'",
      radioButtons("results_tab_view", "Which results would you like to view?",
                   choiceNames = c("Comparison result", "Model QC"),
                   choiceValues = c("ComparisonResult", "ModelQC"),
                   inline = TRUE
      )),
    withSpinner(dataTableOutput("results_tab"))
  )
)

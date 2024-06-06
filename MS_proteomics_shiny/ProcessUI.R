ProcessUI <- tabPanel(
  "Process",
  sidebarPanel(
    # LFQ options ----
    conditionalPanel(
      condition = "input.quant_method == 'LFQ'",
      # Standard options
      radioButtons("normalization",
                   "Normalisation method used between runs",
                   choiceNames = c("Equalize medians", "Quantile", "Global standards", "None"),
                   choiceValues = c("equalizeMedians", "quantile", "globalStandards", FALSE)
      ),
      radioButtons("summaryMethod",
                   "Method used to summarise features",
                   choiceNames = c("Tukey's median polish", "Linear mixed model"),
                   choiceValues = c("TMP", "linear")
      ),
      conditionalPanel(
        condition = "input.summaryMethod == 'TMP'",
        checkboxInput(
          "MBimpute",
          "Impute censored values (Accelated failure model)",
          value = TRUE
        )
      ),
      # Advanced options
      conditionalPanel(
        condition = "input.AdvancedProcessOptionsLFQ == true",
      radioButtons("logTrans",
        "Base of log transformation",
        choices = c(2, 10),
        selected = 2
      ),
      conditionalPanel(
        condition = "input.normalization == 'globalStandards'",
        fileInput("nameStandards", "Standards FASTA file",
          placeholder = "Upload standards.fasta"
        )
      ),
      radioButtons("featureSubset",
        "Feature subset to use",
        choiceNames = c("All", "Top 3", "Top N", "High quality"),
        choiceValues = c("all", "top3", "topN", "highQuality")
      ),
      conditionalPanel(
        condition = "input.featureSubset == 'topN'",
        numericInput("n_top_feature",
          "Number of top features to use",
          value = 3,
          step = 1
        )
      ),
      conditionalPanel(
        condition = "input.featureSubset == 'highQuality'",
        checkboxInput("remove_uninformative_feature_outlier",
          "Remove noisy features and outliers before run-level summarisation",
          value = FALSE
        ),
        numericInput("min_feature_count",
          "Minimum features required to be considered in feature selection algorithm",
          value = 2,
          step = 1
        )
      ),
      conditionalPanel(
        condition = "input.summaryMethod == 'linear'",
        checkboxInput("equalFeatureVar",
          "Account for heterogeneous variation among intensities from different features",
          value = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.summaryMethod == 'TMP'",
        checkboxInput(
          "remove50missing",
          "Remove runs with >50% missing values",
          value = FALSE
        )
      ),
      uiOutput("censoredInt_input"),
      radioButtons(
        "fix_missing",
        "Fix missing values",
        choiceNames = c("No action", "0 -> NA", "NA -> 0"),
        choiceValues = c("NULL", "zero_to_na", "na_to_zero")
      ),
      numericInput(
        "maxQuantileforCensoredLFQ",
        "Maximum quantile for deciding censored missing values",
        value = 0.999,
        step = 0.001
      )),
      # Show advanced options
      checkboxInput(
        "AdvancedProcessOptionsLFQ",
        "Show advanced options",
        value = FALSE
      )
    ),

    # TMT options ----
    conditionalPanel(
      condition = "input.quant_method == 'TMT'",
      # Standard options
      radioButtons(
        "TMTProtSumMethod",
        "Protein-level summarisation method",
        choiceNames = c(
          "MSstats",
          "Median polish",
          "Median",
          "Log sum"
        ),
        choiceValues = c(
          "msstats",
          "MedianPolish",
          "Median",
          "LogSum"
        )
      ),
      conditionalPanel(
        condition = "input.TMTProtSumMethod == 'msstats'",
        checkboxInput(
          "MBimpute",
          "Impute censored values by Accelated failure model",
          value = TRUE
        )
      ),
      # Advanced options
      conditionalPanel(
        condition = "input.AdvancedProcessOptionsTMT == true",
        checkboxInput(
          "global_norm",
          "Global median normalisation (peptide-level)",
          value = TRUE
        ),
      checkboxInput(
        "reference_norm",
        "Reference channel normalisation between runs (protein-level)",
        value = TRUE
      ),
      checkboxInput(
        "remove_norm_channel",
        "Remove reference channels",
        value = TRUE
      ),
      numericInput(
        "maxQuantileforCensoredTMT",
        "Maximum quantile for deciding censored missing values",
        value = 0.999,
        step = 0.001
      )),
      # Show advanced options
      checkboxInput(
        "AdvancedProcessOptionsTMT",
        "Show advanced options",
        value = FALSE
      )
    ),

    actionButton("go_process", "Process!"),
    hr(style = "border-top: 2px solid #000000;"),
    downloadButton("processed_protein_tsv", "Save protein data as .tsv"),
    downloadButton("processed_feature_tsv", "Save feature data as .tsv"),
    downloadButton("processed_rda", "Save as .rda")
  ),
  mainPanel(
    "Summarise and normalise data",
    radioButtons("processed_tab_view",
      "Which processed data would you like to view?",
      choiceNames = c("Protein level data", "Feature level data"),
      choiceValues = c("ProteinLevelData", "FeatureLevelData"),
      inline = TRUE
    ),
    withSpinner(dataTableOutput("MSstats_processed_tab"))
  )
)

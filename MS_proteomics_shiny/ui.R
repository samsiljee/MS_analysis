# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages ----
library(shinycssloaders)

ui <- navbarPage(
  title = "MS analysis",

# Instructions ----

  tabPanel("Instructions",
    
    "Welcome to my proteomics analysis pipeline.", br(),
    "Please move sequentially through the tabs to complete the analysis.", br()),
  
# Input ----

  tabPanel("Input",
    
    "Please upload your dataset as exported from Proteome discoverer and the corresponding annotations table here:",
    
    sidebarPanel(
      
      fileInput("annotations", "Annotations file",
        buttonLabel = "Browse",
        placeholder = "Upload annotations file here"),
      
      radioButtons("annotations_sep", "Separator",
        choices = c(
          Tab = "\t",
          Comma = ",",
          Semicolon = ";"),
        selected = ","),
      
      tags$hr(style = "border-top: 2px solid #000000;"),
      
      fileInput("PSMs", "PSMs file",
        buttonLabel = "Browse",
        placeholder = "Upload PSMs file here"),
      
      radioButtons("PSMs_sep", "Separator",
        choices = c(
          Tab = "\t",
          Comma = ",",
          Semicolon = ";"),
                   selected = "\t")),
    
    mainPanel(
              
      h3("Annotations"),
      withSpinner(dataTableOutput("annotation_tab")),
      
      tags$hr(style = "border-top: 2px solid #000000;"),
      
      h3("PSM data"),
      withSpinner(dataTableOutput("PSMs_tab")))),
  
# Format ----
tabPanel("Format", "Format and pre-filter your data to work in MSstats, options can be changed on the left.",
  sidebarPanel(h4("MSstats formating options"),
    checkboxInput("useNumProteinsColumn",
                  "Remove peptides with more than one in \"number of proteins\" column of PD output",
                  value = FALSE),
    checkboxInput("useUniquePeptide",
                  "Remove peptides assigned to more than one protein",
                  value = TRUE),
    checkboxInput("removeFewMeasurements",
                  "Remove features with one or two measurements across runs",
                  value = TRUE),
    checkboxInput("removeOxidationMpeptides",
                  "Remove peptides with methionine oxidation",
                  value = FALSE),
    checkboxInput("removeProtein_with1Peptide",
                  "Remove proteins with only one peptide and charge",
                  value = TRUE),
    radioButtons("summaryforMultipleRows",
                 "Summary method for multiple rows",
                 choiceNames = c("Max", "Sum"),
                 choiceValues = c("max", "sum")),
    radioButtons("which.quantification",
                 "Column to be used for quantification",
                 choiceNames = c("Precursor area", "Intensity", "Area"),
                 choiceValues = c("Precursor.Area", "Intensity", "Area")),
    radioButtons("which.proteinid",
                 "Column to be used for protein names",
                 choiceNames = c("Protein accessions", "Master protein accessions"),
                 choiceValues = c("Protein.Accessions", "Master.Protein.Accessions")),
    radioButtons("which.sequence",
                 "Column to be used for peptide sequences",
                 choiceNames = c("Sequence", "Annotated sequence"),
                 choiceValues = c("Sequence", "Annotated.Sequence")),
    actionButton("go_format", "Format!"),
    tags$hr(style = "border-top: 2px solid #000000;"),
    downloadButton("formatted_csv",
                   "Save as .csv"),
    downloadButton("formatted_rda",
                   "Save as .rda")),
  
  mainPanel("Preview of formatted input data",
            "I still need to add functionality to download as .rda, .csv, or .tsv format, and a way to save the log file.",
    withSpinner(dataTableOutput("MSstats_input_tab")))),

# Process ----

tabPanel("Process",
  sidebarPanel(h4("MSstats processing options"),
    radioButtons("logTrans",
      "Base of log transformation",
      choices = c(2, 10),
      selected = 2),
    radioButtons("normalization",
      "Normalisation method used to remove bias between runs",
      choiceNames = c("Equalize medians", "Quantile", "Global standards", "None"),
      choiceValues = c("equalizeMedians", "quantile", "globalStandards", FALSE)),
    conditionalPanel(
      condition = "input.normalization == 'globalStandards'",
    textInput("nameStandards",
      "Named vector for standard peptides (not yet working)")),
    radioButtons("featureSubset",
      "feature subset to use",
      choiceNames = c("All", "Top 3", "Top N", "High quality"),
      choiceValues = c("all", "top3", "topN", "highQuality")),
    conditionalPanel(
      condition = "input.featureSubset == 'topN'",
    numericInput("n_top_feature",
      "Number of top features to use",
      value = 3)),
    conditionalPanel(
      condition = "input.featureSubset == 'highQuality'",
    checkboxInput("remove_uninformative_feature_outlier",
      "Remove noisy features and outliers before run-level summarisation",
      value = FALSE)),
    conditionalPanel(
      condition = "input.featureSubset == 'highQuality'",
    numericInput("min_feature_count",
      "Minimum features required to be considered in feature selection algorythm",
      value = 2)),
    radioButtons("summaryMethod",
      "Method used to summarise features",
      choiceNames = c("Tukey's median polish", "Linear mixed model"),
      choiceValues = c("TMP", "linear")),
    conditionalPanel(
      condition = "input.summaryMethod == 'linear'",
    checkboxInput("equalFeatureVar",
      "Account for heterogeneous variation among intensities from different features",
      value = TRUE)),
    conditionalPanel(
      condition = "input.summaryMethod == 'TMP'",
    checkboxInput("MBimpute",
      "Impute censored values by Accelated failure model",
      value = TRUE)),
    conditionalPanel(
      condition = "input.summaryMethod == 'TMP'",
    checkboxInput("remove50missing",
      "Remove runs with >50% missing values",
      value = FALSE)),
    radioButtons("censoredInt",
      "Missing values are censored or at random",
      choiceNames = c("NA", "0", "Null"),
      choiceValues = c("NA", "0", "NULL")),
    radioButtons("fix_missing",
      "fix missing values (uncertain how this works)",
      choiceNames = c("No action", "0 -> NA", "NA -> 0"),
      choiceValues = c("NULL", "zero_to_na", "na_to_zero")),
    numericInput("maxQuantileforCensored",
      "Maximum quantile for deciding censored missing values",
      value = 0.999),
    actionButton("go_process", "Process!"),
    tags$hr(style = "border-top: 2px solid #000000;"),
    downloadButton("processed_protein_csv",
                   "Save protein data as .csv"),
    downloadButton("processed_feature_csv",
                   "Save feature data as .csv"),
    downloadButton("processed_rda",
                   "Save as .rda")),
  
  mainPanel("This section will be where MSstats processing happens. There will be drop down options here too for the settings.",
            "Note that currently I've not got the \"Global standards\" method working as it takes a named vector as input",
            radioButtons("processed_tab_view",
              "Which processed data would you like to view?",
              choiceNames = c("Protein level data", "Feature level data"),
              choiceValues = c("ProteinLevelData", "FeatureLevelData"),
              inline = TRUE),
            withSpinner(dataTableOutput("MSstats_processed_tab")))),

# Comparison ----

  tabPanel("Comparison",
    "This section will be where MSstats is computed. There will be drop down options here too for the settings.",
    
    sidebarPanel(h4("Add new comparisons"),
      textInput("comparison_name", "Comparison name"),
      uiOutput("select_numerator"),
      uiOutput("select_denominator"),
      actionButton("add_comparison", "Add comparison"),
      tags$hr(style = "border-top: 2px solid #000000;"),
      checkboxInput("save_fitted_models", "Save fitted models to .rda output", value = TRUE),
      actionButton("go_compare", "Compare!"),
      tags$hr(style = "border-top: 2px solid #000000;"),
      downloadButton("results_csv",
                     "Save results as .csv"),
      downloadButton("model_qc_csv",
                     "Save model QC as .csv"),
      downloadButton("comparisons_rda",
                     "Save as .rda")),
    
    mainPanel("Comparison/s to be made:",
      tableOutput("comparison_matrix_tab"),
      tags$hr(style = "border-top: 2px solid #000000;"),
      radioButtons("results_tab_view",
                   "Which results would you like to view?",
                   choiceNames = c("Comparison result", "Model QC"),
                   choiceValues = c("ComparisonResult", "ModelQC"),
                   inline = TRUE),
      withSpinner(dataTableOutput("results_tab")))),

# Visualisation ----

  tabPanel("Visualisation",
    "This section will be for making the graphs. Again a sidebar panel to select the types of graphs.",
    
   )
# Close UI
)

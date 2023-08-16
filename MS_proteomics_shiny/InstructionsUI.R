InstructionsUI <- tabPanel(
  "Instructions",
  radioButtons("platform",
    "Search platform",
    choiceNames = c("Proteome Discoverer", "MaxQuant"),
    choiceValues = c("PD", "MQ")
  ),
  radioButtons("quant_method",
    "Quantitation method",
    choices = c("LFQ", "TMT")
  ),
  "Welcome to my proteomics analysis pipeline.",
  "Please move sequentially through the tabs to complete the pipeline. Analysis however can be skipped if desired, however not all plots will be available in the visualisation tab.", br(),
  hr(style = "border-top: 2px solid #000000;"),
  conditionalPanel(
    condition = "input.quant_method == 'LFQ'",
    "Please prepare an annotation file as a .tsv or .csv with the following columns:", br(),
    "\"Run\" exact filenames of the raw data files, with .raw extension for PD, and without for MQ.", br(),
    "\"Condition\" describing the experimental group.", br(),
    "\"BioReplicate\" describing the biological replicate.", br(),
    "\"Fraction\" set all to 1 if no fractionation was done.", br()
  ),
  conditionalPanel(
    condition = "input.quant_method == 'TMT'",
    "Please prepare two annotation files as .tsv or .csv with the following columns:", br(),
    h4("Run annotations"),
    "\"Run\" exact filenames of the raw data files, with .raw extension for PD, and without for MQ.", br(),
    "\"Mixture\" batch or mixture of TMT channels. Set all to 1 if only one mixture was run.", br(),
    "\"Fraction\" set all to 1 if no fractionation was done.", br(),
    "\"TechRepMixture\" technical replicates of distinct mixtures.", br(),
    h4("Channel annotations"),
    "\"Mixture\" batch or mixture of TMT channels. Set all to 1 if only one mixture was run.", br(),
    "\"Channel\" TMT channel within each miture, must match the PSMs file.", br(),
    "\"Condition\" describing the experimental group. Normalisation channel/s must be set to \"Norm\".", br(),
    "\"BioReplicate\" describing the biological replicate. Normalisation channel/s must be set to \"Norm\".", br()
  ),
  "Technical replicates are automatically detected. Save it as a csv or tsv file.", br(),
  hr(style = "border-top: 2px solid #000000;"),
  "Debugging output here, please ignore", br(),
  verbatimTextOutput("test_text"),
  tableOutput("test_table")
)

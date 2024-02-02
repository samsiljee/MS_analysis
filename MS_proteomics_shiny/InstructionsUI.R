InstructionsUI <- tabPanel(
  "Instructions",
  "Welcome to my proteomics analysis pipeline.", br(),
  "Please move sequentially through the tabs to complete the pipeline. Analysis however can be skipped if desired, however not all plots will be available in the visualisation tab.", br(),
  selectInput("platform",
    "Search platform",
    choices = c(
      "Proteome Discoverer" = "PD",
      "MaxQuant" = "MQ",
      "DIA-NN" = "DIANN"
    ),
    multiple = FALSE
  ),
  uiOutput("quant_method_input"),
  selectInput("species", "Species",
              choices = c(
                "Human",
                "Rat",
                "Other (analysis tab will not be functional)" = "Other"          
                ),
              multiple = FALSE
  ),
  conditionalPanel(
    condition = "input.species == 'Other'",
    "Please email samsiljee@gmail.com, or ask on Github for your species to be added"
  ),
 
  hr(style = "border-top: 2px solid #000000;"),
  conditionalPanel(
    condition = "input.quant_method == 'LFQ'",
    "Please use the annotation wizard on the \"Input\" tab to create an annotation file, or prepare a .tsv or .csv file with the following columns:", br(),
    "\"Run\" exact filenames of the raw data files, with .raw extension for PD, and without for MQ or DIA-NN.", br(),
    "\"Condition\" describing the experimental group.", br(),
    "\"BioReplicate\" describing the biological replicate.", br(),
    "\"Fraction\" set all to 1 if no fractionation was done.", br()
  ),
  conditionalPanel(
    condition = "input.quant_method == 'TMT'",
    "Please use the annotation wizard on the \"Input\" tab to create an annotation file, or prepare two annotation files as .tsv or .csv with the following columns:", br(),
    h4("Run annotations"),
    "\"Run\" exact filenames of the raw data files, with .raw extension for PD, and without for MQ.", br(),
    "\"Mixture\" batch or mixture of TMT channels. Set all to 1 if only one mixture was run.", br(),
    "\"Fraction\" set all to 1 if no fractionation was done.", br(),
    "\"TechRepMixture\" technical replicates of distinct mixtures. Set all to 1 if only one replicate was run.", br(),
    h4("Channel annotations"),
    "\"Mixture\" batch or mixture of TMT channels. Set all to 1 if only one mixture was run.", br(),
    "\"Channel\" TMT channel within each miture, must match the PSMs file.", br(),
    "\"Condition\" describing the experimental group. Normalisation channel/s must be set to \"Norm\".", br(),
    "\"BioReplicate\" describing the biological replicate. Normalisation channel/s must be set to \"Norm\".", br()
  ),
  "Technical replicates are automatically detected.", br(),
  hr(style = "border-top: 2px solid #000000;"),
  "Debugging output here, please ignore", br(),
  verbatimTextOutput("test_text")
)

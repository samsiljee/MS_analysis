FormatUI <- tabPanel(
  "Format",
  sidebarPanel(
    # Universal options
    checkboxInput(
      "useUniquePeptide",
      "Remove peptides assigned to more than one protein",
      value = TRUE
    ),
    uiOutput("select_summary_method"),
    checkboxInput(
      "removeFewMeasurements",
      "Remove features with one or two measurements across runs",
      value = TRUE
    ),

    # PD conditional options
    conditionalPanel(
      condition = "input.platform == 'PD'",
      checkboxInput(
        "useNumProteinsColumn",
        "Remove peptides with more than one in \"number of proteins\" column",
        value = FALSE
      ),
      radioButtons(
        "which.proteinid",
        "Protein ID",
        choiceNames = c("Master protein accessions", "Protein accessions"),
        choiceValues = c("Master.Protein.Accessions", "Protein.Accessions")
      )
    ),

    # LFQ conditional options
    conditionalPanel(
      condition = "input.quant_method == 'LFQ'",
      checkboxInput(
        "removeOxidationMpeptides",
        "Remove peptides with methionine oxidation",
        value = FALSE
      ),
      checkboxInput(
        "removeProtein_with1Peptide",
        "Remove proteins with only one peptide and charge",
        value = TRUE
      ),

      # LFQ and PD conditional options
      conditionalPanel(
        condition = "input.platform == 'PD'",
        radioButtons(
          "which.quantification",
          "Column to be used for quantification",
          choiceNames = c("Precursor area", "Intensity", "Area"),
          choiceValues = c("Precursor.Area", "Intensity", "Area")
        ),
        radioButtons(
          "which.sequence",
          "Column to be used for peptide sequences",
          choiceNames = c("Sequence", "Annotated sequence"),
          choiceValues = c("Sequence", "Annotated.Sequence")
        )
      ),

      # LFQ and MQ conditional options
      conditionalPanel(
        condition = "input.platform == 'MQ'",
        radioButtons(
          "MQLFQproteinID",
          "Protein ID",
          choiceNames = c("Proteins", "Leading razor protein"),
          choiceValues = c("Proteins", "Leading.razor.protein")
        ),
        checkboxInput(
          "removeMpeptides",
          "Remove peptides including \'M\' sequence",
          value = FALSE
        )
      )
    ),

    # TMT conditional options
    conditionalPanel(
      condition = "input.quant_method == 'TMT'",
      checkboxInput(
        "rmProtein_with1Feature",
        "Remove proteins with only 1 peptide and charge",
        value = FALSE
      ),

      # TMT and MQ conditional options
      conditionalPanel(
        condition = "input.platform == 'MQ'",
        radioButtons(
          "MQTMTproteinID",
          "Protein ID",
          choiceNames = c(
            "Proteins",
            "Leading proteins",
            "Leading razor protein",
            "Gene names"
          ),
          choiceValues = c(
            "Proteins",
            "Leading.proteins",
            "Leading.razor.protein",
            "Gene.names"
          )
        ),
        checkboxInput(
          "rmProt_Only.identified.by.site",
          "Remove proteins only identified by a modification site",
          value = FALSE
        )
      )
    ),
    actionButton(
      "go_format", "Format!"
    ),
    hr(style = "border-top: 2px solid #000000;"),
    downloadButton("formatted_tsv", "Save as .tsv"),
    downloadButton("formatted_rda", "Save as .rda"),
    downloadButton("formatted_log", "Save log")
  ),
  mainPanel(
    "Preview of formatted input data",
    withSpinner(dataTableOutput("MSstats_input_tab"))
  )
)

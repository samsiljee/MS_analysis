FormatUI <- tabPanel(
  "Format",
  sidebarPanel(
    # LFQ/PD options ----
    conditionalPanel(
      condition = "input.quant_method == 'LFQ' & input.platform == 'PD'",
      checkboxInput(
        "LFQPDuseNumProteinsColumn",
        "Remove peptides with more than one in \"number of proteins\" column",
        value = TRUE
      ),
      checkboxInput(
        "LFQPDuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      radioButtons(
        "LFQPDsummaryforMultipleRows",
        "Summary method for multiple rows",
        choiceNames = c("Max", "Sum"),
        choiceValues = c("max", "sum"),
        selected = "max"
      ),
      checkboxInput(
        "LFQPDremoveFewMeasurements",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      checkboxInput(
        "LFQPDremoveOxidationMpeptides",
        "Remove peptides with methionine oxidation",
        value = FALSE
      ),
      checkboxInput(
        "LFQPDremoveProtein_with1Peptide",
        "Remove proteins with only one peptide and charge",
        value = FALSE
      ),
      radioButtons(
        "LFQPDwhich.quantification",
        "Column to be used for quantification",
        choiceNames = c("Precursor area", "Intensity", "Area"),
        choiceValues = c("Precursor.Area", "Intensity", "Area")
      ),
      radioButtons(
        "LFQPDwhich.proteinid",
        "Protein ID",
        choiceNames = c("Protein accessions", "Master protein accessions"),
        choiceValues = c("Protein.Accessions", "Master.Protein.Accessions")
      ),
      radioButtons(
        "LFQPDwhich.sequence",
        "Column to be used for peptide sequences",
        choiceNames = c("Sequence", "Annotated sequence"),
        choiceValues = c("Sequence", "Annotated.Sequence")
      )
    ),
    
    # LFQ/MQ options ----
    conditionalPanel(
      condition = "input.quant_method == 'LFQ' & input.platform == 'MQ'",
      checkboxInput(
        "LFQPDuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      checkboxInput(
        "LFQPDremoveFewMeasurements",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      
    ),
    
    # LFQ/DIA-NN ----
    conditionalPanel(
      condition = "input.quant_method == 'LFQ' & input.platform == 'DIANN'",
      checkboxInput(
        "LFQPDuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      checkboxInput(
        "LFQPDremoveFewMeasurements",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      
    ),
    
    # TMT/PD options ----
    conditionalPanel(
      condition = "input.quant_method == 'TMT' & input.platform == 'PD'",
      checkboxInput(
        "LFQPDuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      checkboxInput(
        "LFQPDremoveFewMeasurements",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      
    ),
    
    # TMT/MQ options ----
    conditionalPanel(
      condition = "input.quant_method == 'TMT' & input.platform == 'MQ'",
      checkboxInput(
        "LFQPDuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      checkboxInput(
        "LFQPDremoveFewMeasurements",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      
    ),
    
    # Old options ----
    
    # Universal options ----
    checkboxInput(
      "useUniquePeptide",
      "Remove peptides assigned to more than one protein",
      value = TRUE
    ),
    checkboxInput(
      "removeFewMeasurements",
      "Remove features with one or two measurements across runs",
      value = TRUE
    ),

    # PD conditional options ----
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
    
    # DIA-NN conditional options ----
    conditionalPanel(
      condition = "input.platform == 'DIANN'",
      numericInput(
        "global_qvalue_cutoff",
        "Global qvalue cutoff",
        value = 0.01,
        step = 0.01),
      numericInput(
        "qvalue_cutoff",
        "qvalue cutoff",
        value = 0.01,
        step = 0.01),
      numericInput(
        "pg_qvalue_cutoff",
        "Protein groups qvalue cutoff",
        value = 0.01,
        step = 0.01),
      checkboxInput(
        "removeProtein_with1Feature",
        "remove proteins with a single feature",
        value = TRUE
      ),
      checkboxInput(
        "MBR",
        "Was match between runs used in DIA-NN?",
        value = TRUE
      )
    ),
    
    # PD or MQ conditional options ----
    conditionalPanel(
      condition = "input.platform !== 'DIANN'",
      uiOutput("select_summary_method")
    ),

    # LFQ conditional options ----
    conditionalPanel(
      condition = "input.quant_method == 'LFQ'",
      checkboxInput(
        "removeOxidationMpeptides",
        "Remove peptides with methionine oxidation",
        value = FALSE
      ),
      
      # LFQ and PD or MQ conditional options ----
      conditionalPanel(
        condition = "input.platform == 'PD' || input.platform == 'MQ'",
        checkboxInput(
          "removeProtein_with1Peptide",
          "Remove proteins with only one peptide and charge",
          value = TRUE
        ),
      ),

      # LFQ and PD conditional options ----
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

      # LFQ and MQ conditional options ----
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

    # TMT conditional options ----
    conditionalPanel(
      condition = "input.quant_method == 'TMT'",
      checkboxInput(
        "rmProtein_with1Feature",
        "Remove proteins with only 1 peptide and charge",
        value = FALSE
      ),

      # TMT and MQ conditional options ----
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
    downloadButton("formatted_rda", "Save as .rda")
  ),
  mainPanel(
    "Preview of formatted input data",
    withSpinner(dataTableOutput("MSstats_input_tab"))
  )
)

FormatUI <- tabPanel(
  "Format",
  sidebarPanel(
    # LFQ/PD options ----
    conditionalPanel(
      condition = "input.quant_method == 'LFQ' & input.platform == 'PD'",
      # Standard options
      radioButtons(
        "LFQPDwhich.quantification",
        "Column to be used for quantification",
        choiceNames = c("Precursor area", "Intensity", "Area"),
        choiceValues = c("Precursor.Area", "Intensity", "Area")
      ),
      checkboxInput(
        "LFQPDremoveFewMeasurements",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      checkboxInput(
        "LFQPDuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      # Advanced options
      conditionalPanel(
        condition = "input.AdvancedOptionsLFQPD == true",
        checkboxInput(
          "LFQPDuseNumProteinsColumn",
          "Remove peptides with more than one in \"number of proteins\" column",
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
          "LFQPDwhich.proteinid",
          "Protein ID",
          choiceNames = c("Protein accessions", "Master protein accessions"),
          choiceValues = c("Protein.Accessions", "Master.Protein.Accessions")
        ),
        radioButtons(
          "LFQPDwhich.sequence",
          "Column to be used for peptide sequences",
          choiceNames = c("Annotated sequence", "Sequence"),
          choiceValues = c("Annotated.Sequence", "Sequence")
        )
      ),
      # Show advanced options
      checkboxInput(
        "AdvancedOptionsLFQPD",
        "Show advanced options",
        value = FALSE
      )
    ),

    # LFQ/MQ options ----
    conditionalPanel(
      condition = "input.quant_method == 'LFQ' & input.platform == 'MQ'",
      # Standard options
      checkboxInput(
        "LFQMQremoveFewMeasurements",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      checkboxInput(
        "LFQMQuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      # Advanced options
      conditionalPanel(
        condition = "input.AdvancedOptionsLFQMQ == true",
        radioButtons(
          "LFQMQproteinID",
          "Protein ID",
          choiceNames = c("Proteins", "Leading razor protein"),
          choiceValues = c("Proteins", "Leading.razor.protein")
        ),
        radioButtons(
          "LFQMQsummaryforMultipleRows",
          "Summary method for multiple rows",
          choiceNames = c("Sum", "Max"),
          choiceValues = c("sum", "max"),
          selected = "max"
        ),
        checkboxInput(
          "LFQMQremoveMpeptides",
          "Remove peptides including \'M\' sequence",
          value = FALSE
        ),
        checkboxInput(
          "LFQMQremoveOxidationMpeptides",
          "Remove peptides with methionine oxidation",
          value = FALSE
        ),
        checkboxInput(
          "LFQMQremoveProtein_with1Peptide",
          "Remove proteins with only one peptide and charge",
          value = FALSE
        )
      ),
      # Show advanced options
      checkboxInput(
        "AdvancedOptionsLFQMQ",
        "Show advanced options",
        value = FALSE
      )
    ),

    # LFQ/DIA-NN ----
    conditionalPanel(
      condition = "input.platform == 'DIANN'",
      # Standard options
      checkboxInput(
        "LFQDIANNuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      checkboxInput(
        "LFQDIANNremoveFewMeasurements",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      checkboxInput(
        "LFQDIANNMBR",
        "Was match between runs used in DIA-NN?",
        value = TRUE
      ),
      # Advanced options
      conditionalPanel(
        condition = "input.AdvancedOptionsLFQDIANN == true",
        numericInput(
          "LFQDIANNglobal_qvalue_cutoff",
          "Global qvalue cutoff",
          value = 0.01,
          step = 0.01
        ),
        numericInput(
          "LFQDIANNqvalue_cutoff",
          "qvalue cutoff",
          value = 0.01,
          step = 0.01
        ),
        numericInput(
          "LFQDIANNpg_qvalue_cutoff",
          "Protein groups qvalue cutoff",
          value = 0.01,
          step = 0.01
        ),
        checkboxInput(
          "LFQDIANNremoveOxidationMpeptides",
          "Remove peptides with methionine oxidation",
          value = TRUE
        ),
        checkboxInput(
          "LFQDIANNremoveProtein_with1Feature",
          "remove proteins with a single feature",
          value = TRUE
        )
      ),
      # Show advanced options
      checkboxInput(
        "AdvancedOptionsLFQDIANN",
        "Show advanced options",
        value = FALSE
      )
    ),

    # LFQ/Spectronaut ----
    conditionalPanel(
      condition = "input.platform == 'SN'",
      # Standard options
      checkboxInput(
        "LFQSNuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      checkboxInput(
        "LFQSNremoveFewMeasurements",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      checkboxInput(
        "LFQSNMBR",
        "Was match between runs used in DIA-NN?",
        value = TRUE
      ),
      # Advanced options
      conditionalPanel(
        condition = "input.AdvancedOptionsLFQSN == true",
        numericInput(
          "LFQSNglobal_qvalue_cutoff",
          "Global qvalue cutoff",
          value = 0.01,
          step = 0.01
        ),
        numericInput(
          "LFQSNqvalue_cutoff",
          "qvalue cutoff",
          value = 0.01,
          step = 0.01
        ),
        numericInput(
          "LFQSNpg_qvalue_cutoff",
          "Protein groups qvalue cutoff",
          value = 0.01,
          step = 0.01
        ),
        checkboxInput(
          "LFQSNremoveOxidationMpeptides",
          "Remove peptides with methionine oxidation",
          value = TRUE
        ),
        checkboxInput(
          "LFQSNremoveProtein_with1Feature",
          "remove proteins with a single feature",
          value = TRUE
        )
      ),
      # Show advanced options
      checkboxInput(
        "AdvancedOptionsLFQSN",
        "Show advanced options",
        value = FALSE
      )
    ),
    
    # TMT/PD options ----
    conditionalPanel(
      condition = "input.quant_method == 'TMT' & input.platform == 'PD'",
      # Standard options
      checkboxInput(
        "TMTPDuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      checkboxInput(
        "TMTPDrmPSM_withfewMea_withinRun",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      # Advanced options
      conditionalPanel(
        condition = "input.AdvancedOptionsTMTPD == true",
        radioButtons(
          "TMTPDwhich.proteinid",
          "Protein ID",
          choiceNames = c("Protein accessions", "Master protein accessions"),
          choiceValues = c("Protein.Accessions", "Master.Protein.Accessions")
        ),
        checkboxInput(
          "TMTPDuseNumProteinsColumn",
          "Remove peptides with more than one in \"number of proteins\" column",
          value = TRUE
        ),
        checkboxInput(
          "TMTPDrmProtein_with1Feature",
          "Remove proteins with only one peptide and charge",
          value = FALSE
        ),
        radioButtons(
          "TMTPDsummaryforMultipleRows",
          "Summary method for multiple rows",
          choiceNames = c("Sum", "Max"),
          choiceValues = c("sum", "max"),
          selected = "sum"
        )
      ),
      # Show advanced options
      checkboxInput(
        "AdvancedOptionsTMTPD",
        "Show advanced options",
        value = FALSE
      )
    ),

    # TMT/MQ options ----
    conditionalPanel(
      condition = "input.quant_method == 'TMT' & input.platform == 'MQ'",
      # Standard options
      checkboxInput(
        "LFQPDuseUniquePeptide",
        "Remove peptides assigned to more than one protein",
        value = TRUE
      ),
      checkboxInput(
        "rmPSM_withfewMea_withinRun",
        "Remove features with one or two measurements across runs",
        value = TRUE
      ),
      # Advanced options
      conditionalPanel(
        condition = "input.AdvancedOptionsTMTMQ == true",
        radioButtons(
          "TMTMQwhich.proteinid",
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
        ),
        checkboxInput(
          "rmProtein_with1Feature",
          "Remove proteins with only 1 peptide and charge",
          value = FALSE
        ),
        radioButtons(
          "summaryforMultipleRows",
          "Summary method for multiple rows",
          choiceNames = c("Sum", "Max"),
          choiceValues = c("sum", "max"),
          selected = "sum"
        )
      ),
      # Show advanced options
      checkboxInput(
        "AdvancedOptionsTMTMQ",
        "Show advanced options",
        value = FALSE
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

InputUI <- tabPanel(
  "Input",
  sidebarPanel(
    h4("Input files"),

    # Raw files input
    uiOutput("psm_input"),
    conditionalPanel(
      condition = "input.platform == 'MQ'",
      fileInput("proteinGroups", "MQ protein groups file",
        buttonLabel = "Browse",
        placeholder = "Upload proteinGroups.txt"
      ),
      checkboxInput("keep_contaminants", "Keep potential contaminants",
                    value = FALSE
      )
    ),
    hr(style = "border-top: 2px solid #000000;"),

    # LFQ annotations
    conditionalPanel(
      condition = "input.quant_method == 'LFQ'",
      fileInput("annotations", "Annotations file",
        buttonLabel = "Browse",
        placeholder = "Upload annotations"
      )
    ),

    # TMT annotations
    conditionalPanel(
      condition = "input.quant_method == 'TMT'",
      fileInput("channel_annotations", "Channel annotations file",
        buttonLabel = "Browse",
        placeholder = "Upload channel annotations"
      ),
      fileInput("run_annotations", "Run annotations file",
        buttonLabel = "Browse",
        placeholder = "Upload run annotations"
      )
    ),

    # Annotations wizard
    uiOutput("wizard_launch"),
    
    # Show annotation downloads if using the wizard
    conditionalPanel(
      condition = "input.doneButton > 0",
      # Annotations for LFQ
      conditionalPanel(
        condition = "input.quant_method == 'LFQ'",
        downloadButton("wizard_annotations_tsv", "Save annotations")
      ),
      # Annotations for TMT
      conditionalPanel(
        condition = "input.quant_method == 'TMT'",
        downloadButton("wizard_channels_annotations_tsv", "Save channel annotations"),
        downloadButton("wizard_runs_annotations_tsv", "Save run annotations")
      )
    ),
    
   
    
  ), # sidebar panel

  mainPanel(
    h3("Annotations"),
    withSpinner(dataTableOutput("annotation_tab")),
    hr(style = "border-top: 2px solid #000000;"),
    h3("PSMs"),
    withSpinner(dataTableOutput("PSMs_tab")),
    conditionalPanel(
      condition = "input.platform == 'MQ'",
      hr(style = "border-top: 2px solid #000000;"),
      h3("Protein groups"),
      withSpinner(dataTableOutput("proteinGroups_tab"))
    )
  )
)

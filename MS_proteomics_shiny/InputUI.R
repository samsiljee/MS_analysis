InputUI <- tabPanel(
  "Input",
  sidebarPanel(
    h4("Input files"),

    # Raw files input
    uiOutput("psm_input"),
    conditionalPanel(
      condition = "input.platform == 'MQ'",
      checkboxInput("keep_contaminants", "Keep potential contaminants",
        value = FALSE
      ),
      fileInput("proteinGroups", "MQ protein groups file",
        buttonLabel = "Browse",
        placeholder = "Upload proteinGroups.txt"
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
    uiOutput("wizard_launch")
    
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

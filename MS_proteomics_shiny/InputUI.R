InputUI <- tabPanel(
    "Input",
    sidebarPanel(
        h4("Input files"),     
        fileInput("annotations", "Annotations file",
                  buttonLabel = "Browse",
                  placeholder = "Upload annotations"),
        hr(style = "border-top: 2px solid #000000;"),
        uiOutput("psm_input"),
        conditionalPanel(
            condition = "input.platform == 'MQ'",
            checkboxInput("keep_contaminants", "Keep potential contaminants", 
                          value = FALSE),
            hr(style = "border-top: 2px solid #000000;"),
            fileInput("proteinGroups", "MQ protein groups file",
                      buttonLabel = "Browse",
                      placeholder = "Upload proteinGroups.txt"))),
    
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
            withSpinner(dataTableOutput("proteinGroups_tab"))))
)
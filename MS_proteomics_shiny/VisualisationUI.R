VisualisationUI <- tabPanel(
    "Visualisation",
    "This section will be for making the graphs. Again a sidebar panel to select the types of graphs.",
    sidebarPanel(
        selectInput("select_theme", "Select theme",
                    choices = c("B&W",
                                "Gray",
                                "Classic",
                                "Minimal",
                                "Void"),
                    multiple = FALSE),
        selectInput("plot_type", "Plot type",
                    choices = c("Volcano",
                                "PCA",
                                "Heatmap",
                                "GO enrichment",
                                "STRING network"),
                    multiple = FALSE),
        hr(style = "border-top: 2px solid #000000;"),
        conditionalPanel(condition = "input.plot_type == 'Volcano'",
                         uiOutput("select_comparison")),
        conditionalPanel(
            condition = "input.plot_type == 'Heatmap'",
            uiOutput("select_heatmap_filter")),
        conditionalPanel(
            condition = "input.plot_type == 'GO enrichment'",
            uiOutput("go_select_comparison"),
            uiOutput("go_select_direction"),
            uiOutput("go_select_ont"),
            numericInput("go_top_n", "Number to show",
                         value = 10,
                         step = 1)),
        conditionalPanel(
            condition = "input.plot_type == 'STRING network'",
            numericInput("STRING_n", "Nodes to plot",
                         value = 50,
                         step = 1)),
        actionButton("go_plot", "Plot/update!"),
        hr(style = "border-top: 2px solid #000000;"),
        numericInput("plot_width", "Plot width (mm)", value = 240, step = 10),
        numericInput("plot_height", "Plot height (mm)", value = 160, step = 10),
        numericInput("plot_dpi", "DPI", value = 600, step = 100),
        downloadButton("plot_download", "Download plot")
    ),
    
    mainPanel(
        withSpinner(plotOutput("plot"))
    )
    
)
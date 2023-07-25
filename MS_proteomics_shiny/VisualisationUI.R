VisualisationUI <- tabPanel(
    "Visualisation",
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
        #title
        conditionalPanel(
            condition = "input.plot_type == 'Volcano' ||
            input.plot_type == 'PCA' ||
            input.plot_type == 'Heatmap' ||
            input.plot_type == 'GO enrichment'",
            hr(style = "border-top: 2px solid #000000;"),
            uiOutput("plot_title_input")
        ),
        #X lab
        conditionalPanel(
            condition = "input.plot_type == 'Volcano' ||
            input.plot_type == 'PCA' ||
            input.plot_type == 'GO enrichment'",
            uiOutput("plot_x_lab_input")
        ),
        #Y lab
        conditionalPanel(
            condition = "input.plot_type == 'Volcano' ||
            input.plot_type == 'PCA' ||
            input.plot_type == 'Heatmap' ||
            input.plot_type == 'GO enrichment'",
            uiOutput("plot_y_lab_input")
        ),
        
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

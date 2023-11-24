QCUI <- tabPanel(
  "QC",
  "QC will go here",
  sidebarPanel(
    downloadButton("downloadReport", "Save methods summary"),
    hr(style = "border-top: 2px solid #000000;"),
    selectInput("select_theme_qc", "Select theme",
      choices = c(
        "B&W",
        "Gray",
        "Classic",
        "Minimal",
        "Void"
      ),
      multiple = FALSE
    ),
    selectInput("plot_type_qc", "Plot type",
      choices = c(
        "Abundance"
      ),
      multiple = FALSE
    ),
    # title
    uiOutput("plot_title_input_qc"),
    # X lab
    uiOutput("plot_x_lab_input_qc"),
    # Y lab
    uiOutput("plot_y_lab_input_qc"),
    # Plot saving options
    hr(style = "border-top: 2px solid #000000;"),
    numericInput("plot_width_qc", "Plot width (mm)", value = 240, step = 10),
    numericInput("plot_height_qc", "Plot height (mm)", value = 160, step = 10),
    numericInput("plot_dpi_qc", "DPI", value = 600, step = 100),
    downloadButton("plot_qc_download", "Download plot")
  ),
  mainPanel(
    withSpinner(plotOutput("plot_qc"))
  )
)

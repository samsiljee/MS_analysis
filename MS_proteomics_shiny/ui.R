# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Source files
source("InstructionsUI.R")
source("InputUI.R")
source("FormatUI.R")
source("ProcessUI.R")
source("ComparisonUI.R")
source("AnalysisUI.R")
source("VisualisationUI.R")
source("QCUI.R")

ui <- navbarPage(
  useShinyjs(), # Initialise shinyjs
  # Code to make the modal pop-up larger
  tags$head(
    tags$style(
      HTML("
      .modal-content {
        width: 180% !important;  /* Adjust the width */
        margin-left: -5%;  /* Adjust the negative margin */
        max-width: 120%;  /* Set a maximum width to prevent excessive width */
      }
    ")
    )
  ),
  
  title = "MS analysis",
  tabsetPanel(
    InstructionsUI,
    InputUI,
    FormatUI,
    ProcessUI,
    ComparisonUI,
    AnalysisUI,
    VisualisationUI,
    QCUI
  )
)

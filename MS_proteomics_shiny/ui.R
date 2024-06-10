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
source("CustomCSS.R")

ui <- navbarPage(
  useShinyjs(), # Initialise shinyjs
  
  # Use the custom CSS code
  Custom_CSS,
  
  # Code 
  
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

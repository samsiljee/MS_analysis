# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages
library(shinycssloaders)
library(DT)

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

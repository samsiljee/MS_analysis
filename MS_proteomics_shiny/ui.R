# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages
library(shinycssloaders)
library(DT)

# Source files
source("InstructionsUI.R")
source("Input.R")
source("Format.R")
source("Process.R")
source("Comparison.R")
source("Analysis.R")
source("Visualisation.R")
source("QC.R")

ui <- navbarPage(
  title = "MS analysis",

  tabsetPanel(InstructionsUI,
              Input,
              Format,
              Process,
              Comparison,
              Analysis,
              Visualisation,
              QC)
)

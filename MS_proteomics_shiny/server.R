# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

# Packages
library(shiny)
library(ComplexHeatmap)
library(vroom)
library(janitor)
library(clusterProfiler) # may be replaced with topGO, or a GO tool via API
library(STRINGdb)
library(DT)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(tibble)
library(rmarkdown)
library(tinytex)

# Setting option to increase allowed file size to 30MB, I may have to increase this further
options(shiny.maxRequestSize = 30 * 1024^3)

server <- function(input, output, session) {
  # Source files
  source("AnnotationWizardServer.R", local = TRUE)
  source("InputServer.R", local = TRUE)
  source("FormatServer.R", local = TRUE)
  source("ProcessServer.R", local = TRUE)
  source("ComparisonServer.R", local = TRUE)
  source("AnalysisServer.R", local = TRUE)
  source("VisualisationServer.R", local = TRUE)
  source("QCServer.R", local = TRUE)
  source("DownloadsServer.R", local = TRUE)

  # Load packages depending on input selected
  observeEvent(input$quant_method, {
    selected_quant <- input$quant_method
    switch(selected_quant,
      "LFQ" = library(MSstats),
      "TMT" = library(MSstatsTMT)
    )
  })

  observeEvent(input$species, {
    selected_species <- input$species
    switch(selected_species,
      "Human" = library(org.Hs.eg.db),
      "Rat" = library(org.Rn.eg.db)
    )
  })

  # Testing
  output$test_text <- renderPrint("test text")

  output$test_table <- renderTable(wizard_data())
}

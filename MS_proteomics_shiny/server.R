# Shiny app to run proteomics analysis in R
# Written by Sam Siljee - (c) 2023
# Created 04/04/2023

server <- function(input, output, session) {
  # Source files
  source("LFQAnnotationWizardServer.R", local = TRUE)
  source("TMTAnnotationWizardServer.R", local = TRUE)
  source("InputServer.R", local = TRUE)
  source("FormatServer.R", local = TRUE)
  source("ProcessServer.R", local = TRUE)
  source("ComparisonServer.R", local = TRUE)
  source("AnalysisServer.R", local = TRUE)
  source("VisualisationServer.R", local = TRUE)
  source("QCServer.R", local = TRUE)
  source("DownloadsServer.R", local = TRUE)
  
  # Source files for Wizard UI elements
  source("LFQAnnotationWizardUI.R", local = TRUE)
  source("TMTAnnotationWizardUI.R", local = TRUE)

  # Load packages depending on input selected
  observeEvent(input$quant_method, {
    selected_quant <- input$quant_method
    switch(selected_quant,
      "LFQ" = library(MSstats),
      "TMT" = library(MSstatsTMT)
    )
  })

  # Load/install the dataset for STRING analysis - species specific
  observeEvent(input$species, {
    selected_species <- input$species
    
    # Function to check and install Bioconductor annotation packages
    install_bioconductor_package <- function(package_name) {
      if (!requireNamespace(package_name, quietly = TRUE)) {
        if (!requireNamespace("BiocManager", quietly = TRUE)) {
          install.packages("BiocManager")
        }
        BiocManager::install(package_name)
      }
      library(package_name, character.only = TRUE)
    }
    
    # Switch based on selected species
    switch(selected_species,
           "Human" = install_bioconductor_package("org.Hs.eg.db"),
           "Rat" = install_bioconductor_package("org.Rn.eg.db")
    )
  })

  # Testing
  output$test_text <- renderPrint({
    c(class(MSstats_input()), colnames(annot_col()))
  })
}

# Launch wizard
observeEvent(input$launch_wizard, {
  if (nrow(raw()) != 0) { # only run wizard if the raw files are uploaded
    # Wizard "server"
    # Create variables
    
      # Unique runs from raw data
      wizard_runs <- unique(
          switch(input$platform,
                 PD = {
                     raw()$Run
                 },
                 MQ = {
                     raw()$Raw.file
                 }
          )
      )
    
    # Create a data frame with the unique runs
    wizard_runs_df <- data.frame(
        Run = wizard_runs,
        Fraction = ifelse(input$wizardNotFractionated, "1", NA)
        )
    
    # Render the data frame as a DataTable
    output$wizard_runs_table <- renderDT({
      datatable(wizard_runs_df, options = list(dom = 't', paging = FALSE, ordering = FALSE), 
                rownames = FALSE, colnames = c('Runs'), class = 'cell-border stripe')
    }) 
 
    # Event handler to change the page
    wizard_page <- reactiveVal(1)
    observeEvent(input$nextButtonConditions, {
      wizard_page(wizard_page() + 1)
    })
    observeEvent(input$nextButtonBioReplicates, {
        wizard_page(wizard_page() + 1)
    })
    observeEvent(input$nextButtonFractions, {
        wizard_page(wizard_page() + 1)
    })
    observeEvent(input$backButton, {
      wizard_page(wizard_page() - 1)
    })
    
    # Render the current wizard page
    observe({
        output$wizardPageContent <- renderUI ({
            switch(wizard_page(),
                   "1" = (conditions_wizard_ui()),
                   "2" = (bio_replicates_wizard_ui()),
                   "3" = (fractions_wizard_ui()),
                   "4" = (runs_wizard_ui())
                   )
        })
    })
    
    # Wizard "UI"
    # UI for the "Conditions" page
    conditions_wizard_ui <- function() {
      fluidPage(
        h2("Conditions"),
        # Text entry for conditions
        textAreaInput("wizardConditions", "Enter one condition per line:", ""),
        # Hide "back" button on first page, and "next buttons from other pages.
        shinyjs::hide("backButton"),
        shinyjs::hide("nextButtonBioReplicates"),
        shinyjs::hide("nextButtonFractions"),
        # Show conditions next button
        shinyjs::show("nextButtonConditions")
      )
    }
    
    # UI for the "BioReplicates" page
    bio_replicates_wizard_ui <- function() {
        fluidPage(
            h2("Biological replicates"),
            # Text entry for conditions
            textAreaInput("wizardBioReplicates", "Enter one biological replicate per line:", ""),
            # Hide "next" buttons from other pages.
            shinyjs::hide("nextButtonConditions"),
            shinyjs::hide("nextButtonFractions"),
            # Show "back" and bioreplicates "next" button
            shinyjs::show("nextButtonBioReplicates"),
            shinyjs::show("backButton")
        )
    }
    
    # UI for the "Fractions" page
    fractions_wizard_ui <- function() {
        fluidPage(
            h2("Fractions"),
            conditionalPanel(
                condition = "input.wizardFractionated == false",
                numericInput("wizardFractions", "Number of fractions:", value = 8, step = 1)
            ),
            checkboxInput("wizardNotFractionated", "Not fractionated", value = FALSE),
            # Hide "next" buttons from other pages
            shinyjs::hide("nextButtonConditions"),
            shinyjs::hide("nextButtonBioReplicates"),
            # Show next and back button on subsequent pages
            shinyjs::show("nextButtonFractions"),
            shinyjs::show("backButton")
        )
    }
    
    # UI for the "Runs" page
    runs_wizard_ui <- function() {
      fluidPage(
        h2("Runs"),
        DTOutput("wizard_runs_table"),
        # Hide "next" buttons from other pages
        shinyjs::hide("nextButtonConditions"),
        shinyjs::hide("nextButtonBioReplicates"),
        shinyjs::hide("nextButtonFractions"),
        # Show back button on subsequent pages
        shinyjs::show("backButton")
      )
    }
    
    # Launch wizard
    showModal(modalDialog(
      title = "Annotations wizard",
      uiOutput("wizardPageContent"),
      footer = tagList(
        actionButton("backButton", "Back"),
        actionButton("nextButtonConditions", "Next"),
        actionButton("nextButtonBioReplicates", "Next"),
        actionButton("nextButtonFractions", "Next"),
        modalButton("Dismiss")
      ),
      easyClose = FALSE
    ))
    
  } else { # "error" if no raw data uploaded yet
    showModal(modalDialog(
      title = "Annotations wizard",
      "Please upload PSM/evidence/proteinGroups files first",
      easyClose = TRUE
    ))
  }
  
})

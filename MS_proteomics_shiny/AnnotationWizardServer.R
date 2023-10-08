# Launch wizard
observeEvent(input$launch_wizard, {
  if (nrow(raw()) != 0) { # only run wizard if the raw files are uploaded
    # Wizard "server"
    # Create variables
    
    # Unique runs from raw data
    wizard_runs <- unique(raw()$Run)
    
    # Create a data frame with the unique runs
    wizard_runs_df <- data.frame(Runs = wizard_runs)
    
    # Render the data frame as a DataTable
    output$wizard_runs_table <- renderDT({
      datatable(wizard_runs_df, options = list(dom = 't', paging = FALSE, ordering = FALSE), 
                rownames = FALSE, colnames = c('Runs'), class = 'cell-border stripe')
    }) 
 
    # Event handler to change the page
    wizard_page <- reactiveVal(1)
    observeEvent(input$nextButton, {
      wizard_page(wizard_page() + 1)
    })
    observeEvent(input$backButton, {
      wizard_page(wizard_page() - 1)
    })
    
    # Render the current wizard page
    observe({
      if (wizard_page() == 1) {
        output$wizardPageContent <- renderUI({
          conditions_wizard_ui()
        })
      } else if (wizard_page() == 2) {
        output$wizardPageContent <- renderUI({
          runs_wizard_ui()
        })
      }
    })
    
    # Wizard "UI"
    # UI for the "Conditions" page
    conditions_wizard_ui <- function() {
      fluidPage(
        h2("Conditions"),
        # Hide "back" button on first page
        shinyjs::hide("backButton")
      )
    }
    
    # UI for the "Runs" page
    runs_wizard_ui <- function() {
      fluidPage(
        h2("Runs"),
        DTOutput("wizard_runs_table"),
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
        actionButton("nextButton", "Next"),
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

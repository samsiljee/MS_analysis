# Source script for UI elements

# Initialise data frame to store wizard data - initialised outside of the modal to make it globally available
wizard_data <- reactiveVal(NULL)

# Launch wizard
observeEvent(input$launch_wizard, {
  if (input$quant_method == "LFQ") { # Only run for LFQ experiments
    if (nrow(raw()) != 0) { # only run wizard if the raw files are loaded
      # Create variables
      # Vector of unique runs from raw data
      wizard_runs <- reactive({
        switch(input$platform,
          PD = {
            raw()$Run
          },
          MQ = {
            if (length(raw()$Raw.file) != 0) {
              raw()$Raw.file
            } else {
              raw()$`Raw file`
            }
          }
        ) %>%
          unique() %>%
          sort()
      })

      # Initialise blank columns for data frame
      wizard_conditions <- reactiveVal(rep(NA, length(wizard_runs())))
      wizard_bioreplicates <- reactiveVal(rep(NA, length(wizard_runs())))
      wizard_fractions <- reactiveVal(rep(NA, length(wizard_runs())))

      # Wizard "server" ----

      # Update wizard_data
      observe({
        wizard_data(
          data.frame(
            Run = wizard_runs(),
            Condition = wizard_conditions(),
            BioReplicate = wizard_bioreplicates(),
            Fraction = wizard_fractions()
          )
        )
      })

      # Render the data frame as a DataTable
      output$wizard_table <- DT::renderDataTable({
        datatable(
          wizard_data(),
          options = list(
            dom = "t",
            paging = FALSE,
            ordering = FALSE
          ),
          rownames = FALSE,
          editable = TRUE,
          class = "cell-border stripe",
          width = "100%"
        )
      })

      # Variable for selected rows
      selected_rows <- reactiveVal(NULL)
      observe({
        selected_rows(input$wizard_table_rows_selected)
      })

      # Handler to edit Condition
      observeEvent(input$LFQ_addCondition, {
        if (!is.null(selected_rows())) {
          current_wizard_conditions <- wizard_conditions()
          current_wizard_conditions[selected_rows()] <- input$wizardCondition
          wizard_conditions(current_wizard_conditions)
        } else {
          showNotification(
            "Please select one or more rows first",
            type = "error",
            duration = 5,
            closeButton = TRUE
          )
        }
      })

      # Handler to edit BioReplicate
      observeEvent(input$LFQ_addBioReplicate, {
        if (!is.null(selected_rows())) {
          current_wizard_bioreplicates <- wizard_bioreplicates()
          current_wizard_bioreplicates[selected_rows()] <- input$wizardBioReplicates
          wizard_bioreplicates(current_wizard_bioreplicates)
        } else {
          showNotification(
            "Please select one or more rows first",
            type = "error",
            duration = 5,
            closeButton = TRUE
          )
        }
      })

      # Handler to edit Fraction if adding fractions manually
      observeEvent(input$LFQ_addFraction, {
        if (!is.null(selected_rows())) {
          current_wizard_fractions <- wizard_fractions()
          current_wizard_fractions[selected_rows()] <- input$wizardFraction
          wizard_fractions(current_wizard_fractions)
        } else {
          # Handle the case when no rows are selected
          showNotification(
            "Please select one or more rows first",
            type = "error",
            duration = 5,
            closeButton = TRUE
          )
        }
      })

      # Handler to edit Fraction if not fractionated
      observeEvent(input$wizardFractionated, {
        if (input$wizardFractionated) {
          wizard_fractions(1)
        } else {
          wizard_fractions(rep(NA, length(wizard_runs())))
        }
      })

      # Event handler to change the page
      wizard_page <- reactiveVal(1)
      observeEvent(input$nextButton, {
        wizard_page(wizard_page() + 1)
      })
      observeEvent(input$backButton, {
        wizard_page(wizard_page() - 1)
      })

      # Event handler to close the modal with the "Done" button
      observeEvent(input$doneButton, {
        removeModal()
      })

      # Render the current wizard page
      observe({
        output$wizardPageContent <- renderUI({
          switch(wizard_page(),
            "1" = (conditions_wizard_ui()),
            "2" = (bio_replicates_wizard_ui()),
            "3" = (fractions_wizard_ui())
          )
        })
      })

      # Launch wizard
      showModal(modalDialog(
        title = "Annotations wizard",
        uiOutput("wizardPageContent"),
        footer = tagList(
          actionButton("backButton", "Back"),
          actionButton("nextButton", "Next"),
          actionButton("doneButton", "Done"),
          actionButton("dismissButton","Cancel")
        ),
        size = "xl",
        easyClose = FALSE
      ))
      
      # Event handler to set wizard_data to NULL when the "Dismiss" button is clicked
      observeEvent(input$dismissButton, {
        wizard_data(NULL)
        removeModal()
      })
      
    } else { # "error" if no raw data uploaded yet
      showNotification(
        "Please upload PSM/evidence/proteinGroups files first",
        type = "error",
        duration = 5,
        closeButton = TRUE
      )
    } # Close warning if no raw data is uploaded
  } # Close catch to run only if method quant method is LFQ
}) # Close launch wizard

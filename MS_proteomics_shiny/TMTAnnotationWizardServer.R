# Initialise data frames to store wizard data - initialised outside of the modal to make it globally available
wizard_runs_data <- reactiveVal(NULL)
wizard_channels_data <- reactiveVal(NULL)

# Launch wizard
observeEvent(input$launch_wizard, {
  if (input$quant_method == "TMT") { # Only run for TMT experiments
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

      # Initialise blank columns for runs data frame
      wizard_runs_mixtures <- reactiveVal(rep(NA, length(wizard_runs())))
      wizard_fractions <- reactiveVal(rep(NA, length(wizard_runs())))
      wizard_techrepmixtures <- reactiveVal(rep(NA, length(wizard_runs())))
      
      # Initialise blank columns for channels data frame
      wizard_channels_mixtures <- reactiveVal(rep(NA, length(wizard_runs())))
      wizard_channels <- reactiveVal(rep(NA, length(wizard_runs())))
      wizard_conditions <- reactiveVal(rep(NA, length(wizard_runs())))
      wizard_bioreplicates <- reactiveVal(rep(NA, length(wizard_runs())))

      # Wizard "server"----

      # Update wizard_runs_data
      observe({
        wizard_runs_data(
          data.frame(
            Run = wizard_runs(),
            Mixture = wizard_runs_mixtures(),
            Fraction = wizard_fractions(),
            TechRepMixture = wizard_techrepmixtures()
          )
        )
      })
      
      # Update wizard_channels_data
      observe({
        wizard_channels_data(
          data.frame(
            Mixture = wizard_channels_mixtures(),
            Channel = wizard_channels(),
            Condition = wizard_conditions(),
            BioReplicate = wizard_bioreplicates()
          )
        )
      })

      # Render the run level data frame as a DataTable
      output$wizard_runs_table <- DT::renderDataTable({
        datatable(
          wizard_runs_data(),
          options = list(
            dom = "t",
            paging = FALSE,
            ordering = FALSE
          ),
          rownames = FALSE,
          editable = TRUE,
          class = "cell-border stripe"
        )
      })
      
      # Render the channel level data frame as a DataTable
      output$wizard_channels_table <- DT::renderDataTable({
        datatable(
          wizard_channels_data(),
          options = list(
            dom = "t",
            paging = FALSE,
            ordering = FALSE
          ),
          rownames = FALSE,
          editable = TRUE,
          class = "cell-border stripe"
        )
      })

      # Variable for selected rows, for runs and channels
      runs_selected_rows <- reactiveVal(NULL)
      observe({
        runs_selected_rows(input$wizard_runs_table_rows_selected)
      })
      channels_selected_rows <- reactiveVal(NULL)
      observe({
        channels_selected_rows(input$wizard_channels_table_rows_selected)
      })
      
      # Handler to edit Run mixtures
      observeEvent(input$addMixture, {
        if (!is.null(runs_selected_rows())) {
          current_wizard_runs_mixtures <- wizard_runs_mixtures()
          current_wizard_runs_mixtures[runs_selected_rows()] <- input$wizardMixture
          wizard_runs_mixtures(current_wizard_runs_mixtures)
        } else {
          showNotification(
            "Please select one or more rows first",
            type = "error",
            duration = NULL,
            closeButton = TRUE
          )
        }
      })

      # Handler to edit Fraction if adding fractions manually
      observeEvent(input$addFraction, {
        if (!is.null(runs_selected_rows())) {
          current_wizard_fractions <- wizard_fractions()
          current_wizard_fractions[runs_selected_rows()] <- input$wizardFraction
          wizard_fractions(current_wizard_fractions)
        } else {
          # Handle the case when no rows are selected
          showNotification(
            "Please select one or more rows first",
            type = "error",
            duration = NULL,
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
      
      # Handler to edit TechRepMixture if adding replicates manually
      observeEvent(input$addReplicate, {
        if (!is.null(runs_selected_rows())) {
          current_wizard_techrepmixtures <- wizard_techrepmixtures()
          current_wizard_techrepmixtures[runs_selected_rows()] <- input$wizardTechRepMixture
          wizard_techrepmixtures(current_wizard_techrepmixtures)
        } else {
          # Handle the case when no rows are selected
          showNotification(
            "Please select one or more rows first",
            type = "error",
            duration = NULL,
            closeButton = TRUE
          )
        }
      })
      
      # Handler to edit TechRepMixture if not replicated
      observeEvent(input$wizardReplicated, {
        if (input$wizardReplicated) {
          wizard_techrepmixtures(1)
        } else {
          wizard_techrepmixtures(rep(NA, length(wizard_runs())))
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
            "1" = (wizard_mixtures_ui()),
            "2" = (wizard_fractions_ui()),
            "3" = (wizard_techrepmixtures_ui()),
            "4" = (wizard_channels_ui()),
            "5" = (wizard_conditions_ui()),
            "6" = (wizard_bioreplicates_ui()),
          )
        })
      })

      # Wizard "UI"----
      # UI for the "Runs - Mixtures" page
      wizard_mixtures_ui <- function() {
        fluidPage(
          h2("Runs: Mixtures"),
          # Text entry for conditions
          "Select one or more rows, and enter mixture below:",
          DT::dataTableOutput("wizard_runs_table"),
          numericInput("wizardMixture", "", value = 1),
          actionButton("addMixture", "Add mixture"),

          # Hide buttons
          shinyjs::hide("backButton"),
          shinyjs::hide("doneButton"),
          # Show buttons
          shinyjs::show("nextButton")
        )
      }
      
      # UI for the "Runs - Fractions" page
      wizard_fractions_ui <- function() {
        fluidPage(
          h2("Runs: Fractions"),
          "Select one or more rows and enter fraction, or select \"Not fractionated\"",
          DT::dataTableOutput("wizard_runs_table"),
          checkboxInput("wizardFractionated", "Not fractionated", value = TRUE),
          conditionalPanel(
            condition = "input.wizardFractionated == false",
            # Numeric entry for fraction
            numericInput("wizardFraction", "", value = 1),
            actionButton("addFraction", "Add fraction"),
          ),
          
          # Hide buttons
          shinyjs::hide("doneButton"),
          # Show buttons
          shinyjs::show("nextButton"),
          shinyjs::show("backButton")
        )
      }
      
      # UI for the "Runs - TechRepMixture" page
      wizard_techrepmixtures_ui <- function() {
        fluidPage(
          h2("Runs: Mixture Technical Replicates"),
          "Select one or more rows and enter mixture technical replicate, or select \"No technical replicates\"",
          DT::dataTableOutput("wizard_runs_table"),
          checkboxInput("wizardReplicated", "No technical replicates", value = TRUE),
          conditionalPanel(
            condition = "input.wizardReplicated == false",
            # Numeric entry for fraction
            numericInput("wizardTechRepMixture", "", value = 1),
            actionButton("addReplicate", "Add replicate"),
          ),
          
          # Hide buttons
          shinyjs::hide("doneButton"),
          # Show buttons
          shinyjs::show("nextButton"),
          shinyjs::show("backButton")
        )
      }
      
      # UI for the "Conditions" page
      conditions_wizard_ui <- function() {
        fluidPage(
          h2("Conditions"),
          # Text entry for conditions
          "Select one or more rows, and enter condition below:",
          DT::dataTableOutput("wizard_table"),
          textInput("wizardCondition", "", ""),
          actionButton("addCondition", "Add condition"),
          
          # Hide "back" button on first page, and "next buttons from other pages.
          shinyjs::hide("backButton"),
          shinyjs::hide("nextButtonBioReplicates"),
          
          shinyjs::hide("doneWizard"),
          # Show conditions next button
          shinyjs::show("nextButtonConditions")
        )
      }

      # UI for the "BioReplicates" page
      bio_replicates_wizard_ui <- function() {
        fluidPage(
          h2("Biological replicates"),
          # Text entry for biological replicates
          "Select one or more rows, and enter biological replicate below:",
          DT::dataTableOutput("wizard_table"),
          textInput("wizardBioReplicates", "", ""),
          actionButton("addBioReplicate", "Add biological replicate"),

          # Hide "next" buttons from other pages.
          shinyjs::hide("nextButtonConditions"),
          
          shinyjs::hide("doneWizard"),
          # Show "back" and bioreplicates "next" button
          shinyjs::show("nextButtonBioReplicates"),
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
          actionButton("doneButton", "Done"),
          modalButton("Dismiss")
        ),
        size = "m",
        easyClose = FALSE
      ))
    } else { # "error" if no raw data uploaded yet
      showNotification(
        "Please upload PSM/evidence/proteinGroups files first",
        type = "error",
        duration = NULL,
        closeButton = TRUE
      )
    }
  }
}) # Close launch wizard

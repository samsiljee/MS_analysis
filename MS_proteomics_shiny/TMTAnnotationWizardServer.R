# Variables ----
# list of TMT Plexes
TMT_vector <- c(
  "126",
  paste0(rep(127:134, each = 2), c("N", "C")),
  "135N"
)
TMT_Plexes <- list(
  TMTPro_18 = TMT_vector,
  TMTPro_16 = TMT_vector[1:16],
  TMT_11 = TMT_vector[1:11],
  TMT_10 = c(TMT_vector[1:9], "131"),
  TMT_6 = as.character(126:131),
  TMT_2 = c("126", "127")
)

# Reactive variables
# Initialise blank data frames
TMT_wizard_channels_data <- reactiveVal(NULL)
TMT_wizard_runs_data <- reactiveVal(NULL)

# Launch wizard
observeEvent(input$launch_wizard, {
  if (input$quant_method == "TMT") { # Only run for TMT experiments
    if (nrow(raw()) != 0) { # only run wizard if the raw files are loaded

      # Channels level server ----

      # Variable for selected channels rows
      channels_selected_rows <- reactiveVal(integer(0))
      observe({
        channels_selected_rows(input$TMT_wizard_channels_table_rows_selected)
      })

      # Update channels
      TMT_wizard_channels <- reactive({
        if (input$TMT_wizardCustomPlex) {
          input$TMT_wizardCustomChannels %>%
            str_split(pattern = "\n") %>%
            unlist()
        } else {
          TMT_Plexes[[input$TMT_wizardPlexSelected]]
        }
      })

      # Read in mixtures
      TMT_wizard_channels_mixtures <- reactive({
        1:input$TMT_wizard_channels_mixtures
      })

      # Create the first two columns of the channels table
      first_two_columns <- reactive({
        expand.grid(Channel = TMT_wizard_channels(), Mixture = TMT_wizard_channels_mixtures())
      })

      # Initialise blank columns
      TMT_wizard_conditions <- reactiveVal(NA)
      TMT_wizard_bioreplicates <- reactiveVal(NA)

      # Update blank columns to correct number of rows
      observeEvent(c(
        input$TMT_wizardPlexSelected,
        input$TMT_wizardCustomChannels,
        input$TMT_wizard_channels_mixtures
      ), {
        TMT_wizard_conditions({
          rep(NA, nrow(first_two_columns()))
        })
        TMT_wizard_bioreplicates({
          rep(NA, nrow(first_two_columns()))
        })
      })

      # Handler to edit conditions
      observeEvent(input$addCondition, {
        if (!is.null(channels_selected_rows())) {
          current_TMT_wizard_conditions <- TMT_wizard_conditions()
          current_TMT_wizard_conditions[channels_selected_rows()] <- input$TMT_wizardCondition
          TMT_wizard_conditions(current_TMT_wizard_conditions)
        } else {
          showNotification(
            "Please select one or more rows first",
            type = "error",
            duration = 5,
            closeButton = TRUE
          )
        }
      })

      # Handler to edit BioRepilicates
      observeEvent(input$addBioReplicate, {
        if (!is.null(channels_selected_rows())) {
          current_TMT_wizard_bioreplicates <- TMT_wizard_bioreplicates()
          current_TMT_wizard_bioreplicates[channels_selected_rows()] <- input$TMT_wizardBioReplicate
          TMT_wizard_bioreplicates(current_TMT_wizard_bioreplicates)
        } else {
          showNotification(
            "Please select one or more rows first",
            type = "error",
            duration = 5,
            closeButton = TRUE
          )
        }
      })

      # Handler to set normalisation channels, for both Condition and BioReplicate
      observeEvent(input$setNorm, {
        if (!is.null(channels_selected_rows())) {
          current_TMT_wizard_conditions <- TMT_wizard_conditions()
          current_TMT_wizard_conditions[channels_selected_rows()] <- "Norm"
          TMT_wizard_conditions(current_TMT_wizard_conditions)
          current_TMT_wizard_bioreplicates <- TMT_wizard_bioreplicates()
          current_TMT_wizard_bioreplicates[channels_selected_rows()] <- "Norm"
          TMT_wizard_bioreplicates(current_TMT_wizard_bioreplicates)
        } else {
          showNotification(
            "Please select one or more rows first",
            type = "error",
            duration = 5,
            closeButton = TRUE
          )
        }
      })
      
      # Reactive expression for intermittent channels data
      interim_TMT_channel_data <- reactive({
        data.frame(
          first_two_columns(),
          Condition = TMT_wizard_conditions(),
          BioReplicate = TMT_wizard_bioreplicates()
        )
      })
      
      # Update wizard_channels_data
      observeEvent(input$doneButton, {
        TMT_wizard_channels_data(interim_TMT_channel_data())
      })
      
      # Render the channel level data frame as a DataTable
      output$TMT_wizard_channels_table <- DT::renderDataTable({
        datatable(
          interim_TMT_channel_data(),
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

      # Runs level server ----
      # Reactive variables for the runs

      # Variable for selected rows, for runs and channels
      runs_selected_rows <- reactiveVal(NULL)
      observe({
        runs_selected_rows(input$TMT_wizard_runs_table_rows_selected)
      })

      # Vector of unique runs from raw data
      TMT_wizard_runs_runs <- reactive({
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
      TMT_wizard_runs_mixtures <- reactiveVal(NA)
      TMT_wizard_fractions <- reactiveVal(NA)
      TMT_wizard_techrepmixtures <- reactiveVal(NA)

      # Update based on length of number of runs
      observe({
        TMT_wizard_runs_mixtures(rep(NA, length(TMT_wizard_runs_runs())))
        TMT_wizard_fractions(rep(NA, length(TMT_wizard_runs_runs())))
        TMT_wizard_techrepmixtures(rep(NA, length(TMT_wizard_runs_runs())))
      })

      # Set mixture to 1 if only one mixture
      observe({
        req(input$TMT_wizard_channels_mixtures)
        if (input$TMT_wizard_channels_mixtures == 1) {
          TMT_wizard_runs_mixtures(rep(1, length(TMT_wizard_runs_runs())))
        } else {
          TMT_wizard_runs_mixtures(rep(NA, length(TMT_wizard_runs_runs())))
        }
      })

      # Reactive UI for runs mixtures
      output$addRunsMixture <- renderUI({
        selectInput(
          "TMT_wizardRunMixture",
          "Add mixture",
          choices = TMT_wizard_channels_mixtures()
        )
      })

      # Handler to add mixture
      observeEvent(input$addMixture, {
        if (!is.null(runs_selected_rows())) {
          current_TMT_wizard_runs_mixtures <- TMT_wizard_runs_mixtures()
          current_TMT_wizard_runs_mixtures[runs_selected_rows()] <- input$TMT_wizardRunMixture
          TMT_wizard_runs_mixtures(current_TMT_wizard_runs_mixtures)
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

      # Handler to edit Fraction if adding fractions manually
      observeEvent(input$addFraction, {
        if (!is.null(runs_selected_rows())) {
          current_TMT_wizard_fractions <- TMT_wizard_fractions()
          current_TMT_wizard_fractions[runs_selected_rows()] <- input$TMT_wizardFraction
          TMT_wizard_fractions(current_TMT_wizard_fractions)
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
      observeEvent(input$TMT_wizardFractionated, {
        if (input$TMT_wizardFractionated) {
          TMT_wizard_fractions(1)
        } else {
          TMT_wizard_fractions(rep(NA, length(TMT_wizard_runs_runs())))
        }
      })

      # Handler to edit TechRepMixture if adding replicates manually
      observeEvent(input$addReplicate, {
        if (!is.null(runs_selected_rows())) {
          current_TMT_wizard_techrepmixtures <- TMT_wizard_techrepmixtures()
          current_TMT_wizard_techrepmixtures[runs_selected_rows()] <- input$TMT_wizardTechRepMixture
          TMT_wizard_techrepmixtures(current_TMT_wizard_techrepmixtures)
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

      # Handler to edit TechRepMixture if not replicated
      observeEvent(input$TMT_wizardReplicated, {
        if (input$TMT_wizardReplicated) {
          TMT_wizard_techrepmixtures(1)
        } else {
          TMT_wizard_techrepmixtures(rep(NA, length(TMT_wizard_runs_runs())))
        }
      })

      # Update TMT_wizard_runs_data
      observe({
        TMT_wizard_runs_data(
          data.frame(
            Run = TMT_wizard_runs_runs(),
            Mixture = TMT_wizard_runs_mixtures(),
            Fraction = TMT_wizard_fractions(),
            TechRepMixture = TMT_wizard_techrepmixtures()
          )
        )
      })

      # Render the run level data frame as a DataTable
      output$TMT_wizard_runs_table <- DT::renderDataTable({
        datatable(
          TMT_wizard_runs_data(),
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

      # Modal functions ----


      # Initialise variable for TMT wizard page
      TMT_wizard_page <- reactiveVal(1)

      # Event handler to change to the next page - with skipping
      observeEvent(input$nextButton, {
        req(TMT_wizard_page())

        if (input$TMT_wizard_channels_mixtures == 1 && TMT_wizard_page() == 3) { # skip mixtures page if only one replicate
          TMT_wizard_page(TMT_wizard_page() + 2)
        } else {
          TMT_wizard_page(TMT_wizard_page() + 1)
        }
      })

      # Event handler to change to the previous page - with skipping
      observeEvent(input$backButton, {
        req(TMT_wizard_page())

        if (input$TMT_wizard_channels_mixtures == 1 && TMT_wizard_page() == 5) { # skip mixtures page if only one replicate
          TMT_wizard_page(TMT_wizard_page() - 2)
        } else {
          TMT_wizard_page(TMT_wizard_page() - 1)
        }
      })

      # Event handler to close the modal with the "Done" button
      observeEvent(input$doneButton, {
        removeModal()
      })

      # Render the current wizard page
      observe({
        output$TMT_wizardPageContent <- renderUI({
          switch(TMT_wizard_page(),
            "1" = (TMT_wizard_channels_ui()),
            "2" = (TMT_wizard_conditions_ui()),
            "3" = (TMT_wizard_bioreplicates_ui()),
            "4" = (TMT_wizard_mixtures_ui()),
            "5" = (TMT_wizard_fractions_ui()),
            "6" = (TMT_wizard_techrepmixtures_ui())
          )
        })
      })

      # Show the wizard
      showModal(modalDialog(
        title = "Annotations wizard",
        uiOutput("TMT_wizardPageContent"),
        footer = tagList(
          actionButton("backButton", "Back"),
          actionButton("nextButton", "Next"),
          actionButton("doneButton", "Done"),
          actionButton("dismissButton","Cancel")
        ),
        size = "m",
        easyClose = FALSE
      ))
      
      # Event handler to set wizard_data to NULL when the "Dismiss" button is clicked
      observeEvent(input$dismissButton, {
        TMT_wizard_channels_data(NULL)
        TMT_wizard_runs_data(NULL)
        removeModal()
      })
      
    } else { # "error" if no raw data uploaded yet
      showNotification(
        "Please upload PSM/evidence/proteinGroups files first",
        type = "error",
        duration = 5,
        closeButton = TRUE
      )
    } # Close error pop-up if no raw data is uploaded
  } # Close catch to only run for TMT epxeriments
}) # Close launch wizard

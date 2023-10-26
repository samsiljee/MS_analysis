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
      wizard_fractions <- reactiveVal(NA)
      wizard_techrepmixtures <- reactiveVal(NA)

      # Initialise blank columns for channels data frame
      wizard_channels_mixtures <- reactiveVal(NA)
      wizard_channels <- reactiveVal(NA)
      wizard_conditions <- reactiveVal(NA)
      wizard_bioreplicates <- reactiveVal(NA)

      # Vector of custom channels
      wizard_custom_channels <- reactiveVal(NA)

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

      # Runs handlers ----

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

      # Channels handlers ----

      # Update mixtures for the channels table
      observeEvent(input$wizardPlexSelected, {
        mixtures_list <- rep(
          unique(wizard_runs_mixtures()),
          each = if (input$wizardCustomPlex) {
            length(wizard_custom_channels())
            # length(channels_temp_test())
          } else {
            length(TMT_Plexes[[input$wizardPlexSelected]])
          }
        )
        wizard_channels_mixtures(mixtures_list)
      })

      # Update custom channels vector
      observeEvent(input$addCustomChannels, {
        wizard_custom_channels(input$wizardCustomChannels %>%
          str_split(pattern = "\n") %>%
          unlist())
      })

      # Run test
      channels_temp_test <- reactive({
        channels <- if (input$wizardCustomPlex) {
          input$wizardCustomChannels %>%
            str_split(pattern = "\n") %>%
            unlist()
        } else {
          TMT_Plexes[[input$wizardPlexSelected]]
        }
        return(rep(channels, wizard_runs_mixtures() %>%
          unique() %>%
          length()))
      })

      output$channels_test <- renderPrint(channels_temp_test())
      
      # # Update channels
      # observeEvent(input$addCustomChannels | input$wizardPlexSelected, {
      #   # Update mixtures
      #   mixtures_list <- rep(
      #     unique(wizard_runs_mixtures()),
      #     each = if (input$wizardCustomPlex) {
      #       length(wizard_custom_channels())
      #       # length(channels_temp_test())
      #     } else {
      #       length(TMT_Plexes[[input$wizardPlexSelected]])
      #     }
      #   )
      #   wizard_channels_mixtures(mixtures_list)
      #   # Update channels
      #   wizard_channels(channels_temp_test())
      # })

      # Update channels for the channels table - custom channels
      observeEvent(input$addCustomChannels, {
        channels_list <- rep(wizard_custom_channels(), wizard_runs_mixtures() %>%
          unique() %>%
          length())
        wizard_channels(channels_list)
      })

      # Update channels for the channels table - standard channels
      observeEvent(input$wizardPlexSelected, {
        channels_list <- rep(TMT_Plexes[[input$wizardPlexSelected]], wizard_runs_mixtures() %>%
          unique() %>%
          length())
        wizard_channels(channels_list)
      })

      # Handler to edit Channel Conditions
      observeEvent(input$addCondition, {
        if (!is.null(channels_selected_rows())) {
          current_wizard_conditions <- wizard_conditions()
          current_wizard_conditions[channels_selected_rows()] <- input$wizardCondition
          wizard_conditions(current_wizard_conditions)
        } else {
          showNotification(
            "Please select one or more rows first",
            type = "error",
            duration = NULL,
            closeButton = TRUE
          )
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

      # Run the modal ----

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

      # Launch wizard
      showModal(modalDialog(
        title = "Annotations wizard",
        uiOutput("wizardPageContent"),
        footer = tagList(
          actionButton("backButton", "Back"),
          actionButton("nextButton", "Next"),
          downloadButton("wizard_runs_annotations_tsv", "Save run annotations"),
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
    } # Close error pop-up if no raw data is uploaded
  } # Close catch to only run for TMT epxeriments
}) # Close launch wizard

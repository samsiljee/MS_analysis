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

# Channels level server ----
# Reactive variables
# Initialise blank channels data frame
TMT_wizard_channels_data <- reactiveVal(NULL)

# Variable for selected channels rows
channels_selected_rows <- reactiveVal(integer(0))
observe({
  channels_selected_rows(input$TMT_wizard_channels_table_rows_selected)
})

# Read in channels
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
observeEvent(input$TMT_wizardPlexSelected, {
      TMT_wizard_conditions({
        rep(NA, nrow(first_two_columns()))
      })
      TMT_wizard_bioreplicates({
        rep(NA, nrow(first_two_columns()))
      })
    })
observeEvent(input$TMT_wizardCustomChannels, {
  TMT_wizard_conditions({
    rep(NA, nrow(first_two_columns()))
  })
  TMT_wizard_bioreplicates({
    rep(NA, nrow(first_two_columns()))
  })
})
observeEvent(input$TMT_wizard_channels_mixtures, {
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
      duration = NULL,
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
      duration = NULL,
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
      duration = NULL,
      closeButton = TRUE
    )
  }
})

# Update wizard_channels_data
TMT_wizard_channels_data <- reactive({
  data.frame(
    first_two_columns(),
    Condition = TMT_wizard_conditions(),
    BioReplicate = TMT_wizard_bioreplicates()
  )
})

# Render the channel level data frame as a DataTable
output$TMT_wizard_channels_table <- DT::renderDataTable({
  datatable(
    TMT_wizard_channels_data(),
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

# Runs level server ----
# Reactive variables for the runs
# Initialise data frame for run level wizard data
TMT_wizard_runs_data <- reactiveVal(NULL)

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
      duration = NULL,
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
      duration = NULL,
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
      Runs = TMT_wizard_runs_runs(),
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
    class = "cell-border stripe"
  )
})

# Modal functions ----
# Launch wizard
observeEvent(input$launch_wizard, {
  if (input$quant_method == "TMT") { # Only run for TMT experiments
    if (nrow(raw()) != 0) { # only run wizard if the raw files are loaded

      # Event handler to change the page
      TMT_wizard_page <- reactiveVal(1)
      observeEvent(input$nextButton, {
        TMT_wizard_page(TMT_wizard_page() + 1)
      })
      observeEvent(input$backButton, {
        TMT_wizard_page(TMT_wizard_page() - 1)
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

      
      # Launch wizard
      showModal(modalDialog(
        title = "Annotations wizard",
        uiOutput("TMT_wizardPageContent"),
        footer = tagList(
          actionButton("backButton", "Back"),
          actionButton("nextButton", "Next"),
          downloadButton("wizard_channels_annotations_tsv", "Save channel annotations"),
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

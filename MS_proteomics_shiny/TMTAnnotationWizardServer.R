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
wizard_channels_data <- reactiveVal(NULL)
# Initialise blank columns
wizard_conditions <- reactiveVal(NA)
wizard_bioreplicates <- reactiveVal(NA)

# Variable for selected channels rows
channels_selected_rows <- reactiveVal(integer(0))
observe({
  channels_selected_rows(input$wizard_channels_table_rows_selected)
})

# Read in channels
wizard_channels <- reactive({
  if (input$wizardCustomPlex) {
    input$wizardCustomChannels %>%
      str_split(pattern = "\n") %>%
      unlist()
  } else {
    TMT_Plexes[[input$wizardPlexSelected]]
  }
})

# Read in mixtures
wizard_channels_mixtures <- reactive({
  1:input$wizard_channels_mixtures
})

# Create the first two columns of the channels table
first_two_columns <- reactive({
  channels <- wizard_channels()
  mixtures <- wizard_channels_mixtures()

  expand.grid(Channel = channels, Mixture = mixtures)
})

# Handler to edit conditions using eventReactive
wizard_conditions_updated <- eventReactive(input$addCondition, {
  selected_rows <- channels_selected_rows()
  if (!is.null(selected_rows)) {
    current_wizard_conditions <- wizard_conditions()
    input_condition <- input$wizardCondition
    
    # Update conditions for selected rows with the input value
    current_wizard_conditions[selected_rows] <- input_condition
    
    # Return the updated conditions
    return(rep(input_condition, length(current_wizard_conditions)))
  } else {
    showNotification(
      "Please select one or more rows first",
      type = "error",
      duration = NULL,
      closeButton = TRUE
    )
    # Return the current conditions if no rows are selected
    return(wizard_conditions())
  }
})

# Update wizard_conditions using the eventReactive expression
observe({
  wizard_conditions(wizard_conditions_updated())
})

observe({
  print(channels_selected_rows())
})

# Update wizard_channels_data
wizard_channels_data <- reactive({
  data.frame(
    first_two_columns(),
    Condition = wizard_conditions(),
    BioReplicate = wizard_bioreplicates()
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

# Runs level server ----
# Reactive variables for the runs
# Initialise data frame for run level wizard data
wizard_runs_data <- reactiveVal(NULL)
# Initialise blank columns for runs data frame
wizard_runs_mixtures <- reactiveVal(NA)
wizard_fractions <- reactiveVal(NA)
wizard_techrepmixtures <- reactiveVal(NA)

# Variable for selected rows, for runs and channels
runs_selected_rows <- reactiveVal(NULL)
observe({
  runs_selected_rows(input$wizard_runs_table_rows_selected)
})

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

# Update wizard_runs_data
observe({
  wizard_runs_data(
    data.frame(
      Fraction = wizard_fractions(),
      TechRepMixture = wizard_techrepmixtures()
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

# Modal functions ----
# Launch wizard
observeEvent(input$launch_wizard, {
  if (input$quant_method == "TMT") { # Only run for TMT experiments
    if (nrow(raw()) != 0) { # only run wizard if the raw files are loaded

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
            "1" = (wizard_channels_ui()),
            "2" = (wizard_conditions_ui()),
            "3" = (wizard_bioreplicates_ui()),
            "4" = (wizard_mixtures_ui()),
            "5" = (wizard_fractions_ui()),
            "6" = (wizard_techrepmixtures_ui())
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

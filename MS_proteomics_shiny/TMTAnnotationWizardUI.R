# TMT annotation wizard UI elements

# Runs ----
wizard_mixtures_ui <- function() {
  fluidPage(
    h2("Runs: Mixtures"),
    "Select one or more rows, and enter mixture,  or select \"One mixture\"",
    DT::dataTableOutput("wizard_runs_table"),
    checkboxInput("wizardOneMixture", "One mixture", value = TRUE),
    conditionalPanel(
      condition = "input.wizardOneMixture == false",
      # Numeric entry for Mixtures
      numericInput("wizardMixture", "", value = 1),
      actionButton("addMixture", "Add mixture"),
    ),

    # Hide buttons
    shinyjs::hide("backButton"),
    shinyjs::hide("doneButton"),
    shinyjs::hide("wizard_runs_annotations_tsv"),
    # Show buttons
    shinyjs::show("nextButton")
  )
}

wizard_fractions_ui <- function() {
  fluidPage(
    h2("Runs: Fractions"),
    "Select one or more rows and enter fraction, or select \"One fraction\"",
    DT::dataTableOutput("wizard_runs_table"),
    checkboxInput("wizardFractionated", "One fraction", value = TRUE),
    conditionalPanel(
      condition = "input.wizardFractionated == false",
      # Numeric entry for fraction
      numericInput("wizardFraction", "", value = 1),
      actionButton("addFraction", "Add fraction"),
    ),

    # Hide buttons
    shinyjs::hide("doneButton"),
    shinyjs::hide("wizard_runs_annotations_tsv"),
    # Show buttons
    shinyjs::show("nextButton"),
    shinyjs::show("backButton")
  )
}

wizard_techrepmixtures_ui <- function() {
  fluidPage(
    h2("Runs: Mixture Technical Replicates"),
    "Select one or more rows and enter mixture technical replicate, or select \"One mixture replicate\"",
    DT::dataTableOutput("wizard_runs_table"),
    checkboxInput("wizardReplicated", "One mixture replicate", value = TRUE),
    conditionalPanel(
      condition = "input.wizardReplicated == false",
      # Numeric entry for fraction
      numericInput("wizardTechRepMixture", "", value = 1),
      actionButton("addReplicate", "Add replicate"),
    ),

    # Hide buttons
    shinyjs::hide("doneButton"),
    # Show buttons
    shinyjs::show("wizard_runs_annotations_tsv"),
    shinyjs::show("nextButton"),
    shinyjs::show("backButton")
  )
}

# Channels ----

wizard_channels_ui <- function() {
  fluidPage(
    h2("Channels: Channels"),
    "Select TMT plex, or enter custom channels",
    conditionalPanel(
      condition = "input.wizardCustomPlex == false",
      selectInput("wizardPlexSelected",
        "",
        c(
          "TMTPro 18-plex" = "TMTPro_18",
          "TMTPro 16-plex" = "TMTPro_16",
          "TMT 11-plex" = "TMT_11",
          "TMT 10-plex" = "TMT_10",
          "TMT 6-plex" = "TMT_6",
          "TMT 2-plex" = "TMT_2"
        ),
        multiple = FALSE
      )
    ),
    checkboxInput("wizardCustomPlex", "Custom channels", value = FALSE),
    conditionalPanel(
      condition = "input.wizardCustomPlex == true",
      "Please enter all channels, each on a new line:",
      textAreaInput("wizardCustomChannels", "", ""),
      actionButton("addCustomChannels", "Add custom channels")
    ),
    verbatimTextOutput("channels_test"),
    DT::dataTableOutput("wizard_channels_table"),
    # Hide buttons
    shinyjs::hide("doneButton"),
    shinyjs::hide("wizard_runs_annotations_tsv"),
    # Show buttons
    shinyjs::show("nextButton"),
    shinyjs::show("backButton")
  )
}

wizard_conditions_ui <- function() {
  fluidPage(
    h2("Conditions"),
    # Text entry for conditions
    "Select one or more rows, and enter condition below:",
    DT::dataTableOutput("wizard_channels_table"),
    textInput("wizardCondition", "", ""),
    actionButton("addCondition", "Add condition"),

    # Hide buttons
    shinyjs::hide("wizard_runs_annotations_tsv"),
    shinyjs::hide("doneWizard"),

    # Show buttons
    shinyjs::show("backButton"),
    shinyjs::show("nextButtonConditions")
  )
}

wizard_bioreplicates_ui <- function() {
  fluidPage(
    h2("Biological replicates"),
    # Text entry for biological replicates
    "Select one or more rows, and enter biological replicate below:",
    DT::dataTableOutput("wizard_channels_table"),
    textInput("wizardBioReplicate", "", ""),
    actionButton("addBioReplicate", "Add biological replicate"),

    # Hide "next" buttons from other pages.
    shinyjs::hide("nextButtonConditions"),
    shinyjs::hide("doneWizard"),
    # Show "back" and bioreplicates "next" button
    shinyjs::show("nextButtonBioReplicates"),
    shinyjs::show("backButton")
  )
}

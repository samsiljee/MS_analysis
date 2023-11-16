# TMT annotation wizard UI elements

# Channels ----

TMT_wizard_channels_ui <- function() {
  fluidPage(
    h2("Channels: Channels/mixtures"),
    "Select number of mixtures, and TMT plex, or enter custom channels",
    numericInput(
      "TMT_wizard_channels_mixtures",
      "Select number of mixtures",
      value = 1,
      step = 1),
    conditionalPanel(
      condition = "input.TMT_wizardCustomPlex == false",
      selectInput("TMT_wizardPlexSelected",
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
    checkboxInput("TMT_wizardCustomPlex", "Custom channels", value = FALSE),
    conditionalPanel(
      condition = "input.TMT_wizardCustomPlex == true",
      "Please enter all channels, each on a new line:",
      textAreaInput("TMT_wizardCustomChannels", "", "")
    ),
    DT::dataTableOutput("TMT_wizard_channels_table"),
    # Hide buttons
    shinyjs::hide("doneButton"),
    shinyjs::hide("backButton"),
    # Show buttons
    shinyjs::show("nextButton")
  )
}

TMT_wizard_conditions_ui <- function() {
  fluidPage(
    h2("Conditions"),
    # Text entry for conditions
    "Select one or more rows, and enter condition below:",
    DT::dataTableOutput("TMT_wizard_channels_table"),
    textInput("TMT_wizardCondition", "", ""),
    fluidRow(
      column(3, actionButton("addCondition", "Add condition")),
      column(3, actionButton("setNorm", "Set as normalisation"))),

    # Hide buttons
    shinyjs::hide("doneWizard"),
    # Show buttons
    shinyjs::show("backButton"),
    shinyjs::show("nextButton")
  )
}

TMT_wizard_bioreplicates_ui <- function() {
  fluidPage(
    h2("Biological replicates"),
    # Text entry for biological replicates
    "Select one or more rows, and enter biological replicate below:",
    DT::dataTableOutput("TMT_wizard_channels_table"),
    textInput("TMT_wizardBioReplicate", "", ""),
    fluidRow(
      column(4, actionButton("addBioReplicate", "Add biological replicate")),
      column(3, actionButton("setNorm", "Set as normalisation"))),

    # Hide "next" buttons from other pages.
    shinyjs::hide("doneWizard"),
    # Show "back" and bioreplicates "next" button
    shinyjs::show("nextButton"),
    shinyjs::show("backButton")
  )
}

# Runs ----
TMT_wizard_mixtures_ui <- function() {
  fluidPage(
    h2("Runs: Mixtures"),
    "Select one or more rows, and enter mixture",
    DT::dataTableOutput("TMT_wizard_runs_table"),
    uiOutput("addRunsMixture"),
    actionButton("addMixture", "Add mixture"),

    # Hide buttons
    shinyjs::hide("doneButton"),
    # Show buttons
    shinyjs::show("backButton"),
    shinyjs::show("nextButton")
  )
}

TMT_wizard_fractions_ui <- function() {
  fluidPage(
    h2("Runs: Fractions"),
    "Select one or more rows and enter fraction, or select \"One fraction\"",
    DT::dataTableOutput("TMT_wizard_runs_table"),
    checkboxInput("TMT_wizardFractionated", "One fraction", value = TRUE),
    conditionalPanel(
      condition = "input.TMT_wizardFractionated == false",
      # Numeric entry for fraction
      numericInput("TMT_wizardFraction", "", value = 1),
      actionButton("addFraction", "Add fraction"),
    ),

    # Hide buttons
    shinyjs::hide("doneButton"),
    # Show buttons
    shinyjs::show("nextButton"),
    shinyjs::show("backButton")
  )
}

TMT_wizard_techrepmixtures_ui <- function() {
  fluidPage(
    h2("Runs: Mixture Technical Replicates"),
    "Select one or more rows and enter mixture technical replicate, or select \"One mixture replicate\"",
    DT::dataTableOutput("TMT_wizard_runs_table"),
    checkboxInput("TMT_wizardReplicated", "One mixture replicate", value = TRUE),
    conditionalPanel(
      condition = "input.TMT_wizardReplicated == false",
      # Numeric entry for fraction
      numericInput("TMT_wizardTechRepMixture", "", value = 1),
      actionButton("addReplicate", "Add replicate"),
    ),

    # Hide buttons
    shinyjs::hide("nextButton"),
    # Show buttons
    shinyjs::show("doneButton"),
    shinyjs::show("backButton")
  )
}

# TMT annotation wizard UI elements

# Runs ----
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
        shinyjs::hide("wizard_runs_annotations_tsv"),
        # Show buttons
        shinyjs::show("nextButton")
    )
}

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
        shinyjs::hide("wizard_runs_annotations_tsv"),
        # Show buttons
        shinyjs::show("nextButton"),
        shinyjs::show("backButton")
    )
}

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

wizard_bioreplicates_ui <- function() {
    fluidPage(
        h2("Biological replicates"),
        # Text entry for biological replicates
        "Select one or more rows, and enter biological replicate below:",
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

# Wizard UI pages

# UI for the "Conditions" page
conditions_wizard_ui <- function() {
    fluidPage(
        h2("Conditions"),
        # Text entry for conditions
        "Select one or more rows, and enter condition below:",
        DT::dataTableOutput("wizard_table"),
        textInput("wizardCondition", "", ""),
        actionButton("LFQ_addCondition", "Add condition"),
        
        # Hide back, download, and done buttons
        shinyjs::hide("backButton"),
        shinyjs::hide("doneButton"),
        # Show next button
        shinyjs::show("nextButton")
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
        actionButton("LFQ_addBioReplicate", "Add biological replicate"),
        
        # Hide done and download buttons
        shinyjs::hide("doneButton"),
        # Show next and back buttons
        shinyjs::show("nextButton"),
        shinyjs::show("backButton")
    )
}

# UI for the "Fractions" page
fractions_wizard_ui <- function() {
    fluidPage(
        h2("Fractions"),
        "Select one or more rows and enter fraction, or select \"Not fractionated\"",
        DT::dataTableOutput("wizard_table"),
        checkboxInput("wizardFractionated", "Not fractionated", value = TRUE),
        conditionalPanel(
            condition = "input.wizardFractionated == false",
            # Numeric entry for fraction
            numericInput("wizardFraction", "", value = 1),
            actionButton("LFQ_addFraction", "Add fraction"),
        ),
        
        # Hide "next" button
        shinyjs::hide("nextButton"),
        # Show other buttons
        shinyjs::show("doneButton"),
        shinyjs::show("backButton")
    )
}
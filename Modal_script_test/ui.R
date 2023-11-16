library(shiny)

# Define UI for application that draws a histogram
fluidPage(
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
    ),
    numericInput("mixtures", "Mixtures", 2),
    textInput("wizardCondition", "", ""),
    actionButton("addCondition", "Add condition"),
    DT::dataTableOutput("table")
)

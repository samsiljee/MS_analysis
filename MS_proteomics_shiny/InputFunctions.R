# Script to make functions for input options that adds an explanation questionmark next to the box
# Sam Siljee
# 10th June 2024

# Select input with help
selectInputHelp <- function(..., helpText) {
  div(
    
    selectInput(...),
    tags$i(
      class = "glyphicon glyphicon-question-sign",
      title = helpText,
    )
  )
}

# Checkbox input with help
checkboxInputHelp <- function(..., helpText) {
  div(
    class = "checkbox-help-container",
    checkboxInput(...),
    tags$i(class = "glyphicon glyphicon-question-sign help-icon", title = helpText)
  )
}

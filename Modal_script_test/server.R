library(shiny)

# Source the UI and server scripts
source("ModalUI.R")
source("ModalServer.R")

function(input, output, session) {
    observeEvent(input$launch_wizard, {
        showModal(modalDialog(
            title = "Annotations wizard",
            size = "m",
            easyClose = FALSE,
            modalUI(),                     # Call the modalUI function to get the UI components
            modalServer(input, output, session)    # Call the modalServer function with input, output, and session objects
        ))
    })
}

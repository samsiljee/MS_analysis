library(shiny)

ui = fluidPage(
    mainPanel(
        actionButton("createfile", "Create"),
        actionButton("deletefile", "Delete")
    )
)

# Define server logic required to draw a histogram
server = function(session, input, output) {
    
    observeEvent(input$createfile, {
        showModal(modalDialog(
            tagList(
                textInput("newfilename", label = "Filename", placeholder = "my_file.txt")
            ), 
            title="Create a file",
            footer = tagList(actionButton("confirmCreate", "Create"),
                             modalButton("Cancel")
            )
        ))
    })
    
    
    observeEvent(input$deletefile, {
        showModal(modalDialog(
            tagList(
                selectInput("deletefilename", label = "Delete a file", choices = list.files(pattern="*.txt"))
            ), 
            title="Delete a file",
            footer = tagList(actionButton("confirmDelete", "Delete"),
                             modalButton("Cancel")
            )
        ))
    })
    
    observeEvent(input$confirmCreate, {
        req(input$newfilename)
        file.create(input$newfilename)
        removeModal()
    })
    
    observeEvent(input$confirmDelete, {
        req(input$deletefilename)
        file.remove(input$deletefilename)
        removeModal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

modalServer <- function(input, output, session) {
    
    testing_text <- reactiveVal("not checked")
    
    observe({
        req(input$test)  # Ensure input$test is not NULL
        
        testing_text_val <- ifelse(input$test, "checked", "not checked")
        testing_text(testing_text_val)
    })
    
    output$testing_text <- renderText({
        testing_text()
    })
}

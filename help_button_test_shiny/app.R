library(shiny)

ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/css/bootstrap.min.css"),
        tags$script(src = "https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"),
        tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js")
    ),
    fluidRow(
        column(4,
               actionButton("btn1", "Button 1"),
               tags$i(class = "glyphicon glyphicon-question-sign", 
                      title = "This is additional information for Button 1", 
                      "data-toggle" = "tooltip")
        ),
        column(4,
               actionButton("btn2", "Button 2"),
               tags$i(class = "glyphicon glyphicon-question-sign", 
                      title = "This is additional information for Button 2", 
                      "data-toggle" = "tooltip")
        )
    )
)

server <- function(input, output, session) {
    # Enable Bootstrap tooltips
    observe({
        shinyjs::runjs("
      $('[data-toggle=\"tooltip\"]').tooltip();
    ")
    })
}

shinyApp(ui, server)

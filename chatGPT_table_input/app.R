library(shiny)
library(dplyr)

ui <- fluidPage(
  fileInput("file", "Choose a file"),
  numericInput("input1", "Input 1", value = 1),
  numericInput("input2", "Input 2", value = 1),
  actionButton("add_row", "Add Row"),
  tableOutput("table")
)

server <- function(input, output) {
  
  # Reactive value to store the number of columns
  num_cols <- reactive({
    if (!is.null(input$file)) {
      dat <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      nlevels(factor(dat$Condition))
    }
  })
  
  # Generate the blank dataframe based on the number of columns
  blank_df <- reactive({
    data.frame(matrix(nrow = 0, ncol = num_cols())) 
  })
  
  # Add new rows to the dataframe when the button is clicked
  observeEvent(input$add_row, {
    new_row <- data.frame(Input1 = input$input1, Input2 = input$input2)
    blank_df(bind_rows(blank_df(), new_row))
  })
  
  # Render the table with the dataframe
  output$table <- renderTable({
    blank_df()
  })
}

shinyApp(ui, server)
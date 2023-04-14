library(shiny)
library(DT)
library(tidyverse)

ui <- fluidPage(
  
  # Application title
  titlePanel("Editable Dataframe and Plot"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      DTOutput("my_datatable"),
      actionButton("go",label = "Plot Data")
    ),
    
    # Show plot
    mainPanel(
      plotOutput("my_plot")
    )
  )
)

server <- function(input, output) {
  
  #initialize a blank dataframe
  v <- reactiveValues(data = { 
    data.frame(x = numeric(0),y = numeric(0)) %>% 
      add_row(x = rep(0,10),y = rep(0,10))
  })
  
  #output the datatable based on the dataframe (and make it editable)
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE)
  })
  
  #when there is any edit to a cell, write that edit to the initial dataframe
  #check to make sure it's positive, if not convert
  observeEvent(input$my_datatable_cell_edit, {
    #get values
    info = input$my_datatable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    v$data[i,j] <- k
  })
  
  #render plot
  output$my_plot <- renderPlot({
    req(input$go) #require the input button to be non-0 (ie: don't load the plot when the app first loads)
    isolate(v$data) %>%  #don't react to any changes in the data
      ggplot(aes(x,y)) +
      geom_point() +
      geom_smooth(method = "lm")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
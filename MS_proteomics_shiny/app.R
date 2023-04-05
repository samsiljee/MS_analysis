# Shiny app to run proteomics analysis in R
# Created by Sam Siljee - (c) 2023
# Created 04/04/2023

# Pacakges ----
library(shiny)
library(markdown)

# UI ----

# Display the UI, with tabs for each section

ui <- navbarPage("MS proteomics analysis",
  tabPanel("Welcome",
           p("Welcome to my proteomics analysis pipeline."),
           p("Please move sequentially through the tabs to complete the analysis.")
           ),              
  tabPanel("Input",
           p("Please upload your dataset as exported from Proteome discoverer and the corresponding annotations table here:"),
           fileInput(inputId = "PSMs",
                     label = "PSMs file",
                     buttonLabel = "Browse",
                     placeholder = "Upload PSMs file here"),
           fileInput(inputId = "annotations",
                     label = "Annotations file",
                     buttonLabel = "Browse",
                     placeholder = "Upload annotations file here")
           ),
  tabPanel("MSstats",
           verbatimTextOutput("summary")
           ),
  navbarMenu("More",
             tabPanel("Table",
                      DT::dataTableOutput("table")
                      ),
             tabPanel("About",
                      fluidRow(
                          column(3,
                                 img(class="img-polaroid",
                                     src=paste0("http://upload.wikimedia.org/",
                                                "wikipedia/commons/9/92/",
                                                "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                 tags$small(
                                     "Source: Photographed at the Bay State Antique ",
                                     "Automobile Club's July 10, 2005 show at the ",
                                     "Endicott Estate in Dedham, MA by ",
                                     a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                       "User:Sfoskett")
                                     )
                                 )
                          )
                      )
             )
)
# Server ----
# Define server logic required to draw a histogram


server <- function(input, output, session) {
    output$plot <- renderPlot({
        plot(cars, type=input$plotType)
    })
    
    output$summary <- renderPrint({
        summary(cars)
    })
    
    output$table <- DT::renderDataTable({
        DT::datatable(cars)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(DT)

function(input, output, session) {
    
    # Constant variables
    TMT_vector <- c(
        "126",
        paste0(rep(127:134, each = 2), c("N", "C")),
        "135N"
    )
    TMT_Plexes <- list(
        TMTPro_18 = TMT_vector,
        TMTPro_16 = TMT_vector[1:16],
        TMT_11 = TMT_vector[1:11],
        TMT_10 = c(TMT_vector[1:9], "131"),
        TMT_6 = as.character(126:131),
        TMT_2 = c("126", "127")
    )
    
    # Reactive variables
    TMT <- reactive({
        TMT_Plexes[[input$wizardPlexSelected]]
    })
        
    channels <- reactive({
        1:input$channels
    })
    
    conditions <- reactiveVal({
        NA
    })
    
    TMT_rep <- reactive({
        rep(TMT(), length(channels()))
    })
    
    channels_rep <- reactive({
        rep(channels(), each = length(TMT()))
    })
    
    output$table <- DT::renderDataTable({
        datatable(
            data.frame(Mixture = channels_rep(), Channel = TMT_rep(), Condition = conditions()),
            options = list(
                dom = "t",
                paging = FALSE,
                ordering = FALSE
            ),
            rownames = FALSE,
            editable = TRUE,
            class = "cell-border stripe"
        )
    })
    
    channels_selected_rows <- reactiveVal(NULL)
    observe({
        channels_selected_rows(input$wizard_channels_table_rows_selected)
    })
    
    observeEvent(input$addCondition, {
        if (!is.null(channels_selected_rows())) {
            current_wizard_conditions <- wizard_conditions()
            current_wizard_conditions[channels_selected_rows()] <- input$wizardCondition
            wizard_conditions(current_wizard_conditions)
        } else {
            showNotification(
                "Please select one or more rows first",
                type = "error",
                duration = NULL,
                closeButton = TRUE
            )
        }
    })
    
    output$text_1 <- renderPrint({
        rep(TMT_Plexes[[input$wizardPlexSelected]], input$channels)
    })
    
    output$text_2 <- renderPrint({
        rep(1:input$channels, each = length(TMT_Plexes[[input$wizardPlexSelected]]))
    })
}
